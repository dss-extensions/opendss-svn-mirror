unit Diakoptics;

interface

uses
    Circuit,
    Solution,
    DSSGlobals,
    SysUtils,
    DSSClassDefs,
    {$IFDEF FPC}
CmdForms
    {$ELSE}
    DSSForms,
    ScriptEdit
    {$ENDIF}
    ;

function Solve_Diakoptics(): Integer;
function ADiakoptics_Tearing(): Integer;
procedure ADiakopticsInit();
function Calc_C_Matrix(PLinks: PString; NLinks: Integer): Integer;
function Calc_ZLL(PLinks: PString; NLinks: Integer): Integer;
procedure Calc_ZCC(Links: Integer);
procedure Calc_Y4();

implementation

uses
    ExecHelper,
    Executive,
    ParserDel,
    YMatrix,
    KLUSolve,
    Ucomplex,
    Sparse_Math,
    UcMatrix;

function Solve_Diakoptics(): Integer;
begin
  {Space left empty to implement the simplified Diakoptics algorithm}
    with ActiveCircuit[1].Solution do
    begin

    end;
    Result := 0;
end;

{*******************************************************************************
*              Inverts ZCC to obtain its admittance equivalent Y4              *
*                      This is the heart of ADiakoptics                        *
*******************************************************************************}
procedure Calc_Y4();
var
    col,
    idx: Integer;
    TempMat: TcMatrix;
// 4 Debugging
    myFile: TextFile;
    Text: String;

begin
    with ActiveCircuit[1], ActiveCircuit[1].Solution do
    begin
    //  Moves ZCC into an equivalent compatible with TcMatrix
        TempMat := TCmatrix.CreateMatrix(ZCC.NRows);
        for idx := 0 to High(ZCC.CData) do
        begin
            TempMat.SetElement(ZCC.CData[idx].Row + 1, ZCC.CData[idx].Col + 1, ZCC.CData[idx].Value);
        end;
    //  Inverts the ZCC equivalent
        TempMat.Invert;
        Y4.sparse_matrix_Cmplx(ZCC.NRows, ZCC.NCols);
    // Moves the inverse into Y4 for furhter use
        for idx := 0 to (ZCC.NRows - 1) do
        begin
            for col := 0 to ZCC.NCols - 1 do
                Y4.insert(idx, col, TempMat.GetElement(idx + 1, col + 1));
        end;
{
//********************Dbug************************************
    AssignFile(myFile, 'C:\Temp\Y4Mat.csv');
    ReWrite(myFile);
    Text        :=  '';
    for idx := 0 to (length(Y4.CData)- 1) do
    Begin
        Text  :=  inttostr(Y4.CData[idx].Row) + ',' + inttostr(Y4.CData[idx].Col) +
        ',' + floattostr(Y4.CData[idx].Value.re);
        if Y4.CData[idx].Value.im < 0 then
          Text  :=  Text  + '-i' +  floattostr(-1*Y4.CData[idx].Value.im)
        else
          Text  :=  Text  + '+i' +  floattostr(Y4.CData[idx].Value.im);
        WriteLn(myFile,Text);
    End;
    CloseFile(myFile);
}
    end;
end;

{*******************************************************************************
*              Calculates the Connections matrix ZCC in the                    *
*                      contours-contours domain                                *
*******************************************************************************}
procedure Calc_ZCC(Links: Integer);
var
    row,
    col,
    idx3,
    idx2,
    idx: Integer;
    NumNodes: Longword;
    CVector,
    ZVector: pComplexArray;
    Ctemp: Complex;
// 4 Debugging
    myFile: TextFile;
    Text: String;

begin
    with ActiveCircuit[1], ActiveCircuit[1].Solution do
    begin
        GetSize(hY, @NumNodes);
        col := NumNodes;
        dec(Links);
        ZCT.sparse_matrix_Cmplx(Links * 3, NumNodes);
        CVector := allocmem(NumNodes * 2);                    // Real and imag parts
        ZVector := allocmem(NumNodes * 2);
        idx3 := Links * 3 - 1;

        for idx2 := 0 to idx3 do
        begin

            for idx := 1 to NumNodes do
                CVector^[idx] := cZERO;  // Makes it zero

            for idx := 1 to length(Contours.CData) do
            begin
                if Contours.CData[idx - 1].col = idx2 then
                begin
                    row := Contours.CData[idx - 1].row + 1;
                    CVector^[row] := Contours.CData[idx - 1].Value;
                end;
            end;
            SolveSparseSet(hy, @ZVector^[1], @CVector^[1]);

            for idx := 1 to col do           // inserts result into the ZCT matrix
            begin
                CTemp := ZVector^[idx];
                if (CTemp.re <> 0) and (CTemp.im <> 0) then
                    ZCT.insert(idx2, (idx - 1), ZVector[idx]);
            end;

        end;

        ZCC := ZCT.multiply(Contours);    // Calculates ZCC with no Link impedances
        ZCC := ZCC.Add(ZLL);              // Adds the link impedance
{
//********************Dbug************************************
    AssignFile(myFile, 'C:\Temp\ZCCMat.csv');
    ReWrite(myFile);
    Text        :=  '';
    for idx2 := 0 to (length(ZCC.CData)- 1) do
    Begin
        Text  :=  inttostr(ZCC.CData[idx2].Row) + ',' + inttostr(ZCC.CData[idx2].Col) +
        ',' + floattostr(ZCC.CData[idx2].Value.re);
        if ZCC.CData[idx2].Value.im < 0 then
          Text  :=  Text  + '-i' +  floattostr(-1*ZCC.CData[idx2].Value.im)
        else
          Text  :=  Text  + '+i' +  floattostr(ZCC.CData[idx2].Value.im);
        WriteLn(myFile,Text);
    End;
    CloseFile(myFile);
}

    end;
end;

{*******************************************************************************
*                   Calculates the contours matrix based                       *
*             on the location in the graph of the link branches                *
*             if there is an error returns <> 0                                *
*******************************************************************************}
function Calc_C_Matrix(PLinks: PString; NLinks: Integer): Integer;
var
    LIdx, k, l,
    j, CDirection,
    i: Integer;
    Elem_Buses,
    Node_Names: array of String;
    temp: String;
    Go_Flag: Boolean;

    myFile: TextFile;         // For debugging
begin
    ActiveActor := 1;
    with ActiveCircuit[ActiveActor] do
    begin

        setlength(Elem_Buses, 2);
        setlength(Node_Names, 0);
        for i := 1 to NumNodes do
        begin
            setlength(Node_Names, (length(Node_Names) + 1));
            with MapNodeToBus^[i] do
                Node_Names[High(Node_names)] := Format('%s.%-d', [lowercase(BusList.Get(Busref)), NodeNum]);
        end;

        Contours.sparse_matrix_Cmplx(length(Node_Names), (NLinks - 1) * 3);

        for LIdx := 1 to (NLinks - 1) do
        begin

            inc(PLinks);                  // Pointing to the Next link branch (starting in 1)

            i := SetElementActive(String(PLinks^));
      // Gest the names of the buses fot this PDElement
            for i := 1 to ActiveCktElement.Nterms do
            begin
                Elem_Buses[i - 1] := ActiveCktElement.GetBus(i);
                j := ansipos('.', Elem_Buses[i - 1]);
                if j <> 0 then
                    Elem_Buses[i - 1] := copy(Elem_Buses[i - 1], 0, j);
            end;
      //  Marks the connection point in the contours matrix
            for l := 1 to ActiveCktElement.NPhases do
            begin

                for i := 0 to 1 do
                begin

                    temp := Elem_Buses[i] + inttostr(l);
                    Go_Flag := true;
                    j := 0;
                    while Go_Flag and (j <= High(Node_Names)) do
                    begin

                        k := ansipos(temp, Node_Names[j]);
                        if k <> 0 then
                        begin
                            if i = 0 then
                                CDirection := 1
                            else
                                CDirection := -1;
                            Contours.insert(j, (l - 1), cmplx(CDirection, 0));
                            Go_Flag := false;
                        end;
                        inc(j);

                    end;

                end;

            end;

        end;
        if Contours.NZero <> 0 then
            Result := 0
        else
            Result := 1;
    end;
end;

{*******************************************************************************
*            Calculates the Link brnahes matrix for further use                *
*                if there is an error returns <> 0                             *
*******************************************************************************}
function Calc_ZLL(PLinks: PString; NLinks: Integer): Integer;
var
    NValues,
    idx, k, j,
    row, col,
    count,
    i: Integer;
    cValues: pComplexArray;
    ErrorFlag: Boolean;
    localMat: TSparse_Complex;
    LinkPrim: TcMatrix;
begin
    dec(NLinks);
    ErrorFlag := false;
    LinkPrim := TCmatrix.CreateMatrix(3);
    ActiveActor := 1;
    with ActiveCircuit[ActiveActor] do
    begin
        ZLL.Sparse_matrix_Cmplx(NLinks * 3, NLinks * 3);
        for i := 1 to NLinks do
        begin
            inc(PLinks);
            idx := SetElementActive(String(PLinks^));

            if ActiveCktElement <> nil then
                with ActiveCktElement do
                begin
                    NValues := SQR(Yorder);
                    cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
                    if cValues <> nil then
                    begin
                        k := 1;
                        idx := (i - 1) * 3;
                        row := 1;
                        col := 1;
                        count := 0;
            // Extracts the YPrim of the Link branch
                        for j := 1 to (NValues div 4) do
                        begin
                            LinkPrim.SetElement(row, col, cValues^[k]);
                            inc(count);
                            if count > 2 then
                            begin
                                inc(row);
                                Col := 1;
                                Count := 0;
                                k := k + 4;
                            end
                            else
                            begin
                                inc(Col);
                                inc(k);
                            end;
                        end;
            // Inverts the Y primitive
                        LinkPrim.Invert;
            // Inserts the Z primitive values into ZLL
                        row := 0;
                        col := 0;
                        count := 0;
                        for j := 1 to (NValues div 4) do
                        begin
                            ZLL.insert((row + idx), (col + idx), LinkPrim.GetElement(row + 1, col + 1));
                            inc(count);
                            if count > 2 then
                            begin
                                inc(row);
                                Col := 0;
                                Count := 0;
                            end
                            else
                                inc(Col);
                        end;
                    end
                    else
                        ErrorFlag := true;

                end

        end;
        if ErrorFlag then
            Result := 1
        else
            Result := 0;

    end;
end;


{*******************************************************************************
*           Tears the system using considering the number of                   *
*           circuits specified by the user                                     *
*******************************************************************************}
function ADiakoptics_Tearing(): Integer;
var
    Prev_Mode,                              // Stores the previous solution mode
    Num_Ckts: Integer;                  // Stores the number of Sub-Circuits created
begin
    with ActiveCircuit[ActiveActor].Solution do
    begin
        Num_Ckts := ActiveCircuit[ActiveActor].Tear_Circuit();
        Prev_mode := Dynavars.SolutionMode;
        Dynavars.SolutionMode := 0;          // Shapshot mode
        solve(ActiveActor);
        ActiveCircuit[ActiveActor].Save_SubCircuits();
        Dynavars.SolutionMode := Prev_mode;  // Goes back to the previous solution mode
        ActiveCircuit[1].Num_SubCkts := Num_Ckts;
        GlobalResult := 'Sub-Circuits Created: ' + inttostr(Num_Ckts);
        Result := 0;          // No error handling here
    end;
end;

{*******************************************************************************
*            Generates the subsystems, actors and memory space                 *
*                     For using the A-Diakoptics parallelism                   *
*******************************************************************************}
procedure ADiakopticsInit();
var
    Local_State,
    Num_States,
    ErrorCode,
    DIdx,
    Diak_Actors: Integer;
    Dir, Proj_Dir,
    prog_Str,
    ErrorStr,
    FileRoot: String;
    Links: array of String;                        // List of the Link Branches
    MQuit: Boolean;                                // To quit the State Machine
    {$IFNDEF FPC}
    ScriptEd: TScriptEdit;
    {$ENDIF}

begin
// The program is built as a state machine to facilitate the error detection
// and quitting the routines after an error is detected wihtout killing the prog
    MQuit := false;
    Num_States := 7;                          // Number of states of the machine
    Local_State := 0;                          // Current state
    prog_str := 'A-Diakoptics initialization sumary:' + CRLF + CRLF;
    ActiveActor := 1;
  // Checks if the number of actors is within a reasonable limit
    if ActiveCircuit[1].Num_SubCkts > (CPU_Cores - 2) then
        ActiveCircuit[1].Num_SubCkts := CPU_Cores - 2;

    while (not MQuit) do
    begin
        case Local_State of
            0:
            begin                       // Create subcircuits
                prog_Str := prog_str + '- Creating SubCircuits...' + CRLF;

                ErrorCode := ADiakoptics_Tearing();
                prog_Str := prog_str + '  ' + inttostr(ActiveCircuit[1].Num_SubCkts) + ' Sub-Circuits Created' + CRLF;

            end;
            1:
            begin                      // Saves the Link Branch list locally
                Diak_Actors := ActiveCircuit[1].Num_SubCkts + 1;
                prog_Str := prog_str + '- Indexing link branches...';

                setlength(Links, length(ActiveCircuit[1].Link_Branches));
                for DIdx := 0 to High(Links) do
                    Links[DIdx] := ActiveCircuit[1].Link_Branches[DIdx];

                prog_Str := prog_str + 'Done';
                ErrorCode := 0;          // No error handling here

            end;
            2:
            begin                      // Compile subsystems
                prog_Str := prog_str + CRLF + '- Setting up the Actors...';
        // Clears everything to craete the actors and compile the subsystems
                DSSExecutive.ClearAll;
                Fileroot := GetCurrentDir;    //  Gets the current directory
                SolutionAbort := false;
                DssExecutive.Command := 'ClearAll';

        // Compiles the interconnected Circuit for further calculations on actor 1
                ActiveActor := 1;
                Proj_Dir := 'compile "' + Fileroot + '\Torn_Circuit\master_interconnected.dss"';
                DssExecutive.Command := Proj_Dir;

        // Creates the other actors
                for DIdx := 2 to Diak_Actors do
                begin
                    inc(NumOfActors);
                    ActiveActor := NumOfActors;
                    ActorCPU[ActiveActor] := ActiveActor - 1;
                    DSSExecutive := TExecutive.Create;  // Make a DSS object
                    Parser[ActiveActor] := TParser.Create;
                    AuxParser[ActiveActor] := TParser.Create;
                    DSSExecutive.CreateDefaultDSSItems;
                    Parallel_enabled := false;

                    if DIdx = 2 then
                        Dir := ''
                    else
                        Dir := 'zone_' + inttostr(DIdx - 1) + '\';
                    Proj_Dir := 'compile "' + Fileroot + '\Torn_Circuit\' + Dir + 'master.dss"';
                    DssExecutive.Command := Proj_Dir;
                    if DIdx > 2 then
                        DssExecutive.Command := Links[DIdx - 2] + '.enabled=False';
                    DssExecutive.Command := 'solve';
                end;
                prog_Str := prog_str + 'Done';
                ErrorCode := 0;
            end;
            3:
            begin                      // Creates the contours matrix
                ActiveActor := 1;
                prog_Str := prog_str + CRLF + '- Building Contour matrix...';
        // Builds the contour matrix
                ErrorCode := Calc_C_Matrix(@Links[0], length(Links));
                if ErrorCode <> 0 then
                    ErrorStr := 'Error'
                else
                    ErrorStr := 'Done';
                prog_Str := prog_str + ErrorStr;

            end;
            4:
            begin                       // Builds the ZLL matrix
                prog_Str := prog_str + CRLF + '- Building ZLL...';
                ErrorCode := Calc_ZLL(@Links[0], length(Links));
                if ErrorCode <> 0 then
                    ErrorStr := 'Error'
                else
                    ErrorStr := 'Done';
                prog_Str := prog_str + ErrorStr;

            end;
            5:
            begin
        // Opens the link branches in the interconnected Circuit and recalculates the YBus
        // The opening happens by replacing the line with a very high series impedance
                prog_Str := prog_str + CRLF + '- Opening link branches...';
                for DIdx := 1 to High(Links) do
                begin
                    DssExecutive.Command := Links[DIdx] + '.r0=10000000';
                    DssExecutive.Command := Links[DIdx] + '.r1=10000000';
                    DssExecutive.Command := Links[DIdx] + '.x0=0';
                    DssExecutive.Command := Links[DIdx] + '.x1=0';
                end;
                Ymatrix.BuildYMatrix(WHOLEMATRIX, false, ActiveActor);
                prog_Str := prog_str + 'Done';
                ErrorCode := 0;          // No error handling here
            end;
            6:
            begin                      // Builds the ZCC matrix
                prog_Str := prog_str + CRLF + '- Building ZCC...';
                Calc_ZCC(length(Links));
                prog_Str := prog_str + 'Done';
            end;
            7:
            begin                      // Inverts ZCC to get Y4
                prog_Str := prog_str + CRLF + '- Building Y4 ...';
                Calc_Y4();
                prog_Str := prog_str + 'Done' + CRLF;
            end
        else
        begin

        end;
        end;
        inc(Local_State);
        MQuit := (Local_State > Num_States) or (ErrorCode <> 0);
    end;

    ActiveActor := 1;
    if ErrorCode <> 0 then
    begin
        ErrorStr := 'One or more errors found';
        ADiakoptics := false;
    end
    else
        ErrorStr := 'A-Diakoptics initialized';

    prog_Str := prog_str + CRLF + ErrorStr + CRLF;
    GlobalResult := ErrorStr;

    {$IFNDEF FPC}
    if not IsDLL then
        ScriptEd.PublishMessage(prog_Str)
    else
        GlobalResult := prog_str;
    {$ELSE}
    GlobalResult  :=  prog_str;
    {$ENDIF}
  // TEMc: TODO: should we report something here under FPC?
  // Davis: Done: This will add the needed report
    SolutionAbort := false;

end;

end.
