unit Diakoptics;

interface

uses
    Circuit,
    Solution,
    DSSGlobals,
    SysUtils,
    DSSClassDefs,
    DSSForms,
    ScriptEdit;

function Solve_Diakoptics(): Integer;
procedure ADiakoptics_Tearing();
procedure ADiakopticsInit();
function Calc_C_Matrix(PLinks: PString; NLinks: Integer): Integer;
function Calc_ZLL(PLinks: PString; NLinks: Integer): Integer;
procedure Calc_ZCT();
procedure Calc_ZCC();

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

    Result := 0;
end;


{*******************************************************************************
*              Calculates the Connections matrix ZCC in the                    *
*                      contours-contours domain                                *
*******************************************************************************}
procedure Calc_ZCC();
begin
    ActiveActor := 1;
    with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
    begin
  {Space left empty to implement the simplified Diakoptics algorithm}
    end;
end;


{*******************************************************************************
*                   Calculates the Lateral matrix ZCT in                       *
*                         contours-trees domain                                *
*******************************************************************************}

  {Probably to be removed}
procedure Calc_ZCT();
var
    i, j,
    Ret,
    LIdx: Integer;
    VContours,
    VZCT: array of Complex;
    temp: String;
    myFile: TextFile;         // For debugging
begin
    ActiveActor := 1;
    with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
    begin
//    setlength(ZCT,length(Contours));

    end;
end;

{*******************************************************************************
*                   Calculates the contours matrix based                       *
*             on the location in the graph of the link branches                *
*             if there is an error returns 0, otherwise 1                      *
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
            Result := 1
        else
            Result := 0;
    end;


end;

{*******************************************************************************
*            Calculates the Link brnahes matrix for further use                *
*                if there is an error returns 0, otherwise 1                   *
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
            Result := 0
        else
            Result := 1;

    end;
end;


{*******************************************************************************
*           Tears the system using considering the number of                   *
*           available CPUs as reference                                        *
*******************************************************************************}
procedure ADiakoptics_Tearing();
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
    end;
end;

{*******************************************************************************
*            Generates the subsystems, actors and memory space                 *
*                     For using the A-Diakoptics parallelism                   *
*******************************************************************************}
procedure ADiakopticsInit();
var
    ErrorCode,
    DIdx,
    Diak_Actors: Integer;
    Dir, Proj_Dir,
    prog_Str,
    ErrorStr,
    FileRoot: String;
    Links: array of String;                        // List of the Link Branches
    ScriptEd: TScriptEdit;

begin
    prog_str := 'A-Diakoptics initialization sumary:' + CRLF + CRLF;
    ActiveActor := 1;
    if ActiveCircuit[1].Num_SubCkts > (CPU_Cores - 2) then
        ActiveCircuit[1].Num_SubCkts := CPU_Cores - 2;

    prog_Str := prog_str + '- Creating Sub-Circuits...' + CRLF;

    ADiakoptics_Tearing();
    prog_Str := prog_str + '  ' + inttostr(ActiveCircuit[1].Num_SubCkts) + ' Sub-Circuits Created' + CRLF;

    Diak_Actors := ActiveCircuit[1].Num_SubCkts + 1;
  // Saves the Link Branch list locally

    prog_Str := prog_str + '- Indexing link branches...';

    setlength(Links, length(ActiveCircuit[1].Link_Branches));
    for DIdx := 0 to High(Links) do
        Links[DIdx] := ActiveCircuit[1].Link_Branches[DIdx];

    prog_Str := prog_str + 'Done' + CRLF + '- Setting up the Actors...';

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

    ActiveActor := 1;
    prog_Str := prog_str + 'Done' + CRLF + '- Building Contour matrix...';

  // Builds the contour matrix
    ErrorCode := Calc_C_Matrix(@Links[0], length(Links));
    if ErrorCode = 0 then
        ErrorStr := 'Error'
    else
        ErrorStr := 'Done';
  // Builds the ZLL matrix
    prog_Str := prog_str + ErrorStr + CRLF + '- Building ZLL...';

    ErrorCode := Calc_ZLL(@Links[0], length(Links));
    if ErrorCode = 0 then
        ErrorStr := 'Error'
    else
        ErrorStr := 'Done';

    prog_Str := prog_str + ErrorStr + CRLF + '- Opening link branches...';
  // Opens the link branches in the interconnected Circuit and recalculates the YBus
  // The opening happens by replacing the line with a very high series impedance
    for DIdx := 1 to High(Links) do
    begin
        DssExecutive.Command := Links[DIdx] + '.r0=1000000000';
        DssExecutive.Command := Links[DIdx] + '.r1=1000000000';
        DssExecutive.Command := Links[DIdx] + '.x0=0';
        DssExecutive.Command := Links[DIdx] + '.x1=0';
    end;
    Ymatrix.BuildYMatrix(WHOLEMATRIX, false, ActiveActor);

  // Builds the ZCC matrix
    prog_Str := prog_str + 'Done' + CRLF + '- Building ZCC...';

//  Calc_ZCT();
//  Calc_ZCC();

    prog_Str := prog_str + 'Done' + CRLF;

    ActiveActor := 1;
    if ErrorCode = 0 then
        ErrorStr := 'One or more errors found'
    else
        ErrorStr := 'A-Diakoptics initialized';

    prog_Str := prog_str + CRLF + ErrorStr + CRLF;
    GlobalResult := ErrorStr;

    ScriptEd.PublishMessage(prog_Str);
    SolutionAbort := false;

end;

end.
