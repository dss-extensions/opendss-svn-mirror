unit ExecHelper;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Functions for performing DSS Exec Commands and Options}
{
 8-17-00  Updated Property Dump to handle wildcards
 10-23-00 Fixed EnergyMeters iteration error in DoAllocateLoadsCmd
 7/6/01  Fixed autobuslist command parsing of file
 7/19/01 Added DoMeterTotals
 8/1/01 Revised the Capacity Command return values
 9/12/02 Added Classes and UserClasses
 3/29/03 Implemented DoPlotCmd and Buscoords
 4/24/03  Implemented Keep list and other stuff related to circuit reduction
}

{$WARN UNIT_PLATFORM OFF}

interface


function DoNewCmd: Integer;
function DoEditCmd: Integer;
function DoBatchEditCmd: Integer;
function DoSelectCmd: Integer;
function DoMoreCmd: Integer;
function DoRedirect(IsCompile: Boolean): Integer;
function DoSaveCmd: Integer;
function DoSampleCmd(ActorID: Integer): Integer;


function DoSolveCmd: Integer;
function DoEnableCmd: Integer;
function DoDisableCmd: Integer;

function DoOpenCmd: Integer;
function DoResetCmd(ActorID: Integer): Integer;
function DoNextCmd: Integer;
function DoFormEditCmd: Integer;
function DoClassesCmd: Integer;
function DoUserClassesCmd: Integer;
function DoHelpCmd: Integer;
function DoClearCmd: Integer;
function DoClearAllCmd: Integer;
function DoReduceCmd: Integer;
function DoInterpolateCmd: Integer;

function DoCloseCmd: Integer;
function DoResetMonitors(ActorID: Integer): Integer;

function DoFileEditCmd: Integer;
function DoQueryCmd: Integer;
function DoResetMeters(ActorID: Integer): Integer;
procedure DoAboutBox;
function DoSetVoltageBases(ActorID: Integer): Integer;
function DoSetkVBase: Integer;

procedure DoLegalVoltageBases;
procedure DoAutoAddBusList(const S: String);
procedure DoKeeperBusList(const S: String);
procedure DoSetReduceStrategy(const S: String);
procedure DoSetAllocationFactors(const X: Double);
procedure DoSetCFactors(const X: Double);

function DovoltagesCmd(const PerUnit: Boolean): Integer;
function DocurrentsCmd: Integer;
function DopowersCmd: Integer;
function DoseqvoltagesCmd: Integer;
function DoseqcurrentsCmd: Integer;
function DoseqpowersCmd: Integer;
function DolossesCmd: Integer;
function DophaselossesCmd: Integer;
function DocktlossesCmd: Integer;
function DoAllocateLoadsCmd(ActorID: Integer): Integer;
function DoHarmonicsList(const S: String): Integer;
function DoMeterTotals: Integer;
function DoCapacityCmd: Integer;
function DoZscCmd(Zmatrix: Boolean): Integer;
function DoZsc10Cmd: Integer;
function DoZscRefresh(ActorID: Integer): Integer;

function DoBusCoordsCmd(SwapXY: Boolean): Integer;
function DoGuidsCmd: Integer;
function DoSetLoadAndGenKVCmd: Integer;
function DoVarValuesCmd: Integer;
function DoVarNamesCmd: Integer;

function DoMakePosSeq: Integer;
function DoAlignFileCmd: Integer;
function DoTOPCmd: Integer;
function DoRotateCmd: Integer;
function DoVDiffCmd: Integer;
function DoSummaryCmd: Integer;
function DoDistributeCmd: Integer;
function DoDI_PlotCmd: Integer;
function DoCompareCasesCmd: Integer;
function DoYearlyCurvesCmd: Integer;
function DoVisualizeCmd: Integer;
function DoCloseDICmd: Integer;
function DoADOScmd: Integer;
function DoEstimateCmd: Integer;
function DoReconductorCmd: Integer;
function DoAddMarkerCmd: Integer;
function DoCvrtLoadshapesCmd: Integer;
function DoNodeDiffCmd: Integer;
function DoRephaseCmd: Integer;
function DoSetBusXYCmd: Integer;
function DoUpdateStorageCmd: Integer;
function DoPstCalc: Integer;
function DoValVarCmd: Integer;
function DoLambdaCalcs: Integer;
function DoVarCmd: Integer;
function DoNodeListCmd: Integer;
function DoRemoveCmd: Integer;

procedure DoSetNormal(pctNormal: Double);


procedure Set_Time;

procedure ParseObjName(const fullname: String; var objname, propname: String);

procedure GetObjClassAndName(var ObjClass, ObjName: String);

function AddObject(const ObjType, name: String): Integer;
function EditObject(const ObjType, name: String): Integer;

procedure SetActiveCircuit(const cktname: String);

function SetActiveCktElement: Integer;

function DoPropertyDump: Integer;


implementation

uses
    Command,
    ArrayDef,
    ParserDel,
    SysUtils,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Monitor, {ShowResults, ExportResults,}
    DSSClass,
    DSSObject,
    Utilities,
    Solution,
    EnergyMeter,
    Generator,
    LoadShape,
    Load,
    PCElement,
    CktElement,
    uComplex,
    mathutil,
    Bus,
    SolutionAlgs,
    {$IFNDEF FPC}
    DSSForms,
    DssPlot,
    {$ELSE}
CmdForms,
    {$ENDIF}
    ExecCommands,
    Executive,
    Dynamics,
    Capacitor,
    Reactor,
    Line,
    Lineunits,
    Math,
    Classes,
    CktElementClass,
    Sensor,  { ExportCIMXML,} NamedObject,
    {$IFNDEF FPC}
    RegularExpressionsCore,
    {$ELSE}
RegExpr,
    {$ENDIF}
    PstCalc,
    PDELement,
    ReduceAlgs;

var
    SaveCommands, DistributeCommands, DI_PlotCommands,
    ReconductorCommands, RephaseCommands, AddMarkerCommands,
    SetBusXYCommands, PstCalcCommands, RemoveCommands: TCommandList;


//----------------------------------------------------------------------------
procedure GetObjClassAndName(var ObjClass, ObjName: String);
var
    ParamName: String;
    Param: String;

begin

{
  We're looking for Object Definition:

      ParamName = 'object' IF given
     and the name of the object

     Object=Capacitor.C1
    or just Capacitor.C1

  If no dot, last class is assumed
}
    ObjClass := '';
    ObjName := '';
    ParamName := LowerCase(Parser[ActiveActor].NextParam);
    Param := Parser[ActiveActor].StrValue;
    if Length(ParamName) > 0 then
    begin   // IF specified, must be object or an abbreviation
        if ComparetextShortest(ParamName, 'object') <> 0 then
        begin
            DoSimpleMsg('object=Class.Name expected as first parameter in command.' + CRLF + parser[ActiveActor].CmdString, 240);
            Exit;
        end;
    end;

    ParseObjectClassandName(Param, ObjClass, ObjName);     // see DSSGlobals

end;


//----------------------------------------------------------------------------
function DoNewCmd: Integer;

// Process the New Command
// new type=xxxx name=xxxx  editstring

// IF the device being added already exists, the default behavior is to
// treat the New command as an Edit command.  This may be overridden
// by setting the DuplicatesAllowed VARiable to true, in which CASE,
// the New command always results in a new device being added.

var
    ObjClass, ObjName: String;
    handle: Integer;

begin

    Result := 0;
    Handle := 0;

    GetObjClassAndName(ObjClass, ObjName);

    if CompareText(ObjClass, 'solution') = 0 then
    begin
        DoSimpleMsg('You cannot create new Solution objects through the command interface.', 241);
        Exit;
    end;

    if CompareText(ObjClass, 'circuit') = 0 then
    begin
        MakeNewCircuit(ObjName);  // Make a new circuit
        ClearEventLog;      // Start the event log in the current directory
        ClearErrorLog;
    end
    else    // Everything else must be a circuit element or DSS Object
    begin
        Handle := AddObject(ObjClass, ObjName);
    end;

    if Handle = 0 then
        Result := 1;

end;

//----------------------------------------------------------------------------
function DoEditCmd: Integer;

// edit type=xxxx name=xxxx  editstring
var
    ObjType, ObjName: String;

begin

    Result := 0;

    GetObjClassAndName(ObjType, ObjName);

    if CompareText(ObjType, 'circuit') = 0 then
    begin
                 // Do nothing
    end
    else
    begin

        // Everything ELSE must be a circuit element
        Result := EditObject(ObjType, ObjName);

    end;

end;

//----------------------------------------------------------------------------
function DoBatchEditCmd: Integer;
// batchedit type=xxxx name=pattern  editstring
    {$IFDEF FPC}
VAR
   ObjType, Pattern:String;
   RegEx1: TRegExpr;
   pObj: TDSSObject;
   Params: Integer;
Begin
  Result := 0;
  GetObjClassAndName(ObjType, Pattern);
  IF CompareText(ObjType, 'circuit')=0 THEN Begin
    // Do nothing
  End ELSE Begin

    LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

    CASE LastClassReferenced[ActiveActor] of
      0: Begin
        DoSimpleMsg('BatchEdit Command: Object Type "' + ObjType + '" not found.'+ CRLF + parser[ActiveActor].CmdString, 267);
        Exit;
        End;{Error}
    ELSE
      Params:=Parser[ActiveActor].Position;
      ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
      RegEx1:=TRegExpr.Create;
//      RegEx1.Options:=[preCaseLess];
      RegEx1.Expression:=UTF8String(Pattern);
      If ActiveDSSClass[ActiveActor].First>0 then pObj:=ActiveDSSObject[ActiveActor] else pObj := Nil;
      while pObj <> Nil do begin
        if RegEx1.Exec(UTF8String(pObj.Name)) then begin
          Parser[ActiveActor].Position:=Params;
          ActiveDSSClass[ActiveActor].Edit(ActiveActor);
        end;
        If ActiveDSSClass[ActiveActor].Next>0 then pObj:=ActiveDSSObject[ActiveActor] else pObj := Nil;
      end;
      RegEx1.Free;
    End;
  End;
End;
    {$ELSE}
var
    ObjType, Pattern: String;
    RegEx1: TPerlRegEx;
    pObj: TDSSObject;
    Params: Integer;
    iElement: Integer;
begin
    Result := 0;
    GetObjClassAndName(ObjType, Pattern);
    if CompareText(ObjType, 'circuit') = 0 then
    begin
    // Do nothing
    end
    else
    begin

        LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

        case LastClassReferenced[ActiveActor] of
            0:
            begin
                DoSimpleMsg('BatchEdit Command: Object Type "' + ObjType + '" not found.' + CRLF + parser[ActiveActor].CmdString, 267);
                Exit;
            end;{Error}
        else
            Params := Parser[ActiveActor].Position;
            ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
            RegEx1 := TPerlRegEx.Create;
            RegEx1.Options := [preCaseLess];
            RegEx1.RegEx := Pattern; // AnsiString(Pattern);
            if ActiveDSSClass[ActiveActor].First > 0 then
                pObj := ActiveDSSObject[ActiveActor]
            else
                pObj := nil;
            while pObj <> nil do
            begin
                RegEx1.Subject := pObj.Name; //(pObj.Name);
                if RegEx1.Match then
                begin
                    Parser[ActiveActor].Position := Params;
                    ActiveDSSClass[ActiveActor].Edit(ActiveActor);
                end;
                if ActiveDSSClass[ActiveActor].Next > 0 then
                    pObj := ActiveDSSObject[ActiveActor]
                else
                    pObj := nil;
            end;
        end;
    end;
end;
{$ENDIF}

//----------------------------------------------------------------------------
function DoRedirect(IsCompile: Boolean): Integer;

//  This routine should be recursive
//  So you can redirect input an arbitrary number of times

// If Compile, makes directory of the file the new home directory
// If not Compile (is simple redirect), return to where we started

var
    Fin: TextFile;
    ParamName, InputLine, CurrDir, SaveDir: String;
    LocalCompFileName: String;
    InBlockComment: Boolean;

begin
    Result := 0;
    InBlockComment := false;  // Discareded off stack upon return
    // Therefore extent of block comment does not extend beyond a file
    // Going back up the redirect stack

    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActiveActor].NextParam;
    ReDirFile := ExpandFileName(Parser[ActiveActor].StrValue);

    if ReDirFile <> '' then
    begin

        SaveDir := GetCurrentDir;

        try
            AssignFile(Fin, ReDirFile);
            Reset(Fin);
            if IsCompile then
            begin
                LastFileCompiled := ReDirFile;
                LocalCompFileName := ReDirFile;
            end;
        except

         // Couldn't find file  Try appending a '.dss' to the file name
         // If it doesn't already have an extension

            if Pos('.', ReDirFile) = 0 then
            begin
                ReDirFile := ReDirFile + '.dss';
                try
                    AssignFile(Fin, ReDirFile);
                    Reset(Fin);
                except
                    DoSimpleMsg('Redirect File: "' + ReDirFile + '" Not Found.', 242);
                    SolutionAbort := true;
                    Exit;
                end;
            end
            else
            begin
                DoSimpleMsg('Redirect File: "' + ReDirFile + '" Not Found.', 243);
                SolutionAbort := true;
                Exit;  // Already had an extension, so just Bail
            end;

        end;

    // OK, we finally got one open, so we're going to continue
        try
            try
             // Change Directory to path specified by file in CASE that
             // loads in more files
                CurrDir := ExtractFileDir(ReDirFile);
                SetCurrentDir(CurrDir);
                if IsCompile then
                    SetDataPath(CurrDir);  // change datadirectory

                Redirect_Abort := false;
                In_Redirect := true;

                while not ((EOF(Fin)) or (Redirect_Abort)) do
                begin
                    Readln(Fin, InputLine);
                    if Length(InputLine) > 0 then
                    begin
                        if not InBlockComment then     // look for '/*'  at baginning of line
                            case InputLine[1] of
                                '/':
                                    if (Length(InputLine) > 1) and (InputLine[2] = '*') then
                                        InBlockComment := true;
                            end;

                        if not InBlockComment then   // process the command line
                            if not SolutionAbort then
                                ProcessCommand(InputLine)
                            else
                                Redirect_Abort := true;  // Abort file if solution was aborted

                      // in block comment ... look for */   and cancel block comment (whole line)
                        if InBlockComment then
                            if Pos('*/', Inputline) > 0 then
                                InBlockComment := false;
                    end;
                end;

                if ActiveCircuit[ActiveActor] <> nil then
                    ActiveCircuit[ActiveActor].CurrentDirectory := CurrDir + '\';

            except
                On E: Exception do
                    DoErrorMsg('DoRedirect' + CRLF + 'Error Processing Input Stream in Compile/Redirect.',
                        E.Message,
                        'Error in File: "' + ReDirFile + '" or Filename itself.', 244);
            end;
        finally
            CloseFile(Fin);
            In_Redirect := false;
            ParserVars.Add('@lastfile', ReDirFile);

            if IsCompile then
            begin
                SetDataPath(CurrDir); // change datadirectory
                LastCommandWasCompile := true;
                ParserVars.Add('@lastcompilefile', LocalCompFileName); // will be last one off the stack
            end
            else
            begin
                SetCurrentDir(SaveDir);    // set back to where we were for redirect, but not compile
                ParserVars.Add('@lastredirectfile', ReDirFile);

            end;
        end;

    end;  // ELSE ignore altogether IF null filename


end;

//----------------------------------------------------------------------------
function DoSelectCmd: Integer;

// select active object
// select element=elementname terminal=terminalnumber
var
    ObjClass, ObjName,
    ParamName, Param: String;

begin

    Result := 1;

    GetObjClassAndName(ObjClass, ObjName);  // Parse Object class and name

    if (Length(ObjClass) = 0) and (Length(ObjName) = 0) then
        Exit;  // select active obj if any

    if CompareText(ObjClass, 'circuit') = 0 then
    begin
        SetActiveCircuit(ObjName);
    end
    else
    begin

        // Everything else must be a circuit element
        if Length(ObjClass) > 0 then
            SetObjectClass(ObjClass);

        ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
        if ActiveDSSClass[ActiveActor] <> nil then
        begin
            if not ActiveDSSClass[ActiveActor].SetActive(Objname) then
            begin // scroll through list of objects untill a match
                DoSimpleMsg('Error! Object "' + ObjName + '" not found.' + CRLF + parser[ActiveActor].CmdString, 245);
                Result := 0;
            end
            else
                with ActiveCircuit[ActiveActor] do
                begin
                    case ActiveDSSObject[ActiveActor].DSSObjType of
                        DSS_OBJECT: ;  // do nothing for general DSS object

                    else
                    begin   // for circuit types, set ActiveCircuit[ActiveActor] Element, too
                        ActiveCktElement := ActiveDSSClass[ActiveActor].GetActiveObj;
                   // Now check for active terminal designation
                        ParamName := LowerCase(Parser[ActiveActor].NextParam);
                        Param := Parser[ActiveActor].StrValue;
                        if Length(Param) > 0 then
                            ActiveCktElement.ActiveTerminalIdx := Parser[ActiveActor].Intvalue
                        else
                            ActiveCktElement.ActiveTerminalIdx := 1;  {default to 1}
                        with ActiveCktElement do
                            SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
                    end;
                    end;
                end;
        end
        else
        begin
            DoSimpleMsg('Error! Active object type/class is not set.', 246);
            Result := 0;
        end;

    end;

end;

//----------------------------------------------------------------------------
function DoMoreCmd: Integer;

// more editstring  (assumes active circuit element)
begin
    if ActiveDSSClass[ActiveActor] <> nil then
        Result := ActiveDSSClass[ActiveActor].Edit(ActiveActor)
    else
        Result := 0;
end;


//----------------------------------------------------------------------------
function DoSaveCmd: Integer;

// Save current values in both monitors and Meters

var
    pMon: TMonitorObj;
    pMtr: TEnergyMeterObj;
    i: Integer;

    ParamPointer: Integer;
    ParamName,
    Param: String;
    ObjClass: String;
    SaveDir: String;
    saveFile: String;
    DSSClass: TDSSClass;

begin
    Result := 0;
    ObjClass := '';
    SaveDir := '';
    SaveFile := '';
    ParamPointer := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := SaveCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                ObjClass := Parser[ActiveActor].StrValue;
            2:
                Savefile := Parser[ActiveActor].StrValue;   // File name for saving  a class
            3:
                SaveDir := Parser[ActiveActor].StrValue;
        else

        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;

    InShowResults := true;
    if (Length(ObjClass) = 0) or (CompareTextShortest(ObjClass, 'meters') = 0) then
    begin
   // Save monitors and Meters

        with ActiveCircuit[ActiveActor].Monitors do
            for i := 1 to ListSize do
            begin
                pMon := Get(i);
                pMon.Save;
            end;

        with ActiveCircuit[ActiveActor].EnergyMeters do
            for i := 1 to ListSize do
            begin
                pMtr := Get(i);
                pMtr.SaveRegisters(ActiveActor);
            end;

        Exit;
    end;
    if CompareTextShortest(ObjClass, 'circuit') = 0 then
    begin
        if not ActiveCircuit[ActiveActor].Save(SaveDir) then
            Result := 1;
        Exit;
    end;
    if CompareTextShortest(ObjClass, 'voltages') = 0 then
    begin
        ActiveCircuit[ActiveActor].Solution.SaveVoltages;
        Exit;
    end;

   {Assume that we have a class name for a DSS Class}
    DSSClass := GetDSSClassPtr(ObjClass);
    if DSSClass <> nil then
    begin
        if Length(SaveFile) = 0 then
            SaveFile := objClass;
        if Length(SaveDir) > 0 then
        begin
            if not DirectoryExists(SaveDir) then
            try
                mkDir(SaveDir);
            except
                On E: Exception do
                    DoSimpleMsg('Error making Directory: "' + SaveDir + '". ' + E.Message, 247);
            end;
            SaveFile := SaveDir + '\' + SaveFile;
        end;
        WriteClassFile(DSSClass, SaveFile, false); // just write the class with no checks
    end;

    SetLastResultFile(SaveFile);
    GlobalResult := SaveFile;

end;


//----------------------------------------------------------------------------
function DoClearCmd: Integer;

begin

    DSSExecutive.Clear;

    Result := 0;

end;
//----------------------------------------------------------------------------
function DoClearAllCmd: Integer;

begin

    DSSExecutive.ClearAll;

    Result := 0;

end;
//----------------------------------------------------------------------------
function DoHelpCmd: Integer;
begin
    ShowHelpForm; // DSSForms Unit
    Result := 0;
end;


//----------------------------------------------------------------------------
function DoSampleCmd(ActorID: Integer): Integer;

// FORce all monitors and meters in active circuit to take a sample


begin

    MonitorClass[ActorID].SampleAll(ActorID);

    EnergyMeterClass[ActorID].SampleAll(ActorID);  // gets generators too


    Result := 0;

end;


//----------------------------------------------------------------------------
function DoSolveCmd: Integer;
begin
   // just invoke solution obj's editor to pick up parsing and execute rest of command
    ActiveSolutionObj := ActiveCircuit[ActiveActor].Solution;
    Result := SolutionClass[ActiveActor].Edit(ActiveActor);
end;


//----------------------------------------------------------------------------
function SetActiveCktElement: Integer;

// Parses the object off the line and sets it active as a circuitelement.

var
    ObjType, ObjName: String;

begin

    Result := 0;

    GetObjClassAndName(ObjType, ObjName);

    if CompareText(ObjType, 'circuit') = 0 then
    begin
                 // Do nothing
    end
    else
    begin

        if CompareText(ObjType, ActiveDSSClass[ActiveActor].Name) <> 0 then
            LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

        case LastClassReferenced[ActiveActor] of
            0:
            begin
                DoSimpleMsg('Object Type "' + ObjType + '" not found.' + CRLF + parser[ActiveActor].CmdString, 253);
                Result := 0;
                Exit;
            end;{Error}
        else

        // intrinsic and user Defined models
            ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
            if ActiveDSSClass[ActiveActor].SetActive(ObjName) then
                with ActiveCircuit[ActiveActor] do
                begin // scroll through list of objects until a match
                    case ActiveDSSObject[ActiveActor].DSSObjType of
                        DSS_OBJECT:
                            DoSimpleMsg('Error in SetActiveCktElement: Object not a circuit Element.' + CRLF + parser[ActiveActor].CmdString, 254);
                    else
                    begin
                        ActiveCktElement := ActiveDSSClass[ActiveActor].GetActiveObj;
                        Result := 1;
                    end;
                    end;
                end;
        end;
    end;
end;


//----------------------------------------------------------------------------
function DoEnableCmd: Integer;

var
    Objtype, ObjName: String;
    ClassPtr: TDSSClass;
    CktElem: TDSSCktElement;
    i: Integer;


begin

  //   Result := SetActiveCktElement;
  //  IF Result>0 THEN ActiveCircuit[ActiveActor].ActiveCktElement.Enabled := True;

    Result := 0;

    GetObjClassAndName(ObjType, ObjName);

    if CompareText(ObjType, 'circuit') = 0 then
    begin
                 // Do nothing
    end
    else
    if Length(ObjType) > 0 then
    begin
      // only applies to CktElementClass objects
        ClassPtr := GetDSSClassPtr(ObjType);
        if ClassPtr <> nil then
        begin

            if (ClassPtr.DSSClassType and BASECLASSMASK) > 0 then
            begin
              // Everything else must be a circuit element
                if CompareText(ObjName, '*') = 0 then
                begin
               // Enable all elements of this class
                    for i := 1 to ClassPtr.ElementCount do
                    begin
                        CktElem := ClassPtr.ElementList.Get(i);
                        CktElem.Enabled := true;
                    end;

                end
                else
                begin

              // just load up the parser and call the edit routine for the object in question

                    Parser[ActiveActor].CmdString := 'Enabled=true';  // Will only work for CktElements
                    Result := EditObject(ObjType, ObjName);
                end;
            end;
        end;
    end;

end;

//----------------------------------------------------------------------------
function DoDisableCmd: Integer;

var
    Objtype, ObjName: String;
    ClassPtr: TDSSClass;
    CktElem: TDSSCktElement;
    i: Integer;


begin
    Result := 0;

    GetObjClassAndName(ObjType, ObjName);

    if CompareText(ObjType, 'circuit') = 0 then
    begin
                 // Do nothing
    end
    else
    if Length(ObjType) > 0 then
    begin
      // only applies to CktElementClass objects
        ClassPtr := GetDSSClassPtr(ObjType);
        if ClassPtr <> nil then
        begin

            if (ClassPtr.DSSClassType and BASECLASSMASK) > 0 then
            begin
              // Everything else must be a circuit element
                if CompareText(ObjName, '*') = 0 then
                begin
               // Disable all elements of this class
                    for i := 1 to ClassPtr.ElementCount do
                    begin
                        CktElem := ClassPtr.ElementList.Get(i);
                        CktElem.Enabled := false;
                    end;

                end
                else
                begin

              // just load up the parser and call the edit routine for the object in question

                    Parser[ActiveActor].CmdString := 'Enabled=false';  // Will only work for CktElements
                    Result := EditObject(ObjType, ObjName);
                end;
            end;
        end;
    end;

//     Result := SetActiveCktElement;
//     IF Result>0 THEN ActiveCircuit[ActiveActor].ActiveCktElement.Enabled := False;
end;

//----------------------------------------------------------------------------
function DoPropertyDump: Integer;

var
    pObject: TDSSObject;
    F: TextFile;
    SingleObject, Debugdump, IsSolution: Boolean;
    i: Integer;
    FileName: String;
    ParamName: String;
    Param, Param2, ObjClass, ObjName: String;

begin

    Result := 0;
    SingleObject := false;
    IsSolution := false;
    DebugDump := false;
    ObjClass := ' ';  // make sure these have at least one character
    ObjName := ' ';

 // Continue parsing command line - check for object name
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    if Length(Param) > 0 then
    begin

        if CompareText(Param, 'commands') = 0 then
            if not NoFormsAllowed then
            begin
                DumpAllDSSCommands(FileName);
                FireOffEditor(FileName);
                Exit;
            end;

    {dump bus names hash list}
        if CompareText(Param, 'buslist') = 0 then
            if not NoFormsAllowed then
            begin
                FileName := GetOutputDirectory + 'Bus_Hash_List.Txt';
                ActiveCircuit[ActiveActor].BusList.DumpToFile(FileName);
                FireOffEditor(FileName);
                Exit;
            end;

    {dump device names hash list}
        if CompareText(Param, 'devicelist') = 0 then
            if not NoFormsAllowed then
            begin
                FileName := GetOutputDirectory + 'Device_Hash_List.Txt';
                ActiveCircuit[ActiveActor].DeviceList.DumpToFile(FileName);
                FireOffEditor(FileName);
                Exit;
            end;

        if CompareText(Copy(lowercase(Param), 1, 5), 'alloc') = 0 then
        begin
            FileName := GetOutputDirectory + 'AllocationFactors.Txt';
            DumpAllocationFactors(FileName);
            FireOffEditor(FileName);
            Exit;
        end;

        if CompareText(Param, 'debug') = 0 then
            DebugDump := true
        else
        begin

            if CompareText(Param, 'solution') = 0 then
            begin
          // Assume active circuit solution IF not qualified
                ActiveDSSClass[ActiveActor] := SolutionClass[ActiveActor];
                ActiveDSSObject[ActiveActor] := ActiveCircuit[ActiveActor].Solution;
                IsSolution := true;
            end
            else
            begin
                SingleObject := true;
           // Check to see IF we want a debugdump on this object
                ParamName := Parser[ActiveActor].NextParam;
                Param2 := Parser[ActiveActor].StrValue;
                if CompareText(Param2, 'debug') = 0 then
                    DebugDump := true;
            // Set active Element to be value in Param
                Parser[ActiveActor].CmdString := '"' + Param + '"';  // put param back into parser
                GetObjClassAndName(ObjClass, ObjName);
            // IF DoSelectCmd=0 THEN Exit;  8-17-00
                if SetObjectClass(ObjClass) then
                begin
                    ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
                    if ActiveDSSClass[ActiveActor] = nil then
                        Exit;
                end
                else
                    Exit;
            end;
        end;
    end;

    try
        AssignFile(F, GetOutputDirectory + CircuitName_[ActiveActor] + 'PropertyDump.Txt');
        Rewrite(F);
    except
        On E: Exception do
        begin
            DoErrorMsg('DoPropertyDump - opening ' + GetOutputDirectory + ' DSS_PropertyDump.txt for writing in ' + Getcurrentdir, E.Message, 'Disk protected or other file error', 255);
            Exit;
        end;
    end;


    try

        if SingleObject then
        begin

        {IF ObjName='*' then we dump all objects of this class}
            case ObjName[1] of
                '*':
                begin
                    for i := 1 to ActiveDSSClass[ActiveActor].ElementCount do
                    begin
                        ActiveDSSClass[ActiveActor].Active := i;
                        ActiveDSSObject[ActiveActor].DumpProperties(F, DebugDump);
                    end;
                end;
            else
                if not ActiveDSSClass[ActiveActor].SetActive(Objname) then
                begin
                    DoSimpleMsg('Error! Object "' + ObjName + '" not found.', 256);
                    Exit;
                end
                else
                    ActiveDSSObject[ActiveActor].DumpProperties(F, DebugDump);  // Dump only properties of active circuit element
            end;

        end
        else
        if IsSolution then
        begin
            ActiveDSSObject[ActiveActor].DumpProperties(F, DebugDump);
        end
        else
        begin

        // Dump general Circuit stuff

            if DebugDump then
                ActiveCircuit[ActiveActor].DebugDump(F);
        // Dump circuit objects
            try
                pObject := ActiveCircuit[ActiveActor].CktElements.First;
                while pObject <> nil do
                begin
                    pObject.DumpProperties(F, DebugDump);
                    pObject := ActiveCircuit[ActiveActor].CktElements.Next;
                end;
                pObject := DSSObjs[ActiveActor].First;
                while pObject <> nil do
                begin
                    pObject.DumpProperties(F, DebugDump);
                    pObject := DSSObjs[ActiveActor].Next;
                end;
            except
                On E: Exception do
                    DoErrorMsg('DoPropertyDump - Problem writing file.', E.Message, 'File may be read only, in use, or disk full?', 257);
            end;

            ActiveCircuit[ActiveActor].Solution.DumpProperties(F, DebugDump);
        end;

    finally

        CloseFile(F);
    end;  {TRY}

    FireOffEditor(GetOutputDirectory + CircuitName_[ActiveActor] + 'PropertyDump.Txt');

end;


//----------------------------------------------------------------------------
procedure Set_Time;

// for interpreting time specified as an array "hour, sec"
var

    TimeArray: array[1..2] of Double;

begin
    Parser[ActiveActor].ParseAsVector(2, @TimeArray);
    with ActiveCircuit[ActiveActor].Solution do
    begin
        DynaVars.intHour := Round(TimeArray[1]);
        DynaVars.t := TimeArray[2];
        Update_dblHour;
    end;
end;

//----------------------------------------------------------------------------
procedure SetActiveCircuit(const cktname: String);

var
    pCkt: TDSSCircuit;
begin

    pCkt := Circuits.First;
    while pCkt <> nil do
    begin
        if CompareText(pCkt.Name, cktname) = 0 then
        begin
            ActiveCircuit[ActiveActor] := pCkt;
            Exit;
        end;
        pCkt := Circuits.Next;
    end;

   // IF none is found, just leave as is after giving error

    DoSimpleMsg('Error! No circuit named "' + cktname + '" found.' + CRLF +
        'Active circuit not changed.', 258);
end;

{-------------------------------------------}
procedure DoLegalVoltageBases;

var
    Dummy: pDoubleArray;
    i,
    Num: Integer;

begin

    Dummy := AllocMem(Sizeof(Dummy^[1]) * 100); // Big Buffer
    Num := Parser[ActiveActor].ParseAsVector(100, Dummy);
     {Parsing zero-fills the array}

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

    with ActiveCircuit[ActiveActor] do
    begin
        Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1]) * (Num + 1));
        for i := 1 to Num + 1 do
            LegalVoltageBases^[i] := Dummy^[i];
    end;

    Reallocmem(Dummy, 0);
end;


//----------------------------------------------------------------------------
function DoOpenCmd: Integer;
// Opens a terminal and conductor of a ckt Element
var
    retval: Integer;
    Terminal: Integer;
    Conductor: Integer;
    ParamName: String;

// syntax:  "Open class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened.

begin
    retval := SetActiveCktElement;
    if retval > 0 then
    begin
        ParamName := Parser[ActiveActor].NextParam;
        Terminal := Parser[ActiveActor].IntValue;
        ParamName := Parser[ActiveActor].NextParam;
        Conductor := Parser[ActiveActor].IntValue;

        with ActiveCircuit[ActiveActor] do
        begin
            ActiveCktElement.ActiveTerminalIdx := Terminal;
            ActiveCktElement.Closed[Conductor, ActiveActor] := false;
            with ActiveCktElement do
                SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
        end;
    end
    else
    begin
        DoSimpleMsg('Error in Open Command: Circuit Element Not Found.' + CRLF + Parser[ActiveActor].CmdString, 259);
    end;
    Result := 0;
end;


//----------------------------------------------------------------------------
function DoCloseCmd: Integer;
// Closes a terminal and conductor of a ckt Element
var
    retval: Integer;
    Terminal: Integer;
    Conductor: Integer;
    ParamName: String;

// syntax:  "Close class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened

begin
    retval := SetActiveCktElement;
    if retval > 0 then
    begin
        ParamName := Parser[ActiveActor].NextParam;
        Terminal := Parser[ActiveActor].IntValue;
        ParamName := Parser[ActiveActor].NextParam;
        Conductor := Parser[ActiveActor].IntValue;

        with ActiveCircuit[ActiveActor] do
        begin
            ActiveCktElement.ActiveTerminalIdx := Terminal;
            ActiveCktElement.Closed[Conductor, ActiveActor] := true;
            with ActiveCktElement do
                SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
        end;

    end
    else
    begin
        DoSimpleMsg('Error in Close Command: Circuit Element Not Found.' + CRLF + Parser[ActiveActor].CmdString, 260);
    end;
    Result := 0;

end;

//----------------------------------------------------------------------------
function DoResetCmd(ActorID: Integer): Integer;
var
    ParamName, Param: String;

begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActorID].NextParam;
    Param := UpperCase(Parser[ActorID].StrValue);
    if Length(Param) = 0 then
    begin
        DoResetMonitors(ACtorID);
        DoResetMeters(ActorID);
        DoResetFaults;
        DoResetControls;
        ClearEventLog;
        ClearErrorLog;
        DoResetKeepList;
    end
    else
        case Param[1] of
            'M':
                case Param[2] of
                    'O'{MOnitor}:
                        DoResetMonitors(ActiveActor);
                    'E'{MEter}:
                        DoResetMeters(ActiveActor);
                end;
            'F'{Faults}:
                DoResetFaults;
            'C'{Controls}:
                DoResetControls;
            'E'{EventLog and ErrorLog}:
            begin
                ClearEventLog;
                ClearErrorLog;
            end;
            'K':
                DoResetKeepList;

        else

            DoSimpleMsg('Unknown argument to Reset Command: "' + Param + '"', 261);

        end;

end;

procedure MarkCapandReactorBuses;
var
    pClass: TDSSClass;
    pCapElement: TCapacitorObj;
    pReacElement: TReactorObj;
    ObjRef: Integer;

begin
{Mark all buses as keepers if there are capacitors or reactors on them}
    pClass := GetDSSClassPtr('capacitor');
    if pClass <> nil then
    begin
        ObjRef := pClass.First;
        while Objref > 0 do
        begin
            pCapElement := TCapacitorObj(ActiveDSSObject[ActiveActor]);
            if pCapElement.IsShunt then
            begin
                if pCapElement.Enabled then
                    ActiveCircuit[ActiveActor].Buses^[pCapElement.Terminals^[1].Busref].Keep := true;
            end;
            ObjRef := pClass.Next;
        end;
    end;

    {Now Get the Reactors}

    pClass := GetDSSClassPtr('reactor');
    if pClass <> nil then
    begin
        ObjRef := pClass.First;
        while Objref > 0 do
        begin
            pReacElement := TReactorObj(ActiveDSSObject[ActiveActor]);
            if pReacElement.IsShunt then
            try
                if pReacElement.Enabled then
                    ActiveCircuit[ActiveActor].Buses^[pReacElement.Terminals^[1].Busref].Keep := true;
            except
                On E: Exception do
                begin
                    DoSimpleMsg(Format('%s %s Reactor=%s Bus No.=%d ', [E.Message, CRLF, pReacElement.Name, pReacElement.NodeRef^[1]]), 9999);
                    Break;
                end;
            end;
            ObjRef := pClass.Next;
        end;
    end;
end;

//----------------------------------------------------------------------------
function DoReduceCmd: Integer;
var
    MetObj: TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    ParamName, Param: String;
    DevClassIndex: Integer;

begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActiveActor].NextParam;
    Param := UpperCase(Parser[ActiveActor].StrValue);

    {Mark Capacitor and Reactor buses as Keep so we don't lose them}
    MarkCapandReactorBuses;

    if Length(Param) = 0 then
        Param := 'A';
    case Param[1] of
        'A':
        begin
            metobj := ActiveCircuit[ActiveActor].EnergyMeters.First;
            while metobj <> nil do
            begin
                MetObj.ReduceZone(ActiveActor);
                MetObj := ActiveCircuit[ActiveActor].EnergyMeters.Next;
            end;
        end;

    else
       {Reduce a specific meter}
        DevClassIndex := ClassNames[ActiveActor].Find('energymeter');
        if DevClassIndex > 0 then
        begin
            MeterClass := DSSClassList[ActiveActor].Get(DevClassIndex);
            if MeterClass.SetActive(Param) then   // Try to set it active
            begin
                MetObj := MeterClass.GetActiveObj;
                MetObj.ReduceZone(ActiveActor);
            end
            else
                DoSimpleMsg('EnergyMeter "' + Param + '" not found.', 262);
        end;
    end;

end;

//----------------------------------------------------------------------------
function DoResetMonitors(ActorID: Integer): Integer;
var
    pMon: TMonitorObj;

begin

    with ActiveCircuit[ActorID] do
    begin

        pMon := Monitors.First;
        while pMon <> nil do
        begin
            pMon.ResetIt(ActorID);
            pMon := Monitors.Next;
        end;
        Result := 0;

    end;

end;

//----------------------------------------------------------------------------
function DoFileEditCmd: Integer;

var
    ParamName, Param: String;

begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    if FileExists(Param) then
        FireOffEditor(Param)
    else
    begin
        GlobalResult := 'File "' + param + '" does not exist.';
        Result := 1;
    end;
end;

//----------------------------------------------------------------------------
procedure ParseObjName(const fullname: String; var objname, propname: String);

{ Parse strings such as

    1. Classname.Objectname,Property    (full name)
    2. Objectname.Property   (classname omitted)
    3. Property           (classname and objectname omitted
}

var
    DotPos1, DotPos2: Integer;

begin
    DotPos1 := Pos('.', fullname);
    case Dotpos1 of

        0:
        begin
            Objname := '';
            PropName := FullName;
        end;

    else
    begin

        PropName := Copy(FullName, Dotpos1 + 1, (Length(FullName) - DotPos1));
        DotPos2 := Pos('.', PropName);
        case DotPos2 of

            0:
            begin
                ObjName := Copy(FullName, 1, DotPos1 - 1);
            end;
        else
        begin
            ObjName := Copy(FullName, 1, Dotpos1 + DotPos2 - 1);
            PropName := Copy(PropName, Dotpos2 + 1, (Length(PropName) - DotPos2));
        end;

        end;

    end;
    end;
end;

function DoQueryCmd: Integer;
{ ? Command }
{ Syntax:  ? Line.Line1.R1}
var
    ParamName: String;
    Param, ObjName, PropName: String;
    PropIndex: Integer;


begin

    Result := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    ParseObjName(Param, ObjName, PropName);

    if CompareText(ObjName, 'solution') = 0 then
    begin  // special for solution
        ActiveDSSClass[ActiveActor] := SolutionClass[ActiveActor];
        ActiveDSSObject[ActiveActor] := ActiveCircuit[ActiveActor].Solution;
    end
    else
    begin
         // Set Object Active
        parser[ActiveActor].cmdstring := '"' + Objname + '"';
        DoSelectCmd;
    end;

     // Put property value in global VARiable
    PropIndex := ActiveDSSClass[ActiveActor].Propertyindex(PropName);
    if PropIndex > 0 then
        GlobalPropertyValue := ActiveDSSObject[ActiveActor].GetPropertyValue(PropIndex)
    else
        GlobalPropertyValue := 'Property Unknown';

    GlobalResult := GlobalPropertyValue;

    if LogQueries then
        WriteQueryLogFile(param, GlobalResult); // write time-stamped query

end;

//----------------------------------------------------------------------------
function DoResetMeters(ActorID: Integer): Integer;

begin
    Result := 0;
    EnergyMeterClass[ActorID].ResetAll(ActorID)
end;


//----------------------------------------------------------------------------
function DoNextCmd: Integer;
var
    ParamName, Param: String;

begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    with ActiveCircuit[ActiveActor].Solution do
        case UpCase(Param[1]) of

            'Y'{Year}:
                Year := Year + 1;
            'H'{Hour}:
                Inc(DynaVars.intHour);
            'T'{Time}:
                Increment_time;
        else

        end;

end;

//----------------------------------------------------------------------------
procedure DoAboutBox;

begin

    if NoFormsAllowed then
        Exit;

    ShowAboutBox;


end;

//----------------------------------------------------------------------------
function DoSetVoltageBases(ActorID: Integer): Integer;


begin

    Result := 0;

    ActiveCircuit[ActiveActor].Solution.SetVoltageBases(ActorID);

end;
//----------------------------------------------------------------------------
function AddObject(const ObjType, Name: String): Integer;


begin

    Result := 0;

   // Search for class IF not already active
   // IF nothing specified, LastClassReferenced remains
    if CompareText(Objtype, ActiveDssClass[ActiveActor].Name) <> 0 then
        LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

    case LastClassReferenced[ActiveActor] of
        0:
        begin
            DoSimpleMsg('New Command: Object Type "' + ObjType + '" not found.' + CRLF + parser[ActiveActor].CmdString, 263);
            Result := 0;
            Exit;
        end;{Error}
    else

     // intrinsic and user Defined models
     // Make a new circuit element
        ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);

      // Name must be supplied
        if Length(Name) = 0 then
        begin
            DoSimpleMsg('Object Name Missing' + CRLF + parser[ActiveActor].CmdString, 264);
            Exit;
        end;


   // now let's make a new object or set an existing one active, whatever the case
        case ActiveDSSClass[ActiveActor].DSSClassType of
            // These can be added WITHout having an active circuit
            // Duplicates not allowed in general DSS objects;
            DSS_OBJECT:
                if not ActiveDSSClass[ActiveActor].SetActive(Name) then
                begin
                    Result := ActiveDSSClass[ActiveActor].NewObject(Name);
                    DSSObjs[ActiveActor].Add(ActiveDSSObject[ActiveActor]);  // Stick in pointer list to keep track of it
                end;
        else
            // These are circuit elements
            if ActiveActor = 0 then
            begin
                DoSimpleMsg('You Must Create a circuit first: "new circuit.yourcktname"', 265);
                Exit;
            end;

          // IF Object already exists.  Treat as an Edit IF dulicates not allowed
            if ActiveCircuit[ActiveActor].DuplicatesAllowed then
            begin
                Result := ActiveDSSClass[ActiveActor].NewObject(Name); // Returns index into this class
                ActiveCircuit[ActiveActor].AddCktElement(Result);   // Adds active object to active circuit
            end
            else
            begin      // Check to see if we can set it active first
                if not ActiveDSSClass[ActiveActor].SetActive(Name) then
                begin
                    Result := ActiveDSSClass[ActiveActor].NewObject(Name);   // Returns index into this class
                    ActiveCircuit[ActiveActor].AddCktElement(Result);   // Adds active object to active circuit
                end
                else
                begin
                    DoSimpleMsg('Warning: Duplicate new element definition: "' + ActiveDSSClass[ActiveActor].Name + '.' + Name + '"' +
                        CRLF + 'Element being redefined.', 266);
                end;
            end;

        end;

        // ActiveDSSObject now points to the object just added
        // IF a circuit element, ActiveCktElement in ActiveCircuit[ActiveActor] is also set

        if Result > 0 then
            ActiveDSSObject[ActiveActor].ClassIndex := Result;

        ActiveDSSClass[ActiveActor].Edit(ActiveActor);    // Process remaining instructions on the command line

    end;
end;


//----------------------------------------------------------------------------
function EditObject(const ObjType, Name: String): Integer;

begin

    Result := 0;
    LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

    case LastClassReferenced[ActiveActor] of
        0:
        begin
            DoSimpleMsg('Edit Command: Object Type "' + ObjType + '" not found.' + CRLF + parser[ActiveActor].CmdString, 267);
            Result := 0;
            Exit;
        end;{Error}
    else

   // intrinsic and user Defined models
   // Edit the DSS object
        ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
        if ActiveDSSClass[ActiveActor].SetActive(Name) then
        begin
            Result := ActiveDSSClass[ActiveActor].Edit(ActiveActor);   // Edit the active object
        end;
    end;

end;

//----------------------------------------------------------------------------
function DoSetkVBase: Integer;

var
    ParamName, BusName: String;
    kVValue: Double;

begin

// Parse off next two items on line
    ParamName := Parser[ActiveActor].NextParam;
    BusName := LowerCase(Parser[ActiveActor].StrValue);

    ParamName := Parser[ActiveActor].NextParam;
    kVValue := Parser[ActiveActor].DblValue;

   // Now find the bus and set the value

    with ActiveCircuit[ActiveActor] do
    begin
        ActiveBusIndex := BusList.Find(BusName);

        if ActiveBusIndex > 0 then
        begin
            if Comparetext(ParamName, 'kvln') = 0 then
                Buses^[ActiveBusIndex].kVBase := kVValue
            else
                Buses^[ActiveBusIndex].kVBase := kVValue / SQRT3;
            Result := 0;
            Solution.VoltageBaseChanged := true;
           // Solution.SolutionInitialized := FALSE;  // Force reinitialization
        end
        else
        begin
            Result := 1;
            AppendGlobalResult('Bus ' + BusName + ' Not Found.');
        end;
    end;


end;


//----------------------------------------------------------------------------
procedure DoAutoAddBusList(const S: String);

var
    ParmName,
    Param, S2: String;
    F: Textfile;


begin

    ActiveCircuit[ActiveActor].AutoAddBusList.Clear;

     // Load up auxiliary parser to reparse the array list or file name
    Auxparser[ActiveActor].CmdString := S;
    ParmName := Auxparser[ActiveActor].NextParam;
    Param := AuxParser[ActiveActor].StrValue;

     {Syntax can be either a list of bus names or a file specification:  File= ...}

    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file

        try
            AssignFile(F, Param);
            Reset(F);
            while not EOF(F) do
            begin         // Fixed 7/8/01 to handle all sorts of bus names
                Readln(F, S2);
                Auxparser[ActiveActor].CmdString := S2;
                ParmName := Auxparser[ActiveActor].NextParam;
                Param := AuxParser[ActiveActor].StrValue;
                if Length(Param) > 0 then
                    ActiveCircuit[ActiveActor].AutoAddBusList.Add(Param);
            end;
            CloseFile(F);

        except
            On E: Exception do
                DoSimpleMsg('Error trying to read bus list file. Error is: ' + E.message, 268);
        end;


    end
    else
    begin

       // Parse bus names off of array list
        while Length(Param) > 0 do
        begin
            ActiveCircuit[ActiveActor].AutoAddBusList.Add(Param);
            AuxParser[ActiveActor].NextParam;
            Param := AuxParser[ActiveActor].StrValue;
        end;

    end;

end;

//----------------------------------------------------------------------------
procedure DoKeeperBusList(const S: String);


// Created 4/25/03

{Set Keep flag on buses found in list so they aren't eliminated by some reduction
 algorithm.  This command is cumulative. To clear flag, use Reset Keeplist}

var
    ParmName,
    Param, S2: String;
    F: Textfile;
    iBus: Integer;

begin

     // Load up auxiliary parser to reparse the array list or file name
    Auxparser[ActiveActor].CmdString := S;
    ParmName := Auxparser[ActiveActor].NextParam;
    Param := AuxParser[ActiveActor].StrValue;

     {Syntax can be either a list of bus names or a file specification:  File= ...}

    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file

        try
            AssignFile(F, Param);
            Reset(F);
            while not EOF(F) do
            begin         // Fixed 7/8/01 to handle all sorts of bus names
                Readln(F, S2);
                Auxparser[ActiveActor].CmdString := S2;
                ParmName := Auxparser[ActiveActor].NextParam;
                Param := AuxParser[ActiveActor].StrValue;
                if Length(Param) > 0 then
                    with ActiveCircuit[ActiveActor] do
                    begin
                        iBus := BusList.Find(Param);
                        if iBus > 0 then
                            Buses^[iBus].Keep := true;
                    end;
            end;
            CloseFile(F);

        except
            On E: Exception do
                DoSimpleMsg('Error trying to read bus list file "+param+". Error is: ' + E.message, 269);
        end;


    end
    else
    begin

       // Parse bus names off of array list
        while Length(Param) > 0 do
        begin
            with ActiveCircuit[ActiveActor] do
            begin
                iBus := BusList.Find(Param);
                if iBus > 0 then
                    Buses^[iBus].Keep := true;
            end;

            AuxParser[ActiveActor].NextParam;
            Param := AuxParser[ActiveActor].StrValue;
        end;

    end;

end;

//----------------------------------------------------------------------------
function DocktlossesCmd: Integer;
var
    LossValue: complex;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        GlobalResult := '';
        LossValue := ActiveCircuit[ActiveActor].Losses[ActiveActor];
        GlobalResult := Format('%10.5g, %10.5g', [LossValue.re * 0.001, LossValue.im * 0.001]);
    end
    else
        GlobalResult := 'No Active Circuit.';


end;

function DocurrentsCmd: Integer;
var
    cBuffer: pComplexArray;
    NValues, i: Integer;

begin
    Result := 0;

    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            NValues := NConds * Nterms;
            GlobalResult := '';
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
            GetCurrents(cBuffer, ActiveActor);
            for i := 1 to NValues do
            begin
                GlobalResult := GlobalResult + Format('%10.5g, %6.1f,', [cabs(cBuffer^[i]), Cdang(cBuffer^[i])]);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        GlobalResult := 'No Active Circuit.';


end;

function DoNodeListCmd: Integer;
var
    NValues, i: Integer;
    CktElementName, S: String;


begin

    Result := 0;

    if ActiveCircuit[ActiveActor] <> nil then
    begin
        S := Parser[ActiveActor].NextParam;
        CktElementName := Parser[ActiveActor].StrValue;

        if Length(CktElementName) > 0 then
            SetObject(CktElementName);

        if Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) then
            with ActiveCircuit[ActiveActor].ActiveCktElement do
            begin
                NValues := NConds * Nterms;
                GlobalResult := '';
                for i := 1 to NValues do
                begin
                    GlobalResult := GlobalResult + Format('%d, ', [GetNodeNum(NodeRef^[i])]);
                end;
            end
        else
            GlobalResult := 'No Active Circuit.';
    end;


end;


function DolossesCmd: Integer;
var
    LossValue: complex;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> nil then
            begin
                GlobalResult := '';
                LossValue := ActiveCktElement.Losses[ActiveActor];
                GlobalResult := Format('%10.5g, %10.5g', [LossValue.re * 0.001, LossValue.im * 0.001]);
            end;
        end
    else
        GlobalResult := 'No Active Circuit.';

end;

function DophaselossesCmd: Integer;

// Returns Phase losses in kW, kVar

var
    cBuffer: pComplexArray;
    NValues, i: Integer;

begin

    Result := 0;

    if ActiveCircuit[ActiveActor] <> nil then

        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            NValues := NPhases;
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
            GlobalResult := '';
            GetPhaseLosses(NValues, cBuffer, ActiveActor);
            for i := 1 to NValues do
            begin
                GlobalResult := GlobalResult + Format('%10.5g, %10.5g,', [cBuffer^[i].re * 0.001, cBuffer^[i].im * 0.001]);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        GlobalResult := 'No Active Circuit.'


end;

function DopowersCmd: Integer;
var
    cBuffer: pComplexArray;
    NValues, i: Integer;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor].ActiveCktElement do
        begin
            NValues := NConds * Nterms;
            GlobalResult := '';
            cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
            GetPhasePower(cBuffer, ActiveActor);
            for i := 1 to NValues do
            begin
                GlobalResult := GlobalResult + Format('%10.5g, %10.5g,', [cBuffer^[i].re * 0.001, cBuffer^[i].im * 0.001]);
            end;
            Reallocmem(cBuffer, 0);
        end
    else
        GlobalResult := 'No Active Circuit';


end;

function DoseqcurrentsCmd: Integer;
// All sequence currents of active ciruit element
// returns magnitude only.

var
    Nvalues, i, j, k: Integer;
    IPh, I012: array[1..3] of Complex;
    cBuffer: pComplexArray;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> nil then
                with ActiveCktElement do
                begin
                    GlobalResult := '';
                    if Nphases < 3 then
                        for i := 0 to 3 * Nterms - 1 do
                            GlobalResult := GlobalResult + ' -1.0,'  // Signify n/A
                    else
                    begin
                        NValues := NConds * Nterms;
                        cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
                        GetCurrents(cBuffer, ActiveActor);
                        for j := 1 to Nterms do
                        begin
                            k := (j - 1) * NConds;
                            for i := 1 to 3 do
                            begin
                                Iph[i] := cBuffer^[k + i];
                            end;
                            Phase2SymComp(@Iph, @I012);
                            for i := 1 to 3 do
                            begin
                                GlobalResult := GlobalResult + Format('%10.5g, ', [Cabs(I012[i])]);
                            end;
                        end;
                        Reallocmem(cBuffer, 0);
                    end; {ELSE}
                end; {WITH ActiveCktElement}
        end   {IF/WITH ActiveCircuit[ActiveActor]}
    else
        GlobalResult := 'No Active Circuit';


end;

function DoSeqpowersCmd: Integer;
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

var
    Nvalues, i, j, k: Integer;
    S: Complex;
    VPh, V012: array[1..3] of Complex;
    IPh, I012: array[1..3] of Complex;
    cBuffer: pComplexArray;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> nil then
                with ActiveCktElement do
                begin
                    GlobalResult := '';
                    if NPhases < 3 then
                        for i := 0 to 2 * 3 * Nterms - 1 do
                            GlobalResult := GlobalResult + '-1.0, '  // Signify n/A
                    else
                    begin
                        NValues := NConds * Nterms;
                        cBuffer := Allocmem(sizeof(cBuffer^[1]) * NValues);
                        GetCurrents(cBuffer, ActiveActor);
                        for j := 1 to Nterms do
                        begin
                            k := (j - 1) * NConds;
                            for i := 1 to 3 do
                            begin
                                Vph[i] := Solution.NodeV^[Terminals^[j].TermNodeRef^[i]];
                            end;
                            for i := 1 to 3 do
                            begin
                                Iph[i] := cBuffer^[k + i];
                            end;
                            Phase2SymComp(@Iph, @I012);
                            Phase2SymComp(@Vph, @V012);
                            for i := 1 to 3 do
                            begin
                                S := Cmul(V012[i], conjg(I012[i]));
                                GlobalResult := GlobalResult + Format('%10.5g, %10.5g,', [S.re * 0.003, S.im * 0.003]); // 3-phase kW conversion
                            end;
                        end;
                    end;
                    Reallocmem(cBuffer, 0);
                end;
        end
    else
        GlobalResult := 'No Active Circuit';


end;

function DoseqvoltagesCmd: Integer;

// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal

var
    Nvalues, i, j, k, n: Integer;
    VPh, V012: array[1..3] of Complex;
    S: String;

begin
    Result := 0;
    Nvalues := -1; // unassigned, for exception message
    n := -1; // unassigned, for exception message
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement <> nil then
                with ActiveCktElement do
                    if Enabled then
                    begin
                        try
                            Nvalues := NPhases;
                            GlobalResult := '';
                            if Nvalues < 3 then
                                for i := 1 to 3 * Nterms do
                                    GlobalResult := GlobalResult + '-1.0, '  // Signify n/A
                            else
                            begin

                                for j := 1 to Nterms do
                                begin

                                    k := (j - 1) * NConds;
                                    for i := 1 to 3 do
                                    begin
                                        Vph[i] := Solution.NodeV^[NodeRef^[i + k]];
                                    end;
                                    Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

                                    for i := 1 to 3 do  // Stuff it in the result
                                    begin
                                        GlobalResult := GlobalResult + Format('%10.5g, ', [Cabs(V012[i])]);
                                    end;

                                end;
                            end;

                        except
                            On E: Exception do
                            begin
                                S := E.message + CRLF +
                                    'Element=' + ActiveCktElement.Name + CRLF +
                                    'Nvalues=' + IntToStr(NValues) + CRLF +
                                    'Nterms=' + IntToStr(Nterms) + CRLF +
                                    'NConds =' + IntToStr(NConds) + CRLF +
                                    'noderef=' + IntToStr(N);
                                DoSimpleMsg(S, 270);
                            end;
                        end;
                    end
                    else
                        GlobalResult := 'Element Disabled';  // Disabled

        end
    else
        GlobalResult := 'No Active Circuit';


end;

//----------------------------------------------------------------------------
function DovoltagesCmd(const PerUnit: Boolean): Integer;
// Bus Voltages at active terminal

var
    i: Integer;
    Volts: Complex;
    ActiveBus: TDSSBus;
    VMag: Double;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveBusIndex <> 0 then
            begin
                ActiveBus := Buses^[ActiveBusIndex];
                GlobalResult := '';
                for i := 1 to ActiveBus.NumNodesThisBus do
                begin
                    Volts := Solution.NodeV^[ActiveBus.GetRef(i)];
                    Vmag := Cabs(Volts);
                    if PerUnit and (ActiveBus.kvbase > 0.0) then
                    begin
                        Vmag := Vmag * 0.001 / ActiveBus.kVBase;
                        GlobalResult := GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
                    end
                    else
                        GlobalResult := GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
                end;
            end
            else
                GlobalResult := 'No Active Bus.';
        end
    else
        GlobalResult := 'No Active Circuit.';

end;

//----------------------------------------------------------------------------
function DoZscCmd(Zmatrix: Boolean): Integer;
// Bus Short Circuit matrix

var
    i, j: Integer;
    ActiveBus: TDSSBus;
    Z: Complex;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveBusIndex <> 0 then
            begin
                ActiveBus := Buses^[ActiveBusIndex];
                GlobalResult := '';
                if not assigned(ActiveBus.Zsc) then
                    Exit;
                with ActiveBus do
                    for i := 1 to NumNodesThisBus do
                    begin
                        for j := 1 to NumNodesThisBus do
                        begin

                            if ZMatrix then
                                Z := Zsc.GetElement(i, j)
                            else
                                Z := Ysc.GetElement(i, j);
                            GlobalResult := GlobalResult + Format('%-.5g, %-.5g,   ', [Z.re, Z.im]);

                        end;

                    end;
            end
            else
                GlobalResult := 'No Active Bus.';
        end
    else
        GlobalResult := 'No Active Circuit.';

end;

//----------------------------------------------------------------------------
function DoZsc10Cmd: Integer;
// Bus Short Circuit matrix

var
    ActiveBus: TDSSBus;
    Z: Complex;

begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveBusIndex <> 0 then
            begin
                ActiveBus := Buses^[ActiveBusIndex];
                GlobalResult := '';
                if not assigned(ActiveBus.Zsc) then
                    Exit;
                with ActiveBus do
                begin

                    Z := Zsc1;
                    GlobalResult := GlobalResult + Format('Z1, %-.5g, %-.5g, ', [Z.re, Z.im]) + CRLF;

                    Z := Zsc0;
                    GlobalResult := GlobalResult + Format('Z0, %-.5g, %-.5g, ', [Z.re, Z.im]);
                end;

            end
            else
                GlobalResult := 'No Active Bus.';
        end
    else
        GlobalResult := 'No Active Circuit.';

end;


//----------------------------------------------------------------------------
function DoAllocateLoadsCmd(ActorID: Integer): Integer;

{ Requires an EnergyMeter Object at the head of the feeder
  Adjusts loads defined by connected kVA or kWh billing
}

var
    pMeter: TEnergyMeterObj;
    pSensor: TSensorObj;
    iterCount: Integer;

begin
    Result := 0;
    with ActiveCircuit[ActorID] do
    begin
        LoadMultiplier := 1.0;   // Property .. has side effects
        with Solution do
        begin
            if Mode <> SNAPSHOT then
                Mode := SNAPSHOT;   // Resets meters, etc. if not in snapshot mode
            Solve(ActorID);  {Make guess based on present allocationfactors}
        end;

         {Allocation loop -- make MaxAllocationIterations iterations}
        for iterCount := 1 to MaxAllocationIterations do
        begin

           {Do EnergyMeters}
            pMeter := EnergyMeters.First;
            while pMeter <> nil do
            begin
                pMeter.CalcAllocationFactors(ActorID);
                pMeter := EnergyMeters.Next;
            end;

           {Now do other Sensors}
            pSensor := Sensors.First;
            while pSensor <> nil do
            begin
                pSensor.CalcAllocationFactors(ActorID);
                pSensor := Sensors.Next;
            end;

           {Now let the EnergyMeters run down the circuit setting the loads}
            pMeter := EnergyMeters.First;
            while pMeter <> nil do
            begin
                pMeter.AllocateLoad(ActorID);
                pMeter := EnergyMeters.Next;
            end;
            Solution.Solve(ActorID);  {Update the solution}

        end;
    end;
end;

//----------------------------------------------------------------------------
procedure DoSetAllocationFactors(const X: Double);

var
    pLoad: TLoadObj;

begin
    if X <= 0.0 then
        DoSimpleMsg('Allocation Factor must be greater than zero.', 271)
    else
        with ActiveCircuit[ActiveActor] do
        begin
            pLoad := Loads.First;
            while pLoad <> nil do
            begin
                pLoad.kVAAllocationFactor := X;
                pLoad := Loads.Next;
            end;
        end;
end;

procedure DoSetCFactors(const X: Double);

var
    pLoad: TLoadObj;

begin
    if X <= 0.0 then
        DoSimpleMsg('CFactor must be greater than zero.', 271)
    else
        with ActiveCircuit[ActiveActor] do
        begin
            pLoad := Loads.First;
            while pLoad <> nil do
            begin
                pLoad.CFactor := X;
                pLoad := Loads.Next;
            end;
        end;
end;

//----------------------------------------------------------------------------
function DoHarmonicsList(const S: String): Integer;

var
    Dummy: pDoubleArray;
    i,
    Num: Integer;

begin
    Result := 0;

    with ActiveCircuit[ActiveActor].Solution do
        if CompareText(S, 'ALL') = 0 then
            DoAllHarmonics := true
        else
        begin
            DoAllHarmonics := false;

            Dummy := AllocMem(Sizeof(Dummy^[1]) * 100); // Big Buffer
            Num := Parser[ActiveActor].ParseAsVector(100, Dummy);
       {Parsing zero-fills the array}

            HarmonicListSize := Num;
            Reallocmem(HarmonicList, SizeOf(HarmonicList^[1]) * HarmonicListSize);
            for i := 1 to HarmonicListSize do
                HarmonicList^[i] := Dummy^[i];

            Reallocmem(Dummy, 0);
        end;
end;


//----------------------------------------------------------------------------
function DoFormEditCmd: Integer;

begin

    Result := 0;
    if NoFormsAllowed then
        Exit;
    DoSelectCmd;  // Select ActiveObject
    if ActiveDSSObject[ActiveActor] <> nil then
    begin

        ShowPropEditForm;

    end
    else
    begin
        DoSimpleMsg('Element Not Found.', 272);
        Result := 1;
    end;
end;


//----------------------------------------------------------------------------
function DoMeterTotals: Integer;
var
    i: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        ActiveCircuit[ActiveActor].TotalizeMeters;
        // Now export to global result
        for i := 1 to NumEMregisters do
        begin
            AppendGlobalResult(Format('%-.6g', [ActiveCircuit[ActiveActor].RegisterTotals[i]]));
        end;
    end;
end;

//----------------------------------------------------------------------------
function DoCapacityCmd: Integer;

var
    ParamPointer: Integer;
    Param, ParamName: String;

begin
    Result := 0;

    ParamPointer := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            case ParamName[1] of
                's':
                    ParamPointer := 1;
                'i':
                    ParamPointer := 2;
            else
                ParamPointer := 0;
            end;

        case ParamPointer of
            0:
                DoSimpleMsg('Unknown parameter "' + ParamName + '" for Capacity Command', 273);
            1:
                ActiveCircuit[ActiveActor].CapacityStart := Parser[ActiveActor].DblValue;
            2:
                ActiveCircuit[ActiveActor].CapacityIncrement := Parser[ActiveActor].DblValue;

        else

        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;

    with ActiveCircuit[ActiveActor] do
        if ComputeCapacity(ActiveActor) then
        begin   // Totalizes EnergyMeters at End

            GlobalResult := Format('%-.6g', [(ActiveCircuit[ActiveActor].RegisterTotals[3] + ActiveCircuit[ActiveActor].RegisterTotals[19])]);  // Peak KW in Meters
            AppendGlobalResult(Format('%-.6g', [LoadMultiplier]));
        end;
end;

//----------------------------------------------------------------------------
function DoClassesCmd: Integer;

var
    i: Integer;
begin
    for i := 1 to NumIntrinsicClasses do
    begin
        AppendGlobalResult(TDSSClass(DSSClassList[ActiveActor].Get(i)).Name);
    end;
    Result := 0;
end;

//----------------------------------------------------------------------------
function DoUserClassesCmd: Integer;
var
    i: Integer;
begin
    Result := 0;
    if NumUserClasses = 0 then
    begin
        AppendGlobalResult('No User Classes Defined.');
    end
    else
        for i := NumIntrinsicClasses + 1 to DSSClassList[ActiveActor].ListSize do
        begin
            AppendGlobalResult(TDSSClass(DSSClassList[ActiveActor].Get(i)).Name);
        end;
end;

//----------------------------------------------------------------------------
function DoZscRefresh(ActorID: Integer): Integer;

var
    j: Integer;

begin
    Result := 1;

    try

        with ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution do
        begin
            for j := 1 to NumNodes do
                Currents^[j] := cZERO;  // Clear Currents array

            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                if not assigned(Buses^[ActiveBusIndex].Zsc) then
                    Buses^[ActiveBusIndex].AllocateBusQuantities;
                SolutionAlgs.ComputeYsc(ActiveBusIndex, ActorID);      // Compute YSC for active Bus
                Result := 0;
            end;
        end;

    except
        On E: Exception do
            DoSimpleMsg('ZscRefresh Error: ' + E.message + CRLF, 274);
    end;


end;


function DoVarValuesCmd: Integer;

var
    i: Integer;
  // PcElem:TPCElement;
begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
         {Check if PCElement}
            case (ActiveCktElement.DSSObjType and BASECLASSMASK) of
                PC_ELEMENT:
                    with ActiveCktElement as TPCElement do
                    begin
                        for i := 1 to NumVariables do
                            AppendGlobalResult(Format('%-.6g', [Variable[i]]));
                    end;
            else
                AppendGlobalResult('Null');
            end;
        end;

end;

function DoValVarCmd: Integer;

{Geg value of specified variable by name of index,}
var
    ParamName, Param: String;
    VarIndex: Integer;
    PropIndex: Integer;
    PCElem: TPCElement;

begin

    Result := 0;

    {Check to make sure this is a PC Element. If not, return null string in global result}

    if (ActiveCircuit[ActiveActor].ActiveCktElement.DSSObjType and BASECLASSMASK) <> PC_ELEMENT then

        GlobalResult := ''

    else
    begin

        PCElem := ActiveCircuit[ActiveActor].ActiveCktElement as TPCElement;

        {Get next parameter on command line}

        ParamName := UpperCase(Parser[ActiveActor].NextParam);
        Param := Parser[ActiveActor].StrValue;

        PropIndex := 1;
        if Length(ParamName) > 0 then
            case ParamName[1] of
                'N':
                    PropIndex := 1;
                'I':
                    PropIndex := 2;
            end;

        VarIndex := 0;

        case PropIndex of
            1:
                VarIndex := PCElem.LookupVariable(Param);  // Look up property index
            2:
                VarIndex := Parser[ActiveActor].IntValue;
        end;

        if (VarIndex > 0) and (VarIndex <= PCElem.NumVariables) then

            GlobalResult := Format('%.8g', [PCElem.Variable[VarIndex]])

        else
            GlobalResult := '';   {Invalid var name or index}

    end;


end;

function DoVarNamesCmd: Integer;

var
    i: Integer;
begin

    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
        begin
         {Check if PCElement}
            case (ActiveCktElement.DSSObjType and BASECLASSMASK) of
                PC_ELEMENT:
                    with (ActiveCktElement as TPCElement) do
                    begin
                        for i := 1 to NumVariables do
                            AppendGlobalResult(VariableName(i));
                    end;
            else
                AppendGlobalResult('Null');
            end;
        end;

end;

function DoBusCoordsCmd(SwapXY: Boolean): Integer;

{
 Format of File should be

   Busname, x, y

   (x, y are real values)

   If SwapXY is true, x and y values are swapped

}

var

    F: TextFile;
    ParamName, Param,
    S,
    BusName: String;
    iB: Integer;
    iLine: Integer;

begin
    Result := 0;

    {Get next parameter on command line}

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    try
        iLine := -1;
        try
            AssignFile(F, Param);
            Reset(F);
            iLine := 0;
            while not EOF(F) do
            begin
                Inc(iLine);
                Readln(F, S);      // Read line in from file

                with AuxParser[ActiveActor] do
                begin      // User Auxparser to parse line
                    CmdString := S;
                    NextParam;
                    BusName := StrValue;
                    iB := ActiveCircuit[ActiveActor].Buslist.Find(BusName);
                    if iB > 0 then
                    begin
                        with ActiveCircuit[ActiveActor].Buses^[iB] do
                        begin     // Returns TBus object
                            NextParam;
                            if SwapXY then
                                y := DblValue
                            else
                                x := DblValue;
                            NextParam;
                            if SwapXY then
                                x := DblValue
                            else
                                y := DblValue;
                            CoordDefined := true;
                        end;
                    end;
                end;
              {Else just ignore a bus that's not in the circuit}
            end;

        except
      {**CHANGE THIS ERROR MESSAGE**}
            ON E: Exception do
            begin
                if iLine = -1 then
                    DoSimpleMsg('Bus Coordinate file: "' + Param + '" not found; ' + E.Message, 275)
                else
                    DoSimpleMsg('Bus Coordinate file: Error Reading Line ' + InttoStr(Iline) + '; ' + E.Message, 275);
            end;
        end;

    finally
        CloseFile(F);
    end;

end;

function DoMakePosSeq: Integer;

var
    CktElem: TDSSCktElement;

begin
    Result := 0;

    ActiveCircuit[ActiveActor].PositiveSequence := true;

    CktElem := ActiveCircuit[ActiveActor].CktElements.First;
    while CktElem <> nil do
    begin
        CktElem.MakePosSequence(ActiveActor);
        CktElem := ActiveCircuit[ActiveActor].CktElements.Next;
    end;

end;


procedure DoSetReduceStrategy(const S: String);


    function AtLeast(i, j: Integer): Integer;
    begin
        if j < i then
            Result := i
        else
            Result := j;
    end;

begin
    ActiveCircuit[ActiveActor].ReductionStrategyString := S;

    ActiveCircuit[ActiveActor].ReductionStrategy := rsDefault;
    if Length(S) = 0 then
        Exit;  {No option given}

    case UpperCase(S)[1] of

        'B':
            ActiveCircuit[ActiveActor].ReductionStrategy := rsBreakLoop;
        'D':
            ActiveCircuit[ActiveActor].ReductionStrategy := rsDefault;  {Default}
        'E':
            ActiveCircuit[ActiveActor].ReductionStrategy := rsDangling;  {Ends}
        'L':
        begin {Laterals}
            ActiveCircuit[ActiveActor].ReductionStrategy := rsLaterals;
        end;
        'M':
            ActiveCircuit[ActiveActor].ReductionStrategy := rsMergeParallel;
        {*
       'T': Begin
              ActiveCircuit[ActiveActor].ReductionStrategy := rsTapEnds;
              ActiveCircuit[ActiveActor].ReductionMaxAngle := 15.0;
              If Length(param2) > 0 Then  ActiveCircuit[ActiveActor].ReductionMaxAngle := Auxparser.DblValue;
            End;
            *}
        'S':
        begin  {Shortlines or Switch}
            if CompareTextShortest(S, 'SWITCH') = 0 then
            begin
                ActiveCircuit[ActiveActor].ReductionStrategy := rsSwitches;
            end
            else
            begin
                 { ActiveCircuit.ReductionZmag is now set in main ExecOptions     }
                ActiveCircuit[ActiveActor].ReductionStrategy := rsShortlines;
            end;
        end;
    else
        DoSimpleMsg('Unknown Reduction Strategy: "' + S + '".', 276);
    end;

end;

function DoInterpolateCmd: Integer;

{Interpolate bus coordinates in meter zones}

var
    MetObj: TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    ParamName, Param: String;
    DevClassIndex: Integer;
    CktElem: TDSSCktElement;

begin
    Result := 0;

    ParamName := Parser[ActiveActor].NextParam;
    Param := UpperCase(Parser[ActiveActor].StrValue);

    // initialize the Checked Flag FOR all circuit Elements
    with ActiveCircuit[ActiveActor] do
    begin
        CktElem := CktElements.First;
        while (CktElem <> nil) do
        begin
            CktElem.Checked := false;
            CktElem := CktElements.Next;
        end;
    end;


    if Length(Param) = 0 then
        Param := 'A';
    case Param[1] of
        'A':
        begin
            metobj := ActiveCircuit[ActiveActor].EnergyMeters.First;
            while metobj <> nil do
            begin
                MetObj.InterpolateCoordinates;
                MetObj := ActiveCircuit[ActiveActor].EnergyMeters.Next;
            end;
        end;

    else
       {Interpolate a specific meter}
        DevClassIndex := ClassNames[ActiveActor].Find('energymeter');
        if DevClassIndex > 0 then
        begin
            MeterClass := DSSClassList[ActiveActor].Get(DevClassIndex);
            if MeterClass.SetActive(Param) then   // Try to set it active
            begin
                MetObj := MeterClass.GetActiveObj;
                MetObj.InterpolateCoordinates;
            end
            else
                DoSimpleMsg('EnergyMeter "' + Param + '" not found.', 277);
        end;
    end;

end;

function DoAlignFileCmd: Integer;
{Rewrites designated file, aligning the fields into columns}
var
    ParamName, Param: String;

begin
    Result := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;


    if FileExists(Param) then
    begin
        if not RewriteAlignedFile(Param) then
            Result := 1;
    end
    else
    begin
        DoSimpleMsg('File "' + Param + '" does not exist.', 278);
        Result := 1;
    end;

    if Result = 0 then
        FireOffEditor(GlobalResult);

end; {DoAlignfileCmd}

function DoTOPCmd: Integer;
{ Sends Monitors, Loadshapes, GrowthShapes, or TCC Curves to TOP as an STO file}

var
    ParamName, Param, ObjName: String;

begin
    Result := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := UpperCase(Parser[ActiveActor].StrValue);

    ParamName := Parser[ActiveActor].NextParam;
    ObjName := UpperCase(Parser[ActiveActor].StrValue);

    if Length(ObjName) = 0 then
        ObjName := 'ALL';


    case Param[1] of
        'L':
            LoadShapeClass[ActiveActor].TOPExport(ObjName);
        'T':
            TshapeClass[ActiveActor].TOPExport(ObjName);
        {
          'G': GrowthShapeClass.TOPExportAll;
          'T': TCC_CurveClass.TOPExportAll;
        }
    else
        MonitorClass[ActiveActor].TOPExport(ObjName);
    end;


end;

procedure DoSetNormal(pctNormal: Double);

var
    i: Integer;
    pLine: TLineObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pctNormal := pctNormal * 0.01;  // local copy only
        for i := 1 to ActiveCircuit[ActiveActor].Lines.ListSize do
        begin
            pLine := ActiveCircuit[ActiveActor].Lines.Get(i);
            pLine.Normamps := pctNormal * pLine.EmergAmps;
        end;
    end;
end;

function DoRotateCmd: Integer;

{rotate about the center of the coordinates}

var
    i: Integer;
    Angle, xmin, xmax, ymin, ymax, xc, yc: Double;
    ParamName: String;
    a, vector: Complex;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin

        ParamName := Parser[ActiveActor].NextParam;
        Angle := Parser[ActiveActor].DblValue * PI / 180.0;   // Deg to rad

        a := cmplx(cos(Angle), Sin(Angle));
        with ActiveCircuit[ActiveActor] do
        begin
            Xmin := 1.0e50;
            Xmax := -1.0e50;
            Ymin := 1.0e50;
            Ymax := -1.0e50;
            for i := 1 to Numbuses do
            begin
                if Buses^[i].CoordDefined then
                begin
                    with Buses^[i] do
                    begin
                        Xmax := Max(Xmax, x);
                        XMin := Min(Xmin, x);
                        ymax := Max(ymax, y);
                        yMin := Min(ymin, y);
                    end;
                end;
            end;

            Xc := (Xmax + Xmin) / 2.0;
            Yc := (Ymax + Ymin) / 2.0;

            for i := 1 to Numbuses do
            begin
                if Buses^[i].CoordDefined then
                begin
                    with Buses^[i] do
                    begin
                        vector := cmplx(x - xc, y - yc);
                        Vector := Cmul(Vector, a);
                        x := xc + vector.re;
                        y := yc + vector.im;
                    end;
                end;
            end;
        end;
    end;

end;


function DoVDiffCmd: Integer;
var
    Fin, Fout: TextFile;
    BusName, Line: String;
    i, node, busIndex: Integer;
    Vmag, Diff: Double;

begin
    Result := 0;
    if FileExists(CircuitName_[ActiveActor] + 'SavedVoltages.Txt') then
    begin
        try
            try

                AssignFile(Fin, CircuitName_[ActiveActor] + 'SavedVoltages.Txt');
                Reset(Fin);

                AssignFile(Fout, CircuitName_[ActiveActor] + 'VDIFF.txt');
                Rewrite(Fout);

                while not EOF(Fin) do
                begin
                    Readln(Fin, Line);
                    Auxparser[ActiveActor].CmdString := Line;
                    AuxParser[ActiveActor].NextParam;
                    BusName := Auxparser[ActiveActor].StrValue;
                    if Length(BusName) > 0 then
                    begin
                        BusIndex := ActiveCircuit[ActiveActor].BusList.Find(BusName);
                        if BusIndex > 0 then
                        begin
                            AuxParser[ActiveActor].Nextparam;
                            node := AuxParser[ActiveActor].Intvalue;
                            with ActiveCircuit[ActiveActor].Buses^[BusIndex] do
                                for i := 1 to NumNodesThisBus do
                                begin
                                    if GetNum(i) = node then
                                    begin
                                        AuxParser[ActiveActor].Nextparam;
                                        Vmag := AuxParser[ActiveActor].Dblvalue;
                                        Diff := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[GetRef(i)]) - Vmag;
                                        if Vmag <> 0.0 then
                                        begin
                                            Writeln(Fout, BusName, '.', node, ', ', (Diff / Vmag * 100.0): 7: 2, ', %');
                                        end
                                        else
                                            Writeln(Fout, BusName, '.', node, ', ', format('%-.5g', [Diff]), ', Volts');
                                    end;
                                end;

                        end;
                    end;
                end;


            except
                On E: Exception do
                begin
                    DoSimpleMsg('Error opening Saved Voltages or VDIFF File: ' + E.message, 280);
                    Exit;
                end;

            end;


        finally

            CloseFile(Fin);
            CloseFile(Fout);

            FireOffEditor(CircuitName_[ActiveActor] + 'VDIFF.txt');

        end;

    end
    else
        DoSimpleMsg('Error: No Saved Voltages.', 281);

end;

function DoSummaryCmd: Integer;

// Returns summary in global result String

var
    S: String;
    cLosses,
    cPower: Complex;

begin
    Result := 0;
    S := '';
    if ActiveCircuit[ActiveActor].Issolved then
        S := S + 'Status = SOLVED' + CRLF
    else
    begin
        S := S + 'Status = NOT Solved' + CRLF;
    end;
    S := S + 'Solution Mode = ' + GetSolutionModeID + CRLF;
    S := S + 'Number = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.NumberofTimes) + CRLF;
    S := S + 'Load Mult = ' + Format('%5.3f', [ActiveCircuit[ActiveActor].LoadMultiplier]) + CRLF;
    S := S + 'Devices = ' + Format('%d', [ActiveCircuit[ActiveActor].NumDevices]) + CRLF;
    S := S + 'Buses = ' + Format('%d', [ActiveCircuit[ActiveActor].NumBuses]) + CRLF;
    S := S + 'Nodes = ' + Format('%d', [ActiveCircuit[ActiveActor].NumNodes]) + CRLF;
    S := S + 'Control Mode =' + GetControlModeID + CRLF;
    S := S + 'Total Iterations = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.Iteration) + CRLF;
    S := S + 'Control Iterations = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.ControlIteration) + CRLF;
    S := S + 'Max Sol Iter = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.MostIterationsDone) + CRLF;
    S := S + ' ' + CRLF;
    S := S + ' - Circuit Summary -' + CRLF;
    S := S + ' ' + CRLF;
    if ActiveCircuit[ActiveActor] <> nil then
    begin

        S := S + Format('Year = %d ', [ActiveCircuit[ActiveActor].Solution.Year]) + CRLF;
        S := S + Format('Hour = %d ', [ActiveCircuit[ActiveActor].Solution.DynaVars.intHour]) + CRLF;
        S := S + 'Max pu. voltage = ' + Format('%-.5g ', [GetMaxPUVoltage]) + CRLF;
        S := S + 'Min pu. voltage = ' + Format('%-.5g ', [GetMinPUVoltage(true)]) + CRLF;
        cPower := CmulReal(GetTotalPowerFromSources(ActiveActor), 0.000001);  // MVA
        S := S + Format('Total Active Power:   %-.6g MW', [cpower.re]) + CRLF;
        S := S + Format('Total Reactive Power: %-.6g Mvar', [cpower.im]) + CRLF;
        cLosses := CmulReal(ActiveCircuit[ActiveActor].Losses[ActiveActor], 0.000001);
        if cPower.re <> 0.0 then
            S := S + Format('Total Active Losses:   %-.6g MW, (%-.4g %%)', [cLosses.re, (Closses.re / cPower.re * 100.0)]) + CRLF
        else
            S := S + 'Total Active Losses:   ****** MW, (**** %%)' + CRLF;
        S := S + Format('Total Reactive Losses: %-.6g Mvar', [cLosses.im]) + CRLF;
        S := S + Format('Frequency = %-g Hz', [ActiveCircuit[ActiveActor].Solution.Frequency]) + CRLF;
        S := S + 'Mode = ' + GetSolutionModeID + CRLF;
        S := S + 'Control Mode = ' + GetControlModeID + CRLF;
        S := S + 'Load Model = ' + GetLoadModel + CRLF;
    end;

    GlobalResult := S;
end;

function DoDistributeCmd: Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;

    DoGenerators: Boolean;

    kW, PF: Double;
    Skip: Integer;
    How,
    FilName: String;

begin
    Result := 0;
    ParamPointer := 0;
     {Defaults}
    kW := 1000.0;
    How := 'Proportional';
    Skip := 1;
    PF := 1.0;
    FilName := 'DistGenerators.dss';
    DoGenerators := true;

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := DistributeCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                kW := Parser[ActiveActor].DblValue;
            2:
                How := Parser[ActiveActor].StrValue;
            3:
                Skip := Parser[ActiveActor].IntValue;
            4:
                PF := Parser[ActiveActor].DblValue;
            5:
                FilName := Parser[ActiveActor].StrValue;
            6:
                kW := Parser[ActiveActor].DblValue * 1000.0;
            7:
                if (Uppercase(Param)[1] = 'L') then
                    DoGenerators := false
                else
                    DoGenerators := true;  // Load or Generator

        else
             // ignore unnamed and extra parms
        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;

    if not DoGenerators then
        FilName := 'DistLoads.dss';

    MakeDistributedGenerators(kW, PF, How, Skip, FilName, DoGenerators);  // in Utilities

end;

function DoDI_PlotCmd: Integer;
    {$IF not (defined(DLL_ENGINE) or defined(FPC))}
var
    ParamName, Param: String;
    ParamPointer, i: Integer;
    CaseName: String;
    MeterName: String;
    CaseYear: Integer;
    dRegisters: array[1..NumEMRegisters] of Double;
    iRegisters: array of Integer;
    NumRegs: Integer;
    PeakDay: Boolean;
    {$ENDIF}
begin
    {$IF not (defined(DLL_ENGINE) or defined(FPC))}
    if DIFilesAreOpen[ActiveActor] then
        EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
    if not Assigned(DSSPlotObj) then
        DSSPlotObj := TDSSPlot.Create;
     {Defaults}
    NumRegs := 1;
    SetLength(IRegisters, NumRegs);
    iRegisters[0] := 9;
    PeakDay := false;
    CaseYear := 1;
    CaseName := '';
    MeterName := 'DI_Totals';

    ParamPointer := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := DI_PlotCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                CaseName := Param;
            2:
                CaseYear := Parser[ActiveActor].Intvalue;
            3:
            begin
                NumRegs := Parser[ActiveActor].ParseAsVector(NumEMREgisters, @dRegisters);
                SetLength(iRegisters, NumRegs);
                for i := 1 to NumRegs do
                    iRegisters[i - 1] := Round(dRegisters[i]);
            end;
            4:
                PeakDay := InterpretYesNo(Param);
            5:
                MeterName := Param;

        else
             // ignore unnamed and extra parms
        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;
    DSSPlotObj.DoDI_Plot(CaseName, CaseYear, iRegisters, PeakDay, MeterName);
    iRegisters := nil;
    {$ENDIF}
    Result := 0;
end;

function DoCompareCasesCmd: Integer;
    {$IFNDEF DLL_ENGINE}
var
    ParamName, Param: String;
    ParamPointer: Integer;
    UnKnown: Boolean;
    Reg: Integer;
    CaseName1,
    CaseName2, WhichFile: String;
    {$ENDIF}
begin
    {$IF not (defined(DLL_ENGINE) or defined(FPC))}
    if DIFilesAreOpen[ActiveActor] then
        EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
    if not Assigned(DSSPlotObj) then
        DSSPlotObj := TDSSPlot.Create;
    CaseName1 := 'base';
    CaseName2 := '';
    Reg := 9;    // Overload EEN
    WhichFile := 'Totals';

    ParamPointer := 0;
    ParamName := UpperCase(Parser[ActiveActor].NextParam);
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        Unknown := false;
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)

        else
        begin
            if CompareTextShortest(ParamName, 'CASE1') = 0 then
                ParamPointer := 1
            else
            if CompareTextShortest(ParamName, 'CASE2') = 0 then
                ParamPointer := 2
            else
            if CompareTextShortest(ParamName, 'REGISTER') = 0 then
                ParamPointer := 3
            else
            if CompareTextShortest(ParamName, 'METER') = 0 then
                ParamPointer := 4
            else
                Unknown := true;
        end;


        if not Unknown then
            case ParamPointer of
                1:
                    CaseName1 := Param;
                2:
                    CaseName2 := Param;
                3:
                    Reg := Parser[ActiveActor].IntValue;
                4:
                    WhichFile := Param;
            else
             // ignore unnamed and extra parms
            end;

        ParamName := UpperCase(Parser[ActiveActor].NextParam);
        Param := Parser[ActiveActor].StrValue;
    end;
    DSSPlotObj.DoCompareCases(CaseName1, CaseName2, WhichFile, Reg);
    {$ENDIF}
    Result := 0;
end;

function DoYearlyCurvesCmd: Integer;
    {$IF not (defined(DLL_ENGINE) or defined(FPC))}
var
    ParamName, Param: String;
    ParamPointer, i: Integer;
    UnKnown: Boolean;
    CaseNames: TStringList;
    dRegisters: array[1..NumEMRegisters] of Double;
    iRegisters: array of Integer;
    Nregs: Integer;
    WhichFile: String;
    {$ENDIF}
begin
    {$IF not (defined(DLL_ENGINE) or defined(FPC))}
    if DIFilesAreOpen[ActiveActor] then
        EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
    if not Assigned(DSSPlotObj) then
        DSSPlotObj := TDSSPlot.Create;

    Nregs := 1;
    SetLength(iRegisters, Nregs);
    CaseNames := TStringList.Create;
    CaseNames.Clear;
    WhichFile := 'Totals';


    ParamPointer := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        Unknown := false;
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)

        else
            case Uppercase(ParamName)[1] of
                'C':
                    ParamPointer := 1;
                'R':
                    ParamPointer := 2;
                'M':
                    ParamPointer := 3; {meter=}
            else
                Unknown := true;
            end;

        if not Unknown then
            case ParamPointer of
                1:
                begin  // List of case names
                    AuxParser[ActiveActor].CmdString := Param;
                    AuxParser[ActiveActor].NextParam;
                    Param := AuxParser[ActiveActor].StrValue;
                    while Length(Param) > 0 do
                    begin
                        CaseNames.Add(Param);
                        AuxParser[ActiveActor].NextParam;
                        Param := AuxParser[ActiveActor].StrValue;
                    end;
                end;
                2:
                begin
                    NRegs := Parser[ActiveActor].ParseAsVector(NumEMRegisters, @dRegisters);
                    SetLength(iRegisters, Nregs);
                    for i := 1 to NRegs do
                        iRegisters[i - 1] := Round(dRegisters[i]);
                end;
                3:
                    WhichFile := Param;
            else
             // ignore unnamed and extra parms
            end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;
    DSSPlotObj.DoYearlyCurvePlot(CaseNames, WhichFile, iRegisters);
    iRegisters := nil;
    CaseNames.Free;
    {$ENDIF}
    Result := 0;
end;

function DoVisualizeCmd: Integer;
    {$IF not defined(FPC)}
var
    DevIndex: Integer;
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    Unknown: Boolean;
    Quantity: Integer;
    ElemName: String;
    pElem: TDSSObject;
    {$ENDIF}
begin
    Result := 0;
    {$IF not defined(FPC)}
     // Abort if no circuit or solution
    if not assigned(ActiveCircuit[ActiveActor]) then
    begin
        DoSimpleMsg('No circuit created.', 24721);
        Exit;
    end;
    if not assigned(ActiveCircuit[ActiveActor].Solution) or not assigned(ActiveCircuit[ActiveActor].Solution.NodeV) then
    begin
        DoSimpleMsg('The circuit must be solved before you can do this.', 24722);
        Exit;
    end;
    Quantity := vizCURRENT;
    Quantity := 1;
    ElemName := '';
      {Parse rest of command line}
    ParamPointer := 0;
    ParamName := UpperCase(Parser[ActiveActor].NextParam);
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        Unknown := false;
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)

        else
        begin
            if CompareTextShortest(ParamName, 'WHAT') = 0 then
                ParamPointer := 1
            else
            if CompareTextShortest(ParamName, 'ELEMENT') = 0 then
                ParamPointer := 2
            else
                Unknown := true;
        end;

        if not Unknown then
            case ParamPointer of
                1:
                    case Lowercase(Param)[1] of
                        {$IFDEF MSWINDOWS}
                        'c':
                            Quantity := vizCURRENT;
                        'v':
                            Quantity := vizVOLTAGE;
                        'p':
                            Quantity := vizPOWER;
                        {$ELSE}
                'c':  Quantity := 1;
                'v':  Quantity := 2;
                'p':  Quantity := 3;
                        {$ENDIF}
                    end;
                2:
                    ElemName := Param;
            else
             // ignore unnamed and extra parms
            end;

        ParamName := UpperCase(Parser[ActiveActor].NextParam);
        Param := Parser[ActiveActor].StrValue;
    end;  {WHILE}

     {--------------------------------------------------------------}

    Devindex := GetCktElementIndex(ElemName); // Global function
    if DevIndex > 0 then
    begin  //  element must already exist
        pElem := ActiveCircuit[ActiveActor].CktElements.Get(DevIndex);
        if pElem is TDSSCktElement then
        begin
            DSSPlotObj.DoVisualizationPlot(TDSSCktElement(pElem), Quantity);
        end
        else
        begin
            DoSimpleMsg(pElem.Name + ' must be a circuit element type!', 282);   // Wrong type
        end;
    end
    else
    begin
        DoSimpleMsg('Requested Circuit Element: "' + ElemName + '" Not Found.', 282); // Did not find it ..
    end;
    {$ENDIF}
end;

function DoCloseDICmd: Integer;

begin
    Result := 0;
    EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
end;

function DoADOScmd: Integer;

begin
    Result := 0;
    DoDOScmd(Parser[ActiveActor].Remainder);
end;

function DoEstimateCmd: Integer;


begin
    Result := 0;

    {Load current Estimation is driven by Energy Meters at head of feeders.}
    DoAllocateLoadsCmd(ActiveActor);

    {Let's look to see how well we did}
    if not AutoShowExport then
        DSSExecutive.Command := 'Set showexport=yes';
    DSSExecutive.Command := 'Export Estimation';

end;


function DoReconductorCmd: Integer;

var
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    Line1, Line2,
    Linecode,
    Geometry,
    EditString,
    MyEditString: String;
    LineCodeSpecified,
    GeometrySpecified: Boolean;
    pLine1, pLine2: TLineObj;
    LineClass: TLine;
    TraceDirection: Integer;
    NPhases: Integer;


begin
    Result := 0;
    ParamPointer := 0;
    LineCodeSpecified := false;
    GeometrySpecified := false;
    Line1 := '';
    Line2 := '';
    MyEditString := '';
    NPhases := 0; // no filtering by number of phases
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := ReconductorCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                Line1 := Param;
            2:
                Line2 := Param;
            3:
            begin
                Linecode := Param;
                LineCodeSpecified := true;
                GeometrySpecified := false;
            end;
            4:
            begin
                Geometry := Param;
                LineCodeSpecified := false;
                GeometrySpecified := true;
            end;
            5:
                MyEditString := Param;
            6:
                Nphases := Parser[ActiveActor].IntValue;
        else
            DoSimpleMsg('Error: Unknown Parameter on command line: ' + Param, 28701);
        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;

     {Check for Errors}

     {If user specified full line name, get rid of "line."}
    Line1 := StripClassName(Line1);
    Line2 := StripClassName(Line2);

    if (Length(Line1) = 0) or (Length(Line2) = 0) then
    begin
        DoSimpleMsg('Both Line1 and Line2 must be specified!', 28702);
        Exit;
    end;

    if (not LineCodeSpecified) and (not GeometrySpecified) then
    begin
        DoSimpleMsg('Either a new LineCode or a Geometry must be specified!', 28703);
        Exit;
    end;

    LineClass := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('Line'));
    pLine1 := LineClass.Find(Line1);
    pLine2 := LineCLass.Find(Line2);

    if (pLine1 = nil) or (pLine2 = nil) then
    begin
        if pLine1 = nil then
            doSimpleMsg('Line.' + Line1 + ' not found.', 28704)
        else
        if pLine2 = nil then
            doSimpleMsg('Line.' + Line2 + ' not found.', 28704);
        Exit;
    end;

     {Now check to make sure they are in the same meter's zone}
    if (pLine1.MeterObj = nil) or (pLine2.MeterObj = nil) then
    begin
        DoSimpleMsg('Error: Both Lines must be in the same EnergyMeter zone. One or both are not in any meter zone.', 28705);
        Exit;
    end;

    if pLine1.MeterObj <> pline2.MeterObj then
    begin
        DoSimpleMsg('Error: Line1 is in EnergyMeter.' + pLine1.MeterObj.Name +
            ' zone while Line2 is in EnergyMeter.' + pLine2.MeterObj.Name + ' zone. Both must be in the same Zone.', 28706);
        Exit;
    end;

     {Since the lines can be given in either order, Have to check to see which direction they are specified and find the path between them}
    TraceDirection := 0;
    if IsPathBetween(pLine1, pLine2) then
        TraceDirection := 1;
    if IsPathBetween(pLine2, pLine1) then
        TraceDirection := 2;

    if LineCodeSpecified then
        EditString := 'Linecode=' + LineCode
    else
        EditString := 'Geometry=' + Geometry;

     // Append MyEditString onto the end of the edit string to change the linecode  or geometry
    EditString := Format('%s  %s', [EditString, MyEditString]);

    case TraceDirection of
        1:
            TraceAndEdit(pLine1, pLine2, NPhases, Editstring);
        2:
            TraceAndEdit(pLine2, pLine1, NPhases, Editstring);
    else
        DoSimpleMsg('Traceback path not found between Line1 and Line2.', 28707);
        Exit;
    end;

end;

function DoAddMarkerCmd: Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;
    BusMarker: TBusMarker;

begin
    Result := 0;
    ParamPointer := 0;

    BusMarker := TBusMarker.Create;
    ActiveCircuit[ActiveActor].BusMarkerList.Add(BusMarker);

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := AddmarkerCommands.GetCommand(ParamName);

        with BusMarker do
            case ParamPointer of
                1:
                    BusName := Param;
                2:
                    AddMarkerCode := Parser[ActiveActor].IntValue;
                3:
                    AddMarkerColor := InterpretColorName(Param);
                4:
                    AddMarkerSize := Parser[ActiveActor].IntValue;

            else
             // ignore unnamed and extra parms
            end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;

end;

function DoSetLoadAndGenKVCmd: Integer;
var
    pLoad: TLoadObj;
    pGen: TGeneratorObj;
    pBus: TDSSBus;
    sBus: String;
    iBus, i: Integer;
    kvln: Double;
begin
    Result := 0;
    pLoad := ActiveCircuit[ActiveActor].Loads.First;
    while pLoad <> nil do
    begin
        ActiveLoadObj := pLoad; // for UpdateVoltageBases to work
        sBus := StripExtension(pLoad.GetBus(1));
        iBus := ActiveCircuit[ActiveActor].BusList.Find(sBus);
        pBus := ActiveCircuit[ActiveActor].Buses^[iBus];
        kvln := pBus.kVBase;
        if (pLoad.Connection = 1) or (pLoad.NPhases = 3) then
            pLoad.kVLoadBase := kvln * sqrt(3.0)
        else
            pLoad.kVLoadBase := kvln;
        pLoad.UpdateVoltageBases;
        pLoad.RecalcElementData(ActiveActor);
        pLoad := ActiveCircuit[ActiveActor].Loads.Next;
    end;

    for i := 1 to ActiveCircuit[ActiveActor].Generators.ListSize do
    begin
        pGen := ActiveCircuit[ActiveActor].Generators.Get(i);
        sBus := StripExtension(pGen.GetBus(1));
        iBus := ActiveCircuit[ActiveActor].BusList.Find(sBus);
        pBus := ActiveCircuit[ActiveActor].Buses^[iBus];
        kvln := pBus.kVBase;
        if (pGen.Connection = 1) or (pGen.NPhases > 1) then
            pGen.PresentKV := kvln * sqrt(3.0)
        else
            pGen.PresentKV := kvln;
        pGen.RecalcElementData(ActiveActor);
    end;

end;

function DoGuidsCmd: Integer;
var
    F: TextFile;
    ParamName, Param, S, NameVal, GuidVal, DevClass, DevName: String;
    pName: TNamedObject;
begin
    Result := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    try
        AssignFile(F, Param);
        Reset(F);
        while not EOF(F) do
        begin
            Readln(F, S);
            with AuxParser[ActiveActor] do
            begin
                pName := nil;
                CmdString := S;
                NextParam;
                NameVal := StrValue;
                NextParam;
                GuidVal := StrValue;
        // format the GUID properly
                if Pos('{', GuidVal) < 1 then
                    GuidVal := '{' + GuidVal + '}';
        // find this object
                ParseObjectClassAndName(NameVal, DevClass, DevName);
                if CompareText(DevClass, 'circuit') = 0 then
                begin
                    pName := ActiveCircuit[ActiveActor]
                end
                else
                begin
                    LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(DevClass);
                    ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
                    if ActiveDSSClass[ActiveActor] <> nil then
                    begin
                        ActiveDSSClass[ActiveActor].SetActive(DevName);
                        pName := ActiveDSSClass[ActiveActor].GetActiveObj;
                    end;
                end;
        // re-assign its GUID
                if pName <> nil then
                    pName.GUID := StringToGuid(GuidVal);
            end;
        end;
    finally
        CloseFile(F);
    end;
end;

function DoCvrtLoadshapesCmd: Integer;
var
    pLoadshape: TLoadShapeObj;
    iLoadshape: Integer;
    LoadShapeClass: TLoadShape;
    ParamName: String;
    Param: String;
    Action: String;
    F: TextFile;
    Fname: String;

begin
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    if length(param) = 0 then
        Param := 's';

    {Double file or Single file?}
    case lowercase(param)[1] of
        'd':
            Action := 'action=dblsave';
    else
        Action := 'action=sngsave';   // default
    end;

    LoadShapeClass := GetDSSClassPtr('loadshape') as TLoadShape;

    Fname := 'ReloadLoadshapes.DSS';
    AssignFile(F, Fname);
    Rewrite(F);

    iLoadshape := LoadShapeClass.First;
    while iLoadshape > 0 do
    begin
        pLoadShape := LoadShapeClass.GetActiveObj;
        Parser[ActiveActor].CmdString := Action;
        pLoadShape.Edit(ActiveActor);
        Writeln(F, Format('New Loadshape.%s Npts=%d Interval=%.8g %s', [pLoadShape.Name, pLoadShape.NumPoints, pLoadShape.Interval, GlobalResult]));
        iLoadshape := LoadShapeClass.Next;
    end;

    CloseFile(F);
    FireOffEditor(Fname);
    Result := 0;
end;

function DoNodeDiffCmd: Integer;

var
    ParamName: String;
    Param: String;
    sNode1, sNode2: String;
    SBusName: String;
    V1, V2,
    VNodeDiff: Complex;
    iBusidx: Integer;
    B1ref: Integer;
    B2ref: Integer;
    NumNodes: Integer;
    NodeBuffer: array[1..50] of Integer;


begin

    Result := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    sNode1 := Param;
    if Pos('2', ParamName) > 0 then
        sNode2 := Param;

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    sNode2 := Param;
    if Pos('1', ParamName) > 0 then
        sNode1 := Param;

    // Get first node voltage
    AuxParser[ActiveActor].Token := sNode1;
    NodeBuffer[1] := 1;
    sBusName := AuxParser[ActiveActor].ParseAsBusName(numNodes, @NodeBuffer, ActiveActor);
    iBusidx := ActiveCircuit[ActiveActor].Buslist.Find(sBusName);
    if iBusidx > 0 then
    begin
        B1Ref := ActiveCircuit[ActiveActor].Buses^[iBusidx].Find(NodeBuffer[1])
    end
    else
    begin
        DoSimpleMsg(Format('Bus %s not found.', [sBusName]), 28709);
        Exit;
    end;

    V1 := ActiveCircuit[ActiveActor].Solution.NodeV^[B1Ref];

    // Get 2nd node voltage
    AuxParser[ActiveActor].Token := sNode2;
    NodeBuffer[1] := 1;
    sBusName := AuxParser[ActiveActor].ParseAsBusName(numNodes, @NodeBuffer, ActiveActor);
    iBusidx := ActiveCircuit[ActiveActor].Buslist.Find(sBusName);
    if iBusidx > 0 then
    begin
        B2Ref := ActiveCircuit[ActiveActor].Buses^[iBusidx].Find(NodeBuffer[1])
    end
    else
    begin
        DoSimpleMsg(Format('Bus %s not found.', [sBusName]), 28710);
        Exit;
    end;

    V2 := ActiveCircuit[ActiveActor].Solution.NodeV^[B2Ref];

    VNodeDiff := CSub(V1, V2);
    GlobalResult := Format('%.7g, V,    %.7g, deg  ', [Cabs(VNodeDiff), CDang(VNodeDiff)]);

end;

function DoRephaseCmd: Integer;
var
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    StartLine: String;
    NewPhases: String;
    MyEditString: String;
    ScriptfileName: String;
    pStartLine: TLineObj;
    LineClass: TLine;
    TransfStop: Boolean;

begin
    Result := 0;
    ParamPointer := 0;
    MyEditString := '';
    ScriptfileName := 'RephaseEditScript.DSS';
    TransfStop := true;  // Stop at Transformers

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := RephaseCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                StartLine := Param;
            2:
                NewPhases := Param;
            3:
                MyEditString := Param;
            4:
                ScriptFileName := Param;
            5:
                TransfStop := InterpretYesNo(Param);
        else
            DoSimpleMsg('Error: Unknown Parameter on command line: ' + Param, 28711);
        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;

    LineClass := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('Line'));
    pStartLine := LineClass.Find(StripClassName(StartLine));
    if pStartLine = nil then
    begin
        DosimpleMsg('Starting Line (' + StartLine + ') not found.', 28712);
        Exit;
    end;
     {Check for some error conditions and abort if necessary}
    if pStartLine.MeterObj = nil then
    begin
        DosimpleMsg('Starting Line must be in an EnergyMeter zone.', 28713);
        Exit;
    end;

    if not (pStartLine.MeterObj is TEnergyMeterObj) then
    begin
        DosimpleMsg('Starting Line must be in an EnergyMeter zone.', 28714);
        Exit;
    end;

    GoForwardandRephase(pStartLine, NewPhases, MyEditString, ScriptfileName, TransfStop);

end;

function DoSetBusXYCmd: Integer;

var
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    BusName: String;
    Xval: Double;
    Yval: Double;
    iB: Integer;

begin

    Result := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    ParamPointer := 0;
    Xval := 0.0;
    Yval := 0.0;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := SetBusXYCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                BusName := Param;
            2:
                Xval := Parser[ActiveActor].DblValue;
            3:
                Yval := Parser[ActiveActor].DblValue;
        else
            DoSimpleMsg('Error: Unknown Parameter on command line: ' + Param, 28721);
        end;

        iB := ActiveCircuit[ActiveActor].Buslist.Find(BusName);
        if iB > 0 then
        begin
            with ActiveCircuit[ActiveActor].Buses^[iB] do
            begin     // Returns TBus object
                x := Xval;
                y := Yval;
                CoordDefined := true;
            end;
        end
        else
        begin
            DosimpleMsg('Error: Bus "' + BusName + '" Not Found.', 28722);
        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;


end;

function DoUpdateStorageCmd: Integer;

begin
    StorageClass[ActiveActor].UpdateAll(ActiveActor);
    Result := 0;
end;

function DoPstCalc;

var
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    Npts: Integer;
    Varray: pDoubleArray;
    CyclesPerSample: Integer;
    Lamp: Integer;
    PstArray: pDoubleArray;
    nPst: Integer;
    i: Integer;
    S: String;
    Freq: Double;

begin

    Result := 0;
    Varray := nil;
    PstArray := nil;
    Npts := 0;
    Lamp := 120;  // 120 or 230
    CyclesPerSample := 60;
    Freq := DefaultBaseFreq;

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    ParamPointer := 0;
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then
            Inc(ParamPointer)
        else
            ParamPointer := PstCalcCommands.GetCommand(ParamName);
         // 'Npts', 'Voltages', 'cycles', 'lamp'
        case ParamPointer of
            1:
            begin
                Npts := Parser[ActiveActor].IntValue;
                Reallocmem(Varray, SizeOf(Varray^[1]) * Npts);
            end;
            2:
                Npts := InterpretDblArray(Param, Npts, Varray);
            3:
                CyclesPerSample := Round(ActiveCircuit[ActiveActor].Solution.Frequency * Parser[ActiveActor].dblvalue);
            4:
                Freq := Parser[ActiveActor].DblValue;
            5:
                Lamp := Parser[ActiveActor].IntValue;
        else
            DoSimpleMsg('Error: Unknown Parameter on command line: ' + Param, 28722);
        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;

    if Npts > 10 then
    begin

        nPst := PstRMS(PstArray, Varray, Freq, CyclesPerSample, Npts, Lamp);
         // put resulting pst array in the result string
        S := '';
        for i := 1 to nPst do
            S := S + Format('%.8g, ', [PstArray^[i]]);
        GlobalResult := S;
    end
    else
        DoSimpleMsg('Insuffient number of points for Pst Calculation.', 28723);


    Reallocmem(Varray, 0);   // discard temp arrays
    Reallocmem(PstArray, 0);
end;

function DoLambdaCalcs: Integer;
{Execute fault rate and bus number of interruptions calc}

var
    pMeter: TEnergyMeterObj;
    i: Integer;
    ParamName,
    Param: String;
    AssumeRestoration: Boolean;

begin
    Result := 0;

// Do for each Energymeter object in active circuit
    pMeter := ActiveCircuit[ActiveActor].EnergyMeters.First;
    if pMeter = nil then
    begin
        DoSimpleMsg('No EnergyMeter Objects Defined. EnergyMeter objects required for this function.', 28724);
        Exit;
    end;

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    if Length(Param) > 0 then
        Assumerestoration := InterpretYesNo(param)
    else
        Assumerestoration := false;

       // initialize bus quantities
    with ActiveCircuit[ActiveActor] do
        for i := 1 to NumBuses do
            with Buses^[i] do
            begin
                BusFltRate := 0.0;
                Bus_Num_Interrupt := 0.0;
            end;

    while pMeter <> nil do
    begin
        pMeter.CalcReliabilityIndices(AssumeRestoration, ActiveActor);
        pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Next;
    end;
end;

function DoVarCmd: Integer;
{Process Script variables}

var
    ParamName: String;
    Param: String;
    Str: String;
    iVar: Integer;
    MsgStrings: TStringList;

begin

    Result := 0;

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    if Length(Param) = 0 then  // show all vars
    begin
        if NoFormsAllowed then
            Exit;
          {
          MsgStrings := TStringList.Create;
          MsgStrings.Add('Variable, Value');
          for iVar := 1 to ParserVars.NumVariables  do
              MsgStrings.Add(ParserVars.VarString[iVar] );
          ShowMessageForm(MsgStrings);
          MsgStrings.Free;}
        Str := 'Variable, Value' + CRLF;
        for iVar := 1 to ParserVars.NumVariables do
            Str := Str + ParserVars.VarString[iVar] + CRLF;

        DoSimpleMsg(Str, 999345);


    end
    else
    if Length(ParamName) = 0 then   // show value of this var
    begin
        GlobalResult := Param;  // Parser substitutes @var with value
    end
    else
    begin
        while Length(ParamName) > 0 do
        begin
            case ParamName[1] of
                '@':
                    ParserVars.Add(ParamName, Param);
            else
                DosimpleMsg('Illegal Variable Name: ' + ParamName + '; Must begin with "@"', 28725);
                Exit;
            end;
            ParamName := Parser[ActiveActor].NextParam;
            Param := Parser[ActiveActor].StrValue;
        end;

    end;


end;

function DoRemoveCmd: Integer;
var
    ParamName: String;
    Param: String;
    Str: String;
    ParamPointer: Integer;
    DeviceIndex: Integer;

    FElementName: String;
    FKeepLoad: Boolean;
    FEditString: String;

    pPDElem: TPDelement;
    pMeter: TEnergyMeterObj;
    FMeterName: String;

begin

    Result := 0;

    FKeepLoad := true;
    ParamPointer := 0;

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    while Length(Param) > 0 do
    begin
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        else
            ParamPointer := RemoveCommands.GetCommand(ParamName);

        case ParamPointer of
            1:
                FElementName := Param; {ElementName}
            2:
                FkeepLoad := InterpretYesNo(Param); {KeepLoad}
            3:
                FEditString := Param; {EditString}
        else

        end;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
    end;

     // Check for existence of FelementName
    DeviceIndex := GetCktElementIndex(FElementName);
    if DeviceIndex = 0 then
    begin
        DoSimpleMsg('Error: Element ' + FelementName + ' does not exist in this circuit.', 28726);
    end
    else
    begin // Element exists  GO!

      // Set CktElement active
        SetObject(FelementName);

      // Get Energymeter associated with this element.
        if ActiveCircuit[ActiveActor].ActiveCktElement is TPDElement then
        begin
            pPDElem := ActiveCircuit[ActiveActor].ActiveCktElement as TPDElement;
            if pPDElem.SensorObj = nil then
                DoSimpleMsg(Format('Element %s.%s is not in a meter zone! Add an Energymeter. ', [pPDelem.Parentclass.Name, pPDelem.name]), 287261)
            else
            begin
                FMeterName := Format('%s.%s', [pPDElem.SensorObj.ParentClass.Name, pPDElem.SensorObj.Name]);
                SetObject(FMeterName);

                if ActiveCircuit[ActiveActor].ActiveCktElement is TEnergyMeterObj then
                begin
                    pMeter := ActiveCircuit[ActiveActor].ActiveCktElement as TEnergyMeterObj;
                // in ReduceAlgs
                    DoRemoveBranches(pMeter.BranchList, pPDelem, FKeepLoad, FEditString);
                end
                else
                    DoSimpleMsg('Error: The Sensor Object for ' + FelementName + ' is not an EnergyMeter object', 28727);
            end;
        end
        else
            DoSimpleMsg('Error: Element ' + FelementName + ' is not a power delivery element (PDElement)', 28728);

    end;


end;

initialization

{Initialize Command lists}

    SaveCommands := TCommandList.Create(['class', 'file', 'dir', 'keepdisabled']);
    SaveCommands.Abbrev := true;
    DI_PlotCommands := TCommandList.Create(['case', 'year', 'registers', 'peak', 'meter']);
    DistributeCommands := TCommandList.Create(['kW', 'how', 'skip', 'pf', 'file', 'MW', 'what']);
    DistributeCommands.Abbrev := true;

    ReconductorCommands := TCommandList.Create(['Line1', 'Line2', 'LineCode', 'Geometry', 'EditString', 'Nphases']);
    ReconductorCommands.Abbrev := true;

    RephaseCommands := TCommandList.Create(['StartLine', 'PhaseDesignation', 'EditString', 'ScriptFileName', 'StopAtTransformers']);
    RephaseCommands.Abbrev := true;

    AddMarkerCommands := TCommandList.Create(['Bus', 'code', 'color', 'size']);
    AddMarkerCommands.Abbrev := true;

    SetBusXYCommands := TCommandList.Create(['Bus', 'x', 'y']);
    SetBusXYCommands.Abbrev := true;

    PstCalcCommands := TCommandList.Create(['Npts', 'Voltages', 'dt', 'Frequency', 'lamp']);
    PstCalcCommands.abbrev := true;

    RemoveCommands := TCommandList.Create(['ElementName', 'KeepLoad', 'Editstring']);
    RemoveCommands.abbrev := true;

finalization

    DistributeCommands.Free;
    DI_PlotCommands.Free;
    SaveCommands.Free;
    AddMarkerCommands.Free;
    ReconductorCommands.Free;
    RephaseCommands.Free;
    SetBusXYCommands.Free;
    PstCalcCommands.Free;
    RemoveCommands.Free;

end.
