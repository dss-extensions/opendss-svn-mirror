unit ExecHelper;

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

Uses Command;


         FUNCTION DoNewCmd:Integer;
         FUNCTION DoEditCmd:Integer;
         FUNCTION DoSelectCmd:Integer;
         FUNCTION DoMoreCmd:Integer;
         FUNCTION DoRedirect(IsCompile:Boolean):Integer;
         FUNCTION DoSaveCmd:Integer;
         FUNCTION DoSampleCmd:Integer;
         FUNCTION DoShowCmd:Integer;
         FUNCTION DoExportCmd:Integer;
         FUNCTION DoSolveCmd:Integer;
         FUNCTION DoEnableCmd:Integer;
         FUNCTION DoDisableCmd:Integer;
         FUNCTION DoPlotCmd:Integer;
         FUNCTION DoOpenCmd:Integer;
         FUNCTION DoResetCmd:Integer;
         FUNCTION DoNextCmd:Integer;
         FUNCTION DoFormEditCmd:Integer;  
         FUNCTION DoClassesCmd:Integer;
         FUNCTION DoUserClassesCmd:Integer;
         FUNCTION DoHelpCmd:Integer;
         FUNCTION DoClearCmd:Integer;
         FUNCTION DoReduceCmd:Integer;
         FUNCTION DoInterpolateCmd:Integer;


         FUNCTION DoCloseCmd:Integer;
         FUNCTION DoResetMonitors:Integer;

         FUNCTION DoFileEditCmd:Integer;
         FUNCTION DoQueryCmd:Integer;
         FUNCTION DoResetMeters:Integer;
         PROCEDURE DoAboutBox;
         FUNCTION  DoSetVoltageBases:Integer;
         FUNCTION DoSetkVBase: Integer;

         PROCEDURE DoLegalVoltageBases;
         PROCEDURE DoAutoAddBusList(Const S:String);
         PROCEDURE DoKeeperBusList(Const S:String);
         PROCEDURE DoSetReduceStrategy(Const S:String);
         PROCEDURE DoSetAllocationFactors(Const X:Double);
         PROCEDURE DoSetCFactors(Const X:Double);

         FUNCTION DovoltagesCmd(Const PerUnit:Boolean): Integer;
         FUNCTION DocurrentsCmd :Integer;
         FUNCTION DopowersCmd :Integer;
         FUNCTION DoseqvoltagesCmd :Integer;
         FUNCTION DoseqcurrentsCmd :Integer;
         FUNCTION DoseqpowersCmd :Integer;
         FUNCTION DolossesCmd :Integer;
         FUNCTION DophaselossesCmd :Integer;
         FUNCTION DocktlossesCmd :Integer;
         FUNCTION DoAllocateLoadsCmd :Integer;
         FUNCTION DoHarmonicsList(const S:String):Integer;
         FUNCTION DoMeterTotals:Integer;
         FUNCTION DoCapacityCmd:Integer;
         FUNCTION DoZscCmd(Zmatrix:Boolean): Integer;
         FUNCTION DoZsc10Cmd: Integer;
         FUNCTION DoZscRefresh:Integer;

         FUNCTION DoBusCoordsCmd:Integer;
         FUNCTION DoVarValuesCmd:Integer;
         FUNCTION DoVarNamesCmd :Integer;

         FUNCTION DoMakePosSeq:Integer;
         FUNCTION DoAlignFileCmd:Integer;
         FUNCTION DoTOPCmd:Integer;
         FUNCTION DoRotateCmd:Integer;
         FUNCTION DoVDiffCmd:Integer;
         FUNCTION DoSummaryCmd:Integer;
         Function DoDistributeCmd:Integer;
         FUNCTION DoDI_PlotCmd:Integer;
         FUNCTION DoCompareCasesCmd:Integer;
         FUNCTION DoYearlyCurvesCmd:Integer;
         FUNCTION DoVisualizeCmd:Integer;
         FUNCTION DoCloseDICmd:Integer;
         FUNCTION DoADOScmd:Integer;
         FUNCTION DoEstimateCmd:Integer;

         PROCEDURE DoSetNormal(pctNormal:Double);

         PROCEDURE Set_Time;

         PROCEDURE ParseObjName(const fullname:String; VAR objname, propname:String);

         PROCEDURE GetObjClassAndName(VAR ObjClass,ObjName:String);

         FUNCTION AddObject(const ObjType, name:String):Integer;
         FUNCTION EditObject(const ObjType, name:String):Integer;

         PROCEDURE SetActiveCircuit(const cktname:String);

         FUNCTION SetActiveCktElement:Integer;

         FUNCTION DoPropertyDump:Integer;

Var   ShowCommands:TCommandList;

implementation

USES ArrayDef, ParserDel, SysUtils, DSSGlobals,
     Circuit, Monitor, ShowResults, ExportResults,
     DSSClass, DSSObject, Utilities, Solution,
     EnergyMeter, Generator, LoadShape, Load, PCElement,   CktElement,
     uComplex,  mathutil,  Bus,  SolutionAlgs, 
     DSSForms,  ExecCommands, Executive, DssPlot, Dynamics,
     Capacitor, Reactor, Line, Lineunits, Math, Classes,  CktElementClass, Sensor, FileCtrl;

Var
   SaveCommands, DistributeCommands, PlotCommands, DI_PlotCommands:TCommandList;

//----------------------------------------------------------------------------
PROCEDURE GetObjClassAndName(VAR ObjClass,ObjName:String);
VAR
   ParamName:String;
   Param:String;

Begin

{We're looking for Object Definition:

  ParamName = 'object' IF given
 and the name of the object

 Object=Capacitor.C1
or just Capacitor.C1

If no dot, last class is assumed
}
      ObjClass := '';
      ObjName := '';
      ParamName := LowerCase(Parser.NextParam);
      Param := Parser.StrValue;
      IF Length(ParamName)>0 THEN  Begin   // IF specified, must be object or an abbreviation
        IF ComparetextShortest(ParamName, 'object')<>0 THEN  Begin
          DoSimpleMsg('object=Class.Name expected as first parameter in command.'+ CRLF + parser.CmdString, 240);
          Exit;
        End;
      End;

      ParseObjectClassandName(Param, ObjClass, ObjName);     // see DSSGlobals

End;


//----------------------------------------------------------------------------
FUNCTION DoNewCmd:Integer;

// Process the New Command
// new type=xxxx name=xxxx  editstring

// IF the device being added already exists, the default behavior is to
// treat the New command as an Edit command.  This may be overridden
// by setting the DuplicatesAllowed VARiable to true, in which CASE,
// the New command always results in a new device being added.

VAR
   ObjClass, ObjName:String;
   handle:Integer;

Begin

     Result := 0;
     Handle := 0;

     GetObjClassAndName(ObjClass, ObjName);

     IF CompareText(ObjClass,'solution') = 0
     THEN Begin
         DoSimpleMsg('You cannot create new Solution objects through the command interface.', 241);
         Exit;
     End;

     IF   CompareText(ObjClass,'circuit') = 0
     THEN Begin
            MakeNewCircuit(ObjName);  // Make a new circuit
            ClearEventLog;      // Start the event log in the current directory
          End
     ELSE    // Everything else must be a circuit element or DSS Object
          Handle := AddObject(ObjClass, ObjName);

     IF Handle=0 THEN Result := 1;
     
End;

//----------------------------------------------------------------------------
FUNCTION DoEditCmd:Integer;

// edit type=xxxx name=xxxx  editstring
VAR
   ObjType, ObjName:String;

Begin

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     Begin

        // Everything ELSE must be a circuit element
        Result := EditObject(ObjType, ObjName);

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoRedirect(IsCompile:Boolean):Integer;

//  This routine should be recursive
//  So you can redirect input an arbitrary number of times

// If Compile, makes directory of the file the new home directory
// If not Compile (is simple redirect), return to where we started

VAR
    Fin:TextFile;
    ParamName,  InputLine, CurrDir, SaveDir:String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    ReDirFile := ExpandFileName(Parser.StrValue);

    IF ReDirFile <> '' THEN
    Begin

      SaveDir :=  GetCurrentDir;

      TRY
          AssignFile(Fin, ReDirFile);
          Reset(Fin);
          If IsCompile Then LastFileCompiled := ReDirFile;

      EXCEPT
         // Couldn't find file  Try appending a '.dss' to the file name
         // If it doesn't already have an extension
         IF   Pos('.', ReDirFile)=0
         THEN Begin
            ReDirFile := ReDirFile + '.dss';
            TRY
                AssignFile(Fin, ReDirFile);
                Reset(Fin);
            EXCEPT
                DoSimpleMsg('Redirect File: "' + ReDirFile + '" Not Found.', 242);
                SolutionAbort := TRUE;
                Exit;
            End;
         End
         ELSE Begin
               DoSimpleMsg('Redirect File: "'+ReDirFile+'" Not Found.', 243);
               SolutionAbort := True;
               Exit;  // Already had an extension, so just Bail
         End;

      END;

    // OK, we finally got one open, so we're going to continue
       TRY
          TRY
             // Change Directory to path specified by file in CASE that
             // loads in more files
             CurrDir := ExtractFileDir(ReDirFile);
             SetCurrentDir(CurrDir);
             If  IsCompile Then   SetDataPath(CurrDir);  // change dssdatadirectory

             Redirect_Abort := False;
             In_Redirect    := True;

             WHILE Not ( (EOF(Fin)) or (Redirect_Abort) ) DO
               Begin
                  Readln(Fin, InputLine);
                  If Not SolutionAbort Then ProcessCommand(InputLine)
                  Else Redirect_Abort := True;  // Abort file if solution was aborted
               End;

             IF ActiveCircuit <> Nil THEN ActiveCircuit.CurrentDirectory := CurrDir +'\';

          EXCEPT
             On E: Exception DO
                DoErrorMsg('DoRedirect'+CRLF+'Error Processing Input Stream in Compile/Redirect.',
                            E.Message,
                            'Error in File: "' + ReDirFile + '" or Filename itself.', 244);
          END;
      FINALLY
        CloseFile(Fin);
        In_Redirect := False;
        If  IsCompile Then   Begin
          SetDataPath(CurrDir); // change dssdatadirectory
          LastCommandWasCompile := True;
        End
        Else SetCurrentDir(SaveDir);    // set back to where we were for redirect, but not compile
      END;

    End;  // ELSE ignore altogether IF null filename


End;

//----------------------------------------------------------------------------
FUNCTION DoSelectCmd:Integer;

// select active object
// select element=elementname terminal=terminalnumber
VAR
   ObjClass, ObjName,
   ParamName, Param:String;

Begin

     Result := 1;

     GetObjClassAndName(ObjClass, ObjName);  // Parse Object class and name

     If (Length(ObjClass)=0) and (Length(ObjName)=0) Then Exit;  // select active obj if any

     IF CompareText(ObjClass, 'circuit')=0 THEN
     Begin
           SetActiveCircuit(ObjName);
     End
     ELSE
     Begin

        // Everything else must be a circuit element
        IF Length(ObjClass)>0 THEN SetObjectClass(ObjClass);

        ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
        IF ActiveDSSClass<>Nil THEN
        Begin
          IF Not ActiveDSSClass.SetActive(Objname) THEN
          Begin // scroll through list of objects untill a match
            DoSimpleMsg('Error! Object "' + ObjName + '" not found.'+ CRLF + parser.CmdString, 245);
            Result := 0;
          End
          ELSE
          WITH ActiveCircuit Do
          Begin
             CASE ActiveDSSObject.DSSObjType OF
                  DSS_OBJECT: ;  // do nothing for general DSS object

             ELSE Begin   // for circuit types, set ActiveCircuit Element, too
                   ActiveCktElement := ActiveDSSClass.GetActiveObj;
                   // Now check for active terminal designation
                   ParamName := LowerCase(Parser.NextParam);
                   Param := Parser.StrValue;
                   If Length(Param)>0
                   THEN ActiveCktElement.ActiveTerminalIdx := Parser.Intvalue
                   ELSE ActiveCktElement.ActiveTerminalIdx := 1;  {default to 1}
                   With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
                  End;
             End;
          End;
        End
        ELSE Begin
          DoSimpleMsg('Error! Active object type/class is not set.', 246);
          Result := 0;
        End;

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoMoreCmd:Integer;

// more editstring  (assumes active circuit element)
Begin
      IF ActiveDSSClass<>nil THEN Result := ActiveDSSClass.Edit
                             ELSE Result := 0;
End;


//----------------------------------------------------------------------------
FUNCTION DoSaveCmd:Integer;

// Save current values in both monitors and Meters

VAR
   pMon :TMonitorObj;
   pMtr :TEnergyMeterObj;
   i    :Integer;

   ParamPointer :Integer;
   ParamName,
   Param        :String;
   ObjClass     :String;
   SaveDir      :String;
   saveFile     :String;
   DSSClass     :TDSSClass;

Begin
     Result := 0;
     ObjClass := '';
     SaveDir := '';
     SaveFile := '';
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)  THEN Inc(ParamPointer)
         ELSE ParamPointer := SaveCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: ObjClass := Parser.StrValue;
           2: Savefile := Parser.StrValue;   // File name for saving  a class
           3: SaveDir := Parser.StrValue;
         ELSE

         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

   InShowResults := True;
   If (Length(ObjClass)=0) or (CompareTextShortest( ObjClass, 'meters')=0 ) then Begin
   // Save monitors and Meters

     WITH ActiveCircuit.Monitors Do
     FOR i := 1 to ListSize Do
     Begin
         pMon := Get(i);
         pMon.Save;
     End;

     WITH ActiveCircuit.EnergyMeters Do
     FOR i := 1 to ListSize Do
     Begin
         pMtr := Get(i);
         pMtr.SaveRegisters;
     End;

     Exit;
   End;
   If CompareTextShortest( ObjClass, 'circuit')=0 then  Begin
      IF not ActiveCircuit.Save(SaveDir) Then Result := 1;
      Exit;
   End;
   If CompareTextShortest( ObjClass, 'voltages')=0 then  Begin
      ActiveCircuit.Solution.SaveVoltages;
      Exit;
   End;

   {Assume that we have a class name for a DSS Class}
   DSSClass :=  GetDSSClassPtr(ObjClass);
   If DSSClass <> Nil Then Begin
     IF Length(SaveFile)=0 Then SaveFile := objClass;
     IF Length(SaveDir)>0 Then begin
       If not DirectoryExists(SaveDir) Then
          Try
             mkDir(SaveDir);
          Except
             On E:Exception Do DoSimpleMsg('Error making Directory: "'+SaveDir+'". ' + E.Message, 247);
          End;
       SaveFile := SaveDir+'\'+SaveFile;
     End;
     WriteClassFile(DSSClass, SaveFile, FALSE); // just write the class with no checks
   End;

   GlobalResult := SaveFile;

End;


//----------------------------------------------------------------------------
FUNCTION DoClearCmd:Integer;

Begin

      DSSExecutive.Clear;

      Result := 0;

End;

//----------------------------------------------------------------------------
FUNCTION DoHelpCmd:Integer;
Begin
    ShowHelpForm; // DSSForms Unit
    Result := 0;
End;


//----------------------------------------------------------------------------
FUNCTION DoSampleCmd:Integer;

// FORce all monitors and meters in active circuit to take a sample

VAR
   pMon :TMonitorObj;
   pMtr :TEnergyMeterObj;
   pGen :TGeneratorObj;
   i    :Integer;

Begin

   WITH ActiveCircuit.Monitors Do
   FOR i := 1 to ListSize Do Begin
       pMon := Get(i);
       pMon.TakeSample;
   End;

   WITH ActiveCircuit.EnergyMeters Do
   FOR i := 1 to ListSize Do Begin
       pMtr := Get(i);
       pMtr.TakeSample;
   End;

   WITH ActiveCircuit.Generators Do
   FOR i := 1 to ListSize Do Begin
       pGen := Get(i);
       pGen.TakeSample;
   End;


   Result := 0;

End;



//----------------------------------------------------------------------------
FUNCTION DoShowCmd:Integer;

VAR
   ParamName, Param, Filname:String;
   ParamPointer :Integer;
   pMon:TMonitorObj;

   MVAopt :Integer;
   LLopt:Boolean;
   ShowResid:Boolean;
   ShowOptionCode:Integer;
   BusName:String;
   Freq:Double;
   Units:Integer;


Begin

   ParamName := Parser.NextParam;
   Param := LowerCase(Parser.StrValue);
   ParamPointer := ShowCommands.Getcommand (Param);

   If ParamPointer=0 Then ParamPointer := 13;  {voltages}

   InShowResults := True;

   CASE ParamPointer OF
     1:  Begin {Autoadded}
           FireOffEditor(DSSDataDirectory + CircuitName_ + 'AutoAddedGenerators.Txt');
           FireOffEditor(DSSDataDirectory + CircuitName_ + 'AutoAddedCapacitors.Txt');
         End;
     2: ShowBuses(DSSDataDirectory + CircuitName_ + 'Buses.Txt');
     3: Begin
           ShowOptionCode := 0;
           ShowResid := FALSE;
           ParamName := Parser.NextParam;   // Look for residual
           Param := Uppercase(Parser.StrValue);
           // logic handles show curr y|n|T elements or show curr elements
           If  (Length(Param)> 0) Then
             Case Param[1] of
               'Y','T': ShowResid := TRUE;
               'N': ShowResid := FALSE;
               'E': ShowOptionCode := 1;
             End;
           ParamName := Parser.NextParam;   // Look for another param
           Param := Uppercase(Parser.StrValue);
           If (Length(Param)>0) Then
             Case Param[1] of
                'E':ShowOptionCode := 1;
             END;
           CASE ShowOptionCode of
             0:  Filname := 'Curr_Seq';
             1:  Filname := 'Curr_Elem';
           END;
           ShowCurrents(DSSDataDirectory + CircuitName_ + FilName + '.Txt', ShowResid, ShowOptionCode);
          End;
     4: ActiveCircuit.Solution.WriteConvergenceReport(DSSDataDirectory + CircuitName_ + 'Convergence.TXT');
     5 : Begin
             ParamName := Parser.NextParam;   // Look for another param
             Param := LowerCase(Parser.StrValue);
             ShowElements(DSSDataDirectory + CircuitName_ + 'Elements.Txt', Param);
           End;
     6: ShowFaultStudy(DSSDataDirectory + CircuitName_ + 'FaultStudy.Txt');
     7: ShowIsolated(DSSDataDirectory + CircuitName_ + 'Isolated.Txt');
     8: ShowGenMeters(DSSDataDirectory + CircuitName_ + 'GenMeterOut.Txt');
     9: ShowMeters(DSSDataDirectory + CircuitName_ + 'EMout.Txt');
     10:  Begin     // Show Monitor
             ParamName := Parser.NextParam;
             Param := Parser.StrValue;
             IF Length(Param)>0 THEN
             Begin
               pMon:=MonitorClass.Find(Param);
               IF pMon<>Nil THEN
                 pMon.TranslateToCSV(TRUE)
               ELSE DoSimpleMsg('Monitor "'+param+'" not found.'+ CRLF + parser.CmdString, 248);
             End
             ELSE   DoSimpleMsg('Monitor Name Not Specified.'+ CRLF + parser.CmdString, 249);
          End;
     11: ShowControlPanel;    // see DSSGlobals
     12: Begin
            ShowOptionCode := 0;
            MVAOpt := 0;
            FilName := 'Power';
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            IF Length(Param) > 0 THEN
              CASE Param[1] of
                'm': MVAOpt := 1;
                'e': ShowOptionCode := 1;
              End;
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            IF Length(Param) > 0 THEN IF Param[1]='e' THEN ShowOptionCode := 1;
            If ShowOptionCode=1 Then FilName := FilName + '_elem'
            Else FilName := FilName + '_seq';
            If MVAOpt=1 Then FilName := FilName + '_MVA'
            Else FilName := FilName + '_kVA';

            ShowPowers(DSSDataDirectory + CircuitName_ + filname + '.txt', MVAOpt, ShowOptionCode);
          End;
     13:Begin
            LLOpt := FALSE;      // Line-Line voltage option
            ShowOptionCode := 0;
            {Check for LL or LN option}
            Paramname := parser.nextParam;
            Param := Parser.strvalue;

            FilName := 'VLN';
            IF Length(Param) > 0 THEN IF CompareText(Param, 'LL')=0 THEN
              Begin
               LLopt := TRUE;
               FilName := 'VLL';
              End;
            {Check for Seq | nodes | elements}
            Paramname := parser.nextParam;
            Param := UpperCase(Parser.strvalue);
            If Length(Param)>0 Then
               Case Param[1] of
                 'N': Begin ShowOptionCode := 1;  FilName := FilName + '_Node'; End;
                 'E': Begin ShowOptionCode := 2;  FilName := FilName + '_elem'; End;
               Else
                  FilName := FilName + '_seq';
               End;
            ShowVoltages(DSSDataDirectory + CircuitName_ + FilName + '.Txt', LLopt, ShowOptionCode);
        End;
     14: ShowMeterZone(DSSDataDirectory + CircuitName_ + 'ZoneOut.Txt');
     15: ShowRegulatorTaps(DSSDataDirectory + CircuitName_ + 'RegTaps.Txt');
     16: ShowOverloads(DSSDataDirectory + CircuitName_ + 'Overload.Txt');
     17: Begin
             ParamName := Parser.NextParam;
             Param := Parser.StrValue;
             IF Length(Param)>0
             THEN ShowUnserved(DSSDataDirectory + CircuitName_ + 'Unserved.Txt', TRUE)
             ELSE ShowUnserved(DSSDataDirectory + CircuitName_ + 'Unserved.Txt', FALSE);
          End;
     18: ShowMessageForm(EventStrings);
     19: ShowVariables(DSSDataDirectory + CircuitName_ + 'Variables.Txt');
     20: ShowRatings(DSSDataDirectory + CircuitName_ + 'RatingsOut.Txt');
     21: ShowLoops(DSSDataDirectory + CircuitName_ + 'Loops.Txt');
     22: ShowLosses(DSSDataDirectory + CircuitName_ + 'Losses.Txt');
     23: Begin  // Show Bus Power Report
            ShowOptionCode := 0;
            MVAOpt := 0;
            Paramname := parser.nextParam; // Get busname
            Busname := Parser.strvalue;
            If Length(BusName)>0 Then FilName := BusName
                                 Else FilName := 'BusPower';
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            IF Length(Param) > 0 THEN
              CASE Param[1] of
                'm': MVAOpt := 1;
                'e': ShowOptionCode := 1;
              End;
            Paramname := parser.nextParam;
            Param := LowerCase(Parser.strvalue);
            IF Length(Param) > 0 THEN IF Param[1]='e' THEN ShowOptionCode := 1;
            If ShowOptionCode=1 Then FilName := FilName + '_elem'
            Else FilName := FilName + '_seq';
            If MVAOpt=1 Then FilName := FilName + '_MVA'
            Else FilName := FilName + '_kVA';

            ShowBusPowers(DSSDataDirectory + CircuitName_ + FilName + '.txt', BusName, MVAOpt, ShowOptionCode);
          End;
      24: Begin {ShowLineConstants  Show Lineconstants 60 mi}
             Freq := 60.0;  // Default
             Units := UNITS_KFT; // 'kft'; // default
             ParamName := parser.nextparam;
             If Length(Parser.strvalue)>0 Then Freq := Parser.dblvalue;
             ParamName := parser.nextparam;
             If Length(Parser.strvalue)>0 Then Units := GetUnitsCode(Parser.strvalue);
             ShowLineConstants(DSSDataDirectory + CircuitName_ + 'LineConstants.txt', freq, units);
          End;

      25: If ActiveCircuit<>nil then Begin  {Yprim}
             With ActiveCircuit.ActiveCktElement Do
             ShowYprim(DSSDataDirectory + ParentClass.name + '_' + name + '_Yprim.txt' );
          End;

      26: Begin   {Y}
             ShowY(DSSDataDirectory + CircuitName_  + 'SystemY.txt' );
          end;

   ELSE
   End;

   Result := 0;
   InShowResults := False;

End;

//----------------------------------------------------------------------------
FUNCTION DoExportCmd:Integer;

VAR
   ParamName,
   Parm1,
   Parm2,
   FileName :String;
   TestChar :Char;

   MVAopt :Integer;
   UEonlyOpt:Boolean;
   pMon      :TMonitorObj;

Begin

   ParamName := Parser.NextParam;
   Parm1 := LowerCase(Parser.StrValue);
   IF Length(Parm1)=0 THEN Testchar := 'v' ELSE TestChar := Parm1[1];

   MVAOpt := 0;
   UEonlyOpt := FALSE;

   CASE TestChar OF
     'p': Begin { Trap export powers command and look for MVA/kVA option }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            MVAOpt := 0;
            IF Length(Parm2) > 0 THEN IF Parm2[1]='m' THEN MVAOpt := 1;
          End;

     'm': IF Parm1[2] <> 'e'
          THEN Begin {Get monitor name for export monitors command}
             ParamName := Parser.NextParam;
             Parm2 := Parser.StrValue;
          End;

     'u': Begin { Trap UE only flag  }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            UEonlyOpt := FALSE;
            IF Length(Parm2) > 0 THEN IF Parm2[1]='u' THEN UEonlyOpt := TRUE;
          End;
   End;

   {Pick up last parameter on line, alternate file name, if any}
   ParamName := Parser.NextParam;
   FileName := LowerCase(Parser.StrValue);    // should be full path name to work universally

   InShowResults := True;

   {Assign default file name if alternate not specified}
   IF Length(FileName) = 0 then Begin
       CASE TestChar OF
         'c': FileName := 'EXP_CURRENTS.CSV';
         'e': FileName := 'EXP_ESTIMATION.CSV';   // Estimation error
         'f': FileName := 'EXP_FAULTS.CSV';
         'p': FileName := 'EXP_POWERS.CSV';
         'g': FileName := 'EXP_GENMETERS.CSV';
         'l': FileName := 'EXP_LOADS.CSV';
         'o': FileName := 'EXP_OVERLOADS.CSV';
         'u': FileName := 'EXP_UNSERVED.CSV';
         'm': If Parm1[2]='e' THEN FileName := 'EXP_METERS.CSV';
         's': If Comparetext(Copy(Parm1,1,4), 'seqc')=0
              THEN FileName := 'EXP_SEQCURRENTS.CSV'
              ELSE FileName := 'EXP_SEQVOLTAGES.CSV';
         'y': If Parm1[2]='p' Then Filename := 'EXP_YPRIM.CSV'
                              Else Filename := 'EXP_Y.CSV';
       ELSE
          FileName := 'EXP_VOLTAGES.CSV';
       END;
       FileName := DSSDataDirectory + CircuitName_ + FileName;  // Explicitly define directory
   End;


   CASE TestChar OF
     'c': ExportCurrents(FileName);
     'e': ExportEstimation(FileName);
     'f': ExportFaultStudy(FileName);
     'p': ExportPowers(FileName, MVAOpt);
     'g': ExportGenMeters(FileName);
     'l': ExportLoads(FileName);
     'o': ExportOverLoads(FileName);
     'u': ExportUnserved(FileName, UEOnlyOpt);
     'm': CASE Parm1[2] of
          'e': ExportMeters(FileName);
          ELSE Begin     // Export Monitor
             IF   Length(Parm2) > 0
             THEN Begin
               pMon:=MonitorClass.Find(Parm2);
               IF   pMon <> NIL  THEN pMon.TranslateToCSV(FALSE)
                                 ELSE DoSimpleMsg('Monitor "'+Parm2+'" not found.'+ CRLF + parser.CmdString, 250);
             End
             ELSE   DoSimpleMsg('Monitor Name Not Specified.'+ CRLF + parser.CmdString, 251);
           End; {Else}
          END;  {CASE}

     's': If Comparetext(Copy(Parm1,1,4), 'seqc')=0
          THEN ExportSeqCurrents(FileName)
          ELSE ExportSeqVoltages(FileName);
     'y': If Parm1[2]='p' Then ExportYprim(Filename)
                          Else ExportY(Filename);
   ELSE
      ExportVoltages(FileName);
   End;

   Result := 0;
   InShowResults := False;

End;


//----------------------------------------------------------------------------
FUNCTION DoSolveCmd:Integer;
Begin
   // just invoke solution obj's editor to pick up parsing and execute rest of command
   ActiveSolutionObj := ActiveCircuit.Solution;
   Result := SolutionClass.Edit;

End;


//----------------------------------------------------------------------------
FUNCTION SetActiveCktElement:Integer;

// Parses the object off the line and sets it active as a circuitelement.

VAR
   ObjType, ObjName:String;

Begin

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     Begin

        IF CompareText(ObjType, ActiveDSSClass.Name)<>0 THEN LastClassReferenced := ClassNames.Find(ObjType);

        CASE LastClassReferenced of
          0: Begin
                 DoSimpleMsg('Object Type "' + ObjType + '" not found.'+ CRLF + parser.CmdString, 253);
                 Result := 0;
                 Exit;
             End;{Error}
        ELSE

        // intrinsic and user Defined models
           ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
           IF ActiveDSSClass.SetActive(ObjName) THEN
           WITH ActiveCircuit Do
           Begin // scroll through list of objects until a match
             CASE ActiveDSSObject.DSSObjType OF
                    DSS_OBJECT: DoSimpleMsg('Error in SetActiveCktElement: Object not a circuit Element.'+ CRLF + parser.CmdString, 254);
             ELSE Begin
                    ActiveCktElement := ActiveDSSClass.GetActiveObj;
                    Result:=1;
                  End;
             End;
           End;
        End;
     End;
End;


//----------------------------------------------------------------------------
FUNCTION DoEnableCmd:Integer;

Var Objtype, ObjName:String;
    ClassPtr:TDSSClass;
    CktElem:TCktElement;
    i:Integer;


Begin

  //   Result := SetActiveCktElement;
  //  IF Result>0 THEN ActiveCircuit.ActiveCktElement.Enabled := True;

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     If Length(ObjType)>0 Then Begin
      // only applies to CktElementClass objects
       ClassPtr := GetDSSClassPtr(ObjType);
       If ClassPtr<> Nil Then Begin

         If (ClassPtr.DSSClassType and BASECLASSMASK) > 0  then Begin
              // Everything else must be a circuit element
             If CompareText(ObjName,'*') = 0 Then Begin
               // Enable all elements of this class
               For i := 1 to ClassPtr.ElementCount Do Begin
                 CktElem := ClassPtr.ElementList.Get(i);
                 CktElem.Enabled := TRUE;
               End;

             End
             Else Begin

              // just load up the parser and call the edit routine for the object in question

              Parser.CmdString := 'Enabled=true';  // Will only work for CktElements
              Result := EditObject(ObjType, ObjName);
             End;
         End;
       End;
     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoDisableCmd:Integer;

Var Objtype, ObjName:String;
    ClassPtr:TDSSClass;
    CktElem:TCktElement;
    i:Integer;


Begin
     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     If Length(ObjType)>0 Then Begin
      // only applies to CktElementClass objects
       ClassPtr := GetDSSClassPtr(ObjType);
       If ClassPtr<> Nil Then Begin

         If (ClassPtr.DSSClassType and BASECLASSMASK) > 0  then Begin
              // Everything else must be a circuit element
             If CompareText(ObjName,'*') = 0 Then Begin
               // Disable all elements of this class
               For i := 1 to ClassPtr.ElementCount Do Begin
                 CktElem := ClassPtr.ElementList.Get(i);
                 CktElem.Enabled := FALSE;
               End;

             End
             Else Begin

              // just load up the parser and call the edit routine for the object in question

              Parser.CmdString := 'Enabled=false';  // Will only work for CktElements
              Result := EditObject(ObjType, ObjName);
             End;
         End;
       End;
     End;

//     Result := SetActiveCktElement;
//     IF Result>0 THEN ActiveCircuit.ActiveCktElement.Enabled := False;
End;

//----------------------------------------------------------------------------
FUNCTION DoPropertyDump:Integer;

VAR
   pObject:TDSSObject;
   F:TextFile;
   SingleObject, Debugdump, IsSolution:Boolean;
   i:Integer;
   FileName:String;
   ParamName:String;
   Param, Param2, ObjClass, ObjName:String;

Begin

 Result := 0;
 SingleObject := False;
 IsSolution := False;
 DebugDump := False;
 ObjClass := ' ';  // make sure these have at least one character
 ObjName := ' ';
 
 // Continue parsing command line - check for object name
 ParamName := Parser.NextParam;
 Param := Parser.StrValue;
 IF Length(Param)>0 THEN
 Begin

    IF CompareText(Param, 'commands')=0 THEN
    If Not NoFormsAllowed Then Begin
        DumpAllDSSCommands(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    IF CompareText(Copy(lowercase(Param),1,5), 'alloc')=0 THEN
    Begin
        DumpAllocationFactors(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    IF CompareText(Param,'debug')=0 THEN
       DebugDump := TRUE
    ELSE
    Begin

       IF CompareText(Param,'solution')=0 THEN
         Begin
          // Assume active circuit solution IF not qualified
          ActiveDSSClass := SolutionClass;
          ActiveDSSObject := ActiveCircuit.Solution;
          IsSolution := TRUE;
         End
       ELSE
         Begin
            SingleObject := TRUE;
           // Check to see IF we want a debugdump on this object
            ParamName := Parser.NextParam;
            Param2 := Parser.StrValue;
            IF CompareText(Param2,'debug')=0 THEN DebugDump := TRUE;
            // Set active Element to be value in Param
            Parser.CmdString := '"' + Param + '"';  // put param back into parser
            GetObjClassAndName( ObjClass, ObjName);
            // IF DoSelectCmd=0 THEN Exit;  8-17-00
            IF SetObjectClass(ObjClass)
            THEN Begin
              ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
              IF ActiveDSSClass = NIL Then Exit;
            End
            ELSE Exit;
         End;
    End;
 End;

  TRY
      AssignFile(F, DSSDataDirectory + CircuitName_ + 'PropertyDump.Txt');
      Rewrite(F);
  EXCEPT
      On E:Exception DO
      Begin
        DoErrorMsg('DoPropertyDump - opening '+ DSSDataDirectory +' DSS_PropertyDump.txt for writing in '+Getcurrentdir, E.Message, 'Disk protected or other file error', 255);
        Exit;
      End;
  End;


  TRY

      IF SingleObject THEN
      Begin

        {IF ObjName='*' then we dump all objects of this class}
        CASE ObjName[1] of
           '*':Begin
                  FOR i := 1 to ActiveDSSClass.ElementCount Do
                  Begin
                      ActiveDSSClass.Active := i;
                      ActiveDSSObject.DumpProperties(F, DebugDump);
                  End;
               End;
        ELSE
           IF Not ActiveDSSClass.SetActive(Objname)
           THEN Begin
               DoSimpleMsg('Error! Object "' + ObjName + '" not found.', 256) ;
               Exit;
           End
           ELSE ActiveDSSObject.DumpProperties(F, DebugDump);  // Dump only properties of active circuit element
        END;

      End
      ELSE IF IsSolution THEN  Begin
         ActiveDSSObject.DumpProperties(F, DebugDump);
      End
      ELSE Begin

        // Dump general Circuit stuff

        IF DebugDump THEN ActiveCircuit.DebugDump(F);
        // Dump circuit objects
        TRY
          pObject := ActiveCircuit.CktElements.First;
          WHILE pObject <> Nil DO
          Begin
              pObject.DumpProperties(F, DebugDump);
              pObject := ActiveCircuit.CktElements.Next;
          End;
          pObject := DSSObjs.First;
          WHILE pObject <> Nil DO
          Begin
              pObject.DumpProperties(F, DebugDump);
              pObject := DSSObjs.Next;
          End;
        EXCEPT
            On E:Exception DO
              DoErrorMsg('DoPropertyDump - Problem writing file.', E.Message, 'File may be read only, in use, or disk full?', 257);
        End;

        ActiveCircuit.Solution.DumpProperties(F,DebugDump);
      End;

  FINALLY

         CloseFile(F);
  END;  {TRY}

  FireOffEditor(DSSDataDirectory + CircuitName_ + 'PropertyDump.Txt');

End;



//----------------------------------------------------------------------------
PROCEDURE Set_Time;

// for interpreting time specified as an array "hour, sec"
VAR

   TimeArray:Array[1..2] of double;

Begin
     Parser.ParseAsVector(2, @TimeArray);
     WITH ActiveCircuit.Solution DO
     Begin
        Hour := Round(TimeArray[1]);
        DynaVars.t := TimeArray[2];
     End;
End;

//----------------------------------------------------------------------------
PROCEDURE SetActiveCircuit(const cktname:String);

VAR
   pCkt:TCircuit;
Begin

   pCkt := Circuits.First;
   WHILE pCkt<>nil DO
   Begin
       IF CompareText(pCkt.Name, cktname)=0 THEN
       Begin
           ActiveCircuit := pCkt;
           Exit;
       End;
       pCkt := Circuits.Next;
   End;

   // IF none is found, just leave as is after giving error

   DoSimpleMsg('Error! No circuit named "' + cktname + '" found.' + CRLF +
               'Active circuit not changed.', 258);
End;

{-------------------------------------------}
PROCEDURE DoLegalVoltageBases;

VAR
   Dummy :pDoubleArray;
   i,
   Num   :Integer;

Begin

     Dummy := AllocMem(Sizeof(Dummy^[1]) * 100); // Big Buffer
     Num   := Parser.ParseAsVector(100, Dummy);
     {Parsing zero-fills the array}

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

     WITH ActiveCircuit Do
     Begin
       Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1])*(Num+1));
       FOR i := 1 to Num+1 Do LegalVoltageBases^[i] := Dummy^[i];
     End;

     Reallocmem(Dummy, 0);
End;

//----------------------------------------------------------------------------
FUNCTION DoPlotCmd:Integer;

{
  Produce a plot with the DSSGraphX object
}

Var

   ParamName, Param:String;
   ParamPointer, i:Integer;
   DblBuffer:Array[0..50] of Double;
   NumChannels:Integer;

Begin
    Result := 0;

    If NoFormsAllowed Then Begin Result :=1; Exit; End;

    If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;

    DSSPlotObj.SetDefaults;

    {Get next parameter on command line}
    ParamPointer := 0;
    ParamName := Uppercase(Parser.NextParam);
    Param := Uppercase(Parser.StrValue);
    While Length(Param) > 0 Do
     Begin
      {Interpret Parameter}
       IF   (Length(ParamName) = 0)  THEN Inc(ParamPointer)
       ELSE  ParamPointer := PlotCommands.Getcommand (ParamName);

       With DSSPlotObj Do
       Case ParamPointer of

         1: Case Param[1] of
               'A': Begin
                     PlotType   := ptAutoAddLogPlot;
                     ObjectName := CircuitName_ + 'AutoAddLog.CSV';
                     ValueIndex := 2;
                    End;
               'C': PlotType := ptCircuitplot;
               'G': PlotType := ptGeneralDataPlot;
               'M': PlotType := ptMonitorplot;
               'D': PlotType := ptDaisyplot;
               'Z': PlotType := ptMeterZones;
            Else
            End;
         2: Case Param[1] of
               'V': Quantity := pqVoltage;
               'C': Case Param[2] of
                    'A': Quantity := pqcapacity;
                    'U': Quantity := pqcurrent;
                    End;
               'P': Quantity := pqpower;
               'L': Quantity := pqlosses;
             Else
               Quantity := pqNone;
               Valueindex := Parser.IntValue;
             End;
         3:  Begin
                 MaxScale := Parser.DblValue;
                 If MaxScale>0.0 Then MaxScaleIsSpecified := TRUE;    // Indicate the user wants a particular value
             End;
         4:  Dots := InterpretYesNo(Param);
         5:  Labels := InterpretYesNo(Param);
         6:  ObjectName := Parser.StrValue;
         7:   Begin
                ShowLoops := InterpretYesNo(Param);
                If ShowLoops then PlotType := ptMeterzones;
              End;
         8:   TriColorMax := Parser.DblValue;
         9:   TriColorMid := Parser.DblValue;
         10:  Color1 := Parser.IntValue;
         11:  Color2 := Parser.IntValue;
         12:  Color3 := Parser.IntValue;
         13: Begin    {Channel definitions for Plot Monitor}
               NumChannels := Parser.ParseAsVector(51, @DblBuffer);  // allow up to 50 channels
               If NumChannels>0 Then Begin   // Else take the defaults
                 SetLength(Channels, NumChannels);
                 For i := 0 to NumChannels-1 Do Channels[i] := Round(DblBuffer[i]);
                 SetLength(Bases, NumChannels);
                 For i := 0 to NumChannels-1 Do Bases[i] :=1.0;
               End;
             End;
         14: Begin
               NumChannels  := Parser.ParseAsVector(51, @DblBuffer);  // allow up to 50 channels
               If NumChannels>0 Then Begin
                  SetLength(Bases, NumChannels);
                 For i := 0 to NumChannels-1 Do Bases[i] := DblBuffer[i];
               End;
             End;
         15: ShowSubs := InterpretYesNo(Param);
         16: MaxLineThickness := Parser.IntValue ;
       Else
       End;


      ParamName := Uppercase(Parser.NextParam);
      Param := Uppercase(Parser.StrValue);
     End;

     If Not ActiveCircuit.Issolved Then DSSPlotObj.Quantity := pqNone;

     With DSSPlotObj Do Begin

        Execute;   // makes a new plot based on these options

     End;

End;


//----------------------------------------------------------------------------
FUNCTION DoOpenCmd:Integer;
// Opens a terminal and conductor of a ckt Element
VAR
   retval:Integer;
   Terminal:Integer;
   Conductor:Integer;
   ParamName : string;

// syntax:  "Open class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened.

Begin
  retval := SetActiveCktElement;
  IF retval>0 THEN
  Begin
        ParamName := Parser.NextParam;
        Terminal := Parser.IntValue;
        ParamName := Parser.NextParam;
        Conductor := Parser.IntValue;

        With ActiveCircuit Do
         Begin
          ActiveCktElement.ActiveTerminalIdx := Terminal;
          ActiveCktElement.Closed[Conductor] := FALSE;
          With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
         End;
  End
  ELSE
  Begin
       DoSimpleMsg('Error in Open Command: Circuit Element Not Found.' +CRLF+ Parser.CmdString, 259);
  End;
  Result := 0;
End;



//----------------------------------------------------------------------------
FUNCTION DoCloseCmd:Integer;
// Closes a terminal and conductor of a ckt Element
VAR
   retval:Integer;
   Terminal:Integer;
   Conductor:Integer;
   ParamName : string;

// syntax:  "Close class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened

Begin
  retval := SetActiveCktElement;
  IF retval>0 THEN
    Begin
       ParamName := Parser.NextParam;                 
       Terminal := Parser.IntValue;
       ParamName := Parser.NextParam;
       Conductor := Parser.IntValue;

        With ActiveCircuit Do
         Begin
          ActiveCktElement.ActiveTerminalIdx := Terminal;
          ActiveCktElement.Closed[Conductor] := TRUE;
          With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
         End;

    End
  ELSE
  Begin
       DoSimpleMsg('Error in Close Command: Circuit Element Not Found.' +CRLF+ Parser.CmdString, 260);
  End;
  Result := 0;

End;

//----------------------------------------------------------------------------
FUNCTION DoResetCmd:Integer;
VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    Param := UpperCase(Parser.StrValue);
    IF Length(Param) = 0
       THEN Begin
            DoResetMonitors;
            DoResetMeters;
            DoResetFaults ;
            DoResetControls;
            ClearEventLog;
            DoResetKeepList;
       End
    ELSE
      CASE Param[1] of
       'M': CASE Param[2] of
               'O'{MOnitor}:  DoResetMonitors;
               'E'{MEter}:    DoResetMeters;
            END;
       'F'{Faults}:   DoResetFaults;
       'C'{Controls}: DoResetControls;
       'E'{EventLog}: ClearEventLog;
       'K': DoResetKeepList;

      ELSE

         DoSimpleMsg('Unknown argument to Reset Command: "'+ Param+'"', 261);

      End;

End;

procedure MarkCapandReactorBuses;
Var
    pClass:TDSSClass;
    pCapElement:TCapacitorObj;
    pReacElement:TReactorObj;
    ObjRef:Integer;

begin
{Mark all buses as keepers if there are capacitors or reactors on them}
    pClass :=  GetDSSClassPtr('capacitor');
    If pClass<>Nil then
    Begin
       ObjRef := pClass.First;
       While Objref>0 Do
       Begin
          pCapElement := TCapacitorObj(ActiveDSSObject);
          If pCapElement.IsShunt Then
          Begin
             If pCapElement.Enabled Then With ActiveCircuit do Buses^[pCapElement.Terminals^[1].Busref].Keep := TRUE;
          End;
          ObjRef := pClass.Next;
       End;
    End;

    {Now Get the Reactors}

    pClass :=  GetDSSClassPtr('reactor');
    If pClass<>Nil then
    Begin
       ObjRef := pClass.First;
       While Objref>0 Do
       Begin
          pReacElement := TReactorObj(ActiveDSSObject);
          If pReacElement.IsShunt Then
          Begin
             If pReacElement.Enabled Then ActiveCircuit.Buses^[pReacElement.Noderef^[1]].Keep := TRUE;
          End;
          ObjRef := pClass.Next;
       End;
    End;
end;

//----------------------------------------------------------------------------
FUNCTION DoReduceCmd:Integer;
VAR
    MetObj:TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    ParamName, Param  :String;
    DevClassIndex:Integer;

Begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    Param := UpperCase(Parser.StrValue);

    {Mark Capacitor and Reactor buses as Keep so we don't lose them}
    MarkCapandReactorBuses;

    IF Length(Param) = 0  Then Param := 'A';
    CASE Param[1] of
     'A': Begin
              metobj := ActiveCircuit.EnergyMeters.First;
              While metobj <> nil Do
              Begin
                MetObj.ReduceZone;
                MetObj := ActiveCircuit.EnergyMeters.Next;
              End;
          End;

    ELSE
       {Reduce a specific meter}
       DevClassIndex := ClassNames.Find('energymeter');
       IF DevClassIndex > 0 THEN
       Begin
          MeterClass := DSSClassList.Get(DevClassIndex);
          If MeterClass.SetActive (Param) Then   // Try to set it active
          Begin
            MetObj := MeterClass.GetActiveObj;
            MetObj.ReduceZone;
          End
          Else DoSimpleMsg('EnergyMeter "'+Param+'" not found.', 262);
       End;
    End;

End;

//----------------------------------------------------------------------------
FUNCTION DoResetMonitors:Integer;
VAR
   pMon:TMonitorObj;

Begin

     WITH ActiveCircuit DO
     Begin

        pMon := Monitors.First;
        WHILE pMon<>nil DO
        Begin
            pMon.ResetIt;
            pMon := Monitors.Next;
        End;
        Result :=0;

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoFileEditCmd:Integer;

VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;

    IF  FileExists(Param) THEN FireOffEditor(Param)
    ELSE Begin
       GlobalResult := 'File "'+param+'" does not exist.';
       Result := 1;
    End;
End;

//----------------------------------------------------------------------------
PROCEDURE ParseObjName(const fullname:String; VAR objname, propname:String);

{ Parse strings such as

    1. Classname.Objectname,Property    (full name)
    2. Objectname.Property   (classname omitted)
    3. Property           (classname and objectname omitted
}

VAR
  DotPos1, DotPos2:Integer;

Begin
     DotPos1 := Pos('.',fullname);
     CASE Dotpos1 of

        0: Begin
               Objname  := '';
               PropName := FullName;
           End;

       ELSE Begin

          PropName := Copy(FullName,Dotpos1+1,(Length(FullName)-DotPos1));
          DotPos2  := Pos('.', PropName);
          CASE DotPos2 of

             0: Begin
                    ObjName := Copy(FullName,1,DotPos1-1);
                End;
            ELSE
            Begin
                ObjName  := Copy(FullName,1,Dotpos1+DotPos2-1);
                PropName := Copy(PropName,Dotpos2+1,(Length(PropName)-DotPos2));
            End;

          End;

       End;
     End;
End;

FUNCTION DoQueryCmd:Integer;
{ ? Command }
{ Syntax:  ? Line.Line1.R1}
VAR
   ParamName:String;
   Param, ObjName, PropName:String;
   PropIndex:Integer;


Begin

     Result := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;

     ParseObjName(Param, ObjName, PropName);

     IF CompareText(ObjName,'solution')=0 THEN  Begin  // special for solution
       ActiveDSSClass  := SolutionClass;
       ActiveDSSObject := ActiveCircuit.Solution;
     End ELSE Begin
       // Set Object Active
       parser.cmdstring := '"' + Objname + '"';
       DoSelectCmd;
     End;

     // Put property value in global VARiable
     PropIndex := ActiveDSSClass.Propertyindex(PropName);
     IF PropIndex>0 THEN
        GlobalPropertyValue := ActiveDSSObject.GetPropertyValue(PropIndex)
     ELSE
        GlobalPropertyValue := 'Property Unknown';

     GlobalResult := GlobalPropertyValue;
     //MessageDlg(Param + ' = ' + GlobalPropertyValue,  mtCustom, [mbOK], 0);

End;

//----------------------------------------------------------------------------
FUNCTION DoResetMeters:Integer;

Begin
     Result := 0;
     EnergyMeterClass.ResetAll
End;


//----------------------------------------------------------------------------
FUNCTION DoNextCmd:Integer;
VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser.NextParam;
    Param := Parser.StrValue;

    With ActiveCircuit.Solution Do
    CASE UpCase(Param[1]) of

       'Y'{Year}:  Year := Year + 1;
       'H'{Hour}:  Inc(Hour);
       'T'{Time}:  With DynaVars Do Begin
                        t := t + h;
                        IF t >= 3600.0 THEN Begin
                        Inc(Hour);
                        t := t - 3600.0;
                        End;
                    End;
    ELSE

    END;

End;

//----------------------------------------------------------------------------
PROCEDURE DoAboutBox;

Begin

 If NoFormsAllowed Then Exit;

 ShowAboutBox;


End;

//----------------------------------------------------------------------------
FUNCTION DoSetVoltageBases:integer;


Begin

   Result := 0;

   ActiveCircuit.Solution.SetVoltageBases;

End;
//----------------------------------------------------------------------------
FUNCTION AddObject(const ObjType, Name:String):Integer;

Begin

   Result :=0;

   // Search for class IF not already active
   // IF nothing specified, LastClassReferenced remains
   IF   CompareText(Objtype, ActiveDssClass.Name) <> 0
   THEN LastClassReferenced := ClassNames.Find(ObjType);

   CASE LastClassReferenced of
     0: Begin
            DoSimpleMsg('New Command: Object Type "' + ObjType + '" not found.' + CRLF + parser.CmdString, 263);
            Result := 0;
            Exit;
        End;{Error}
   ELSE

     // intrinsic and user Defined models
     // Make a new circuit element
        ActiveDSSClass := DSSClassList.Get(LastClassReferenced);

      // Name must be supplied
        IF   Length(Name) = 0
        THEN Begin
            DoSimpleMsg('Object Name Missing'+ CRLF + parser.CmdString, 264);
            Exit;
        End;

   // now let's make a new object or set an existing one active, whatever the CASE
        CASE  ActiveDSSClass.DSSClassType Of
            // These can be added WITHout having an active circuit
            // Duplicates not allowed in general DSS objects;
             DSS_OBJECT :  IF  NOT  ActiveDSSClass.SetActive(Name)
                           THEN Begin
                               Result := ActiveDSSClass.NewObject(Name);
                               DSSObjs.Add(ActiveDSSObject);  // Stick in pointer list to keep track of it
                           End;
        ELSE
            // These are circuit elements
            IF   ActiveCircuit = nil
            THEN Begin
                 DoSimpleMsg('You Must Create a circuit first: "new circuit.yourcktname"', 265);
                 Exit;
            End;

          // IF Object already exists.  Treat as an Edit IF dulicates not allowed
            IF    ActiveCircuit.DuplicatesAllowed THEN
             Begin
                 Result := ActiveDSSClass.NewObject(Name); // Returns index into this class
                 ActiveCircuit.AddCktElement(Result);   // Adds active object to active circuit
             End
            ELSE
             Begin      // Check to see if we can set it active first
                IF   Not ActiveDSSClass.SetActive(Name)  THEN
                 Begin
                   Result := ActiveDSSClass.NewObject(Name);   // Returns index into this class
                   ActiveCircuit.AddCktElement(Result);   // Adds active object to active circuit
                 End
                ELSE
                 Begin
                    DoSimpleMsg('Warning: Duplicate new element definition: "'+ ActiveDSSClass.Name+'.'+Name+'"'+
                                 CRLF+ 'Element being redefined.', 266);
                 End;
             End;

        End;

        // ActiveDSSObject now points to the object just added
        // IF a circuit element, ActiveCktElement in ActiveCircuit is also set

        If Result>0 Then ActiveDSSObject.ClassIndex := Result;

        ActiveDSSClass.Edit;    // Process remaining instructions on the command line

  End;
End;


//----------------------------------------------------------------------------
FUNCTION EditObject(const ObjType, Name:String):Integer;

Begin

   Result :=0;
   LastClassReferenced := ClassNames.Find(ObjType);

   CASE LastClassReferenced of
     0: Begin
            DoSimpleMsg('Edit Command: Object Type "' + ObjType + '" not found.'+ CRLF + parser.CmdString, 267);
            Result := 0;
            Exit;
        End;{Error}
   ELSE

   // intrinsic and user Defined models
   // Edit the DSS object
      ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
      IF ActiveDSSClass.SetActive(Name) THEN
      Begin
          Result := ActiveDSSClass.Edit;   // Edit the active object
      End;
   End;

End;

//----------------------------------------------------------------------------
FUNCTION DoSetkVBase: Integer;

VAR
   ParamName, BusName:String;
   kVValue :Double;

Begin

// Parse off next two items on line
   ParamName := Parser.NextParam;
   BusName   := LowerCase(Parser.StrValue);

   ParamName := Parser.NextParam;
   kVValue   := Parser.DblValue;

   // Now find the bus and set the value

   WITH ActiveCircuit Do
   Begin
      ActiveBusIndex := BusList.Find(BusName);

      IF   ActiveBusIndex > 0
      THEN Begin
           IF    Comparetext(ParamName, 'kvln') = 0
           THEN  Buses^[ActiveBusIndex].kVBase := kVValue
           ELSE  Buses^[ActiveBusIndex].kVBase := kVValue / SQRT3;
           Result := 0;
           Solution.VoltageBaseChanged := TRUE;
           // Solution.SolutionInitialized := FALSE;  // Force reinitialization
      End
      ELSE Begin
           Result := 1;
           AppendGlobalResult('Bus ' + BusName + ' Not Found.');
      End;
   End;



End;



//----------------------------------------------------------------------------
PROCEDURE DoAutoAddBusList(const S: String);

VAR
   ParmName,
   Param, S2    :String;
   F :Textfile;


begin

     ActiveCircuit.AutoAddBusList.Clear;

     // Load up auxiliary parser to reparse the array list or file name
     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Param := AuxParser.StrValue;

     {Syntax can be either a list of bus names or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0
     THEN Begin
         // load the list from a file

         TRY
             AssignFile(F, Param);
             Reset(F);
             WHILE Not EOF(F) Do
             Begin         // Fixed 7/8/01 to handle all sorts of bus names
                  Readln(F, S2);
                  Auxparser.CmdString := S2;
                  ParmName := Auxparser.NextParam ;
                  Param := AuxParser.StrValue;
                  IF   Length(Param) > 0
                  THEN ActiveCircuit.AutoAddBusList.Add(Param);
             End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read bus list file. Error is: '+E.message, 268);
         END;


     End
     ELSE Begin

       // Parse bus names off of array list
       WHILE Length(Param) > 0 Do
       BEGIN
            ActiveCircuit.AutoAddBusList.Add(Param);
            AuxParser.NextParam;
            Param := AuxParser.StrValue;
       END;

     End;

end;

//----------------------------------------------------------------------------
PROCEDURE DoKeeperBusList(Const S:String);


// Created 4/25/03

{Set Keep flag on buses found in list so they aren't eliminated by some reduction
 algorithm.  This command is cumulative. To clear flag, use Reset Keeplist}

VAR
   ParmName,
   Param, S2    :String;
   F :Textfile;
   iBus :Integer;

begin

     // Load up auxiliary parser to reparse the array list or file name
     Auxparser.CmdString := S;
     ParmName := Auxparser.NextParam ;
     Param := AuxParser.StrValue;

     {Syntax can be either a list of bus names or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0  THEN
      Begin
         // load the list from a file

         TRY
             AssignFile(F, Param);
             Reset(F);
             WHILE Not EOF(F) Do
             Begin         // Fixed 7/8/01 to handle all sorts of bus names
                  Readln(F, S2);
                  Auxparser.CmdString := S2;
                  ParmName := Auxparser.NextParam ;
                  Param := AuxParser.StrValue;
                  IF   Length(Param) > 0
                  THEN With ActiveCircuit Do
                    Begin
                      iBus := BusList.Find(Param);
                      If iBus>0 Then Buses^[iBus].Keep := TRUE;
                    End;
             End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read bus list file "+param+". Error is: '+E.message, 269);
         END;


     End
     ELSE Begin

       // Parse bus names off of array list
       WHILE Length(Param) > 0 Do
       BEGIN
            With ActiveCircuit Do
            Begin
              iBus := BusList.Find(Param);
              If iBus>0 Then Buses^[iBus].Keep := TRUE;
            End;

            AuxParser.NextParam;
            Param := AuxParser.StrValue;
       END;

     End;

end;

//----------------------------------------------------------------------------
FUNCTION DocktlossesCmd: Integer;
Var
   LossValue :complex;
begin
     Result := 0;
     IF ActiveCircuit <> Nil THEN
      Begin
         GlobalResult := '';
         LossValue := ActiveCircuit.Losses;
         GlobalResult := Format('%10.5g, %10.5g',[LossValue.re * 0.001,  LossValue.im*0.001]);
      End
    ELSE  GlobalResult := 'No Active Circuit.';


end;

FUNCTION DocurrentsCmd: Integer;
VAR
  cBuffer: pComplexArray;
  NValues, i: Integer;

Begin
    Result := 0;

  If ActiveCircuit <> Nil Then
     WITH ActiveCircuit.ActiveCktElement DO
     Begin
         NValues := NConds*Nterms;
         GlobalResult := '';
         cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
         GetCurrents(cBuffer);
         For i := 1 to  NValues DO
         Begin
            GlobalResult := GlobalResult + Format('%10.5g, %6.1f,',[cabs(cBuffer^[i]), Cdang(cBuffer^[i])]);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     GlobalResult := 'No Active Circuit.';


end;

FUNCTION DolossesCmd: Integer;
Var
   LossValue :complex;
begin
    Result := 0;
     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        Begin
         GlobalResult := '';
         LossValue := ActiveCktElement.Losses;
         GlobalResult := Format('%10.5g, %10.5g', [LossValue.re * 0.001, LossValue.im * 0.001]);
        End;
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

FUNCTION DophaselossesCmd: Integer;

// Returns Phase losses in kW, kVar

VAR
  cBuffer:pComplexArray;
  NValues, i : Integer;

Begin

 Result := 0;

 IF ActiveCircuit <> Nil THEN

  WITH ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NPhases;
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GlobalResult := '';
      GetPhaseLosses( NValues, cBuffer);
      For i := 1 to  NValues DO Begin
         GlobalResult := GlobalResult + Format('%10.5g, %10.5g,',[ cBuffer^[i].re*0.001, cBuffer^[i].im*0.001]);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE GlobalResult := 'No Active Circuit.'



end;

FUNCTION DopowersCmd: Integer;
VAR
  cBuffer:pComplexArray;
  NValues, i : Integer;

Begin

 Result := 0;
 IF ActiveCircuit <> Nil THEN
  WITH ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NConds*Nterms;
      GlobalResult := '';
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GetPhasePower(cBuffer);
      For i := 1 to  NValues DO Begin
         GlobalResult := GlobalResult+ Format('%10.5g, %10.5g,', [cBuffer^[i].re*0.001, cBuffer^[i].im*0.001]);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoseqcurrentsCmd: Integer;
// All sequence currents of active ciruit element
// returns magnitude only.

VAR
  Nvalues,i,j,k:Integer;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin

   Result := 0;
   IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       If ActiveCktElement<>Nil THEN
       WITH ActiveCktElement DO
       Begin
        GlobalResult := '';
        IF Nphases<3
        THEN  For i := 0 to  3*Nterms-1 DO GlobalResult := GlobalResult + ' -1.0,'  // Signify n/A
        ELSE Begin
          NValues := NConds * Nterms;
          cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
          GetCurrents(cBuffer);
          For j := 1 to Nterms Do
          Begin
            k := (j-1)*NConds;
            For i := 1 to  3 DO
            Begin
              Iph[i] := cBuffer^[k+i];
            End;
            Phase2SymComp(@Iph, @I012);
            For i := 1 to 3 DO
            Begin
              GlobalResult := GlobalResult + Format('%10.5g, ',[Cabs(I012[i])]);
            End;
          End;
          Reallocmem(cBuffer,0);
        End; {ELSE}
       End; {WITH ActiveCktElement}
     End   {IF/WITH ActiveCircuit}
   ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoSeqpowersCmd: Integer;
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

VAR
  Nvalues,i,j,k :Integer;
  S:Complex;
  VPh, V012 : Array[1..3] of Complex;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin

 Result := 0;
 IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO Begin
      GlobalResult := '';
      IF NPhases < 3 THEN
         For i := 0 to 2*3*Nterms-1 DO GlobalResult := GlobalResult + '-1.0, '  // Signify n/A
      ELSE Begin
        NValues := NConds * Nterms;
        cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
        GetCurrents(cBuffer);
        FOR j := 1 to Nterms Do Begin
         k :=(j-1) * NConds;
         FOR i := 1 to  3 DO Begin
            Vph[i] := Solution.NodeV^[Terminals^[j].TermNodeRef^[i]];
         End;
         For i := 1 to  3 DO Begin
           Iph[i] := cBuffer^[k+i];
         End;
         Phase2SymComp(@Iph, @I012);
         Phase2SymComp(@Vph, @V012);
         For i := 1 to 3 DO  Begin
           S := Cmul(V012[i], conjg(I012[i]));
           GlobalResult := GlobalResult+ Format('%10.5g, %10.5g,',[S.re*0.003, S.im*0.003]); // 3-phase kW conversion
         End;
        End;
      End;
      Reallocmem(cBuffer,0);
     End;
   End
 ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoseqvoltagesCmd: Integer;

// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal

VAR
  Nvalues,i,j,k,n:Integer;
  VPh, V012 : Array[1..3] of Complex;
  S:String;

Begin
  Result := 0;
  Nvalues := -1; // unassigned, for exception message
  n := -1; // unassigned, for exception message
  IF   ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
     TRY
      Nvalues := NPhases;
      GlobalResult :='';
      IF Nvalues < 3 THEN
         For i := 1 to 3*Nterms DO GlobalResult := GlobalResult + '-1.0, '  // Signify n/A
      ELSE
      Begin

       FOR j := 1 to Nterms Do
       Begin

          k :=(j-1)*NConds;
          FOR i := 1 to 3 DO
          Begin
             Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
          End;
          Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             GlobalResult := GlobalResult + Format('%10.5g, ',[Cabs(V012[i])]);
          End;

       End;
      End;

      EXCEPT
         On E:Exception Do
         Begin
            S:= E.message + CRLF +
                'Element='+ ActiveCktElement.Name + CRLF+
                'Nvalues=' + IntToStr(NValues) + CRLF +
                'Nterms=' + IntToStr(Nterms) + CRLF +
                'NConds =' + IntToStr(NConds) + CRLF +
                'noderef=' + IntToStr(N) ;
            DoSimpleMsg(S, 270);
          End;
      END;
     End
     Else
         GlobalResult := 'Element Disabled';  // Disabled

   End
  ELSE GlobalResult := 'No Active Circuit';



End;

//----------------------------------------------------------------------------
FUNCTION DovoltagesCmd(Const PerUnit:Boolean): Integer;
// Bus Voltages at active terminal

VAR
  i:Integer;
  Volts:Complex;
  ActiveBus:TBus;
  VMag:Double;

Begin

    Result := 0;
    IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         FOR i := 1 to  ActiveBus.NumNodesThisBus DO
         Begin
            Volts := Solution.NodeV^[ActiveBus.GetRef(i)];
            Vmag := Cabs(Volts);
            If PerUnit and (ActiveBus.kvbase>0.0) Then Begin
                  Vmag := Vmag *0.001/ActiveBus.kVBase;
                  GlobalResult := GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
            End 
            Else  GlobalResult := GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
         End;
        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

//----------------------------------------------------------------------------
FUNCTION DoZscCmd(Zmatrix:Boolean): Integer;
// Bus Short Circuit matrix

VAR
  i,j:Integer;
  ActiveBus:TBus;
  Z:Complex;

Begin

    Result := 0;
    IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;
         With ActiveBus Do
         FOR i := 1 to  NumNodesThisBus DO Begin
            For j := 1 to  NumNodesThisBus Do  Begin

             If ZMatrix Then Z := Zsc.GetElement(i,j)
             Else Z := Ysc.GetElement(i,j);
             GlobalResult := GlobalResult + Format('%-.5g, %-.5g,   ', [Z.re, Z.im]);

            End;

         End;
        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

//----------------------------------------------------------------------------
FUNCTION DoZsc10Cmd: Integer;
// Bus Short Circuit matrix

VAR
  ActiveBus:TBus;
  Z:Complex;

Begin

    Result := 0;
    IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;
         With ActiveBus Do Begin

             Z := Zsc1;
             GlobalResult := GlobalResult + Format('Z1, %-.5g, %-.5g, ', [Z.re, Z.im]) + CRLF;
             
             Z := Zsc0;
             GlobalResult := GlobalResult + Format('Z0, %-.5g, %-.5g, ', [Z.re, Z.im]);
         End;

        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;


//----------------------------------------------------------------------------
FUNCTION DoAllocateLoadsCmd: Integer;

{ Requires an EnergyMeter Object at the head of the feeder
  Adjusts loads defined by connected kVA or kWh billing
}

VAR
   pMeter :TEnergyMeterObj;
   pSensor:TSensorObj;
   iterCount :Integer;

begin
    Result := 0;
    WITH ActiveCircuit Do
    Begin
         LoadMultiplier := 1.0;   // Property .. has side effects
         Solution.Mode := SNAPSHOT;
         Solution.Solve;  {Make guess based on present allocationfactors}

         {Allocation loop -- make MaxAllocationIterations iterations}
         FOR iterCount := 1 to MaxAllocationIterations Do Begin

           {Do EnergyMeters}
           pMeter := EnergyMeters.First;
           WHILE pMeter <> NIL Do Begin
              pMeter.CalcAllocationFactors;
              pMeter := EnergyMeters.Next;
           End;

           {Now do other Sensors}
           pSensor := Sensors.First;
           WHILE pSensor <> NIL Do Begin
              pSensor.CalcAllocationFactors;
              pSensor := Sensors.Next;
           End;

           {Now let the EnergyMeters run down the circuit setting the loads}
            pMeter := EnergyMeters.First;
            WHILE pMeter <> NIL Do Begin
                pMeter.AllocateLoad;
                pMeter := EnergyMeters.Next;
            End;
            Solution.Solve;  {Update the solution}

         End;
    End;
end;

//----------------------------------------------------------------------------
PROCEDURE DoSetAllocationFactors(const X: Double);

VAR
   pLoad :TLoadObj;

begin
    IF   X <= 0.0
    THEN DoSimpleMsg('Allocation Factor must be greater than zero.', 271)
    ELSE WITH ActiveCircuit Do
    Begin
         pLoad := Loads.First;
         WHILE pLoad <> NIL Do
         Begin
             pLoad.kVAAllocationFactor := X;
             pLoad := Loads.Next;
         End;
    End;
end;

PROCEDURE DoSetCFactors(const X: Double);

VAR
   pLoad :TLoadObj;

begin
    IF   X <= 0.0
    THEN DoSimpleMsg('CFactor must be greater than zero.', 271)
    ELSE WITH ActiveCircuit Do
    Begin
         pLoad := Loads.First;
         WHILE pLoad <> NIL Do
         Begin
             pLoad.CFactor := X;
             pLoad := Loads.Next;
         End;
    End;
end;

//----------------------------------------------------------------------------
FUNCTION DoHarmonicsList(const S:String):Integer;

VAR
   Dummy :pDoubleArray;
   i,
   Num   :Integer;

Begin
   Result := 0;

   WITH ActiveCircuit.Solution Do
   IF CompareText(S, 'ALL') = 0 THEN DoAllHarmonics := TRUE
   ELSE Begin
       DoAllHarmonics := FALSE;

       Dummy := AllocMem(Sizeof(Dummy^[1]) * 100); // Big Buffer
       Num   := Parser.ParseAsVector(100, Dummy);
       {Parsing zero-fills the array}

       HarmonicListSize := Num;
       Reallocmem(HarmonicList, SizeOf(HarmonicList^[1]) * HarmonicListSize);
       FOR i := 1 to HarmonicListSize Do HarmonicList^[i] := Dummy^[i];

       Reallocmem(Dummy, 0);
   End;
End;


//----------------------------------------------------------------------------
FUNCTION DoFormEditCmd:Integer;

Begin

    Result := 0;
    If NoFormsAllowed Then Exit;
    DoSelectCmd;  // Select ActiveObject
    IF ActiveDSSObject <> NIL THEN  Begin

         ShowPropEditForm;

    End
    ELSE   Begin
       DoSimpleMsg('Element Not Found.', 272);
       Result := 1;
    End;
End;


//----------------------------------------------------------------------------
FUNCTION DoMeterTotals:Integer;
Var
   i: Integer;
Begin
    Result := 0;
    If ActiveCircuit <> Nil Then
      Begin
       ActiveCircuit.TotalizeMeters;
        // Now export to global result
        For i := 1 to NumEMregisters Do
          Begin
            AppendGlobalResult(Format('%-.6g',[ActiveCircuit.RegisterTotals[i]]));
          End;
      End;
End;

//----------------------------------------------------------------------------
FUNCTION DoCapacityCmd:Integer;

Var
   ParamPointer     :integer;
   Param, ParamName :String;

Begin
  Result := 0;

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE Case ParamName[1] of
                 's':ParamPointer := 1;
                 'i':ParamPointer := 2;
              ELSE
                  ParamPointer := 0;
              END;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Capacity Command', 273);
            1: ActiveCircuit.CapacityStart := Parser.DblValue;
            2: ActiveCircuit.CapacityIncrement := Parser.DblValue;

         ELSE

         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END;

    WITH ActiveCircuit Do
    IF ComputeCapacity Then Begin   // Totalizes EnergyMeters at End

       GlobalResult := Format('%-.6g', [(ActiveCircuit.RegisterTotals[3] + ActiveCircuit.RegisterTotals[19]) ] );  // Peak KW in Meters
       AppendGlobalResult(Format('%-.6g', [LoadMultiplier]));
    End;
End;

//----------------------------------------------------------------------------
FUNCTION DoClassesCmd:Integer;

VAR  i:Integer;
Begin
     For i := 1 to NumIntrinsicClasses Do Begin
       AppendGlobalResult(TDSSClass(DSSClassList.Get(i)).Name);
     End;
     Result := 0;
End;

//----------------------------------------------------------------------------
FUNCTION DoUserClassesCmd:Integer;
VAR  i:Integer;
Begin
    Result := 0;
    IF NumUserClasses=0 Then Begin
        AppendGlobalResult('No User Classes Defined.');
    End
    ELSE
     For i := NumIntrinsicClasses+1 to DSSClassList.ListSize Do Begin
       AppendGlobalResult(TDSSClass(DSSClassList.Get(i)).Name);
     End;
End;

//----------------------------------------------------------------------------
FUNCTION DoZscRefresh:Integer;

Var j:Integer;

Begin
   Result := 1;

   Try

     WITH ActiveCircuit, ActiveCircuit.Solution Do
     Begin
       FOR j := 1 to NumNodes Do Currents^[j] := cZERO;  // Clear Currents array

       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then Begin
          If not assigned(Buses^[ActiveBusIndex].Zsc) Then Buses^[ActiveBusIndex].AllocateBusQuantities ;
          SolutionAlgs.ComputeYsc(ActiveBusIndex);      // Compute YSC for active Bus
          Result := 0;
       End;
     End;

   Except
       On E:Exception Do DoSimpleMsg('ZscRefresh Error: ' + E.message + CRLF , 274);
   End;


End;


FUNCTION DoVarValuesCmd:Integer;

Var
   i: Integer;
  // PcElem:TPCElement;
Begin

    Result := 0;
    If ActiveCircuit <> Nil Then
    With ActiveCircuit Do
      Begin
         {Check if PCElement}
         CASE (ActiveCktElement.DSSObjType and BASECLASSMASK) OF
           PC_ELEMENT: With ActiveCktElement as TPCElement Do
                       Begin
                         For i := 1 to NumVariables Do
                         AppendGlobalResult(Format('%-.6g',[Variable[i]]));
                       End;
         Else
             AppendGlobalResult('Null');
         End;
      End;

End;

FUNCTION DoVarNamesCmd :Integer;

Var
   i: Integer;
Begin

    Result := 0;
    If ActiveCircuit <> Nil Then
    With ActiveCircuit Do
      Begin
         {Check if PCElement}
         CASE (ActiveCktElement.DSSObjType and BASECLASSMASK) OF
           PC_ELEMENT: With (ActiveCktElement as TPCElement) Do
                       Begin
                         For i := 1 to NumVariables Do
                         AppendGlobalResult(VariableName(i));
                       End;
         Else
             AppendGlobalResult('Null');
         End;
      End;

End;

FUNCTION DoBusCoordsCmd:Integer;

{
 Format of File should be

   Busname, x, y

   (x, y are real values)

}

Var

   F:TextFile;
   ParamName, Param, S, BusName:String;
   iB:Integer;

Begin
    Result := 0;

    {Get next parameter on command line}

    ParamName := Parser.NextParam;
    Param := Parser.StrValue;

    Try

      Try
         AssignFile(F, Param);
         Reset(F);

         While not EOF(F) Do
          Begin
             Readln(F, S);      // Read line in from file
             With AuxParser Do Begin      // User Auxparser to parse line
                   CmdString := S;
                   NextParam;  BusName := StrValue;
                   iB := ActiveCircuit.Buslist.Find(BusName);
                   If iB >0 Then  Begin
                       With ActiveCircuit.Buses^[iB] Do Begin     // Returns TBus object
                         NextParam;  x := DblValue;
                         NextParam;  y := DblValue;
                         CoordDefined := TRUE;
                       End;
                   End;
              End;
              {Else just ignore a bus that's not in the circuit}
          End;

      Except
          ON E:Exception Do DoSimpleMsg('Bus Coordinate file: "' + Param + '" not found.', 275);
      End;

    Finally
        CloseFile(F);
    End;

End;

FUNCTION DoMakePosSeq:Integer;

Var
   CktElem:TCktElement;

Begin
    Result := 0;

    ActiveCircuit.PositiveSequence := TRUE;

    CktElem := ActiveCircuit.CktElements.First;
    While CktElem<>Nil Do
    Begin
       CktElem.MakePosSequence;
       CktElem := ActiveCircuit.CktElements.Next;
    End;

End;


PROCEDURE DoSetReduceStrategy(Const S:String);

Var
    ParamName, Param, Param2:String;

   Function AtLeast(i,j:Integer):Integer;
   Begin If j<i Then Result := i Else Result := j; End;

Begin
     ActiveCircuit.ReductionStrategyString := S;
     AuxParser.CmdString := S;
     paramName := Auxparser.NextParam;
     Param := UpperCase(AuxParser.StrValue);
     paramName := Auxparser.NextParam;
     Param2 := AuxParser.StrValue;

     ActiveCircuit.ReductionStrategy := rsDefault;
     IF Length(Param)=0 Then Exit;  {No option given}

     Case Param[1] of

       'B': ActiveCircuit.ReductionStrategy := rsBreakLoop;
       'D': ActiveCircuit.ReductionStrategy := rsDefault;  {Default}
       'E': ActiveCircuit.ReductionStrategy := rsDangling;  {Ends}
       'M': ActiveCircuit.ReductionStrategy := rsMergeParallel;
       'T': Begin
              ActiveCircuit.ReductionStrategy := rsTapEnds;
              ActiveCircuit.ReductionMaxAngle := 15.0;  {default}
              If Length(param2) > 0 Then  ActiveCircuit.ReductionMaxAngle := Auxparser.DblValue;
            End;
       'S': Begin  {Stubs}
              IF CompareTextShortest(Param, 'SWITCH')=0 Then Begin
                  activeCircuit.ReductionStrategy := rsSwitches;
              End ELSE Begin
                  ActiveCircuit.ReductionZmag := 0.02;
                  ActiveCircuit.ReductionStrategy := rsStubs;
                  If Length(param2) > 0 Then  ActiveCircuit.ReductionZmag := Auxparser.DblValue;
              End;
            End;
     ELSE
         DoSimpleMsg('Unknown Reduction Strategy: "' + S + '".', 276);
     End;

End;

FUNCTION DoInterpolateCmd:Integer;

{Interpolate bus coordinates in meter zones}

VAR
    MetObj:TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    ParamName, Param  :String;
    DevClassIndex:Integer;
    CktElem:TCktElement;

Begin
    Result := 0;

    ParamName := Parser.NextParam;
    Param := UpperCase(Parser.StrValue);

    // initialize the Checked Flag FOR all circuit Elements
    With ActiveCircuit Do
    Begin
     CktElem := CktElements.First;
     WHILE  (CktElem <> NIL) Do
     Begin
         CktElem.Checked := False;
         CktElem := CktElements.Next;
     End;
    End;


    IF Length(Param) = 0  Then Param := 'A';
    CASE Param[1] of
     'A': Begin
              metobj := ActiveCircuit.EnergyMeters.First;
              While metobj <> nil Do
              Begin
                MetObj.InterpolateCoordinates;
                MetObj := ActiveCircuit.EnergyMeters.Next;
              End;
          End;

    ELSE
       {Interpolate a specific meter}
       DevClassIndex := ClassNames.Find('energymeter');
       IF DevClassIndex > 0 THEN
       Begin
          MeterClass := DSSClassList.Get(DevClassIndex);
          If MeterClass.SetActive (Param) Then   // Try to set it active
          Begin
            MetObj := MeterClass.GetActiveObj;
            MetObj.InterpolateCoordinates;
          End
          Else DoSimpleMsg('EnergyMeter "'+Param+'" not found.', 277);
       End;
    End;

End;

FUNCTION DoAlignFileCmd:Integer;
{Rewrites designated file, aligning the fields into columns}
Var
    ParamName, Param  :String;

Begin
  Result := 0;
  ParamName := Parser.NextParam;
  Param := Parser.StrValue;


  If FileExists(Param) Then
    Begin
     If Not RewriteAlignedFile(Param) Then Result := 1;
    End
  Else
    Begin
     DoSimpleMsg('File "'+Param+'" does not exist.', 278);
     Result := 1;
    End;

  If Result=0 Then FireOffEditor(GlobalResult);

End; {DoAlignfileCmd}

FUNCTION DoTOPCmd:Integer;
{ Sends Monitors, Loadshapes, GrowthShapes, or TCC Curves to TOP as an STO file}

Var
    ParamName, Param, ObjName  :String;

Begin
    Result := 0;
    ParamName := Parser.NextParam;
    Param := UpperCase(Parser.StrValue);

    ParamName := Parser.NextParam;
    ObjName := UpperCase(Parser.StrValue);

    If Length(ObjName)=0 Then ObjName := 'ALL';


    Case  Param[1] of
        'L': LoadShapeClass.TOPExport(ObjName);
        {
          'G': GrowthShapeClass.TOPExportAll;
          'T': TCC_CurveClass.TOPExportAll;
        }
    ELSE
        MonitorClass.TOPExport(ObjName);
    End;


End;

Procedure DoSetNormal(pctNormal:Double);

Var i:Integer;
    pLine:TLineObj;

Begin
    If ActiveCircuit <> Nil Then Begin
       pctNormal := pctNormal * 0.01;  // local copy only
       For i := 1 to ActiveCircuit.Lines.ListSize Do  Begin
         pLine := ActiveCircuit.Lines.Get(i);
         pLine.Normamps := pctNormal * pLine.EmergAmps;
       End;
    End;
End;

FUNCTION DoRotateCmd:Integer;

{rotate about the center of the coordinates}

Var
        i:Integer;
        Angle, xmin,xmax, ymin, ymax, xc, yc:Double;
         ParamName:String;
         a, vector: Complex;

Begin
    Result := 0;
    If ActiveCircuit <> NIl then Begin

        ParamName := Parser.NextParam;
        Angle := Parser.DblValue * 3.14159/180.0;   // Deg to rad

        a := cmplx(cos(Angle), Sin(Angle));
        With ActiveCircuit Do Begin
            Xmin := 1.0e50;
            Xmax := -1.0e50;
            Ymin := 1.0e50;
            Ymax := -1.0e50;
            For i := 1 to Numbuses Do Begin
                If Buses^[i].CoordDefined Then Begin
                    With  Buses^[i] Do Begin
                      Xmax := Max(Xmax, x);
                      XMin := Min(Xmin, x);
                      ymax := Max(ymax, y);
                      yMin := Min(ymin, y);
                    End;
                End;
            End;

            Xc := (Xmax + Xmin) / 2.0;
            Yc := (Ymax + Ymin) / 2.0;

            For i := 1 to Numbuses Do Begin
                If Buses^[i].CoordDefined Then Begin
                    With  Buses^[i] Do Begin
                         vector := cmplx(x-xc,y-yc);
                         Vector := Cmul(Vector, a);
                         x := xc+vector.re;
                         y := yc+vector.im;
                    End;
                End;
            End;
        End;
    end;

End;


FUNCTION DoVDiffCmd:Integer;
Var
        Fin, Fout :TextFile;
        BusName, Line:String;
        i,  node, busIndex:Integer;
        Vmag, Diff:Double;

Begin
   Result := 0;
   If FileExists(CircuitName_ + 'SavedVoltages.Txt') Then Begin
   Try
    Try

         AssignFile(Fin, CircuitName_ + 'SavedVoltages.Txt');
         Reset(Fin);

         AssignFile(Fout, CircuitName_ + 'VDIFF.txt');
         Rewrite(Fout);

         While Not EOF(Fin) Do Begin
             Readln(Fin, Line);
             Auxparser.CmdString := Line;
             AuxParser.NextParam;
             BusName := Auxparser.StrValue;
             If Length(BusName) > 0 Then Begin
                 BusIndex := ActiveCircuit.BusList.Find(BusName);
                 If BusIndex>0 Then Begin
                     AuxParser.Nextparam;
                     node := AuxParser.Intvalue;
                     With  ActiveCircuit.Buses^[BusIndex] Do
                     For i := 1 to NumNodesThisBus Do Begin
                         If GetNum(i)=node then Begin
                             AuxParser.Nextparam;
                             Vmag := AuxParser.Dblvalue;
                             Diff := Cabs(ActiveCircuit.Solution.NodeV^[GetRef(i)]) - Vmag;
                             If Vmag<>0.0 then Begin
                                Writeln(Fout, BusName,'.',node,', ', (Diff / Vmag * 100.0):7:2,', %');
                             End
                             Else Writeln(Fout, BusName,'.',node,', ', format('%-.5g',[Diff]),', Volts');
                         End;
                     End;

                 End;
             End;
         End;

      
    Except
          On E:Exception Do Begin
           DoSimpleMsg('Error opening Saved Voltages or VDIFF File: '+E.message, 280);
           Exit;
          End;

    End;


  Finally

   CloseFile(Fin);
   CloseFile(Fout);

   FireOffEditor(CircuitName_ + 'VDIFF.txt');

  End;

  End
  Else  DoSimpleMsg('Error: No Saved Voltages.', 281);

End;

FUNCTION DoSummaryCmd:Integer;

Var
    S:TStringList;
    cPower, cLosses:Complex;

Begin
  Result := 0;
  S := TStringList.Create;
  Try
       S.Add(Format('Year = %d ',[ActiveCircuit.Solution.Year]));
       S.Add(Format('Hour = %d ',[ActiveCircuit.Solution.hour]));
       S.Add('Max pu. voltage = '+Format('%-.5g ',[GetMaxPUVoltage]));
       S.Add('Min pu. voltage = '+Format('%-.5g ',[GetMinPUVoltage(TRUE)]));
       cPower :=  CmulReal(GetTotalPowerFromSources, 0.000001);  // MVA
       S.Add(Format('Total Active Power:   %-.6g MW',[cpower.re]));
       S.Add(Format('Total Reactive Power: %-.6g Mvar',[cpower.im]));
       cLosses := CmulReal(ActiveCircuit.Losses, 0.000001);
			 S.Add(Format('Total Active Losses:   %-.6g MW, (%-.4g %%)',[cLosses.re,(Closses.re/cPower.re*100.0)]));
       S.Add(Format('Total Reactive Losses: %-.6g Mvar',[cLosses.im]));
       S.Add(Format('Frequency = %-g Hz',[ActiveCircuit.Solution.Frequency]));
       S.Add('Mode = '+GetSolutionModeID);
       S.Add('Control Mode = '+GetControlModeID);
       S.Add('Load Model = '+GetLoadModel);
       ShowMessageForm(S);
  Finally
      S.Free;
  End;
 

End;

Function DoDistributeCmd:Integer;
Var
   ParamPointer :Integer;
   ParamName,
   Param:String;

   kW, PF :double;
   Skip:Integer;
   How,
   FilName:String;

Begin
     Result := 0;
     ParamPointer := 0;
     {Defaults}
     kW := 1000.0;
     How := 'Proportional';
     Skip := 1;
     PF := 1.0;
     FilName := 'DistGenerators.dss';

     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := DistributeCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: kW := Parser.DblValue;
           2: How := Parser.StrValue;
           3: Skip := Parser.IntValue;
           4: PF := Parser.DblValue;
           5: FilName := Parser.StrValue;
           6: kW := Parser.DblValue * 1000.0;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     MakeDistributedGenerators(kW, PF, How, Skip, FilName);  // in Utilities

End;

FUNCTION DoDI_PlotCmd:Integer;

Var ParamName, Param:String;
    ParamPointer, i:Integer;
    CaseName:String;
    MeterName:String;
    CaseYear:integer;
    dRegisters: Array[1..NumEMRegisters] of Double;
    iRegisters:Array of Integer;
    NumRegs:Integer;
    PeakDay:Boolean;

Begin

     IF DIFilesAreOpen Then EnergyMeterClass.CloseAllDIFiles;

     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;
     
     {Defaults}
     NumRegs:=1;
     SetLength(IRegisters, NumRegs);
     iRegisters[0] := 9;
     PeakDay := FALSE;
     CaseYear := 1;
     CaseName := '';
     MeterName := 'DI_Totals';

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)
         ELSE ParamPointer := DI_PlotCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: CaseName := Param;
           2: CaseYear := Parser.Intvalue;
           3: Begin
                 NumRegs := Parser.ParseAsVector(NumEMREgisters, @dRegisters);
                 SetLength(iRegisters, NumRegs);
                 For i := 1 to NumRegs Do iRegisters[i-1] := Round(dRegisters[i]);
              End;
           4: PeakDay := InterpretYesNo(Param);
           5: MeterName := Param;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     DSSPlotObj.DoDI_Plot(CaseName, CaseYear, iRegisters, PeakDay, MeterName);

     iRegisters := Nil;

     Result := 0;

End;

FUNCTION DoCompareCasesCmd:Integer;

Var ParamName, Param:String;
    ParamPointer:Integer;
    UnKnown:Boolean;
    Reg:Integer;
    CaseName1,
    CaseName2, WhichFile:String;

Begin

     IF DIFilesAreOpen Then EnergyMeterClass.CloseAllDIFiles;

     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;

     CaseName1 := 'base';
     CaseName2 := '';
     Reg := 9;    // Overload EEN
     WhichFile := 'Totals';

     ParamPointer := 0;
     ParamName := UpperCase(Parser.NextParam);
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Begin
             If  CompareTextShortest(ParamName, 'CASE1')=0 then ParamPointer:=1
             ELSE If  CompareTextShortest(ParamName, 'CASE2')=0 then ParamPointer:=2
             ELSE If  CompareTextShortest(ParamName, 'REGISTER')=0 then ParamPointer:=3
             ELSE If  CompareTextShortest(ParamName, 'METER')=0 then ParamPointer:=4
             ELSE Unknown := TRUE;
         End;


         If Not Unknown then
         CASE ParamPointer OF
           1: CaseName1 := Param;
           2: CaseName2 := Param;
           3: Reg := Parser.IntValue;
           4: WhichFile := Param;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := UpperCase(Parser.NextParam);
         Param := Parser.StrValue;
     End;

     DSSPlotObj.DoCompareCases(CaseName1, CaseName2, WhichFile,  Reg);

     Result := 0;

End;

FUNCTION DoYearlyCurvesCmd:Integer;

Var ParamName, Param:String;
    ParamPointer, i:Integer;
    UnKnown:Boolean;
    CaseNames:TStringList;
    dRegisters:Array[1..NumEMRegisters] of Double;
    iRegisters:Array of Integer;
    Nregs:Integer;
    WhichFile:String;

Begin

     IF DIFilesAreOpen Then EnergyMeterClass.CloseAllDIFiles;

     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;

     Nregs := 1;
     SetLength(iRegisters, Nregs);
     CaseNames := TStringList.Create;
     CaseNames.Clear;
     WhichFile := 'Totals';


     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Case Uppercase(ParamName)[1] of
                    'C':ParamPointer := 1;
                    'R':ParamPointer := 2;
                    'M':ParamPointer := 3; {meter=}
              ELSE
                   Unknown := TRUE;
              END;

         If Not Unknown then
         CASE ParamPointer OF
           1: Begin  // List of case names
                AuxParser.CmdString := Param;
                AuxParser.NextParam;
                Param := AuxParser.StrValue;
                While Length(Param)>0 Do Begin
                    CaseNames.Add(Param);
                    AuxParser.NextParam;
                    Param := AuxParser.StrValue;
                End;
              End;
           2: Begin
                NRegs := Parser.ParseAsVector(NumEMRegisters, @dRegisters);
                SetLength(iRegisters, Nregs);
                For i := 1 to NRegs Do iRegisters[i-1] := Round(dRegisters[i]);
              end;
           3: WhichFile := Param ;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     DSSPlotObj.DoYearlyCurvePlot(CaseNames, WhichFile,  iRegisters);

     iRegisters := Nil;
     CaseNames.Free;
     Result := 0;
End;

FUNCTION DoVisualizeCmd:Integer;

Var  DevIndex    :integer;
     Param       :String;
     ParamName   :String;
     ParamPointer:Integer;
     Unknown     :Boolean;
     Quantity    :Integer;
     ElemName    :String;
     pElem       :TDSSObject;

Begin
     Result := 0;
     Quantity := vizCURRENT;
     ElemName := '';
      {Parse rest of command line}
     ParamPointer := 0;
     ParamName := UpperCase(Parser.NextParam);
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Begin
             If  CompareTextShortest(ParamName, 'WHAT')=0 then ParamPointer:=1
             ELSE If  CompareTextShortest(ParamName, 'ELEMENT')=0 then ParamPointer:=2
             ELSE Unknown := TRUE;
         End;

         If Not Unknown then
         CASE ParamPointer OF
           1: Case Lowercase(Param)[1] of
                'c':  Quantity := vizCURRENT;
                'v':  Quantity := vizVOLTAGE;
                'p':  Quantity := vizPOWER;
               End;
           2: ElemName := Param;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := UpperCase(Parser.NextParam);
         Param := Parser.StrValue;
     End;  {WHILE}

     {--------------------------------------------------------------}

     Devindex := GetCktElementIndex(ElemName); // Global function
     IF DevIndex > 0 THEN Begin  //  element must already exist
        pElem := ActiveCircuit.CktElements.Get(DevIndex);
        If pElem is TCktElement Then Begin
           DSSPlotObj.DoVisualizationPlot(TCktElement(pElem), Quantity);
        End Else Begin
          DoSimpleMsg(pElem.Name + ' must be a circuit element type!', 282);   // Wrong type
        End;
     End Else Begin
        DoSimpleMsg('Requested Circuit Element: "' + ElemName + '" Not Found.',282 ); // Did not find it ..
     End;

End;

FUNCTION DoCloseDICmd:Integer;

Begin
    Result  := 0;
    EnergyMeterClass.CloseAllDIFiles;
End;

FUNCTION DoADOScmd:Integer;

Begin
    Result  := 0;
    DoDOScmd(Parser.Remainder);
End;

FUNCTION DoEstimateCmd:Integer;

Begin
    Result := 0;

    {Load current Estimation is driven by Energy Meters at head of feeders.}
    DoAllocateLoadsCmd;

    {Let's look to see how well we did}
     If not AutoShowExport Then DSSExecutive.Command := 'Set showexport=yes';
     DSSExecutive.Command := 'Export Estimation';

End;


initialization

{Initialize Command lists}

    SaveCommands := TCommandList.Create(['class', 'file', 'dir', 'keepdisabled']);
    SaveCommands.Abbrev := True;
    DI_PlotCommands := TCommandList.Create(['case','year','registers','peak','meter']);
    DistributeCommands := TCommandList.Create(['kW','how','skip','pf','file','MW']);
    DistributeCommands.Abbrev := True;

    PlotCommands := TCommandList.Create(['type', 'quantity', 'max', 'dots', 'labels', 'object','showloops',
                                         'r3','r2','c1','c2', 'c3','channels', 'bases', 'subs', 'thickness']);
    PlotCommands.Abbrev := True;
                                          {  1            2          3           4             5 }
    ShowCommands := TCommandList.Create(['autoadded',  'buses', 'currents',   'convergence', 'elements',
                                         'faults',   'isolated', 'generators', 'meters',    'monitor',
                                         'panel',     'powers',   'voltages', 'zone',       'taps',
                                         'overloads', 'unserved', 'eventlog', 'variables', 'ratings',
                                          'loops',    'losses',   'busflow', 'lineconstants',  'yprim',
                                           'y']);

    ShowCommands.Abbrev := True;

finalization

    DistributeCommands.Free;
    SaveCommands.Free;
    PlotCommands.Free;

end.
