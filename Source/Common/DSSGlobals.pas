unit DSSGlobals;


{ Change Log
 8-14-99  SolutionAbort Added

 10-12-99 AutoAdd constants added;
 4-17-00  Added IsShuntCapacitor routine, Updated constants
 4-11-01  Added Admin trap door
 10-08-02 Moved Control Panel Instantiation and show to here
 11-6-02  Removed load user DLL because it was causing a conflict
}

{$WARN UNIT_PLATFORM OFF}

interface

Uses Circuit, DSSObject, DSSClass, ParserDel, Hashlist, PointerList,
     UComplex, Spectrum,  Arraydef, CktElement,
     LoadShape,
     GrowthShape,
     Monitor,
     EnergyMeter,
     Sensor,
     TCC_Curve,
     Feeder,
     WireData;


CONST
      CRLF = #13#10;

      TwoPi = 2.0 * 3.141592654;

      EPSILON = 1.0e-12;   // Default tiny floating point
      EPSILON2 = 1.0e-3;   // Default for Real number mismatch testing


      BASECLASSMASK: Cardinal = $00000007;
      CLASSMASK: Cardinal     = $FFFFFFF8;

      {Basic element types}
      NON_PCPD_ELEM = 1;  // A circuit Element we don't want enumerated in PD and PC Elements
      PD_ELEMENT    = 2;
      PC_ELEMENT    = 3;
      CTRL_ELEMENT  = 4;
      METER_ELEMENT = 5;

      {Specific element Types}
      MON_ELEMENT  =  1 * 8;
      DSS_OBJECT   =  2 * 8;   // Just a general DSS object, accessible to all circuits
      SOURCE       =  3 * 8;
      XFMR_ELEMENT =  4 * 8;
      SUBSTATION   =  5 * 8;  // not used
      LINE_ELEMENT =  6 * 8;
      LOAD_ELEMENT =  7 * 8;
      FAULTOBJECT  =  8 * 8;
      ENERGY_METER =  9 * 8;
      GEN_ELEMENT  = 10 * 8;
      CAP_CONTROL  = 11 * 8;
      REG_CONTROL  = 12 * 8;
      CAP_ELEMENT  = 13 * 8;
      RELAY_CONTROL = 14 * 8;
      RECLOSER_CONTROL = 15 * 8;
      FUSE_CONTROL     = 16 * 8;
      REACTOR_ELEMENT  = 17 * 8;
      FEEDER_ELEMENT   = 18 * 8;
      GEN_CONTROL      = 19 * 8;
      SENSOR_ELEMENT   = 20 * 8;

      POWERFLOW  = 1;  // Load model types for solution
      ADMITTANCE = 2;

      // For YPrim matrices
      ALL_YPRIM = 0;
      SERIES = 1;
      SHUNT  = 2;

      {Features masks}
      PowerFlowMask :Integer = 1;
      HarmonicsMask :Integer = 2;
      DynamicsMask  :Integer = 4;



      {Control Modes}
      CONTROLSOFF = -1;
      EVENTDRIVEN = 1;
      TIMEDRIVEN  = 2;
      STATIC      = 0;

      {Randomization Constants}
      GAUSSIAN = 1;
      UNIFORM = 2;
      LOGNORMAL = 3;

      {Autoadd Constants}
      GENADD = 1;
      CAPADD = 2;

      {ERRORS}
      SOLUTION_ABORT = 99;


VAR

   DLLFirstTime   :Boolean=TRUE;
   DLLDebugFile   :TextFile;
   DSS_IniFileName:String;
   
   IsDLL,
   NoFormsAllowed  :Boolean;

   ActiveCircuit   :TCircuit;
   NumCircuits     :Integer;
   MaxCircuits     :Integer;
   MaxBusLimit     :Integer; // Set in Validation
   MaxAllocationIterations :Integer;
   Circuits        :TPointerList;
   SolutionClass   :TDSSClass;
   EnergyMeterClass:TEnergyMeter;
   FeederClass     :TFeeder;
   MonitorClass    :TDSSMonitor;
   SensorClass     :TSensor;
   TCC_CurveClass  :TTCC_Curve;
   WireDataClass   :TWireData;

   AuxParser       :TParser;  // Auxiliary parser for use by anybody for reparsing values

//{****} DebugTrace:TextFile;

   ActiveDSSClass   :TDSSClass;
   ActiveDSSObject  :TDSSObject;

   ErrorPending       :Boolean;
   CmdResult,
   ErrorNumber        :Integer;
   LastErrorMessage   :String;

   LastFileCompiled   :String;
   LastCommandWasCompile :Boolean;

   CALPHA             :Complex;  {120-degree shift constant}
   SQRT2              :Double;
   SQRT3              :Double;
   SolutionAbort      :Boolean;
   InShowResults      :Boolean;
   Redirect_Abort     :Boolean;
   In_Redirect        :Boolean;
   DIFilesAreOpen     :Boolean;
   AutoShowExport     :Boolean;

   ClassNames         :THashList;
   LastClassReferenced:Integer;  // index of class of last thing edited
   DSSClassList       :TPointerList; // pointers to the base class types
   NumIntrinsicClasses,
   NumUserClasses     :Integer;
   DSSObjs            :TPointerList;

   GlobalHelpString   :String;
   GlobalPropertyValue:String;
   GlobalResult       :String;
   VersionString      :String;

  // Some commonly used classes   so we can find them easily
   LoadShapeClass     :TLoadShape;
   GrowthShapeClass   :TGrowthShape;
   SpectrumClass      :TSpectrum;

   DefaultEditor    :String;     // normally, Notepad
   DSSFileName      :String;     // Name of current exe or DLL
   DSSDirectory     :String;     // where the current exe resides
   StartupDirectory :String;     // Where we started
   DSSDataDirectory :String;
   CircuitName_     :String;     // Name of Circuit with a "_" appended



PROCEDURE DoErrorMsg(Const S, Emsg, ProbCause :String; ErrNum:Integer);
PROCEDURE DoSimpleMsg(Const S :String; ErrNum:Integer);

PROCEDURE ClearAllCircuits;
PROCEDURE CreateDSSClasses;
PROCEDURE DisposeDSSClasses;

FUNCTION  GetDSSClassPtr(Const ClassName:String):TDSSClass;
FUNCTION  SetObjectClass(const ObjType :string):Boolean;
PROCEDURE SetObject(const param :string);
FUNCTION  SetActiveBus(const BusName:String):Integer;
PROCEDURE SetDataPath(const PathName:String);

PROCEDURE MakeNewCircuit(Const Name:String);

PROCEDURE AppendGlobalResult(Const s:String);
PROCEDURE AppendGlobalResultCRLF(const S:String);  // Separate by CRLF

PROCEDURE WriteDLLDebugFile(Const S:String);


implementation



USES  {Forms,   Controls,} SysUtils, FileCtrl,  Windows,
     DSSForms,
     Executive,
     {Intrinsic Ckt Elements}
     Solution,
     Bus,
     Line,
     VSource,
     Isource,
     LineCode,
    // Spectrum,
    // WireData,
     LineGeometry,
     Load,
    // LoadShape,     (in interface)
    // GrowthShape,
    // TCC_Curve,
     Transformer,
     Capacitor,
     Reactor,
     Fault,
     Generator,
     RegControl,
     CapControl,
     GenDispatcher,
     Relay,
     Recloser,
     Fuse
     //, Sensor
     //, Feeder
     ;

TYPE
   // Class for instantiating DSS Classes
   TDSSClasses = class(Tobject)
   private
     PROCEDURE Set_New(Value:Pointer);

   public
     constructor Create;
     destructor Destroy; override;

     Property New :pointer Write Set_New;

   End;

   THandle = Integer;

   TDSSRegister = function(var ClassName: pchar):Integer;  // Returns base class 1 or 2 are defined
   // Users can only define circuit elements at present

VAR
   DSSClasses : TDSSClasses;

   LastUserDLLHandle: THandle;
   DSSRegisterProc:TDSSRegister;   // of last library loaded


Constructor TDSSClasses.Create;

Begin
     Inherited Create;
End;

Destructor TDSSClasses.Destroy;
Begin
     Inherited Destroy;
End;

{--------------------------------------------------------------}
PROCEDURE TDSSClasses.Set_New(Value:Pointer);

Begin
    DSSClassList.New := Value; // Add to pointer list
    ActiveDSSClass := Value;   // Declare to be active
    ClassNames.Add(ActiveDSSClass.Name); // Add to classname list
End;

{--------------------------------------------------------------}
FUNCTION IsDSSDLL(Fname:String):Boolean;

Begin
    Result := FALSE;

    // Ignore if "DSSLIB.DLL"
    If CompareText(ExtractFileName(Fname),'dsslib.dll')=0 Then Exit;

   LastUserDLLHandle := LoadLibrary(pchar(Fname));
   IF LastUserDLLHandle <> 0 then BEGIN

   // Assign the address of the DSSRegister proc to DSSRegisterProc variable
    @DSSRegisterProc := GetProcAddress(LastUserDLLHandle, 'DSSRegister');
    IF @DSSRegisterProc <> nil THEN Result := TRUE
    ELSE FreeLibrary(LastUserDLLHandle);

  END;

End;



{--------------------------------------------------------------}
PROCEDURE AddUserClass;

Begin
      // ***** ADD STUFF HERE ****

      {Assumes DLL has been loaded by call to LoadLibrary and the Handle is stored
       in LastUserDLLHandle.  Also, assumes DSSRegisterProc has the address of
       the user.}





End;

{--------------------------------------------------------------}
PROCEDURE LoadUserClasses;
VAR
        F:TSearchRec;
Begin

{  Rework This !!!!}

    // Check All DLLs in present directory
    If FindFirst('*.dll', 0, F) =0 Then Begin
       Repeat
        IF IsDSSDLL(F.Name) Then AddUserclass; // Attempt to add (ignored if classname already exists)
       Until FindNext(F) <> 0;
    End;

    // Check All DLLs in DSS Directory   unless that is the directory we just checked
    If comparetext(StartupDirectory,DSSDirectory) <> 0 Then
    If FindFirst(DSSDirectory + '*.dll', 0, F) =0 Then Begin
       Repeat
        IF IsDSSDLL(F.Name) Then AddUserclass; // Attempt to add (ignored if classname already exists)
       Until FindNext(F) <> 0;
    End;

End;

{--------------------------------------------------------------}
PROCEDURE CreateDSSClasses;


Begin

     Classnames   := THashList.Create(25);   // Makes 5 sub lists
     DSSClassList := TPointerList.Create(10);  // 10 is initial size and increment
     DSSClasses   := TDSSClasses.Create;  // class to handle junk for defining DSS classes

     {General DSS objects, not circuit elements}
     DSSObjs := TPointerList.Create(25);  // 25 is initial size and increment

     {instantiate all Intrinsic Object Classes}

     {Generic Object classes first in case others refer to them}
     DSSClasses.New := TSolution.Create;
     SolutionClass  := ActiveDSSClass;     // this is a special class
     DSSClasses.New := TLineCode.Create;
     LoadShapeClass := TLoadShape.Create;
     DSSClasses.New := LoadShapeClass;
     GrowthShapeClass := TGrowthShape.Create;
     DSSClasses.New := GrowthShapeClass;
     TCC_CurveClass := TTCC_Curve.Create;
     DSSClasses.New := TCC_CurveClass;
     SpectrumClass  := TSpectrum.Create;
     DSSClasses.New := SpectrumClass;
     WireDataClass  := TWireData.Create;
     DSSClasses.New := WireDataClass;
     DSSClasses.New := TLineGeometry.Create;

     {Circuit Element Classes}
     DSSClasses.New := TLine.Create;
     DSSClasses.New := TVSource.Create;
     DSSClasses.New := TISource.Create;
     DSSClasses.New := TLoad.Create;
     DSSClasses.New := TTransf.Create;
     DSSClasses.New := TRegControl.Create;
     DSSClasses.New := TCapacitor.Create;
     DSSClasses.New := TReactor.Create;
     DSSClasses.New := TCapControl.Create;
     DSSClasses.New := TFault.Create;
     DSSClasses.New := TGenerator.Create;
     DSSClasses.New := TGenDispatcher.Create;
     DSSClasses.New := TRelay.Create;
     DSSClasses.New := TRecloser.Create;
     DSSClasses.New := TFuse.Create;
     FeederClass:= TFeeder.Create;
     DSSClasses.New := FeederClass;

     MonitorClass   := TDSSMonitor.Create;  // Have to do this AFTER Generator
     DSSClasses.New := MonitorClass;
     EnergyMeterClass := TEnergyMeter.Create;  // Have to do this AFTER Generator
     DSSClasses.New := EnergyMeterClass;
     SensorClass    := TSensor.Create;      // Create state estimation sensors
     DSSClasses.New := SensorClass;

     NumIntrinsicClasses := DSSClassList.ListSize;
     NumUserClasses := 0;


   {Add user-defined objects}

   // Check all DLLs in present directory and home DSS directory to see if they
   // are a user-defined DSS class

   //**** LoadUserClasses;


End;

//----------------------------------------------------------------------------
PROCEDURE   DisposeDSSClasses;

Var
    i :Integer;
    DSSObj :TDSSObject;
    TraceName :String;

begin

  TRY

     For i := 1 to DSSObjs.ListSize Do
     Begin
         DSSObj := DSSObjs.Get(i);
         TraceName := DSSObj.Name;
         DSSObj.Free;
     End;
     TraceName := '(DSSObjs Class)';
     DSSObjs.Free;
  EXCEPT
      On E: Exception Do
        Dosimplemsg('Exception disposing of DSS Obj "'+TraceName+'". '+CRLF + E.Message, 901);
  END;

  TRY
     For i := 1 to DSSClassList.ListSize Do TDSSClass(DSSClassList.Get(i)).Free;
     TraceName := '(DSS Class List)';
     DSSClassList.Free;
     TraceName := '(DSS Classes)';
     DSSClasses.Free;
     TraceName := '(ClassNames)';
     ClassNames.Free;
  EXCEPT
      On E: Exception Do
        Dosimplemsg('Exception disposing of DSS Class"'+TraceName+'". '+CRLF + E.Message, 902);
  END;

End;

//----------------------------------------------------------------------------
PROCEDURE DoErrorMsg(Const S, Emsg, ProbCause:String; ErrNum:Integer);

VAR
    Msg:String;
    Retval:Integer;
Begin

     Msg := Format('Error %d Reported From DSS Intrinsic Function: ', [Errnum])+ CRLF  + S
             + CRLF   + CRLF + 'Error Description: ' + CRLF + Emsg
             + CRLF   + CRLF + 'Probable Cause: ' + CRLF+ ProbCause;

     If Not NoFormsAllowed Then Begin

         If In_Redirect Then
         Begin
           RetVal := DSSMessageDlg(Msg, FALSE);
           If RetVal = -1 Then Redirect_Abort := True;
         End
         Else
           DSSMessageDlg(Msg, TRUE);

     End;

     LastErrorMessage := Msg;
     ErrorNumber := ErrNum;
     AppendGlobalResultCRLF(Msg);
End;

//----------------------------------------------------------------------------
PROCEDURE AppendGlobalResultCRLF(const S:String);

Begin
    If Length(GlobalResult) > 0
    THEN GlobalResult := GlobalResult + CRLF + S
    ELSE GlobalResult := S;
End;

//----------------------------------------------------------------------------
PROCEDURE DoSimpleMsg(Const S:String; ErrNum:Integer);

VAR
    Retval:Integer;
Begin

      IF Not NoFormsAllowed Then Begin
       IF   In_Redirect
       THEN Begin
         RetVal := DSSMessageDlg(Format('(%d) %s%s', [Errnum, CRLF, S]), FALSE);
         IF   RetVal = -1
         THEN Redirect_Abort := True;
       End
       ELSE
         DSSInfoMessageDlg(Format('(%d) %s%s', [Errnum, CRLF, S]));
      End;

     LastErrorMessage := S;
     ErrorNumber := ErrNum;
     AppendGlobalResultCRLF(S);
End;



//----------------------------------------------------------------------------
FUNCTION SetObjectClass(const ObjType :string):Boolean;

VAR Classref :Integer;

Begin

   Classref := ClassNames.Find(ObjType);

   Case Classref of
     0: Begin
            DoSimpleMsg('Error! Object Class "' + ObjType + '" not found.'+ CRLF + parser.CmdString, 903);
            Result := FALSE;
            Exit;
        End;{Error}
   ELSE
        LastClassReferenced := Classref;
   End;

   Result := TRUE;

End;

//----------------------------------------------------------------------------
PROCEDURE SetObject(const param :string);

VAR
   dotpos :Integer;
   ObjName, ObjClass :String;

Begin

      // Split off Obj class and name
      dotpos := Pos('.', Param);
      CASE dotpos OF
         0:ObjName := Copy(Param, 1, Length(Param));  // assume it is all name; class defaults
      ELSE Begin
           ObjClass := Copy(Param, 1, dotpos-1);
           ObjName  := Copy(Param, dotpos+1, Length(Param));
           End;
      End;

      IF Length(ObjClass) > 0 THEN SetObjectClass(ObjClass);

      ActiveDSSClass := DSSClassList.Get(LastClassReferenced);
      IF ActiveDSSClass <> Nil THEN
      Begin
        IF Not ActiveDSSClass.SetActive(Objname) THEN
        Begin // scroll through list of objects untill a match
          DoSimpleMsg('Error! Object "' + ObjName + '" not found.'+ CRLF + parser.CmdString, 904);
        End
        ELSE
        With ActiveCircuit Do
        Begin
           CASE ActiveDSSObject.DSSObjType OF
                DSS_OBJECT: ;  // do nothing for general DSS object

           ELSE Begin   // for circuit types, set ActiveCircuit Element, too
                 ActiveCktElement := ActiveDSSClass.GetActiveObj;
                End;
           End;
        End;
      End
      ELSE
        DoSimpleMsg('Error! Active object type/class is not set.', 905);

End;

//----------------------------------------------------------------------------
FUNCTION SetActiveBus(const BusName:String):Integer;


Begin

   // Now find the bus and set active
   Result := 0;

   WITH ActiveCircuit Do
     Begin
        If BusList.ListSize=0 Then Exit;   // Buslist not yet built
        ActiveBusIndex := BusList.Find(BusName);
        IF   ActiveBusIndex=0 Then
          Begin
            Result := 1;
            AppendGlobalResult('SetActiveBus: Bus ' + BusName + ' Not Found.');
          End;
     End;

End;

PROCEDURE ClearAllCircuits;

Begin
     
    ActiveCircuit := Circuits.First;
     WHILE ActiveCircuit<>nil DO
     Begin
        ActiveCircuit.Free;
        ActiveCircuit := Circuits.Next;
     End;
    Circuits.Free;
    Circuits := TPointerList.Create(2);   // Make a new list of circuits
    NumCircuits := 0;
End;



PROCEDURE MakeNewCircuit(Const Name:String);

//Var
//   handle :Integer;
Var
    S:String;

Begin


     If NumCircuits <= MaxCircuits - 1 Then
     Begin
         ActiveCircuit := TCircuit.Create(Name);
         ActiveDSSObject := ActiveSolutionObj;
         {*Handle := *} Circuits.Add(ActiveCircuit);
         Inc(NumCircuits);
         S := Parser.Remainder;    // Pass remainder of string on to vsource.
         {Create a default Circuit}
         SolutionABort := FALSE;
         {Voltage source named "source" connected to SourceBus}
         DSSExecutive.Command := 'New object=vsource.source Bus1=SourceBus ' + S;  // Load up the parser as if it were read in
     End
     Else
     Begin
         DoErrorMsg('MakeNewCircuit',
                    'Cannot create new circuit.',
                    'Max. Circuits Exceeded.'+CRLF+
                    '(Max no. of circuits='+inttostr(Maxcircuits)+')', 906);
     End;
End;


PROCEDURE AppendGlobalResult(Const S:String);

// Append a string to Global result, separated by commas

Begin
    If Length(GlobalResult)=0 Then
        GlobalResult := S
    Else
        GlobalResult := GlobalResult + ', ' + S;
End;



FUNCTION GetDSSVersion: String;
var

  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
  MajorVer, MinorVer, BuildNo, RelNo :DWORD;


Begin
    Result := 'Unknown.' ;

    InfoSize := GetFileVersionInfoSize(PChar(DSSFileName), Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(DSSFileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then  Begin
            MinorVer := FI.dwFileVersionMS and $FFFF;
            MajorVer := (FI.dwFileVersionMS and $FFFF0000) shr 16;
            BuildNo :=  FI.dwFileVersionLS and $FFFF;
            RelNo := (FI.dwFileVersionLS and $FFFF0000) shr 16;
            Result := IntToStr(MajorVer)+'.'+IntToStr(MinorVer)+ '.'+IntToStr(RelNo)+ ' Build '+IntToStr(BuildNo);

            End;
      finally
        FreeMem(VerBuf);
      end;
    end;

End;


FUNCTION  GetDSSClassPtr(Const ClassName:String):TDSSClass;
Begin
     Result := TDSSClass(DSSClassList.Get(ClassNames.Find(lowercase(ClassName))));
End;

PROCEDURE WriteDLLDebugFile(Const S:String);

Begin

        AssignFile(DLLDebugFile, DSSDataDirectory + 'DSSDLLDebug.TXT');
        If DLLFirstTime then Begin
           Rewrite(DLLDebugFile);
           DLLFirstTime := False;
        end
        Else Append( DLLDebugFile);
        Writeln(DLLDebugFile, S);
        CloseFile(DLLDebugFile);

End;

PROCEDURE SetDataPath(const PathName:String);

// Pathname may be null

BEGIN
    if (Length(PathName) > 0) and not DirectoryExists(PathName) then Begin

    // Try to create the directory
      if not CreateDir(PathName) then Begin
        DosimpleMsg('Cannot create ' + PathName + ' directory.', 907);
        Exit;
      End;

    End;

    DSSDataDirectory := PathName;

    // Put a \ on the end if not supplied. Allow a null specification.
    If Length(DssDataDirectory) > 0 Then Begin
      ChDir(DSSDataDirectory);   // Change to specified directory
      If DSSDataDirectory[Length(DssDataDirectory)] <> '\' Then DSSDataDirectory := DSSDataDirectory + '\';
    End;
END;



initialization

   {Various Constants and Switches}

   CALPHA                := Cmplx(-0.5, -0.866025); // -120 degrees phase shift
   SQRT2                 := Sqrt(2.0);
   SQRT3                 := Sqrt(3.0);
   CmdResult             := 0;
   DIFilesAreOpen        := FALSE;
   ErrorNumber           :=0;
   ErrorPending          := FALSE;
   GlobalHelpString      := '';
   GlobalPropertyValue   := '';
   In_Redirect           := FALSE;
   InShowResults         := FALSE;
   IsDLL                 := FALSE;
   LastCommandWasCompile := FALSE;
   LastErrorMessage      := '';
   MaxCircuits           := 1;  //  This version only allows one circuit at a time
   MaxAllocationIterations := 2;
   SolutionAbort         := FALSE;
   AutoShowExport        := FALSE;

   {Initialize filenames and directories}

   DSSFileName      := GetDSSExeFile;
   DSSDirectory     := ExtractFilePath(DSSFileName);
   VersionString    := 'Version ' + GetDSSVersion;  ;
   StartupDirectory := GetCurrentDir+'\';
   DSSDataDirectory := StartupDirectory;

   DSS_IniFileName  := 'OpenDSSPanel.ini';


   AuxParser      := TParser.Create;
   DefaultEditor  := 'NotePad';
   NoFormsAllowed := FALSE;

   //WriteDLLDebugFile('DSSGlobals');




Finalization

  // Dosimplemsg('Enter DSSGlobals Unit Finalization.');
  Auxparser.Free;
  

End.
