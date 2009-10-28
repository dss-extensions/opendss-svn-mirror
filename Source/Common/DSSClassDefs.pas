unit DSSClassDefs;

interface

USES
     DSSClass,
     PointerList,
     HashList;

CONST

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
      STORAGE_ELEMENT  = 21 * 8;
      STORAGE_CONTROL  = 22 * 8;
      SWT_CONTROL      = 23 * 8;

VAR
   ClassNames         :THashList;
   DSSClassList       :TPointerList; // pointers to the base class types
   NumIntrinsicClasses,
   NumUserClasses     :Integer;

PROCEDURE CreateDSSClasses;
PROCEDURE DisposeDSSClasses;
FUNCTION  GetDSSClassPtr(Const ClassName:String):TDSSClass;
FUNCTION  SetObjectClass(const ObjType :string):Boolean;


implementation

USES
     SysUtils,
     DSSGlobals,
     DSSObject,
     ParserDel,

     Solution,
     Bus,
     Line,
     VSource,
     Isource,
     LineCode,
     Spectrum,
     WireData,
     LineGeometry,
     LineSpacing,
     Load,
     LoadShape,
     Monitor,
     EnergyMeter,
     GrowthShape,
     TCC_Curve,
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
     Fuse,
     Sensor,
     Feeder,
     XfmrCode,
     Storage,
     StorageController,
     SwtControl
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


VAR
   DSSClasses         :TDSSClasses;



{--------------------------------------------------------------}
Constructor TDSSClasses.Create;

Begin
     Inherited Create;
End;

{--------------------------------------------------------------}
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
PROCEDURE CreateDSSClasses;


Begin

     Classnames   := THashList.Create(25);   // Makes 5 sub lists
     DSSClassList := TPointerList.Create(10);  // 10 is initial size and increment
     DSSClasses   := TDSSClasses.Create;  // class to handle junk for defining DSS classes

     {General DSS objects, not circuit elements}
     DSSObjs := TPointerList.Create(25);  // 25 is initial size and increment

     {instantiate all Intrinsic Object Classes}

     {Generic Object classes first in case others refer to them}
     DSSClasses.New := TDSSSolution.Create;
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
     LineSpacingClass := TLineSpacing.Create;
     DSSClasses.New := LineSpacingClass;
     DSSClasses.New := TXfmrCode.Create;

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
     StorageClass   := TStorage.Create;
     DSSClasses.New := StorageClass;
     DSSClasses.New := TStorageController.Create;
     DSSClasses.New := TRelay.Create;
     DSSClasses.New := TRecloser.Create;
     DSSClasses.New := TFuse.Create;
     FeederClass    := TFeeder.Create;
     DSSClasses.New := FeederClass;
     DSSClasses.New := TSwtControl.Create;

     MonitorClass   := TDSSMonitor.Create;  // Have to do this AFTER Generator
     DSSClasses.New := MonitorClass;
     EnergyMeterClass := TEnergyMeter.Create;  // Have to do this AFTER Generator
     DSSClasses.New := EnergyMeterClass;
     SensorClass    := TSensor.Create;      // Create state estimation sensors
     DSSClasses.New := SensorClass;

     NumIntrinsicClasses := DSSClassList.ListSize;
     NumUserClasses := 0;


   {Add user-defined objects}

   {This feature has been disabled - doesn't work in IIS}

   // Check all DLLs in present directory and home DSS directory to see if they
   // are a user-defined DSS class

   //**** LoadUserClasses;


End;

//----------------------------------------------------------------------------
PROCEDURE   DisposeDSSClasses;

VAR
    i :Integer;
    DSSObj :TDSSObject;
    TraceName :String;

BEGIN

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


{--------------------------------------------------------------}
PROCEDURE AddUserClass;

Begin
      // ***** ADD STUFF HERE ****

      {Assumes DLL has been loaded by call to LoadLibrary and the Handle is stored
       in LastUserDLLHandle.  Also, assumes DSSRegisterProc has the address of
       the user.}


     {Needs to be re-done}


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

//----------------------------------------------------------------------------
FUNCTION SetObjectClass(const ObjType :string):Boolean;

// set LastClassReferenced variable by class name

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
FUNCTION  GetDSSClassPtr(Const ClassName:String):TDSSClass;
Begin
     Result := TDSSClass(DSSClassList.Get(ClassNames.Find(lowercase(ClassName))));
End;


end.
