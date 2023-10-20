unit DSSGlobals;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{$HINTS OFF}

{ Change Log
 8-14-99  SolutionAbort Added

 10-12-99 AutoAdd constants added;
 4-17-00  Added IsShuntCapacitor routine, Updated constants
 10-08-02 Moved Control Panel Instantiation and show to here
 11-6-02  Removed load user DLL because it was causing a conflict
}

{$WARN UNIT_PLATFORM OFF}

interface

uses
    Classes,
    DSSClassDefs,
    DSSObject,
    DSSClass,
    ParserDel,
    Hashlist,
    PointerList,
    PDELement,
    UComplex,
    Arraydef,
    CktElement,
    Circuit,
    IniRegSave,
    DynamicExp,
    {$IFNDEF FPC}
    System.IOUtils,
    {$IFNDEF CONSOLE}
    Graphics,
    {$ELSE}
        CmdForms,
    {$ENDIF}
    {$ENDIF}
    inifiles,

     {Some units which have global vars defined here}
    solution,
    Spectrum,
    LoadShape,
    TempShape,
    PriceShape,
    XYCurve,
    GrowthShape,
    Monitor,
    EnergyMeter,
    Sensor,
    TCC_Curve,
    Feeder,
    WireData,
    CNData,
    TSData,
    LineSpacing,
    Storage,
    PVSystem,
    InvControl,
    ExpControl,
    variants,
    {$IFNDEF FPC}
    ShellApi,
    {$IFNDEF CONSOLE}
    ProgressForm,
    vcl.dialogs,
    WinAPI.UrlMon,
    {$ELSE}
         UrlMon,
    {$ENDIF}
    {$ENDIF}
    Strutils,
    Types,
    SyncObjs,
    YMatrix,
    fMonitor,     // by Dahei
    VSource,
    Executive,
    ExecOptions,
//     Parallel_Lib
//   TCP Indy libraries
    {$IFNDEF FPC}
    IdBaseComponent,
    IdComponent,
    IdTCPConnection,
    IdTCPClient,
    IdThreadComponent,
    {$ENDIF}
    NumCPULib,
    Command,
    ISource;

const
    CRLF = sLineBreak; // cross-platform

    PI = system.Pi;

    TwoPi = 2.0 * PI;

    RadiansToDegrees = 180.0 / PI;

    EPSILON = 1.0e-12;   // Default tiny floating point
    EPSILON2 = 1.0e-3;   // Default for Real number mismatch testing

    POWERFLOW = 1;  // Load model types for solution
    ADMITTANCE = 2;

      // For YPrim matrices
    ALL_YPRIM = 0;
    SERIES = 1;
    SHUNT = 2;

      {Control Modes}
    CONTROLSOFF = -1;
    EVENTDRIVEN = 1;
    TIMEDRIVEN = 2;
    MULTIRATE = 3;
    CTRLSTATIC = 0;

      {Randomization Constants}
    GAUSSIAN = 1;
    UNIFORM = 2;
    LOGNORMAL = 3;

      {Autoadd Constants}
    GENADD = 1;
    CAPADD = 2;

      {ERRORS}
    SOLUTION_ABORT = 99;

      {For General Sequential Time Simulations}
    USEDAILY = 0;
    USEYEARLY = 1;
    USEDUTY = 2;
    USENONE = -1;

      {Earth Model}
    SIMPLECARSON = 1;
    FULLCARSON = 2;
    DERI = 3;

      {Profile Plot Constants}
    PROFILE3PH = 9999; // some big number > likely no. of phases
    PROFILEALL = 9998;
    PROFILEALLPRI = 9997;
    PROFILELLALL = 9996;
    PROFILELLPRI = 9995;
    PROFILELL = 9994;
    PROFILEPUKM = 9993;  // not mutually exclusive to the other choices 9999..9994
    PROFILE120KFT = 9992;  // not mutually exclusive to the other choices 9999..9994

type
    TProgressActor = class(TThread)     // Global actor for progress form
        constructor Create(); OVERLOAD;
        procedure Execute; OVERRIDE;
        procedure Doterminate; OVERRIDE;
        destructor Destroy; OVERRIDE;

//*******************************Private components*****************************
    PROTECTED
        FMessage,
        Msg_Cmd: String;
//*******************************Public components******************************
    PUBLIC

    end;

var
//   LibParallel    : TParallel_Lib;
    DLLFirstTime: Boolean = true;
    DLLDebugFile: TextFile;
    ProgramName: String;
    DSS_Registry: TIniRegSave; // Registry   (See Executive)

   // Global variables for the OpenDSS Viewer
    DSS_Viz_installed: Boolean = false; // OpenDSS viewer (flag to mark a local installation)
    DSS_Viz_path: String;
    DSS_Viz_enable: Boolean = false;

   // Global variables for OpenDSS-GIS
    DSS_GIS_installed: Boolean = false; // OpenDSS-GIS (flag to mark a local installation)
    DSS_GIS_path: String;

    IsDLL,
    NoFormsAllowed: Boolean;

    ActiveCircuit: array of TDSSCircuit;
    ActiveDSSClass: array of TDSSClass;
    LastClassReferenced: array of Integer;  // index of class of last thing edited
    ActiveDSSObject: array of TDSSObject;
    NumCircuits: Integer;
    MaxCircuits: Integer;
    MaxBusLimit: Integer; // Set in Validation
    MaxAllocationIterations: Integer;
    Circuits: TPointerList;
    DSSObjs: array of TPointerList;

    AuxParser: array of TParser;  // Auxiliary parser for use by anybody for reparsing values

//{****} DebugTrace:TextFile;


    ErrorPending: Boolean;
    CmdResult,
    ErrorNumber: Integer;
    LastErrorMessage: String;

    DefaultEarthModel: Integer;
    ActiveEarthModel: array of Integer;

    LastFileCompiled: String;
    LastCommandWasCompile: Boolean;

    CALPHA: Complex;  {120-degree shift constant}
    SQRT2: Double;
    SQRT3: Double;
    InvSQRT3: Double;
    InvSQRT3x1000: Double;
    SolutionAbort: Boolean;
    InShowResults: Boolean;
    Redirect_Abort: Boolean;
    In_Redirect: Boolean;
    DIFilesAreOpen: array of Boolean;
    AutoShowExport: Boolean;
    AutoDisplayShowReport: Boolean;
    EventLogDefault: Boolean;
    SolutionWasAttempted: array of Boolean;

    GlobalHelpString: String;
    GlobalPropertyValue: String;
    GlobalResult: String;
    LastResultFile: String;
    VersionString: String;

    LogQueries: Boolean;
    QueryFirstTime: Boolean;
    QueryLogFileName: String;
    QueryLogFile: TextFile;

    DefaultEditor: String;     // normally, Notepad
    DefaultFontSize: Integer;
    DefaultFontName: String;
    DefaultFontStyles:
    {$IFNDEF FPC}
    {$IFNDEF CONSOLE}
    TFontStyles
    {$ELSE}
Integer
    {$ENDIF}
    {$ELSE}
Integer
    {$ENDIF}
    ;
    DSSFileName: String;     // Name of current exe or DLL
    DSSDirectory: String;     // where the current exe resides
    StartupDirectory: String;     // Where we started
    DataDirectory: array of String;     // used to be DSSDataDirectory
    OutputDirectory: array of String;     // output files go here, same as DataDirectory if writable
    CircuitName_: array of String;     // Name of Circuit with a "_" appended
    ActiveYPrim: array of pComplexArray; // Created to solve the problems

    DefaultBaseFreq: Double;
    DaisySize: Double;

   // Some commonly used classes   so we can find them easily
    LoadShapeClass: array of TLoadShape;
    TShapeClass: array of TTshape;
    PriceShapeClass: array of TPriceShape;
    XYCurveClass: array of TXYCurve;
    GrowthShapeClass: array of TGrowthShape;
    SpectrumClass: array of TSpectrum;
    SolutionClass: array of TDSSClass;
    EnergyMeterClass: array of TEnergyMeter;
    FMonitorClass: array of TDSSFMonitor;      // By dahei UCF
    TDynamicExpClass: array of TDynamicExp;
   // FeederClass        :TFeeder;
    MonitorClass: array of TDSSMonitor;
    SensorClass: array of TSensor;
    TCC_CurveClass: array of TTCC_Curve;
    WireDataClass: array of TWireData;
    CNDataClass: array of TCNData;
    TSDataClass: array of TTSData;
    LineSpacingClass: array of TLineSpacing;
    StorageClass: array of TStorage;
    PVSystemClass: array of TPVSystem;
    InvControlClass: array of TInvControl;
    ExpControlClass: array of TExpControl;
    ActiveVSource: array of TVsource;   // created on 01/14/2019 to facilitate actors to modify VSources while simulating

    EventStrings: array of TStringList;
    SavedFileList: array of TStringList;
    ErrorStrings: array of TStringList;

    DSSClassList: array of TPointerList; // pointers to the base class types
    ClassNames: array of THashList;

    UpdateRegistry: Boolean;  // update on program exit
    CPU_Freq: Int64;          // Used to store the CPU frequency
    CPU_Cores: Int32;
    NumNUMA: Integer;        // To store the number of NUMA nodes (should be the same as sockets)
    CPU_Physical: Int32;
    ActiveActor: Integer;
    NumOfActors: Integer;
    ActorCPU: array of Integer;
    ActorStatus: array of Integer;
    ActorProgressCount: array of Integer;
    {$IFNDEF FPC}
    {$IFNDEF CONSOLE}
    ActorProgress: array of TProgress;
    {$ENDIF}
    {$ENDIF}
    ActorPctProgress: array of Integer;
    ActorHandle: array of TSolver;

//***********************A-Diakoptics suite globals*****************************

    AllActors,
    ADiakoptics,
    ADiak_Init,
    ADiak_PCInj,
    UseUserLinks,   // To indicate if the tearing process will take place using the link branches given by the user
    Parallel_enabled,
    ConcatenateReports,

    ProgressCmd,
    IncMat_Ordered: Boolean;
    Parser: array of TParser;
    ActorMA_Msg: array of TEvent;  // Array to handle the events of each actor

   // Default ports
    DSSPrgPort,
    DSSGISPort: Integer;


{*******************************************************************************
*    Nomenclature:                                                             *
*                  OV_ Overloads                                               *
*                  VR_ Voltage report                                          *
*                  DI_ Demand interval for each meter. Moved to EnergyMeter.pas*
*                  SDI_ System Demand interval                                 *
*                  TDI_ DI Totals                                              *
*                  FM_  Meter Totals                                           *
*                  SM_  System Meter                                           *
*                  EMT_  Energy Meter Totals                                   *
*                  PHV_  Phase Voltage Report. Moved to EnergyMeter.pas        *
*     These prefixes are applied to the variables of each file mapped into     *
*     Memory using the MemoryMap_Lib                                           *
********************************************************************************
}
    OV_MHandle: array of TBytesStream;  // a. Handle to the file in memory
    VR_MHandle: array of TBytesStream;
    SDI_MHandle: array of TBytesStream;
    TDI_MHandle: array of TBytesStream;
    SM_MHandle: array of TBytesStream;
    EMT_MHandle: array of TBytesStream;
    FM_MHandle: array of TBytesStream;

//*********** Flags for appending Files*****************************************
    OV_Append: array of Boolean;
    VR_Append: array of Boolean;
    DI_Append: array of Boolean;
    SDI_Append: array of Boolean;
    TDI_Append: array of Boolean;
    SM_Append: array of Boolean;
    EMT_Append: array of Boolean;
    PHV_Append: array of Boolean;
    FM_Append: array of Boolean;

//***********************Seasonal QSTS variables********************************
    SeasonalRating: Boolean;    // Tells the energy meter if the seasonal rating feature is active
    SeasonSignal: String;     // Stores the name of the signal for selecting the rating dynamically

    DSSExecutive: array of TExecutive;

    DSSClasses: TDSSClasses;

    IsourceClass: array of TISource;
    VSourceClass: array of TVsource;

//************************ Progress actor Global defs***************************
    DSSProgressFrm,
    IsProgressON: Boolean;
    Progress_Actor: TProgressActor;
    DSSProgressPath: String;

//************************ OpenDSS-GIS Global defs***************************
    IsGISON: Boolean;
    GISThickness,
    GISColor: String;
    GISCoords: pDoubleArray;


//************************ Line related Global defs***************************
    LineTypeList: TCommandList;

//********************* Globals for DirectDLL Interface ***********************

    myStrArray: array of Byte;
    myDBLArray: array of Double;
    myCmplxArray: array of Complex;
    myPolarArray: array of Polar;
    myIntArray: array of Integer;

procedure WriteStr2Array(myStr: String);
function BArray2Str(myPtr: Pointer; var idx: Integer): String;

//*****************************************************************************

procedure DoErrorMsg(const S, Emsg, ProbCause: String; ErrNum: Integer);
procedure DoSimpleMsg(const S: String; ErrNum: Integer);
procedure DoThreadSafeMsg(const S: String; ErrNum: Integer);

procedure ClearAllCircuits;

procedure SetObject(const param: String);
function SetActiveBus(const BusName: String): Integer;
procedure SetDataPath(const PathName: String);

procedure SetLastResultFile(const Fname: String);

procedure MakeNewCircuit(const Name: String);

procedure AppendGlobalResult(const s: String);
procedure AppendGlobalResultCRLF(const S: String);  // Separate by CRLF

procedure ResetQueryLogFile;
procedure WriteQueryLogFile(const Prop, S: String);

procedure WriteDLLDebugFile(const S: String);

procedure ReadDSS_Registry;
procedure WriteDSS_Registry;

function IsDSSDLL(Fname: String): Boolean;

function GetOutputDirectory: String;

procedure MyReallocMem(var p: Pointer; newsize: Integer);
function MyAllocMem(nbytes: Cardinal): Pointer;

procedure New_Actor_Slot();
procedure New_Actor(ActorID: Integer);
procedure Wait4Actors(WType: Integer);

procedure DoClone();

procedure Delay(TickTime: Integer);

procedure GetDefaultPorts();
procedure Show_COM_Help();
procedure Show_DDLL_Help();

function Check_DSS_WebVersion(myDialog: Boolean): String;

function GetLineTypes(): String;


implementation


uses {Forms,   Controls,}

    {$IFNDEF FPC}
    Windows,
    {$IFDEF MSWINDOWS}
    SHFolder,
    {$ENDIF}
    {$IFNDEF CONSOLE}
    ScriptEdit,
    DSSForms,
    {$ENDIF}
    {$ELSE}
     resource, versiontypes, versionresource, dynlibs, CMDForms,
    {$ENDIF}
    {$IFDEF UNIX}
     BaseUnix,
    {$ENDIF}
    djson,
    SysUtils,
    ExceptionTrace;
     {Intrinsic Ckt Elements}

type

    THandle = Integer;

    TDSSRegister = function(var ClassName: Pchar): Integer;  // Returns base class 1 or 2 are defined
   // Users can only define circuit elements at present

var

    LastUserDLLHandle: THandle;
    DSSRegisterProc: TDSSRegister;        // of last library loaded
    {$IFNDEF FPC}
    idTCPClient: TIdTCPClient;  // ... TIdThreadComponent
    idThreadComponent: TIdThreadComponent;
    {$ENDIF}
{$IFDEF FPC}
FUNCTION GetDefaultDataDirectory: String;
Begin
{$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME') + '/Documents';
{$ENDIF}
{$IF (defined(Windows) or defined(MSWindows))}
  Result := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH') + '\Documents';
{$ENDIF}
end;

FUNCTION GetDefaultScratchDirectory: String;
Begin
  {$IFDEF UNIX}
  Result := '/tmp';
  {$ENDIF}
  {$IF (defined(Windows) or defined(MSWindows))}
  Result := GetEnvironmentVariable('LOCALAPPDATA');
  {$ENDIF}
End;
{$ELSE}
function GetDefaultDataDirectory: String;
var
    ThePath: array[0..MAX_PATH] of Char;
begin
    FillChar(ThePath, SizeOF(ThePath), #0);
    {$IFDEF MSWINDOWS}
    SHGetFolderPath(0, CSIDL_PERSONAL, 0, 0, ThePath);
    {$ENDIF}
    Result := ThePath;
end;

function GetDefaultScratchDirectory: String;
var
    ThePath: array[0..MAX_PATH] of Char;
begin
    FillChar(ThePath, SizeOF(ThePath), #0);
    {$IFDEF MSWINDOWS}
    SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, ThePath);
    {$ENDIF}
    Result := ThePath;
end;
{$ENDIF}

function GetOutputDirectory: String;
begin
    Result := OutputDirectory[ActiveActor];
end;

{--------------------------------------------------------------}
function IsDSSDLL(Fname: String): Boolean;

begin
    Result := false;

    // Ignore if "DSSLIB.DLL"
    if CompareText(ExtractFileName(Fname), 'dsslib.dll') = 0 then
        Exit;

    LastUserDLLHandle := LoadLibrary(Pchar(Fname));
    if LastUserDLLHandle <> 0 then
    begin

   // Assign the address of the DSSRegister proc to DSSRegisterProc variable
        @DSSRegisterProc := GetProcAddress(LastUserDLLHandle, 'DSSRegister');
        if @DSSRegisterProc <> nil then
            Result := true
        else
            FreeLibrary(LastUserDLLHandle);

    end;

end;

//----------------------------------------------------------------------------
procedure DoErrorMsg(const S, Emsg, ProbCause: String; ErrNum: Integer);

var
    Msg: String;
    Retval: Integer;
begin

    Msg := Format('Error %d Reported From OpenDSS Intrinsic Function: ', [Errnum]) + CRLF + S + CRLF + CRLF + 'Error Description: ' + CRLF + Emsg + CRLF + CRLF + 'Probable Cause: ' + CRLF + ProbCause;

    if not NoFormsAllowed then
    begin

        if In_Redirect then
        begin
            RetVal := DSSMessageDlg(Msg, false);
            if RetVal = -1 then
                Redirect_Abort := true;
        end
        else
            DSSMessageDlg(Msg, true);

    end;

    LastErrorMessage := Msg;
    ErrorNumber := ErrNum;
    AppendGlobalResultCRLF(Msg);
    SolutionAbort := true;
end;

//----------------------------------------------------------------------------
procedure AppendGlobalResultCRLF(const S: String);

begin
    if Length(GlobalResult) > 0 then
        GlobalResult := GlobalResult + CRLF + S
    else
        GlobalResult := S;

    ErrorStrings[ActiveActor].Add(Format('(%d) %s', [ErrorNumber, S]));  // Add to Error log
end;

//----------------------------------------------------------------------------
procedure DoSimpleMsg(const S: String; ErrNum: Integer);

var
    Retval: Integer;
begin

    if not NoFormsAllowed then
    begin
        if In_Redirect then
        begin
            RetVal := DSSMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]), false);
            if RetVal = -1 then
                Redirect_Abort := true;
        end
        else
            DSSInfoMessageDlg(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S]));
    end;

    LastErrorMessage := S;
    ErrorNumber := ErrNum;
    AppendGlobalResultCRLF(S);
end;

//----------------------------------------------------------------------------
procedure DoThreadSafeMsg(const S: String; ErrNum: Integer);
// generates a dialog window thread safe using windows API
var
    Retval: Integer;
begin
    {$IFDEF FPC}
     DSSMessageDlg (Format('(%d) OpenDSS %s', [Errnum, S]), True);
    {$ELSE}
    if not NoFormsAllowed then
    begin
        if In_Redirect then
        begin
            RetVal := Windows.MessageBox(0, Pchar(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S])), 'Error', MB_ABORTRETRYIGNORE);
            if RetVal = 3 then
                Redirect_Abort := true;
        end
        else
            Windows.MessageBox(0, Pchar(Format('(%d) OpenDSS %s%s', [Errnum, CRLF, S])), 'Warning', MB_OK);
    end;
    {$ENDIF}
    LastErrorMessage := S;
    ErrorNumber := ErrNum;
    AppendGlobalResultCRLF(S);
end;
//----------------------------------------------------------------------------
procedure SetObject(const param: String);

{Set object active by name}

var
    dotpos: Integer;
    ObjName, ObjClass: String;

begin

      // Split off Obj class and name
    dotpos := Pos('.', Param);
    case dotpos of
        0:
            ObjName := Copy(Param, 1, Length(Param));  // assume it is all name; class defaults
    else
    begin
        ObjClass := Copy(Param, 1, dotpos - 1);
        ObjName := Copy(Param, dotpos + 1, Length(Param));
    end;
    end;

    if Length(ObjClass) > 0 then
        SetObjectClass(ObjClass);

    ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
    if ActiveDSSClass[ActiveActor] <> nil then
    begin
        if not ActiveDSSClass[ActiveActor].SetActive(Objname) then
        begin // scroll through list of objects untill a match
            DoSimpleMsg('Error! Object "' + ObjName + '" not found.' + CRLF + parser[ActiveActor].CmdString, 904);
        end
        else
            with ActiveCircuit[ActiveActor] do
            begin
                case ActiveDSSObject[ActiveActor].DSSObjType of
                    DSS_OBJECT: ;  // do nothing for general DSS object

                else
                begin   // for circuit types, set ActiveCircuit Element, too
                    ActiveCktElement := ActiveDSSClass[ActiveActor].GetActiveObj;
                end;
                end;
            end;
    end
    else
        DoSimpleMsg('Error! Active object type/class is not set.', 905);

end;

//----------------------------------------------------------------------------
function SetActiveBus(const BusName: String): Integer;


begin

   // Now find the bus and set active
    Result := 0;

    with ActiveCircuit[ActiveActor] do
    begin
        if BusList.ListSize = 0 then
            Exit;   // Buslist not yet built
        ActiveBusIndex := BusList.Find(BusName);
        if ActiveBusIndex = 0 then
        begin
            Result := 1;
            AppendGlobalResult('SetActiveBus: Bus ' + BusName + ' Not Found.');
        end;
    end;

end;

procedure ClearAllCircuits;
var
    I: Integer;
begin

    for I := 1 to NumOfActors do
    begin
        if ActiveCircuit[I] <> nil then
        begin
            ActiveActor := I;
            ActiveCircuit[I].NumCircuits := 0;
            FreeAndNil(ActiveCircuit[I]);

        // In case the actor hasn't been destroyed
            if ActorHandle[I] <> nil then
            begin
                ActorHandle[I].Send_Message(EXIT_ACTOR);
                ActorHandle[I].WaitFor;
                FreeAndNil(ActorHandle[I]);
            end;
        end;
    end;
    FreeAndNil(Circuits);
    Circuits := TPointerList.Create(2);   // Make a new list of circuits
    // Revert on key global flags to Original States
    DefaultEarthModel := DERI;
    LogQueries := false;
    MaxAllocationIterations := 2;
    ActiveActor := 1;
end;


procedure MakeNewCircuit(const Name: String);

//Var
//   handle :Integer;
var
    S: String;

begin

    if ActiveActor <= CPU_Cores then
    begin
        if ActiveCircuit[ActiveActor] = nil then
        begin
            ActiveCircuit[ActiveActor] := TDSSCircuit.Create(Name);
            ActiveDSSObject[ActiveActor] := ActiveSolutionObj;
           {*Handle := *}
            Circuits.Add(ActiveCircuit[ActiveActor]);
            Inc(ActiveCircuit[ActiveActor].NumCircuits);
            S := Parser[ActiveActor].Remainder;    // Pass remainder of string on to vsource.
           {Create a default Circuit}
            SolutionABort := false;
           {Voltage source named "source" connected to SourceBus}
            DSSExecutive[ActiveActor].Command := 'New object=vsource.source Bus1=SourceBus ' + S;  // Load up the parser as if it were read in
           // Creates the thread for the actor if not created before
            if ActorHandle[ActiveActor] = nil then
                New_Actor(ActiveActor);


        end
        else
        begin
            DoErrorMsg('MakeNewCircuit',
                'Cannot create new circuit.',
                'Max. Circuits Exceeded.' + CRLF +
                '(Max no. of circuits=' + inttostr(Maxcircuits) + ')', 906);
        end;
    end
    else
    begin
        DoErrorMsg('MakeNewCircuit',
            'Cannot create new circuit.',
            'All the available CPUs have being assigned', 7000);

    end;
end;


procedure AppendGlobalResult(const S: String);

// Append a string to Global result, separated by commas

begin
    if Length(GlobalResult) = 0 then
        GlobalResult := S
    else
        GlobalResult := GlobalResult + ', ' + S;
end;


{$IFDEF FPC}
FUNCTION GetDSSVersion: String;
(* Unlike most of AboutText (below), this takes significant activity at run-    *)
 (* time to extract version/release/build numbers from resource information      *)
 (* appended to the binary.                                                      *)

VAR     Stream: TResourceStream;
         vr: TVersionResource;
         fi: TVersionFixedInfo;

BEGIN
   RESULT:= 'Unknown.';
   TRY

 (* This raises an exception if version info has not been incorporated into the  *)
 (* binary (Lazarus Project -> Project Options -> Version Info -> Version        *)
 (* numbering).                                                                  *)

     Stream:= TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
     TRY
       vr:= TVersionResource.Create;
       TRY
         vr.SetCustomRawDataStream(Stream);
         fi:= vr.FixedInfo;
         RESULT := 'Version ' + IntToStr(fi.FileVersion[0]) + '.' + IntToStr(fi.FileVersion[1]) +
                ' release ' + IntToStr(fi.FileVersion[2]) + ' build ' + IntToStr(fi.FileVersion[3]) + LineEnding;
         vr.SetCustomRawDataStream(nil)
       FINALLY
         FreeAndNil (vr)
       END
     FINALLY
       FreeAndNil (Stream)
     END
   EXCEPT
   END
End;
{$ELSE}
function GetDSSVersion: String;
var
    InfoSize, Wnd: DWORD;
    VerBuf: Pointer;
    FI: PVSFixedFileInfo;
    VerSize: DWORD;
    MajorVer, MinorVer, BuildNo, RelNo: DWORD;
    iLastError: DWord;
begin
    Result := 'Unknown.';
    {$IFDEF MSWINDOWS}
    InfoSize := GetFileVersionInfoSize(Pchar(DSSFileName), Wnd);
    if InfoSize <> 0 then
    begin
        GetMem(VerBuf, InfoSize);
        try
            if GetFileVersionInfo(Pchar(DSSFileName), Wnd, InfoSize, VerBuf) then
                if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
                begin
                    MinorVer := FI.dwFileVersionMS and $FFFF;
                    MajorVer := (FI.dwFileVersionMS and $FFFF0000) shr 16;
                    BuildNo := FI.dwFileVersionLS and $FFFF;
                    RelNo := (FI.dwFileVersionLS and $FFFF0000) shr 16;
                    Result := Format('%d.%d.%d.%d', [MajorVer, MinorVer, RelNo, BuildNo]);
                end;
        finally
            FreeMem(VerBuf);
        end;
    end
    else
    begin
        iLastError := GetLastError;
        Result := Format('GetFileVersionInfo failed: (%d) %s',
            [iLastError, SysErrorMessage(iLastError)]);
    end;
    {$ELSE}
      // Adds the version info as a constant (for now)
      Result := '9.4.0.2';
    {$ENDIF}
end;
{$ENDIF}

procedure WriteDLLDebugFile(const S: String);

begin

    AssignFile(DLLDebugFile, OutputDirectory[ActiveActor] + 'DSSDLLDebug.TXT');
    if DLLFirstTime then
    begin
        Rewrite(DLLDebugFile);
        DLLFirstTime := false;
    end
    else
        Append(DLLDebugFile);
    Writeln(DLLDebugFile, S);
    CloseFile(DLLDebugFile);

end;

{$IFDEF UNIX}
function IsDirectoryWritable(const Dir: String): Boolean;
begin
  Result := (FpAccess(PChar(Dir), X_OK or W_OK) = 0);
end;
{$ELSE}
function IsDirectoryWritable(const Dir: String): Boolean;
var
    TempFile: array[0..MAX_PATH] of Char;
begin
    if GetTempFileName(Pchar(Dir), 'DA', 0, TempFile) <> 0 then
        {$IFDEF FPC}
Result := DeleteFile(TempFile)
        {$ELSE}
        {$IFDEF MSWINDOWS}
        Result := Windows.DeleteFile(TempFile)
    {$ENDIF}
    {$ENDIF}
    else
        Result := false;
end;
{$ENDIF}

procedure SetDataPath(const PathName: String);
var
    ScratchPath: String;
// Pathname may be null
begin
    if (Length(PathName) > 0) and not DirectoryExists(PathName) then
    begin
  // Try to create the directory
        if not CreateDir(PathName) then
        begin
            DosimpleMsg('Cannot create ' + PathName + ' directory.', 907);
            Exit;
        end;
    end;

    DataDirectory[ActiveActor] := PathName;

  // Put a \ on the end if not supplied. Allow a null specification.
    if Length(DataDirectory[ActiveActor]) > 0 then
    begin
        ChDir(DataDirectory[ActiveActor]);   // Change to specified directory
        {$IF (defined(Windows) or defined(MSWindows))}
        if DataDirectory[ActiveActor][Length(DataDirectory[ActiveActor])] <> '\' then
            DataDirectory[ActiveActor] := DataDirectory[ActiveActor] + '\';
        {$ENDIF}
        {$IFDEF UNIX}
    If DataDirectory[ActiveActor][Length(DataDirectory[ActiveActor])] <> '/' Then DataDirectory[ActiveActor] := DataDirectory[ActiveActor] + '/';
        {$ENDIF}
    end;

  // see if DataDirectory is writable. If not, set OutputDirectory to the user's appdata
    if IsDirectoryWritable(DataDirectory[ActiveActor]) then
    begin
        OutputDirectory[ActiveActor] := DataDirectory[ActiveActor];
    end
    else
    begin
        {$IF (defined(Windows) or defined(MSWindows))}
        ScratchPath := GetDefaultScratchDirectory + '\' + ProgramName + '\';
        {$ENDIF}
        {$IFDEF UNIX}
    ScratchPath := GetDefaultScratchDirectory + '/' + ProgramName + '/';
        {$ENDIF}
        if not DirectoryExists(ScratchPath) then
            CreateDir(ScratchPath);
        OutputDirectory[ActiveActor] := ScratchPath;
    end;
end;

procedure ReadDSS_Registry;
var
    TestDataDirectory: String;
begin
    DSS_Registry.Section := 'MainSect';
    {$IFDEF Darwin}
     DefaultEditor    := DSS_Registry.ReadString('Editor', 'open -t');
     DefaultFontSize  := StrToInt(DSS_Registry.ReadString('ScriptFontSize', '12'));
     DefaultFontName  := DSS_Registry.ReadString('ScriptFontName', 'Geneva');
    {$ENDIF}
    {$IFDEF Linux}
     DefaultEditor    := DSS_Registry.ReadString('Editor', 'xdg-open');
     DefaultFontSize  := StrToInt(DSS_Registry.ReadString('ScriptFontSize', '10'));
     DefaultFontName  := DSS_Registry.ReadString('ScriptFontName', 'Arial');
    {$ENDIF}
    {$IF (defined(Windows) or defined(MSWindows))}
    DefaultEditor := DSS_Registry.ReadString('Editor', 'Notepad.exe');
    DefaultFontSize := StrToInt(DSS_Registry.ReadString('ScriptFontSize', '8'));
    DefaultFontName := DSS_Registry.ReadString('ScriptFontName', 'MS Sans Serif');
    {$ENDIF}
    {$IFDEF FPC}
     DefaultFontStyles := 1;
    {$ELSE}
    {$IFDEF CONSOLE}
     DefaultFontStyles := 1;
    {$ELSE}
    DefaultFontStyles := [];
    if DSS_Registry.ReadBool('ScriptFontBold', true) then
        DefaultFontStyles := DefaultFontStyles + [fsbold];
    if DSS_Registry.ReadBool('ScriptFontItalic', false) then
        DefaultFontStyles := DefaultFontStyles + [fsItalic];
    {$ENDIF}
    {$ENDIF}
    DefaultBaseFreq := StrToInt(DSS_Registry.ReadString('BaseFrequency', '60'));
    LastFileCompiled := DSS_Registry.ReadString('LastFile', '');
    TestDataDirectory := DSS_Registry.ReadString('DataPath', DataDirectory[ActiveActor]);
    if SysUtils.DirectoryExists(TestDataDirectory) then
        SetDataPath(TestDataDirectory)
    else
        SetDataPath(DataDirectory[ActiveActor]);
end;


procedure WriteDSS_Registry;
begin
    if UpdateRegistry and Assigned(DSS_Registry) then
    begin
        DSS_Registry.Section := 'MainSect';
        DSS_Registry.WriteString('Editor', DefaultEditor);
        DSS_Registry.WriteString('ScriptFontSize', Format('%d', [DefaultFontSize]));
        DSS_Registry.WriteString('ScriptFontName', Format('%s', [DefaultFontName]));
        DSS_Registry.WriteBool('ScriptFontBold',
            {$IFDEF FPC}
False
            {$ELSE}
            {$IFDEF CONSOLE}
False
            {$ELSE}
            (fsBold in DefaultFontStyles)
            {$ENDIF}
            {$ENDIF}
            );
        DSS_Registry.WriteBool('ScriptFontItalic',
            {$IFDEF FPC}
False
            {$ELSE}
            {$IFDEF CONSOLE}
False
            {$ELSE}
            (fsItalic in DefaultFontStyles)
            {$ENDIF}
            {$ENDIF}
            );
        DSS_Registry.WriteString('BaseFrequency', Format('%d', [Round(DefaultBaseFreq)]));
        DSS_Registry.WriteString('LastFile', LastFileCompiled);
        DSS_Registry.WriteString('DataPath', DataDirectory[ActiveActor]);
    end;
end;

procedure ResetQueryLogFile;
begin
    QueryFirstTime := true;
end;


procedure WriteQueryLogfile(const Prop, S: String);

{Log file is written after a query command if LogQueries is true.}

begin

    try
        QueryLogFileName := OutputDirectory[ActiveActor] + 'QueryLog.CSV';
        AssignFile(QueryLogFile, QueryLogFileName);
        if QueryFirstTime then
        begin
            Rewrite(QueryLogFile);  // clear the file
            Writeln(QueryLogFile, 'Time(h), Property, Result');
            QueryFirstTime := false;
        end
        else
            Append(QueryLogFile);

        Writeln(QueryLogFile, Format('%.10g, %s, %s', [ActiveCircuit[ActiveActor].Solution.DynaVars.dblHour, Prop, S]));
        CloseFile(QueryLogFile);
    except
        On E: Exception do
            DoSimpleMsg('Error writing Query Log file: ' + E.Message, 908);
    end;

end;

procedure SetLastResultFile(const Fname: String);

begin
    LastResultfile := Fname;
    ParserVars.Add('@lastfile', Fname);
end;

function MyAllocMem(nbytes: Cardinal): Pointer;
begin
    Result := AllocMem(Nbytes);
    WriteDLLDebugFile(Format('Allocating %d bytes @ %p', [nbytes, Result]));
end;

procedure MyReallocMem(var p: Pointer; newsize: Integer);

begin
    WriteDLLDebugFile(Format('Reallocating @ %p, new size= %d', [p, newsize]));
    ReallocMem(p, newsize);
end;

// Function to validate the installation and path of the OpenDSS Viewer
function GetIni(s, k: String; d: String; f: String = ''): String; OVERLOAD;
var
    ini: TMemIniFile;
begin
    Result := d;
    if f = '' then
    begin
        ini := TMemIniFile.Create(lowercase(ChangeFileExt(ParamStr(0), '.ini')));
    end
    else
    begin
        if not FileExists(f) then
            Exit;
        ini := TMemIniFile.Create(f);
    end;
    if ini.ReadString(s, k, '') = '' then
    begin
        ini.WriteString(s, k, d);
        ini.UpdateFile;
    end;
    Result := ini.ReadString(s, k, d);
    FreeAndNil(ini);
end;

//***********************DirectDLL interfacing globals**************************

procedure WriteStr2Array(myStr: String);
var
    i: Integer;
begin
// Writes the given string into an array of Bytes char by char
    for i := 1 to High(myStr) do
    begin
        setlength(myStrArray, Length(myStrArray) + 1);
        myStrArray[High(myStrArray)] := Byte(myStr[i]);
    end;
end;

function BArray2Str(myPtr: Pointer; var idx: Integer): String;
var
    S: String;
    Pchar: ^Byte;
begin
// Returns the first string found in the pointed Byte array (until a NULL char is found)
    Result := '';
    S := '';
    Pchar := myPtr;
    inc(Pbyte(Pchar), idx);
    while Pchar^ <> 0 do
    begin
        S := S + Char(Pchar^);
        inc(Pbyte(Pchar), 1);
        inc(idx);
    end;
    inc(idx);
    Result := S;
end;

//******************************************************************************

// Waits for all the actors running tasks
procedure Wait4Actors(WType: Integer);
var
    i: Integer;
    Flag: Boolean;

begin
// WType defines the starting point in which the actors will be evaluated,
// modification introduced in 01-10-2019 to facilitate the coordination
// between actors when a simulation is performed using A-Diakoptics
    for i := (WType + 1) to NumOfActors do
    begin
        try
            while ActorStatus[i] = 0 do
            begin
                Flag := true;
//        while Flag do
//          Flag  := ActorMA_Msg[i].WaitFor(1) = TWaitResult.wrTimeout;
            end;
        except
            On EOutOfMemory do
                Dosimplemsg('Exception Waiting for the parallel thread to finish a job"', 7006);
        end;
    end;
end;

// Clones the active Circuit as many times as requested if possible
procedure DoClone();
var
    i,
    NumClones: Integer;
    Ref_Ckt: String;
begin
    Ref_Ckt := LastFileCompiled;
    Parser[ActiveActor].NextParam;
    NumClones := Parser[ActiveActor].IntValue;
    Parallel_enabled := false;
    if ((NumOfActors + NumClones) <= CPU_Cores) and (NumClones > 0) then
    begin
        for i := 1 to NumClones do
        begin
            New_Actor_Slot;
            DSSExecutive[ActiveActor].Command := 'compile "' + Ref_Ckt + '"';
        // sets the previous maxiterations and controliterations
            ActiveCircuit[ActiveActor].solution.MaxIterations := ActiveCircuit[1].solution.MaxIterations;
            ActiveCircuit[ActiveACtor].solution.MaxControlIterations := ActiveCircuit[1].solution.MaxControlIterations;
        // Solves the circuit
            CmdResult := ExecOptions.DoSetCmd(1);
        end;

    end
    else
    begin
        if NumClones > 0 then
            DoSimpleMsg('There are no more CPUs available', 7001)
        else
            DoSimpleMsg('The number of clones requested is invalid', 7004)
    end;
end;

// Prepares memory to host a new actor
procedure New_Actor_Slot();
begin
    if NumOfActors < CPU_Cores then
    begin
        inc(NumOfActors);
        GlobalResult := inttostr(NumOfActors);
        ActiveActor := NumOfActors;
        ActorCPU[ActiveActor] := -1;       // By default, the actor will have affinity to all processors (-1)
        DSSExecutive[ActiveActor] := TExecutive.Create;  // Make a DSS object
        Parser[ActiveActor] := TParser.Create;
        AuxParser[ActiveActor] := TParser.Create;
        DSSExecutive[ActiveActor].CreateDefaultDSSItems;
    end
    else
        DoSimpleMsg('There are no more CPUs available', 7001)
end;

// Creates a new actor
procedure New_Actor(ActorID: Integer);
{$IFDEF FPC}
Begin
 ActorHandle[ActorID] :=  TSolver.Create(True,ActorCPU[ActorID],ActorID,nil,ActorMA_Msg[ActorID]); // TEMC: TODO: text-mode callback
 ActorHandle[ActorID].Priority :=  tpTimeCritical;
 ActorHandle[ActorID].Start; // Resume;
 ActorStatus[ActorID] :=  1;
End;
{$ELSE}
{$IFDEF CONSOLE}
Begin
 ActorHandle[ActorID] :=  TSolver.Create(True,ActorCPU[ActorID],ActorID,nil,ActorMA_Msg[ActorID]); // TEMC: TODO: text-mode callback
 ActorHandle[ActorID].Priority :=  tpTimeCritical;
 ActorHandle[ActorID].Resume;  // TEMC: TODO: this reportedly does nothing on Unix and Mac
 ActorStatus[ActorID] :=  1;
End;
{$ELSE}
var
    ScriptEd: TScriptEdit;
begin
    ScriptEd := TScriptEdit.Create;
    ActorHandle[ActorID] := TSolver.Create(true, ActorCPU[ActorID], ActorID, ScriptEd.UpdateSummaryform, ActorMA_Msg[ActorID]);
    ActorHandle[ActorID].Priority :=
        {$IFDEF MSWINDOWS}
        tptimecritical
    {$ELSE}
6
    {$ENDIF}
    ;
    ActorHandle[ActorID].Start; // Resume;
    ActorStatus[ActorID] := 1;
    FreeAndNil(ScriptEd);
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
// Validates the installation and path of the OpenDSS Viewer
function CheckOpenDSSViewer(App_Folder: String): Boolean;
var
    FileName: String;
begin
  // to make it compatible with the function
    App_Folder := LowerCase(App_Folder);

  // Stores the
    if App_Folder = 'opendss_viewer' then
    begin
        DSS_Viz_path := GetIni('Application', 'path', '', TPath.GetHomePath + '\' + App_Folder + '\settings.ini');
        FileName := stringreplace(DSS_Viz_path, '\\', '\', [rfReplaceAll, rfIgnoreCase])
    end
    else
    begin
        DSS_GIS_path := GetIni('Application', 'path', '', TPath.GetHomePath + '\' + App_Folder + '\settings.ini');
        FileName := stringreplace(DSS_GIS_path, '\\', '\', [rfReplaceAll, rfIgnoreCase]);
    end;

    FileName := stringreplace(FileName, '"', '', [rfReplaceAll, rfIgnoreCase]);
  // returns true only if the executable exists
    Result := fileexists(FileName);
end;
{$ENDIF}

//{$IFDEF MSWINDOWS}
procedure Delay(TickTime: Integer);
var
    Past: Longword;
begin
    Past := GetTickCount64;
    repeat

    until (GetTickCount64 - Past) >= Longint(TickTime);
end;
//{$ENDIF}

//*********Downloads a file from the internet into the folder specified*********
function DownLoadInternetFile(Source, Dest: String): Boolean;
begin
    {$IFNDEF FPC}
    try
        Result := URLDownloadToFile(nil, Pchar(Source), Pchar(Dest), 0, nil) = 0
    except
        Result := false;
    end;
    {$ELSE}
  Result := False;
    {$ENDIF}
end;

//******Verifies the OpenDSS version using the reference at Sourceforge*********
function Check_DSS_WebVersion(myDialog: Boolean): String;
    {$IFDEF FPC}
begin
  DSSMessageDlg ('Check_DSS_Webversion() not implemented on FPC; needs Indy', False);
  Result := '';
    {$ELSE}
var
    myVersion,
    myText,
    myWebSrc,
    myPath: String;
    myFile: TextFile;
    myIdx: Integer;

begin
    myIdx := 0;
    myPath := TPath.GetTempPath + '\myDSSVersion.txt';
    myWebSrc := 'https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Source/Current_ver.txt';
  // Download the file into the Windows temporary folder
    if DownLoadInternetFile(myWebSrc, myPath) then
    begin
        AssignFile(myFile, myPath);
        Reset(myFile);
        while not Eof(myFile) do
        begin
            ReadLn(myFile, myText);
            myIdx := pos('mydssversion=', lowercase(myText));
            if myIdx > 0 then
                break;
        end;
        CloseFile(myFile);
    end;
    myText := myText.Substring(myIdx + 12);
    myVersion := VersionString.Substring(8);
    myIdx := pos(' ', myVersion);
    myVersion := myVersion.Substring(0, myIdx - 1);

    if myText <> myVersion then
    begin
        myPath := 'There is a new version of OpenDSS available for download' + CRLF +
            'The new version can be located at:' + CRLF + CRLF +
            'https://sourceforge.net/projects/electricdss/';

        if myDialog then
        begin
            {$IFNDEF CONSOLE}
            ShowMessage(myPath);
            {$ELSE}
        DSSMessageDlg(myPath, TRUE);
            {$ENDIF}
        end;

    end
    else
        myPath := 'OpenDSS is up-to-date';

    Result := myPath;

    {$ENDIF}
end;

//**********************Launches the COM Help file******************************
procedure Show_COM_Help();
begin
    {$IFDEF FPC}
  DSSMessageDlg ('Show_COM_Help() not implemented on FPC', False);
    {$ELSE}
    ShellExecute(0, 'open', Pwidechar(DSSDirectory + '\OpenDSS_COM.chm'), nil, nil, SW_SHOWNORMAL);
    {$ENDIF}
end;

//**********************Launches the DirectDLL help file******************************
procedure Show_DDLL_Help();
begin
    {$IFDEF FPC}
  DSSMessageDlg ('Show_DDLL_Help() not implemented on FPC', False);
    {$ELSE}
    ShellExecute(0, 'open', Pwidechar(DSSDirectory + '\DirectDLL_Help.chm'), nil, nil, SW_SHOWNORMAL);
    {$ENDIF}
end;

//*********************Gets the processor information***************************
procedure Get_Processor_Info();
begin
    NumNUMA := 1;
    CPU_Physical := TNumCPULib.GetPhysicalCPUCount();
    CPU_Cores := TNumCPULib.GetLogicalCPUCount();
end;

constructor TProgressActor.Create();
var
    J: Integer;
begin
    {$IFNDEF FPC}
    ShellExecute(Handle, 'open', Pwidechar(DSSProgressPath), nil, nil, SW_SHOWNORMAL);
    sleep(200);
  // ... create TIdTCPClient
    idTCPClient := TIdTCPClient.Create();
  // ... set properties
    idTCPClient.Host := 'localhost';
    idTCPClient.Port := DSSPrgPort;
    idThreadComponent := TIdThreadComponent.Create();

    if ADiakoptics and (ActiveActor = 1) then
        J := 1
    else
        J := NumOfActors;
    try
        IdTCPClient.Connect;
        IdTCPClient.IOHandler.WriteLn('num' + inttostr(J));
        IsProgressON := true;
    except
        on E: Exception do
        begin
            IsProgressON := false;
            raise;
        end;
    end;
    {$ENDIF}
    inherited Create(false);
end;

procedure TProgressActor.Execute;
{$IFDEF FPC}
begin
end;
{$ELSE}
var
    I, J: Integer;
    AbortBtn,
    progStr: String;
    RunFlag: Boolean;
begin

    if IsProgressON then
    begin
        RunFlag := true;
        while RunFlag do
        begin
            sleep(100);
            progStr := '';
            RunFlag := false;
            if ADiakoptics and (ActiveActor = 1) then
                J := 1
            else
                J := NumOfActors;

            for I := 1 to J do
            begin
                progStr := progStr + Format('%.*d', [3, ActorPctProgress[I]]);
                RunFlag := RunFlag or (ActorStatus[I] = 0);
            end;
            IdTCPClient.IOHandler.WriteLn('prg' + progStr);
            AbortBtn := IdTCPClient.IOHandler.ReadLn;
            if AbortBtn.Substring(0, 1) = 'T' then
                SolutionAbort := true;
        end;
        IdTCPClient.IOHandler.WriteLn('ext');
    end;
end;
{$ENDIF}

procedure TProgressActor.DoTerminate;        // Is the end of the thread
begin
    IsProgressON := false;
    inherited;
end;

destructor TProgressActor.Destroy;
begin
    inherited destroy;
end;

procedure GetDefaultPorts();
var
    F: TextFile;
    JSONCfg: TdJSON;
    JSONStr,
    iniFilePath: String;
begin
    iniFilePath := DSSDirectory + 'ComPorts.ini';
    if fileexists(iniFilePath) then
    begin
        AssignFile(F, iniFilePath);
        Reset(F);
        ReadLn(F, JSONStr);
        CloseFile(F);
    // parse the JSON string and extract the values
        JSONCfg := TdJSON.Parse(JSONStr);
        DSSPrgPort := JSONCfg['dssprogress'].AsInteger;
        DSSGISPort := JSONCfg['dssgis'].AsInteger;
    end
    else
    begin                                      // Since the Cfg file is missing, use the defaults
        DSSPrgPort := 20010;
        DSSGISPort := 20011;
    end;

end;

function GetLineTypes(): String;
// Returns a string containing the line types
// the string format is the standard DSS array format (comma separated)
var
    idx: Integer;
    separator: String;
begin

    Result := '[';
    separator := '';
    for idx := 1 to LineTypeList.NumCommands do
    begin
        Result := Result + separator + LineTypeList.Get(idx);
        separator := ',';
    end;

    Result := Result + ']';

end;

procedure LocalFinalization;
var
    Actor: Integer;
begin
    for Actor := 1 to NumOfActors do
    begin
        with DSSExecutive[Actor] do
            if RecorderOn then
                Recorderon := false;
        FreeAndNil(DSSExecutive[Actor]);  {Writes to Registry}
        FreeAndNil(EventStrings[Actor]);
        FreeAndNil(SavedFileList[Actor]);
        FreeAndNil(ErrorStrings[Actor]);
        FreeAndNil(ActorHandle[Actor]);
        FreeAndNil(Auxparser[Actor]);
    end;
    FreeAndNil(DSS_Registry);  {Close Registry}
end;

initialization

try
//***************Initialization for Parallel Processing*************************

    Get_processor_info();

    setlength(ActiveCircuit, CPU_Cores + 1);
    {$IFNDEF FPC}
    {$IFNDEF CONSOLE}
    setlength(ActorProgress, CPU_Cores + 1);
    {$ENDIF}
    {$ENDIF}
    setlength(ActorCPU, CPU_Cores + 1);
    setlength(ActorProgressCount, CPU_Cores + 1);
    setlength(ActiveDSSClass, CPU_Cores + 1);
    setlength(DataDirectory, CPU_Cores + 1);
    setlength(OutputDirectory, CPU_Cores + 1);
    setlength(CircuitName_, CPU_Cores + 1);
    setlength(ActorPctProgress, CPU_Cores + 1);
    setlength(ActiveDSSObject, CPU_Cores + 1);
    setlength(LastClassReferenced, CPU_Cores + 1);
    setlength(DSSObjs, CPU_Cores + 1);
    setlength(ActiveEarthModel, CPU_Cores + 1);
    setlength(DSSClassList, CPU_Cores + 1);
    setlength(ClassNames, CPU_Cores + 1);
    setlength(MonitorClass, CPU_Cores + 1);
    setlength(LoadShapeClass, CPU_Cores + 1);
    setlength(TShapeClass, CPU_Cores + 1);
    setlength(PriceShapeClass, CPU_Cores + 1);
    setlength(XYCurveClass, CPU_Cores + 1);
    setlength(GrowthShapeClass, CPU_Cores + 1);
    setlength(SpectrumClass, CPU_Cores + 1);
    setlength(SolutionClass, CPU_Cores + 1);
    setlength(EnergyMeterClass, CPU_Cores + 1);
    setlength(SensorClass, CPU_Cores + 1);
    setlength(TCC_CurveClass, CPU_Cores + 1);
    setlength(WireDataClass, CPU_Cores + 1);
    setlength(CNDataClass, CPU_Cores + 1);
    setlength(TSDataClass, CPU_Cores + 1);
    setlength(LineSpacingClass, CPU_Cores + 1);
    setlength(StorageClass, CPU_Cores + 1);
    setlength(PVSystemClass, CPU_Cores + 1);
    setlength(InvControlClass, CPU_Cores + 1);
    setlength(ExpControlClass, CPU_Cores + 1);
    setlength(EventStrings, CPU_Cores + 1);
    setlength(SavedFileList, CPU_Cores + 1);
    setlength(ErrorStrings, CPU_Cores + 1);
    setlength(ActorHandle, CPU_Cores + 1);
    setlength(Parser, CPU_Cores + 1);
    setlength(AuxParser, CPU_Cores + 1);
    setlength(ActiveYPrim, CPU_Cores + 1);
    SetLength(SolutionWasAttempted, CPU_Cores + 1);
    SetLength(ActorStatus, CPU_Cores + 1);
    SetLength(ActorMA_Msg, CPU_Cores + 1);
    SetLength(ActiveVSource, CPU_Cores + 1);
    SetLength(TDynamicExpClass, CPU_Cores + 1);

    setlength(FMonitorClass, CPU_Cores + 1);    // by Dahei UCF
   // Init pointer repositories for the EnergyMeter in multiple cores

    SetLength(OV_MHandle, CPU_Cores + 1);
    SetLength(VR_MHandle, CPU_Cores + 1);
    SetLength(SDI_MHandle, CPU_Cores + 1);
    SetLength(TDI_MHandle, CPU_Cores + 1);
    SetLength(SM_MHandle, CPU_Cores + 1);
    SetLength(EMT_MHandle, CPU_Cores + 1);
    SetLength(FM_MHandle, CPU_Cores + 1);
    SetLength(OV_Append, CPU_Cores + 1);
    SetLength(VR_Append, CPU_Cores + 1);
    SetLength(DI_Append, CPU_Cores + 1);
    SetLength(SDI_Append, CPU_Cores + 1);
    SetLength(TDI_Append, CPU_Cores + 1);
    SetLength(SM_Append, CPU_Cores + 1);
    SetLength(EMT_Append, CPU_Cores + 1);
    SetLength(PHV_Append, CPU_Cores + 1);
    SetLength(FM_Append, CPU_Cores + 1);
    SetLength(DIFilesAreOpen, CPU_Cores + 1);
    SetLength(DSSExecutive, CPU_Cores + 1);
    SetLength(IsourceClass, CPU_Cores + 1);
    SetLength(VSourceClass, CPU_Cores + 1);

    for ActiveActor := 1 to CPU_Cores do
    begin
        ActiveCircuit[ActiveActor] := nil;
        {$IFNDEF FPC}
        {$IFNDEF CONSOLE}
        ActorProgress[ActiveActor] := nil;
        {$ENDIF}
        {$ENDIF}
        ActiveDSSClass[ActiveActor] := nil;
        EventStrings[ActiveActor] := TStringList.Create;
        SavedFileList[ActiveActor] := TStringList.Create;
        ErrorStrings[ActiveActor] := TStringList.Create;
        ErrorStrings[ActiveActor].Clear;
        ActorHandle[ActiveActor] := nil;
        Parser[ActiveActor] := nil;
        ActorStatus[ActiveActor] := 1;

        OV_MHandle[ActiveActor] := nil;
        VR_MHandle[ActiveActor] := nil;
        SDI_MHandle[ActiveActor] := nil;
        TDI_MHandle[ActiveActor] := nil;
        SM_MHandle[ActiveActor] := nil;
        EMT_MHandle[ActiveActor] := nil;
        FM_MHandle[ActiveActor] := nil;
        DIFilesAreOpen[ActiveActor] := false;

        ActiveVSource[Activeactor] := nil;
        DSSObjs[ActiveActor] := nil;
        DSSClassList[ActiveActor] := nil;
    end;

    GISThickness := '3';
    GISColor := 'FF0000';
    GISCoords := AllocMem(Sizeof(Double) * 4);
    UseUserLinks := false;
    IsProgressOn := false;

    Progress_Actor := nil;
    DSSClasses := nil;
    ProgressCmd := false;

    Allactors := false;
    ActiveActor := 1;
    NumOfActors := 1;
    ActorCPU[ActiveActor] := -1;
    Parser[ActiveActor] := Tparser.Create;
    ProgramName := 'OpenDSS';
    DSSFileName := GetDSSExeFile;
    DSSDirectory := ExtractFilePath(DSSFileName);
    ADiakoptics := false;  // Disabled by default
    ADiak_Init := false;
    EventLogDefault := false;  // Disabled by default

    GetDefaultPorts();                 // Gets the default ports to get connected to other add-ons
    {$IFNDEF CONSOLE}
    DSSProgressFrm := GetDSSProgress(DSSFileName);
    {$ENDIF}

    SeasonalRating := false;
    SeasonSignal := '';

   {Various Constants and Switches}
    {$IFDEF FPC}
NoFormsAllowed  := TRUE;
    {$ENDIF}

    CALPHA := Cmplx(-0.5, -0.866025); // -120 degrees phase shift
    SQRT2 := Sqrt(2.0);
    SQRT3 := Sqrt(3.0);
    InvSQRT3 := 1.0 / SQRT3;
    InvSQRT3x1000 := InvSQRT3 * 1000.0;
    CmdResult := 0;
   //DIFilesAreOpen        := FALSE;
    ErrorNumber := 0;
    ErrorPending := false;
    GlobalHelpString := '';
    GlobalPropertyValue := '';
    LastResultFile := '';
    In_Redirect := false;
    InShowResults := false;
    IsDLL := false;
    LastCommandWasCompile := false;
    LastErrorMessage := '';
    MaxCircuits := 1;  //  Not required anymore. planning to remove it
    MaxAllocationIterations := 2;
    SolutionAbort := false;
    AutoShowExport := false;
    AutoDisplayShowReport := true;
    SolutionWasAttempted[ActiveActor] := false;

    DefaultBaseFreq := 60.0;
    DaisySize := 1.0;
    DefaultEarthModel := DERI;
    ActiveEarthModel[ActiveActor] := DefaultEarthModel;
    Parallel_enabled := false;
    ConcatenateReports := false;

//******************************************************************************
// Empty Globals for DirectDLL in case they are not used

    setlength(myStrArray, 0);
    setlength(myDBLArray, 0);
    setlength(myCmplxArray, 0);
    setlength(myPolarArray, 0);
    setlength(myIntArray, 0);

//******************************************************************************

   {Initialize filenames and directories}

    {$IFDEF FPC}
   ProgramName      := 'OpenDSSCmd';  // for now...
    {$ELSE}
    ProgramName := 'OpenDSS';
    {$ENDIF}
    DSSFileName := GetDSSExeFile;
    DSSDirectory := ExtractFilePath(DSSFileName);
   // want to know if this was built for 64-bit, not whether running on 64 bits
   // (i.e. we could have a 32-bit build running on 64 bits; not interested in that
    {$IFDEF CPUX64}
    VersionString := 'Version ' + GetDSSVersion + ' (64-bit build)';
    {$ELSE ! CPUX86}
   VersionString    := 'Version ' + GetDSSVersion + ' (32-bit build)';
    {$ENDIF}

    {$IFNDEF FPC}
    StartupDirectory := GetCurrentDir + '\';
    SetDataPath(GetDefaultDataDirectory + '\' + ProgramName + '\');
    DSS_Registry := TIniRegSave.Create('\Software\' + ProgramName);


    {$ELSE}
{$IFDEF WINDOWS} // deliberately different from MSWindows (Delphi)
        StartupDirectory := GetCurrentDir+'\';
        SetDataPath (GetDefaultDataDirectory + '\' + ProgramName + '\');
        DSS_Registry     := TIniRegSave.Create(DataDirectory[ActiveActor] + 'opendsscmd.ini');
{$ENDIF}
{$IFDEF UNIX}
        StartupDirectory := GetCurrentDir+'/';
        SetDataPath (GetDefaultDataDirectory + '/' + ProgramName + '/');
        DSS_Registry     := TIniRegSave.Create(DataDirectory[ActiveActor] + 'opendsscmd.ini');
{$ENDIF}
    {$ENDIF}

    AuxParser[ActiveActor] := TParser.Create;

    {$IFDEF Darwin}
      DefaultEditor   := 'open -t';
      DefaultFontSize := 12;
      DefaultFontName := 'Geneva';
    {$ENDIF}
    {$IFDEF Linux}
      DefaultEditor   := 'xdg-open';
      DefaultFontSize := 10;
      DefaultFontName := 'Arial';
    {$ENDIF}
    {$IF (defined(Windows) or defined(MSWindows))}
    DefaultEditor := 'Notepad.exe';
    DefaultFontSize := 8;
    DefaultFontName := 'MS Sans Serif';
    {$ENDIF}

    {$IFNDEF FPC}
    NoFormsAllowed := false;
    {$ENDIF}

    LogQueries := false;
    QueryLogFileName := '';
    UpdateRegistry := true;
    {$IFDEF FPC}
   CPU_Freq := 1000; // until we can query it
    {$ELSE}
    QueryPerformanceFrequency(CPU_Freq);
    {$ENDIF}
//   CPU_Cores        :=  CPUCount;

    IsMultithread := true;
   //WriteDLLDebugFile('DSSGlobals');

    LineTypeList := TCommandList.Create(
        ['OH', 'UG', 'UG_TS', 'UG_CN', 'SWT_LDBRK', 'SWT_FUSE', 'SWT_SECT', 'SWT_REC', 'SWT_DISC', 'SWT_BRK', 'SWT_ELBOW', 'BUSBAR']);
    LineTypeList.Abbrev := true;  // Allow abbreviations for line type code

    {$IFNDEF FPC}
    DSS_Viz_installed := CheckOpenDSSViewer('OpenDSS_Viewer');  // OpenDSS Viewer (flag for detected installation)
    DSS_GIS_installed := CheckOpenDSSViewer('OpenDSS_GIS');     // OpenDSS GIS (flag for detected installation)
    if not IsDLL then
    begin
        Check_DSS_WebVersion(true);
    end;
    {$ENDIF}

except
    On E: Exception do
        DumpExceptionCallStack(E);
end;

finalization

  // Dosimplemsg('Enter DSSGlobals Unit Finalization.');
//  YBMatrix.Finish_Ymatrix_Critical;   // Ends the critical segment for the YMatrix class

//  ClearAllCircuits; // this is also done later, when Executive destroyed from LocalFinalization
    LocalFinalization;
    LineTypeList.Destroy;
end.
