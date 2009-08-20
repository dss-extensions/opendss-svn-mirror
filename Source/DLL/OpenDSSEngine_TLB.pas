unit OpenDSSengine_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 8291 $
// File generated on 7/8/2009 12:01:32 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\OpenDSS\Source\DLL\OpenDSSengine.tlb (1)
// LIBID: {8BFDE413-245A-4514-B151-B16DCC243796}
// LCID: 0
// Helpfile: 
// HelpString: OpenDSS Engine
// DepndLst: 
//   (1) v1.0 stdole, (C:\WINDOWS\system32\stdole32.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  OpenDSSengineMajorVersion = 1;
  OpenDSSengineMinorVersion = 0;

  LIBID_OpenDSSengine: TGUID = '{8BFDE413-245A-4514-B151-B16DCC243796}';

  IID_IText: TGUID = '{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}';
  CLASS_Text: TGUID = '{6E20BC4C-67C0-4AD3-9E12-BF90C478A1CC}';
  IID_IDSSProperty: TGUID = '{1298D126-0750-4B2A-8462-62EFE7310DF2}';
  CLASS_DSSProperty: TGUID = '{F8410F14-7E85-44A9-B42F-F900DF5F596E}';
  IID_ICktElement: TGUID = '{F20E8327-5B60-478E-8DBD-5EFC75EB929B}';
  CLASS_CktElement: TGUID = '{BC5F55A3-7A0F-4923-B218-098A91F482D8}';
  IID_IError: TGUID = '{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}';
  CLASS_Error: TGUID = '{0038D0EB-28ED-42B0-A247-E212E05ADF4B}';
  IID_ICircuit: TGUID = '{32441C6D-7A27-4164-B5B0-FA054300C217}';
  CLASS_Circuit: TGUID = '{B5B695B1-A1F5-444F-ABC5-836B7EF1DF0D}';
  IID_IBus: TGUID = '{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}';
  CLASS_Bus: TGUID = '{A14C32E4-846B-444D-9070-F7A31E9F5FF9}';
  IID_IDSS: TGUID = '{14644AD0-4909-48FF-B624-24E8C38D1AED}';
  CLASS_DSS: TGUID = '{6FE9D1B8-C064-4877-94C0-F13882ADBDB6}';
  IID_ISolution: TGUID = '{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}';
  CLASS_Solution: TGUID = '{F799E1DE-E7BF-4F86-BCED-6DD01FD00419}';
  IID_IMonitors: TGUID = '{5C339E44-C583-445C-91D1-3B1E49CAD6B0}';
  CLASS_Monitors: TGUID = '{7FF93D6F-4258-40CB-9558-0792422309F3}';
  IID_IMeters: TGUID = '{86705B6C-352A-47F8-A24B-78B750EC3859}';
  CLASS_Meters: TGUID = '{F869D5BB-A023-48AB-A459-01444585B7C1}';
  IID_IGenerators: TGUID = '{2D9B7548-D03E-478A-9FEA-9FC4033C793E}';
  CLASS_Generators: TGUID = '{65F232C9-7D95-4E45-B9FA-40F518CFBB64}';
  IID_IDSSProgress: TGUID = '{315C0C38-929C-4942-BDF8-6DA12D001B47}';
  CLASS_DSSProgress: TGUID = '{4CB900D9-DD2F-41AF-9E48-B999E0AED0A7}';
  IID_ISettings: TGUID = '{4E3928A0-8B75-4127-885F-F4AD6B3F4323}';
  CLASS_Settings: TGUID = '{9D910AA4-0CB3-4907-AEEF-8DD79A58C0AD}';
  IID_ILines: TGUID = '{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}';
  CLASS_Lines: TGUID = '{A1352870-9D53-4E48-B83A-6DB0C8FED65B}';
  IID_ICtrlQueue: TGUID = '{55055001-5EEC-4667-9CCA-63F3A60F31F3}';
  CLASS_CtrlQueue: TGUID = '{19DD7174-7FEE-4E59-97ED-C54F16EDC3F0}';
  IID_ILoads: TGUID = '{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}';
  CLASS_Loads: TGUID = '{1302A34B-A554-4C32-BCED-4AF0A94FF114}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum MonitorModes
type
  MonitorModes = TOleEnum;
const
  dssVI = $00000000;
  dssPower = $00000001;
  dssSequence = $00000010;
  dssMagnitude = $00000020;
  dssPosOnly = $00000040;
  dssTaps = $00000002;
  dssStates = $00000003;

// Constants for enum SolveModes
type
  SolveModes = TOleEnum;
const
  dssSnapShot = $00000000;
  dssDutyCycle = $00000006;
  dssDirect = $00000007;
  dssDaily = $00000001;
  dssMonte1 = $00000003;
  dssMonte2 = $0000000A;
  dssMonte3 = $0000000B;
  dssFaultStudy = $00000009;
  dssYearly = $00000002;
  dssMonteFault = $00000008;
  dssPeakDay = $00000005;
  dssLD1 = $00000004;
  dssLD2 = $0000000C;
  dssAutoAdd = $0000000D;
  dssHarmonic = $0000000F;
  dssDynamic = $0000000E;

// Constants for enum Options
type
  Options = TOleEnum;
const
  dssPowerFlow = $00000001;
  dssAdmittance = $00000002;
  dssNormalSolve = $00000000;
  dssNewtonSolve = $00000001;
  dssStatic = $00000000;
  dssEvent = $00000001;
  dssTime = $00000002;
  dssMultiphase = $00000000;
  dssPositiveSeq = $00000001;
  dssGaussian = $00000001;
  dssUniform = $00000002;
  dssLogNormal = $00000003;
  dssAddGen = $00000001;
  dssAddCap = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IText = interface;
  ITextDisp = dispinterface;
  IDSSProperty = interface;
  IDSSPropertyDisp = dispinterface;
  ICktElement = interface;
  ICktElementDisp = dispinterface;
  IError = interface;
  IErrorDisp = dispinterface;
  ICircuit = interface;
  ICircuitDisp = dispinterface;
  IBus = interface;
  IBusDisp = dispinterface;
  IDSS = interface;
  IDSSDisp = dispinterface;
  ISolution = interface;
  ISolutionDisp = dispinterface;
  IMonitors = interface;
  IMonitorsDisp = dispinterface;
  IMeters = interface;
  IMetersDisp = dispinterface;
  IGenerators = interface;
  IGeneratorsDisp = dispinterface;
  IDSSProgress = interface;
  IDSSProgressDisp = dispinterface;
  ISettings = interface;
  ISettingsDisp = dispinterface;
  ILines = interface;
  ILinesDisp = dispinterface;
  ICtrlQueue = interface;
  ICtrlQueueDisp = dispinterface;
  ILoads = interface;
  ILoadsDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Text = IText;
  DSSProperty = IDSSProperty;
  CktElement = ICktElement;
  Error = IError;
  Circuit = ICircuit;
  Bus = IBus;
  DSS = IDSS;
  Solution = ISolution;
  Monitors = IMonitors;
  Meters = IMeters;
  Generators = IGenerators;
  DSSProgress = IDSSProgress;
  Settings = ISettings;
  Lines = ILines;
  CtrlQueue = ICtrlQueue;
  Loads = ILoads;


// *********************************************************************//
// Interface: IText
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0513A8DC-2C0D-4648-8BD7-2130B82C05FA}
// *********************************************************************//
  IText = interface(IDispatch)
    ['{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}']
    function Get_Command: WideString; safecall;
    procedure Set_Command(const Command: WideString); safecall;
    function Get_Result: WideString; safecall;
    property Command: WideString read Get_Command write Set_Command;
    property Result: WideString read Get_Result;
  end;

// *********************************************************************//
// DispIntf:  ITextDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0513A8DC-2C0D-4648-8BD7-2130B82C05FA}
// *********************************************************************//
  ITextDisp = dispinterface
    ['{0513A8DC-2C0D-4648-8BD7-2130B82C05FA}']
    property Command: WideString dispid 1;
    property Result: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IDSSProperty
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1298D126-0750-4B2A-8462-62EFE7310DF2}
// *********************************************************************//
  IDSSProperty = interface(IDispatch)
    ['{1298D126-0750-4B2A-8462-62EFE7310DF2}']
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_Val: WideString; safecall;
    procedure Set_Val(const Value: WideString); safecall;
    property Name: WideString read Get_Name;
    property Description: WideString read Get_Description;
    property Val: WideString read Get_Val write Set_Val;
  end;

// *********************************************************************//
// DispIntf:  IDSSPropertyDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1298D126-0750-4B2A-8462-62EFE7310DF2}
// *********************************************************************//
  IDSSPropertyDisp = dispinterface
    ['{1298D126-0750-4B2A-8462-62EFE7310DF2}']
    property Name: WideString readonly dispid 1;
    property Description: WideString readonly dispid 3;
    property Val: WideString dispid 2;
  end;

// *********************************************************************//
// Interface: ICktElement
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F20E8327-5B60-478E-8DBD-5EFC75EB929B}
// *********************************************************************//
  ICktElement = interface(IDispatch)
    ['{F20E8327-5B60-478E-8DBD-5EFC75EB929B}']
    function Get_Name: WideString; safecall;
    function Get_NumTerminals: Integer; safecall;
    function Get_NumConductors: Integer; safecall;
    function Get_NumPhases: Integer; safecall;
    function Get_BusNames: OleVariant; safecall;
    procedure Set_BusNames(Value: OleVariant); safecall;
    function Get_Properties(Indx: OleVariant): IDSSProperty; safecall;
    function Get_Voltages: OleVariant; safecall;
    function Get_Currents: OleVariant; safecall;
    function Get_Powers: OleVariant; safecall;
    function Get_Losses: OleVariant; safecall;
    function Get_PhaseLosses: OleVariant; safecall;
    function Get_SeqVoltages: OleVariant; safecall;
    function Get_SeqCurrents: OleVariant; safecall;
    function Get_SeqPowers: OleVariant; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_NormalAmps: Double; safecall;
    procedure Set_NormalAmps(Value: Double); safecall;
    function Get_EmergAmps: Double; safecall;
    procedure Set_EmergAmps(Value: Double); safecall;
    procedure Open(Term: Integer; Phs: Integer); safecall;
    procedure Close(Term: Integer; Phs: Integer); safecall;
    function IsOpen(Term: Integer; Phs: Integer): WordBool; safecall;
    function Get_NumProperties: Integer; safecall;
    function Get_AllPropertyNames: OleVariant; safecall;
    function Get_Residuals: OleVariant; safecall;
    function Get_Yprim: OleVariant; safecall;
    property Name: WideString read Get_Name;
    property NumTerminals: Integer read Get_NumTerminals;
    property NumConductors: Integer read Get_NumConductors;
    property NumPhases: Integer read Get_NumPhases;
    property BusNames: OleVariant read Get_BusNames write Set_BusNames;
    property Properties[Indx: OleVariant]: IDSSProperty read Get_Properties;
    property Voltages: OleVariant read Get_Voltages;
    property Currents: OleVariant read Get_Currents;
    property Powers: OleVariant read Get_Powers;
    property Losses: OleVariant read Get_Losses;
    property PhaseLosses: OleVariant read Get_PhaseLosses;
    property SeqVoltages: OleVariant read Get_SeqVoltages;
    property SeqCurrents: OleVariant read Get_SeqCurrents;
    property SeqPowers: OleVariant read Get_SeqPowers;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property NormalAmps: Double read Get_NormalAmps write Set_NormalAmps;
    property EmergAmps: Double read Get_EmergAmps write Set_EmergAmps;
    property NumProperties: Integer read Get_NumProperties;
    property AllPropertyNames: OleVariant read Get_AllPropertyNames;
    property Residuals: OleVariant read Get_Residuals;
    property Yprim: OleVariant read Get_Yprim;
  end;

// *********************************************************************//
// DispIntf:  ICktElementDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F20E8327-5B60-478E-8DBD-5EFC75EB929B}
// *********************************************************************//
  ICktElementDisp = dispinterface
    ['{F20E8327-5B60-478E-8DBD-5EFC75EB929B}']
    property Name: WideString readonly dispid 1;
    property NumTerminals: Integer readonly dispid 2;
    property NumConductors: Integer readonly dispid 3;
    property NumPhases: Integer readonly dispid 4;
    property BusNames: OleVariant dispid 5;
    property Properties[Indx: OleVariant]: IDSSProperty readonly dispid 6;
    property Voltages: OleVariant readonly dispid 7;
    property Currents: OleVariant readonly dispid 8;
    property Powers: OleVariant readonly dispid 9;
    property Losses: OleVariant readonly dispid 10;
    property PhaseLosses: OleVariant readonly dispid 11;
    property SeqVoltages: OleVariant readonly dispid 12;
    property SeqCurrents: OleVariant readonly dispid 13;
    property SeqPowers: OleVariant readonly dispid 14;
    property Enabled: WordBool dispid 15;
    property NormalAmps: Double dispid 16;
    property EmergAmps: Double dispid 17;
    procedure Open(Term: Integer; Phs: Integer); dispid 18;
    procedure Close(Term: Integer; Phs: Integer); dispid 19;
    function IsOpen(Term: Integer; Phs: Integer): WordBool; dispid 20;
    property NumProperties: Integer readonly dispid 21;
    property AllPropertyNames: OleVariant readonly dispid 22;
    property Residuals: OleVariant readonly dispid 23;
    property Yprim: OleVariant readonly dispid 24;
  end;

// *********************************************************************//
// Interface: IError
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}
// *********************************************************************//
  IError = interface(IDispatch)
    ['{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}']
    function Get_Number: Integer; safecall;
    function Get_Description: WideString; safecall;
    property Number: Integer read Get_Number;
    property Description: WideString read Get_Description;
  end;

// *********************************************************************//
// DispIntf:  IErrorDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}
// *********************************************************************//
  IErrorDisp = dispinterface
    ['{B521E339-8ED2-4BD6-9AEB-FD349CA8D8E3}']
    property Number: Integer readonly dispid 1;
    property Description: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ICircuit
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {32441C6D-7A27-4164-B5B0-FA054300C217}
// *********************************************************************//
  ICircuit = interface(IDispatch)
    ['{32441C6D-7A27-4164-B5B0-FA054300C217}']
    function Get_Name: WideString; safecall;
    function Get_NumCktElements: Integer; safecall;
    function Get_NumBuses: Integer; safecall;
    function Get_NumNodes: Integer; safecall;
    function Get_Buses(Index: OleVariant): IBus; safecall;
    function Get_CktElements(Idx: OleVariant): ICktElement; safecall;
    function Get_Losses: OleVariant; safecall;
    function Get_LineLosses: OleVariant; safecall;
    function Get_SubstationLosses: OleVariant; safecall;
    function Get_TotalPower: OleVariant; safecall;
    function Get_AllBusVolts: OleVariant; safecall;
    function Get_AllBusVmag: OleVariant; safecall;
    function Get_AllElementNames: OleVariant; safecall;
    function Get_ActiveElement: ICktElement; safecall;
    procedure Disable(const Name: WideString); safecall;
    procedure Enable(const Name: WideString); safecall;
    function Get_Solution: ISolution; safecall;
    function Get_ActiveBus: IBus; safecall;
    function FirstPCElement: Integer; safecall;
    function NextPCElement: Integer; safecall;
    function FirstPDElement: Integer; safecall;
    function NextPDElement: Integer; safecall;
    function Get_AllBusNames: OleVariant; safecall;
    function Get_AllElementLosses: OleVariant; safecall;
    procedure Sample; safecall;
    procedure SaveSample; safecall;
    function Get_Monitors: IMonitors; safecall;
    function Get_Meters: IMeters; safecall;
    function Get_Generators: IGenerators; safecall;
    function Get_Settings: ISettings; safecall;
    function Get_Lines: ILines; safecall;
    function SetActiveElement(const FullName: WideString): Integer; safecall;
    function Capacity(Start: Double; Increment: Double): Double; safecall;
    function SetActiveBus(const BusName: WideString): Integer; safecall;
    function SetActiveBusi(BusIndex: Integer): Integer; safecall;
    function Get_AllBusVmagPu: OleVariant; safecall;
    function Get_AllNodeNames: OleVariant; safecall;
    function Get_SystemY: OleVariant; safecall;
    function Get_CtrlQueue: ICtrlQueue; safecall;
    function Get_AllBusDistances: OleVariant; safecall;
    function Get_AllNodeDistances: OleVariant; safecall;
    function Get_AllNodeVmagByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeVmagPUByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeDistancesByPhase(Phase: Integer): OleVariant; safecall;
    function Get_AllNodeNamesByPhase(Phase: Integer): OleVariant; safecall;
    function Get_Loads: Loads; safecall;
    property Name: WideString read Get_Name;
    property NumCktElements: Integer read Get_NumCktElements;
    property NumBuses: Integer read Get_NumBuses;
    property NumNodes: Integer read Get_NumNodes;
    property Buses[Index: OleVariant]: IBus read Get_Buses;
    property CktElements[Idx: OleVariant]: ICktElement read Get_CktElements;
    property Losses: OleVariant read Get_Losses;
    property LineLosses: OleVariant read Get_LineLosses;
    property SubstationLosses: OleVariant read Get_SubstationLosses;
    property TotalPower: OleVariant read Get_TotalPower;
    property AllBusVolts: OleVariant read Get_AllBusVolts;
    property AllBusVmag: OleVariant read Get_AllBusVmag;
    property AllElementNames: OleVariant read Get_AllElementNames;
    property ActiveElement: ICktElement read Get_ActiveElement;
    property Solution: ISolution read Get_Solution;
    property ActiveBus: IBus read Get_ActiveBus;
    property AllBusNames: OleVariant read Get_AllBusNames;
    property AllElementLosses: OleVariant read Get_AllElementLosses;
    property Monitors: IMonitors read Get_Monitors;
    property Meters: IMeters read Get_Meters;
    property Generators: IGenerators read Get_Generators;
    property Settings: ISettings read Get_Settings;
    property Lines: ILines read Get_Lines;
    property AllBusVmagPu: OleVariant read Get_AllBusVmagPu;
    property AllNodeNames: OleVariant read Get_AllNodeNames;
    property SystemY: OleVariant read Get_SystemY;
    property CtrlQueue: ICtrlQueue read Get_CtrlQueue;
    property AllBusDistances: OleVariant read Get_AllBusDistances;
    property AllNodeDistances: OleVariant read Get_AllNodeDistances;
    property AllNodeVmagByPhase[Phase: Integer]: OleVariant read Get_AllNodeVmagByPhase;
    property AllNodeVmagPUByPhase[Phase: Integer]: OleVariant read Get_AllNodeVmagPUByPhase;
    property AllNodeDistancesByPhase[Phase: Integer]: OleVariant read Get_AllNodeDistancesByPhase;
    property AllNodeNamesByPhase[Phase: Integer]: OleVariant read Get_AllNodeNamesByPhase;
    property Loads: Loads read Get_Loads;
  end;

// *********************************************************************//
// DispIntf:  ICircuitDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {32441C6D-7A27-4164-B5B0-FA054300C217}
// *********************************************************************//
  ICircuitDisp = dispinterface
    ['{32441C6D-7A27-4164-B5B0-FA054300C217}']
    property Name: WideString readonly dispid 1;
    property NumCktElements: Integer readonly dispid 2;
    property NumBuses: Integer readonly dispid 3;
    property NumNodes: Integer readonly dispid 4;
    property Buses[Index: OleVariant]: IBus readonly dispid 5;
    property CktElements[Idx: OleVariant]: ICktElement readonly dispid 6;
    property Losses: OleVariant readonly dispid 7;
    property LineLosses: OleVariant readonly dispid 8;
    property SubstationLosses: OleVariant readonly dispid 9;
    property TotalPower: OleVariant readonly dispid 10;
    property AllBusVolts: OleVariant readonly dispid 11;
    property AllBusVmag: OleVariant readonly dispid 12;
    property AllElementNames: OleVariant readonly dispid 13;
    property ActiveElement: ICktElement readonly dispid 14;
    procedure Disable(const Name: WideString); dispid 15;
    procedure Enable(const Name: WideString); dispid 16;
    property Solution: ISolution readonly dispid 17;
    property ActiveBus: IBus readonly dispid 18;
    function FirstPCElement: Integer; dispid 19;
    function NextPCElement: Integer; dispid 20;
    function FirstPDElement: Integer; dispid 21;
    function NextPDElement: Integer; dispid 22;
    property AllBusNames: OleVariant readonly dispid 23;
    property AllElementLosses: OleVariant readonly dispid 24;
    procedure Sample; dispid 25;
    procedure SaveSample; dispid 26;
    property Monitors: IMonitors readonly dispid 27;
    property Meters: IMeters readonly dispid 28;
    property Generators: IGenerators readonly dispid 29;
    property Settings: ISettings readonly dispid 30;
    property Lines: ILines readonly dispid 31;
    function SetActiveElement(const FullName: WideString): Integer; dispid 32;
    function Capacity(Start: Double; Increment: Double): Double; dispid 33;
    function SetActiveBus(const BusName: WideString): Integer; dispid 34;
    function SetActiveBusi(BusIndex: Integer): Integer; dispid 36;
    property AllBusVmagPu: OleVariant readonly dispid 35;
    property AllNodeNames: OleVariant readonly dispid 37;
    property SystemY: OleVariant readonly dispid 38;
    property CtrlQueue: ICtrlQueue readonly dispid 201;
    property AllBusDistances: OleVariant readonly dispid 202;
    property AllNodeDistances: OleVariant readonly dispid 203;
    property AllNodeVmagByPhase[Phase: Integer]: OleVariant readonly dispid 204;
    property AllNodeVmagPUByPhase[Phase: Integer]: OleVariant readonly dispid 205;
    property AllNodeDistancesByPhase[Phase: Integer]: OleVariant readonly dispid 206;
    property AllNodeNamesByPhase[Phase: Integer]: OleVariant readonly dispid 207;
    property Loads: Loads readonly dispid 208;
  end;

// *********************************************************************//
// Interface: IBus
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E5B78C35-88F8-495F-8CD1-EBB5D90ED228}
// *********************************************************************//
  IBus = interface(IDispatch)
    ['{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}']
    function Get_Name: WideString; safecall;
    function Get_NumNodes: Integer; safecall;
    function Get_Voltages: OleVariant; safecall;
    function Get_SeqVoltages: OleVariant; safecall;
    function Get_Nodes: OleVariant; safecall;
    function Get_Voc: OleVariant; safecall;
    function Get_Isc: OleVariant; safecall;
    function Get_puVoltages: OleVariant; safecall;
    function Get_kVBase: Double; safecall;
    function Get_ZscMatrix: OleVariant; safecall;
    function Get_Zsc1: OleVariant; safecall;
    function Get_Zsc0: OleVariant; safecall;
    function ZscRefresh: WordBool; safecall;
    function Get_YscMatrix: OleVariant; safecall;
    function Get_Coorddefined: WordBool; safecall;
    function Get_x: Double; safecall;
    procedure Set_x(Value: Double); safecall;
    function Get_y: Double; safecall;
    procedure Set_y(Value: Double); safecall;
    function Get_Distance: Double; safecall;
    property Name: WideString read Get_Name;
    property NumNodes: Integer read Get_NumNodes;
    property Voltages: OleVariant read Get_Voltages;
    property SeqVoltages: OleVariant read Get_SeqVoltages;
    property Nodes: OleVariant read Get_Nodes;
    property Voc: OleVariant read Get_Voc;
    property Isc: OleVariant read Get_Isc;
    property puVoltages: OleVariant read Get_puVoltages;
    property kVBase: Double read Get_kVBase;
    property ZscMatrix: OleVariant read Get_ZscMatrix;
    property Zsc1: OleVariant read Get_Zsc1;
    property Zsc0: OleVariant read Get_Zsc0;
    property YscMatrix: OleVariant read Get_YscMatrix;
    property Coorddefined: WordBool read Get_Coorddefined;
    property x: Double read Get_x write Set_x;
    property y: Double read Get_y write Set_y;
    property Distance: Double read Get_Distance;
  end;

// *********************************************************************//
// DispIntf:  IBusDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E5B78C35-88F8-495F-8CD1-EBB5D90ED228}
// *********************************************************************//
  IBusDisp = dispinterface
    ['{E5B78C35-88F8-495F-8CD1-EBB5D90ED228}']
    property Name: WideString readonly dispid 1;
    property NumNodes: Integer readonly dispid 2;
    property Voltages: OleVariant readonly dispid 3;
    property SeqVoltages: OleVariant readonly dispid 4;
    property Nodes: OleVariant readonly dispid 5;
    property Voc: OleVariant readonly dispid 6;
    property Isc: OleVariant readonly dispid 7;
    property puVoltages: OleVariant readonly dispid 8;
    property kVBase: Double readonly dispid 9;
    property ZscMatrix: OleVariant readonly dispid 10;
    property Zsc1: OleVariant readonly dispid 11;
    property Zsc0: OleVariant readonly dispid 12;
    function ZscRefresh: WordBool; dispid 13;
    property YscMatrix: OleVariant readonly dispid 14;
    property Coorddefined: WordBool readonly dispid 201;
    property x: Double dispid 202;
    property y: Double dispid 203;
    property Distance: Double readonly dispid 204;
  end;

// *********************************************************************//
// Interface: IDSS
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {14644AD0-4909-48FF-B624-24E8C38D1AED}
// *********************************************************************//
  IDSS = interface(IDispatch)
    ['{14644AD0-4909-48FF-B624-24E8C38D1AED}']
    function Get_NumCircuits: Integer; safecall;
    function Get_Circuits(Idx: OleVariant): ICircuit; safecall;
    function Get_ActiveCircuit: ICircuit; safecall;
    function Get_Text: IText; safecall;
    function Get_Error: IError; safecall;
    function NewCircuit(const Name: WideString): ICircuit; safecall;
    procedure ClearAll; safecall;
    procedure ShowPanel; safecall;
    function Start(code: Integer): WordBool; safecall;
    function Get_Version: WideString; safecall;
    function Get_DSSProgress: IDSSProgress; safecall;
    function Get_Classes: OleVariant; safecall;
    function Get_UserClasses: OleVariant; safecall;
    function Get_NumClasses: Integer; safecall;
    function Get_NumUserClasses: Integer; safecall;
    function Get_DataPath: WideString; safecall;
    procedure Set_DataPath(const Value: WideString); safecall;
    procedure Reset; safecall;
    function Get_AllowForms: WordBool; safecall;
    procedure Set_AllowForms(Value: WordBool); safecall;
    property NumCircuits: Integer read Get_NumCircuits;
    property Circuits[Idx: OleVariant]: ICircuit read Get_Circuits;
    property ActiveCircuit: ICircuit read Get_ActiveCircuit;
    property Text: IText read Get_Text;
    property Error: IError read Get_Error;
    property Version: WideString read Get_Version;
    property DSSProgress: IDSSProgress read Get_DSSProgress;
    property Classes: OleVariant read Get_Classes;
    property UserClasses: OleVariant read Get_UserClasses;
    property NumClasses: Integer read Get_NumClasses;
    property NumUserClasses: Integer read Get_NumUserClasses;
    property DataPath: WideString read Get_DataPath write Set_DataPath;
    property AllowForms: WordBool read Get_AllowForms write Set_AllowForms;
  end;

// *********************************************************************//
// DispIntf:  IDSSDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {14644AD0-4909-48FF-B624-24E8C38D1AED}
// *********************************************************************//
  IDSSDisp = dispinterface
    ['{14644AD0-4909-48FF-B624-24E8C38D1AED}']
    property NumCircuits: Integer readonly dispid 1;
    property Circuits[Idx: OleVariant]: ICircuit readonly dispid 2;
    property ActiveCircuit: ICircuit readonly dispid 3;
    property Text: IText readonly dispid 4;
    property Error: IError readonly dispid 5;
    function NewCircuit(const Name: WideString): ICircuit; dispid 6;
    procedure ClearAll; dispid 7;
    procedure ShowPanel; dispid 8;
    function Start(code: Integer): WordBool; dispid 9;
    property Version: WideString readonly dispid 10;
    property DSSProgress: IDSSProgress readonly dispid 11;
    property Classes: OleVariant readonly dispid 12;
    property UserClasses: OleVariant readonly dispid 13;
    property NumClasses: Integer readonly dispid 14;
    property NumUserClasses: Integer readonly dispid 15;
    property DataPath: WideString dispid 17;
    procedure Reset; dispid 18;
    property AllowForms: WordBool dispid 20;
  end;

// *********************************************************************//
// Interface: ISolution
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F2332365-962A-4DF4-9D1E-218E0B0F2CEF}
// *********************************************************************//
  ISolution = interface(IDispatch)
    ['{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}']
    procedure Solve; safecall;
    function Get_Mode: Integer; safecall;
    procedure Set_Mode(Mode: Integer); safecall;
    function Get_Frequency: Double; safecall;
    procedure Set_Frequency(Frequency: Double); safecall;
    function Get_Hour: Integer; safecall;
    procedure Set_Hour(Hour: Integer); safecall;
    function Get_Seconds: Double; safecall;
    procedure Set_Seconds(Seconds: Double); safecall;
    function Get_StepSize: Double; safecall;
    procedure Set_StepSize(StepSize: Double); safecall;
    function Get_Year: Integer; safecall;
    procedure Set_Year(Year: Integer); safecall;
    function Get_LoadMult: Double; safecall;
    procedure Set_LoadMult(LoadMult: Double); safecall;
    function Get_Iterations: Integer; safecall;
    function Get_MaxIterations: Integer; safecall;
    procedure Set_MaxIterations(MaxIterations: Integer); safecall;
    function Get_Tolerance: Double; safecall;
    procedure Set_Tolerance(Tolerance: Double); safecall;
    function Get_Number: Integer; safecall;
    procedure Set_Number(Number: Integer); safecall;
    function Get_Random: Integer; safecall;
    procedure Set_Random(Random: Integer); safecall;
    function Get_ModeID: WideString; safecall;
    function Get_LoadModel: Integer; safecall;
    procedure Set_LoadModel(Value: Integer); safecall;
    function Get_LDCurve: WideString; safecall;
    procedure Set_LDCurve(const Value: WideString); safecall;
    function Get_pctGrowth: Double; safecall;
    procedure Set_pctGrowth(Value: Double); safecall;
    function Get_AddType: Integer; safecall;
    procedure Set_AddType(Value: Integer); safecall;
    function Get_GenkW: Double; safecall;
    procedure Set_GenkW(Value: Double); safecall;
    function Get_GenPF: Double; safecall;
    procedure Set_GenPF(Value: Double); safecall;
    function Get_Capkvar: Double; safecall;
    procedure Set_Capkvar(Value: Double); safecall;
    function Get_Algorithm: Integer; safecall;
    procedure Set_Algorithm(Value: Integer); safecall;
    function Get_ControlMode: Integer; safecall;
    procedure Set_ControlMode(Value: Integer); safecall;
    function Get_GenMult: Double; safecall;
    procedure Set_GenMult(Value: Double); safecall;
    function Get_DefaultDaily: WideString; safecall;
    procedure Set_DefaultDaily(const Value: WideString); safecall;
    function Get_DefaultYearly: WideString; safecall;
    procedure Set_DefaultYearly(const Value: WideString); safecall;
    function Get_EventLog: OleVariant; safecall;
    function Get_dblHour: Double; safecall;
    procedure Set_dblHour(Value: Double); safecall;
    procedure Set_StepsizeMin(Param1: Double); safecall;
    procedure Set_StepsizeHr(Param1: Double); safecall;
    function Get_ControlIterations: Integer; safecall;
    procedure Set_ControlIterations(Value: Integer); safecall;
    function Get_MaxControlIterations: Integer; safecall;
    procedure Set_MaxControlIterations(Value: Integer); safecall;
    procedure Sample_DoControlActions; safecall;
    procedure CheckFaultStatus; safecall;
    procedure SolveSnap; safecall;
    procedure SolveDirect; safecall;
    procedure SolvePflow; safecall;
    procedure SolveNoControl; safecall;
    procedure SolvePlusControl; safecall;
    procedure InitSnap; safecall;
    procedure CheckControls; safecall;
    procedure SampleControlDevices; safecall;
    procedure DoControlActions; safecall;
    procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Integer); safecall;
    function Get_SystemYChanged: WordBool; safecall;
    function Get_Converged: WordBool; safecall;
    procedure Set_Converged(Value: WordBool); safecall;
    property Mode: Integer read Get_Mode write Set_Mode;
    property Frequency: Double read Get_Frequency write Set_Frequency;
    property Hour: Integer read Get_Hour write Set_Hour;
    property Seconds: Double read Get_Seconds write Set_Seconds;
    property StepSize: Double read Get_StepSize write Set_StepSize;
    property Year: Integer read Get_Year write Set_Year;
    property LoadMult: Double read Get_LoadMult write Set_LoadMult;
    property Iterations: Integer read Get_Iterations;
    property MaxIterations: Integer read Get_MaxIterations write Set_MaxIterations;
    property Tolerance: Double read Get_Tolerance write Set_Tolerance;
    property Number: Integer read Get_Number write Set_Number;
    property Random: Integer read Get_Random write Set_Random;
    property ModeID: WideString read Get_ModeID;
    property LoadModel: Integer read Get_LoadModel write Set_LoadModel;
    property LDCurve: WideString read Get_LDCurve write Set_LDCurve;
    property pctGrowth: Double read Get_pctGrowth write Set_pctGrowth;
    property AddType: Integer read Get_AddType write Set_AddType;
    property GenkW: Double read Get_GenkW write Set_GenkW;
    property GenPF: Double read Get_GenPF write Set_GenPF;
    property Capkvar: Double read Get_Capkvar write Set_Capkvar;
    property Algorithm: Integer read Get_Algorithm write Set_Algorithm;
    property ControlMode: Integer read Get_ControlMode write Set_ControlMode;
    property GenMult: Double read Get_GenMult write Set_GenMult;
    property DefaultDaily: WideString read Get_DefaultDaily write Set_DefaultDaily;
    property DefaultYearly: WideString read Get_DefaultYearly write Set_DefaultYearly;
    property EventLog: OleVariant read Get_EventLog;
    property dblHour: Double read Get_dblHour write Set_dblHour;
    property StepsizeMin: Double write Set_StepsizeMin;
    property StepsizeHr: Double write Set_StepsizeHr;
    property ControlIterations: Integer read Get_ControlIterations write Set_ControlIterations;
    property MaxControlIterations: Integer read Get_MaxControlIterations write Set_MaxControlIterations;
    property SystemYChanged: WordBool read Get_SystemYChanged;
    property Converged: WordBool read Get_Converged write Set_Converged;
  end;

// *********************************************************************//
// DispIntf:  ISolutionDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F2332365-962A-4DF4-9D1E-218E0B0F2CEF}
// *********************************************************************//
  ISolutionDisp = dispinterface
    ['{F2332365-962A-4DF4-9D1E-218E0B0F2CEF}']
    procedure Solve; dispid 1;
    property Mode: Integer dispid 2;
    property Frequency: Double dispid 3;
    property Hour: Integer dispid 4;
    property Seconds: Double dispid 5;
    property StepSize: Double dispid 6;
    property Year: Integer dispid 7;
    property LoadMult: Double dispid 8;
    property Iterations: Integer readonly dispid 9;
    property MaxIterations: Integer dispid 10;
    property Tolerance: Double dispid 11;
    property Number: Integer dispid 12;
    property Random: Integer dispid 13;
    property ModeID: WideString readonly dispid 14;
    property LoadModel: Integer dispid 15;
    property LDCurve: WideString dispid 16;
    property pctGrowth: Double dispid 17;
    property AddType: Integer dispid 18;
    property GenkW: Double dispid 19;
    property GenPF: Double dispid 20;
    property Capkvar: Double dispid 21;
    property Algorithm: Integer dispid 22;
    property ControlMode: Integer dispid 23;
    property GenMult: Double dispid 24;
    property DefaultDaily: WideString dispid 25;
    property DefaultYearly: WideString dispid 26;
    property EventLog: OleVariant readonly dispid 27;
    property dblHour: Double dispid 201;
    property StepsizeMin: Double writeonly dispid 202;
    property StepsizeHr: Double writeonly dispid 203;
    property ControlIterations: Integer dispid 204;
    property MaxControlIterations: Integer dispid 205;
    procedure Sample_DoControlActions; dispid 206;
    procedure CheckFaultStatus; dispid 207;
    procedure SolveSnap; dispid 208;
    procedure SolveDirect; dispid 209;
    procedure SolvePflow; dispid 210;
    procedure SolveNoControl; dispid 211;
    procedure SolvePlusControl; dispid 212;
    procedure InitSnap; dispid 213;
    procedure CheckControls; dispid 214;
    procedure SampleControlDevices; dispid 215;
    procedure DoControlActions; dispid 216;
    procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Integer); dispid 217;
    property SystemYChanged: WordBool readonly dispid 218;
    property Converged: WordBool dispid 219;
  end;

// *********************************************************************//
// Interface: IMonitors
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5C339E44-C583-445C-91D1-3B1E49CAD6B0}
// *********************************************************************//
  IMonitors = interface(IDispatch)
    ['{5C339E44-C583-445C-91D1-3B1E49CAD6B0}']
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    procedure Sample; safecall;
    procedure Save; safecall;
    procedure Show; safecall;
    function Get_FileName: WideString; safecall;
    function Get_Mode: Integer; safecall;
    procedure Set_Mode(Value: Integer); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_ByteStream: OleVariant; safecall;
    function Get_SampleCount: Integer; safecall;
    procedure SampleAll; safecall;
    procedure SaveAll; safecall;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property FileName: WideString read Get_FileName;
    property Mode: Integer read Get_Mode write Set_Mode;
    property Name: WideString read Get_Name write Set_Name;
    property ByteStream: OleVariant read Get_ByteStream;
    property SampleCount: Integer read Get_SampleCount;
  end;

// *********************************************************************//
// DispIntf:  IMonitorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5C339E44-C583-445C-91D1-3B1E49CAD6B0}
// *********************************************************************//
  IMonitorsDisp = dispinterface
    ['{5C339E44-C583-445C-91D1-3B1E49CAD6B0}']
    property AllNames: OleVariant readonly dispid 2;
    property First: Integer readonly dispid 3;
    property Next: Integer readonly dispid 4;
    procedure Reset; dispid 5;
    procedure ResetAll; dispid 6;
    procedure Sample; dispid 7;
    procedure Save; dispid 8;
    procedure Show; dispid 9;
    property FileName: WideString readonly dispid 10;
    property Mode: Integer dispid 11;
    property Name: WideString dispid 1;
    property ByteStream: OleVariant readonly dispid 12;
    property SampleCount: Integer readonly dispid 13;
    procedure SampleAll; dispid 201;
    procedure SaveAll; dispid 202;
  end;

// *********************************************************************//
// Interface: IMeters
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {86705B6C-352A-47F8-A24B-78B750EC3859}
// *********************************************************************//
  IMeters = interface(IDispatch)
    ['{86705B6C-352A-47F8-A24B-78B750EC3859}']
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    procedure Sample; safecall;
    procedure Save; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Totals: OleVariant; safecall;
    function Get_Peakcurrent: OleVariant; safecall;
    procedure Set_Peakcurrent(Value: OleVariant); safecall;
    function Get_CalcCurrent: OleVariant; safecall;
    procedure Set_CalcCurrent(Value: OleVariant); safecall;
    function Get_AllocFactors: OleVariant; safecall;
    procedure Set_AllocFactors(Value: OleVariant); safecall;
    function Get_MeteredElement: WideString; safecall;
    procedure Set_MeteredElement(const Value: WideString); safecall;
    function Get_MeteredTerminal: Integer; safecall;
    procedure Set_MeteredTerminal(Value: Integer); safecall;
    function Get_DIFilesAreOpen: WordBool; safecall;
    procedure SampleAll; safecall;
    procedure SaveAll; safecall;
    procedure OpenAllDIFiles; safecall;
    procedure CloseAllDIFiles; safecall;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property RegisterNames: OleVariant read Get_RegisterNames;
    property RegisterValues: OleVariant read Get_RegisterValues;
    property Name: WideString read Get_Name write Set_Name;
    property Totals: OleVariant read Get_Totals;
    property Peakcurrent: OleVariant read Get_Peakcurrent write Set_Peakcurrent;
    property CalcCurrent: OleVariant read Get_CalcCurrent write Set_CalcCurrent;
    property AllocFactors: OleVariant read Get_AllocFactors write Set_AllocFactors;
    property MeteredElement: WideString read Get_MeteredElement write Set_MeteredElement;
    property MeteredTerminal: Integer read Get_MeteredTerminal write Set_MeteredTerminal;
    property DIFilesAreOpen: WordBool read Get_DIFilesAreOpen;
  end;

// *********************************************************************//
// DispIntf:  IMetersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {86705B6C-352A-47F8-A24B-78B750EC3859}
// *********************************************************************//
  IMetersDisp = dispinterface
    ['{86705B6C-352A-47F8-A24B-78B750EC3859}']
    property AllNames: OleVariant readonly dispid 2;
    property First: Integer readonly dispid 3;
    property Next: Integer readonly dispid 4;
    property RegisterNames: OleVariant readonly dispid 5;
    property RegisterValues: OleVariant readonly dispid 6;
    procedure Reset; dispid 7;
    procedure ResetAll; dispid 8;
    procedure Sample; dispid 9;
    procedure Save; dispid 10;
    property Name: WideString dispid 12;
    property Totals: OleVariant readonly dispid 1;
    property Peakcurrent: OleVariant dispid 201;
    property CalcCurrent: OleVariant dispid 202;
    property AllocFactors: OleVariant dispid 203;
    property MeteredElement: WideString dispid 204;
    property MeteredTerminal: Integer dispid 205;
    property DIFilesAreOpen: WordBool readonly dispid 206;
    procedure SampleAll; dispid 207;
    procedure SaveAll; dispid 208;
    procedure OpenAllDIFiles; dispid 209;
    procedure CloseAllDIFiles; dispid 210;
  end;

// *********************************************************************//
// Interface: IGenerators
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2D9B7548-D03E-478A-9FEA-9FC4033C793E}
// *********************************************************************//
  IGenerators = interface(IDispatch)
    ['{2D9B7548-D03E-478A-9FEA-9FC4033C793E}']
    function Get_AllNames: OleVariant; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_ForcedON: WordBool; safecall;
    procedure Set_ForcedON(Value: WordBool); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_kV: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    function Get_kW: Double; safecall;
    procedure Set_kW(Value: Double); safecall;
    function Get_kvar: Double; safecall;
    procedure Set_kvar(Value: Double); safecall;
    function Get_PF: Double; safecall;
    procedure Set_PF(Value: Double); safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_Phases(Value: Integer); safecall;
    property AllNames: OleVariant read Get_AllNames;
    property RegisterNames: OleVariant read Get_RegisterNames;
    property RegisterValues: OleVariant read Get_RegisterValues;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property ForcedON: WordBool read Get_ForcedON write Set_ForcedON;
    property Name: WideString read Get_Name write Set_Name;
    property kV: Double read Get_kV write Set_kV;
    property kW: Double read Get_kW write Set_kW;
    property kvar: Double read Get_kvar write Set_kvar;
    property PF: Double read Get_PF write Set_PF;
    property Phases: Integer read Get_Phases write Set_Phases;
  end;

// *********************************************************************//
// DispIntf:  IGeneratorsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2D9B7548-D03E-478A-9FEA-9FC4033C793E}
// *********************************************************************//
  IGeneratorsDisp = dispinterface
    ['{2D9B7548-D03E-478A-9FEA-9FC4033C793E}']
    property AllNames: OleVariant readonly dispid 2;
    property RegisterNames: OleVariant readonly dispid 3;
    property RegisterValues: OleVariant readonly dispid 4;
    property First: Integer readonly dispid 5;
    property Next: Integer readonly dispid 6;
    property ForcedON: WordBool dispid 8;
    property Name: WideString dispid 9;
    property kV: Double dispid 201;
    property kW: Double dispid 202;
    property kvar: Double dispid 203;
    property PF: Double dispid 204;
    property Phases: Integer dispid 205;
  end;

// *********************************************************************//
// Interface: IDSSProgress
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {315C0C38-929C-4942-BDF8-6DA12D001B47}
// *********************************************************************//
  IDSSProgress = interface(IDispatch)
    ['{315C0C38-929C-4942-BDF8-6DA12D001B47}']
    procedure Set_PctProgress(Param1: Integer); safecall;
    procedure Set_Caption(const Param1: WideString); safecall;
    procedure Show; safecall;
    procedure Close; safecall;
    property PctProgress: Integer write Set_PctProgress;
    property Caption: WideString write Set_Caption;
  end;

// *********************************************************************//
// DispIntf:  IDSSProgressDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {315C0C38-929C-4942-BDF8-6DA12D001B47}
// *********************************************************************//
  IDSSProgressDisp = dispinterface
    ['{315C0C38-929C-4942-BDF8-6DA12D001B47}']
    property PctProgress: Integer writeonly dispid 1;
    property Caption: WideString writeonly dispid 2;
    procedure Show; dispid 3;
    procedure Close; dispid 4;
  end;

// *********************************************************************//
// Interface: ISettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E3928A0-8B75-4127-885F-F4AD6B3F4323}
// *********************************************************************//
  ISettings = interface(IDispatch)
    ['{4E3928A0-8B75-4127-885F-F4AD6B3F4323}']
    function Get_AllowDuplicates: WordBool; safecall;
    procedure Set_AllowDuplicates(Value: WordBool); safecall;
    function Get_ZoneLock: WordBool; safecall;
    procedure Set_ZoneLock(Value: WordBool); safecall;
    procedure Set_AllocationFactors(Param1: Double); safecall;
    function Get_AutoBusList: WideString; safecall;
    procedure Set_AutoBusList(const Value: WideString); safecall;
    function Get_CktModel: Integer; safecall;
    procedure Set_CktModel(Value: Integer); safecall;
    function Get_NormVminpu: Double; safecall;
    procedure Set_NormVminpu(Value: Double); safecall;
    function Get_NormVmaxpu: Double; safecall;
    procedure Set_NormVmaxpu(Value: Double); safecall;
    function Get_EmergVminpu: Double; safecall;
    procedure Set_EmergVminpu(Value: Double); safecall;
    function Get_EmergVmaxpu: Double; safecall;
    procedure Set_EmergVmaxpu(Value: Double); safecall;
    function Get_UEweight: Double; safecall;
    procedure Set_UEweight(Value: Double); safecall;
    function Get_LossWeight: Double; safecall;
    procedure Set_LossWeight(Value: Double); safecall;
    function Get_UEregs: OleVariant; safecall;
    procedure Set_UEregs(Value: OleVariant); safecall;
    function Get_LossRegs: OleVariant; safecall;
    procedure Set_LossRegs(Value: OleVariant); safecall;
    function Get_Trapezoidal: WordBool; safecall;
    procedure Set_Trapezoidal(Value: WordBool); safecall;
    function Get_VoltageBases: OleVariant; safecall;
    procedure Set_VoltageBases(Value: OleVariant); safecall;
    function Get_ControlTrace: WordBool; safecall;
    procedure Set_ControlTrace(Value: WordBool); safecall;
    function Get_PriceSignal: Double; safecall;
    procedure Set_PriceSignal(Value: Double); safecall;
    function Get_PriceCurve: WideString; safecall;
    procedure Set_PriceCurve(const Value: WideString); safecall;
    property AllowDuplicates: WordBool read Get_AllowDuplicates write Set_AllowDuplicates;
    property ZoneLock: WordBool read Get_ZoneLock write Set_ZoneLock;
    property AllocationFactors: Double write Set_AllocationFactors;
    property AutoBusList: WideString read Get_AutoBusList write Set_AutoBusList;
    property CktModel: Integer read Get_CktModel write Set_CktModel;
    property NormVminpu: Double read Get_NormVminpu write Set_NormVminpu;
    property NormVmaxpu: Double read Get_NormVmaxpu write Set_NormVmaxpu;
    property EmergVminpu: Double read Get_EmergVminpu write Set_EmergVminpu;
    property EmergVmaxpu: Double read Get_EmergVmaxpu write Set_EmergVmaxpu;
    property UEweight: Double read Get_UEweight write Set_UEweight;
    property LossWeight: Double read Get_LossWeight write Set_LossWeight;
    property UEregs: OleVariant read Get_UEregs write Set_UEregs;
    property LossRegs: OleVariant read Get_LossRegs write Set_LossRegs;
    property Trapezoidal: WordBool read Get_Trapezoidal write Set_Trapezoidal;
    property VoltageBases: OleVariant read Get_VoltageBases write Set_VoltageBases;
    property ControlTrace: WordBool read Get_ControlTrace write Set_ControlTrace;
    property PriceSignal: Double read Get_PriceSignal write Set_PriceSignal;
    property PriceCurve: WideString read Get_PriceCurve write Set_PriceCurve;
  end;

// *********************************************************************//
// DispIntf:  ISettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E3928A0-8B75-4127-885F-F4AD6B3F4323}
// *********************************************************************//
  ISettingsDisp = dispinterface
    ['{4E3928A0-8B75-4127-885F-F4AD6B3F4323}']
    property AllowDuplicates: WordBool dispid 1;
    property ZoneLock: WordBool dispid 2;
    property AllocationFactors: Double writeonly dispid 3;
    property AutoBusList: WideString dispid 4;
    property CktModel: Integer dispid 5;
    property NormVminpu: Double dispid 6;
    property NormVmaxpu: Double dispid 7;
    property EmergVminpu: Double dispid 8;
    property EmergVmaxpu: Double dispid 9;
    property UEweight: Double dispid 10;
    property LossWeight: Double dispid 11;
    property UEregs: OleVariant dispid 12;
    property LossRegs: OleVariant dispid 13;
    property Trapezoidal: WordBool dispid 14;
    property VoltageBases: OleVariant dispid 15;
    property ControlTrace: WordBool dispid 16;
    property PriceSignal: Double dispid 17;
    property PriceCurve: WideString dispid 18;
  end;

// *********************************************************************//
// Interface: ILines
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}
// *********************************************************************//
  ILines = interface(IDispatch)
    ['{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function New(const Name: WideString): Integer; safecall;
    function Get_Bus1: WideString; safecall;
    procedure Set_Bus1(const Value: WideString); safecall;
    function Get_Bus2: WideString; safecall;
    procedure Set_Bus2(const Value: WideString); safecall;
    function Get_LineCode: WideString; safecall;
    procedure Set_LineCode(const Value: WideString); safecall;
    function Get_Length: Double; safecall;
    procedure Set_Length(Value: Double); safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_Phases(Value: Integer); safecall;
    function Get_R1: Double; safecall;
    procedure Set_R1(Value: Double); safecall;
    function Get_X1: Double; safecall;
    procedure Set_X1(Value: Double); safecall;
    function Get_R0: Double; safecall;
    procedure Set_R0(Value: Double); safecall;
    function Get_X0: Double; safecall;
    procedure Set_X0(Value: Double); safecall;
    function Get_C1: Double; safecall;
    procedure Set_C1(Value: Double); safecall;
    function Get_C0: Double; safecall;
    procedure Set_C0(Value: Double); safecall;
    function Get_Rmatrix: OleVariant; safecall;
    procedure Set_Rmatrix(Value: OleVariant); safecall;
    function Get_Xmatrix: OleVariant; safecall;
    procedure Set_Xmatrix(Value: OleVariant); safecall;
    function Get_Cmatrix: OleVariant; safecall;
    procedure Set_Cmatrix(Value: OleVariant); safecall;
    function Get_NormAmps: Double; safecall;
    procedure Set_NormAmps(Value: Double); safecall;
    function Get_EmergAmps: Double; safecall;
    procedure Set_EmergAmps(Value: Double); safecall;
    function Get_Geometry: WideString; safecall;
    procedure Set_Geometry(const Value: WideString); safecall;
    function Get_Rg: Double; safecall;
    procedure Set_Rg(Value: Double); safecall;
    function Get_Xg: Double; safecall;
    procedure Set_Xg(Value: Double); safecall;
    function Get_Rho: Double; safecall;
    procedure Set_Rho(Value: Double); safecall;
    function Get_Yprim: OleVariant; safecall;
    procedure Set_Yprim(Value: OleVariant); safecall;
    function Get_NumCust: Integer; safecall;
    function Get_TotalCust: Integer; safecall;
    function Get_Parent: Integer; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Bus1: WideString read Get_Bus1 write Set_Bus1;
    property Bus2: WideString read Get_Bus2 write Set_Bus2;
    property LineCode: WideString read Get_LineCode write Set_LineCode;
    property Length: Double read Get_Length write Set_Length;
    property Phases: Integer read Get_Phases write Set_Phases;
    property R1: Double read Get_R1 write Set_R1;
    property X1: Double read Get_X1 write Set_X1;
    property R0: Double read Get_R0 write Set_R0;
    property X0: Double read Get_X0 write Set_X0;
    property C1: Double read Get_C1 write Set_C1;
    property C0: Double read Get_C0 write Set_C0;
    property Rmatrix: OleVariant read Get_Rmatrix write Set_Rmatrix;
    property Xmatrix: OleVariant read Get_Xmatrix write Set_Xmatrix;
    property Cmatrix: OleVariant read Get_Cmatrix write Set_Cmatrix;
    property NormAmps: Double read Get_NormAmps write Set_NormAmps;
    property EmergAmps: Double read Get_EmergAmps write Set_EmergAmps;
    property Geometry: WideString read Get_Geometry write Set_Geometry;
    property Rg: Double read Get_Rg write Set_Rg;
    property Xg: Double read Get_Xg write Set_Xg;
    property Rho: Double read Get_Rho write Set_Rho;
    property Yprim: OleVariant read Get_Yprim write Set_Yprim;
    property NumCust: Integer read Get_NumCust;
    property TotalCust: Integer read Get_TotalCust;
    property Parent: Integer read Get_Parent;
  end;

// *********************************************************************//
// DispIntf:  ILinesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}
// *********************************************************************//
  ILinesDisp = dispinterface
    ['{E1616BDB-589B-4E5D-A7CE-828ACD73E5D4}']
    property Name: WideString dispid 6;
    property AllNames: OleVariant readonly dispid 7;
    property First: Integer readonly dispid 8;
    property Next: Integer readonly dispid 9;
    function New(const Name: WideString): Integer; dispid 10;
    property Bus1: WideString dispid 11;
    property Bus2: WideString dispid 12;
    property LineCode: WideString dispid 13;
    property Length: Double dispid 14;
    property Phases: Integer dispid 15;
    property R1: Double dispid 16;
    property X1: Double dispid 17;
    property R0: Double dispid 18;
    property X0: Double dispid 19;
    property C1: Double dispid 20;
    property C0: Double dispid 21;
    property Rmatrix: OleVariant dispid 22;
    property Xmatrix: OleVariant dispid 23;
    property Cmatrix: OleVariant dispid 24;
    property NormAmps: Double dispid 25;
    property EmergAmps: Double dispid 26;
    property Geometry: WideString dispid 1;
    property Rg: Double dispid 2;
    property Xg: Double dispid 3;
    property Rho: Double dispid 4;
    property Yprim: OleVariant dispid 5;
    property NumCust: Integer readonly dispid 201;
    property TotalCust: Integer readonly dispid 202;
    property Parent: Integer readonly dispid 203;
  end;

// *********************************************************************//
// Interface: ICtrlQueue
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {55055001-5EEC-4667-9CCA-63F3A60F31F3}
// *********************************************************************//
  ICtrlQueue = interface(IDispatch)
    ['{55055001-5EEC-4667-9CCA-63F3A60F31F3}']
    procedure ClearQueue; safecall;
    procedure Delete(ActionHandle: Integer); safecall;
    function Get_NumActions: Integer; safecall;
    procedure Set_Action(Param1: Integer); safecall;
    function Get_ActionCode: Integer; safecall;
    function Get_DeviceHandle: Integer; safecall;
    function Push(Hour: Integer; Seconds: Double; ActionCode: Integer; DeviceHandle: Integer): Integer; safecall;
    procedure Show; safecall;
    procedure ClearActions; safecall;
    function Get_PopAction: Integer; safecall;
    property NumActions: Integer read Get_NumActions;
    property Action: Integer write Set_Action;
    property ActionCode: Integer read Get_ActionCode;
    property DeviceHandle: Integer read Get_DeviceHandle;
    property PopAction: Integer read Get_PopAction;
  end;

// *********************************************************************//
// DispIntf:  ICtrlQueueDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {55055001-5EEC-4667-9CCA-63F3A60F31F3}
// *********************************************************************//
  ICtrlQueueDisp = dispinterface
    ['{55055001-5EEC-4667-9CCA-63F3A60F31F3}']
    procedure ClearQueue; dispid 101;
    procedure Delete(ActionHandle: Integer); dispid 103;
    property NumActions: Integer readonly dispid 104;
    property Action: Integer writeonly dispid 102;
    property ActionCode: Integer readonly dispid 105;
    property DeviceHandle: Integer readonly dispid 106;
    function Push(Hour: Integer; Seconds: Double; ActionCode: Integer; DeviceHandle: Integer): Integer; dispid 107;
    procedure Show; dispid 108;
    procedure ClearActions; dispid 109;
    property PopAction: Integer readonly dispid 110;
  end;

// *********************************************************************//
// Interface: ILoads
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9A3FFA05-5B82-488C-B08D-FCA2FDB23101}
// *********************************************************************//
  ILoads = interface(IDispatch)
    ['{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}']
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Idx: Integer; safecall;
    procedure Set_Idx(Value: Integer); safecall;
    function Get_kW: Double; safecall;
    procedure Set_kW(Value: Double); safecall;
    function Get_kV: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    function Get_kvar: Double; safecall;
    procedure Set_kvar(Value: Double); safecall;
    function Get_PF: Double; safecall;
    procedure Set_PF(Value: Double); safecall;
    property AllNames: OleVariant read Get_AllNames;
    property First: Integer read Get_First;
    property Next: Integer read Get_Next;
    property Name: WideString read Get_Name write Set_Name;
    property Idx: Integer read Get_Idx write Set_Idx;
    property kW: Double read Get_kW write Set_kW;
    property kV: Double read Get_kV write Set_kV;
    property kvar: Double read Get_kvar write Set_kvar;
    property PF: Double read Get_PF write Set_PF;
  end;

// *********************************************************************//
// DispIntf:  ILoadsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9A3FFA05-5B82-488C-B08D-FCA2FDB23101}
// *********************************************************************//
  ILoadsDisp = dispinterface
    ['{9A3FFA05-5B82-488C-B08D-FCA2FDB23101}']
    property AllNames: OleVariant readonly dispid 201;
    property First: Integer readonly dispid 202;
    property Next: Integer readonly dispid 203;
    property Name: WideString dispid 204;
    property Idx: Integer dispid 205;
    property kW: Double dispid 206;
    property kV: Double dispid 207;
    property kvar: Double dispid 208;
    property PF: Double dispid 209;
  end;

// *********************************************************************//
// The Class CoText provides a Create and CreateRemote method to          
// create instances of the default interface IText exposed by              
// the CoClass Text. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoText = class
    class function Create: IText;
    class function CreateRemote(const MachineName: string): IText;
  end;

// *********************************************************************//
// The Class CoDSSProperty provides a Create and CreateRemote method to          
// create instances of the default interface IDSSProperty exposed by              
// the CoClass DSSProperty. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDSSProperty = class
    class function Create: IDSSProperty;
    class function CreateRemote(const MachineName: string): IDSSProperty;
  end;

// *********************************************************************//
// The Class CoCktElement provides a Create and CreateRemote method to          
// create instances of the default interface ICktElement exposed by              
// the CoClass CktElement. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCktElement = class
    class function Create: ICktElement;
    class function CreateRemote(const MachineName: string): ICktElement;
  end;

// *********************************************************************//
// The Class CoError provides a Create and CreateRemote method to          
// create instances of the default interface IError exposed by              
// the CoClass Error. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoError = class
    class function Create: IError;
    class function CreateRemote(const MachineName: string): IError;
  end;

// *********************************************************************//
// The Class CoCircuit provides a Create and CreateRemote method to          
// create instances of the default interface ICircuit exposed by              
// the CoClass Circuit. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCircuit = class
    class function Create: ICircuit;
    class function CreateRemote(const MachineName: string): ICircuit;
  end;

// *********************************************************************//
// The Class CoBus provides a Create and CreateRemote method to          
// create instances of the default interface IBus exposed by              
// the CoClass Bus. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoBus = class
    class function Create: IBus;
    class function CreateRemote(const MachineName: string): IBus;
  end;

// *********************************************************************//
// The Class CoDSS provides a Create and CreateRemote method to          
// create instances of the default interface IDSS exposed by              
// the CoClass DSS. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDSS = class
    class function Create: IDSS;
    class function CreateRemote(const MachineName: string): IDSS;
  end;

// *********************************************************************//
// The Class CoSolution provides a Create and CreateRemote method to          
// create instances of the default interface ISolution exposed by              
// the CoClass Solution. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSolution = class
    class function Create: ISolution;
    class function CreateRemote(const MachineName: string): ISolution;
  end;

// *********************************************************************//
// The Class CoMonitors provides a Create and CreateRemote method to          
// create instances of the default interface IMonitors exposed by              
// the CoClass Monitors. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMonitors = class
    class function Create: IMonitors;
    class function CreateRemote(const MachineName: string): IMonitors;
  end;

// *********************************************************************//
// The Class CoMeters provides a Create and CreateRemote method to          
// create instances of the default interface IMeters exposed by              
// the CoClass Meters. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMeters = class
    class function Create: IMeters;
    class function CreateRemote(const MachineName: string): IMeters;
  end;

// *********************************************************************//
// The Class CoGenerators provides a Create and CreateRemote method to          
// create instances of the default interface IGenerators exposed by              
// the CoClass Generators. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoGenerators = class
    class function Create: IGenerators;
    class function CreateRemote(const MachineName: string): IGenerators;
  end;

// *********************************************************************//
// The Class CoDSSProgress provides a Create and CreateRemote method to          
// create instances of the default interface IDSSProgress exposed by              
// the CoClass DSSProgress. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDSSProgress = class
    class function Create: IDSSProgress;
    class function CreateRemote(const MachineName: string): IDSSProgress;
  end;

// *********************************************************************//
// The Class CoSettings provides a Create and CreateRemote method to          
// create instances of the default interface ISettings exposed by              
// the CoClass Settings. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSettings = class
    class function Create: ISettings;
    class function CreateRemote(const MachineName: string): ISettings;
  end;

// *********************************************************************//
// The Class CoLines provides a Create and CreateRemote method to          
// create instances of the default interface ILines exposed by              
// the CoClass Lines. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLines = class
    class function Create: ILines;
    class function CreateRemote(const MachineName: string): ILines;
  end;

// *********************************************************************//
// The Class CoCtrlQueue provides a Create and CreateRemote method to          
// create instances of the default interface ICtrlQueue exposed by              
// the CoClass CtrlQueue. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCtrlQueue = class
    class function Create: ICtrlQueue;
    class function CreateRemote(const MachineName: string): ICtrlQueue;
  end;

// *********************************************************************//
// The Class CoLoads provides a Create and CreateRemote method to          
// create instances of the default interface ILoads exposed by              
// the CoClass Loads. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLoads = class
    class function Create: ILoads;
    class function CreateRemote(const MachineName: string): ILoads;
  end;

implementation

uses ComObj;

class function CoText.Create: IText;
begin
  Result := CreateComObject(CLASS_Text) as IText;
end;

class function CoText.CreateRemote(const MachineName: string): IText;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Text) as IText;
end;

class function CoDSSProperty.Create: IDSSProperty;
begin
  Result := CreateComObject(CLASS_DSSProperty) as IDSSProperty;
end;

class function CoDSSProperty.CreateRemote(const MachineName: string): IDSSProperty;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSSProperty) as IDSSProperty;
end;

class function CoCktElement.Create: ICktElement;
begin
  Result := CreateComObject(CLASS_CktElement) as ICktElement;
end;

class function CoCktElement.CreateRemote(const MachineName: string): ICktElement;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CktElement) as ICktElement;
end;

class function CoError.Create: IError;
begin
  Result := CreateComObject(CLASS_Error) as IError;
end;

class function CoError.CreateRemote(const MachineName: string): IError;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Error) as IError;
end;

class function CoCircuit.Create: ICircuit;
begin
  Result := CreateComObject(CLASS_Circuit) as ICircuit;
end;

class function CoCircuit.CreateRemote(const MachineName: string): ICircuit;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Circuit) as ICircuit;
end;

class function CoBus.Create: IBus;
begin
  Result := CreateComObject(CLASS_Bus) as IBus;
end;

class function CoBus.CreateRemote(const MachineName: string): IBus;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Bus) as IBus;
end;

class function CoDSS.Create: IDSS;
begin
  Result := CreateComObject(CLASS_DSS) as IDSS;
end;

class function CoDSS.CreateRemote(const MachineName: string): IDSS;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSS) as IDSS;
end;

class function CoSolution.Create: ISolution;
begin
  Result := CreateComObject(CLASS_Solution) as ISolution;
end;

class function CoSolution.CreateRemote(const MachineName: string): ISolution;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Solution) as ISolution;
end;

class function CoMonitors.Create: IMonitors;
begin
  Result := CreateComObject(CLASS_Monitors) as IMonitors;
end;

class function CoMonitors.CreateRemote(const MachineName: string): IMonitors;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Monitors) as IMonitors;
end;

class function CoMeters.Create: IMeters;
begin
  Result := CreateComObject(CLASS_Meters) as IMeters;
end;

class function CoMeters.CreateRemote(const MachineName: string): IMeters;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Meters) as IMeters;
end;

class function CoGenerators.Create: IGenerators;
begin
  Result := CreateComObject(CLASS_Generators) as IGenerators;
end;

class function CoGenerators.CreateRemote(const MachineName: string): IGenerators;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Generators) as IGenerators;
end;

class function CoDSSProgress.Create: IDSSProgress;
begin
  Result := CreateComObject(CLASS_DSSProgress) as IDSSProgress;
end;

class function CoDSSProgress.CreateRemote(const MachineName: string): IDSSProgress;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DSSProgress) as IDSSProgress;
end;

class function CoSettings.Create: ISettings;
begin
  Result := CreateComObject(CLASS_Settings) as ISettings;
end;

class function CoSettings.CreateRemote(const MachineName: string): ISettings;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Settings) as ISettings;
end;

class function CoLines.Create: ILines;
begin
  Result := CreateComObject(CLASS_Lines) as ILines;
end;

class function CoLines.CreateRemote(const MachineName: string): ILines;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Lines) as ILines;
end;

class function CoCtrlQueue.Create: ICtrlQueue;
begin
  Result := CreateComObject(CLASS_CtrlQueue) as ICtrlQueue;
end;

class function CoCtrlQueue.CreateRemote(const MachineName: string): ICtrlQueue;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CtrlQueue) as ICtrlQueue;
end;

class function CoLoads.Create: ILoads;
begin
  Result := CreateComObject(CLASS_Loads) as ILoads;
end;

class function CoLoads.CreateRemote(const MachineName: string): ILoads;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Loads) as ILoads;
end;

end.
