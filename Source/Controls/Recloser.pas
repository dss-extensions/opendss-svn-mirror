unit Recloser;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
    Created 11-1-00 from Relay Control


}
{
  A Recloser is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  CktElement to be controlled must already exist.

  7-18-2002  Fixed typos in help
  5-1-2006  Added Time Delays to be compatible with relays

}

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    ucomplex,
    utilities,
    TCC_Curve,
    Math,
    StrUtils;

const
    RECLOSERCONTROLMAXDIM = 6;

type

    pStateArray = ^StateArray;
    StateArray = array[1..RECLOSERCONTROLMAXDIM] of EControlAction;  // 0 = open 1 = close

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TRecloser = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const RecloserName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TRecloserObj = class(TControlElem)
    PRIVATE

        PhaseDelayed,
        GroundDelayed,
        PhaseFast,
        GroundFast: TTCC_CurveObj;

        ResetTime,
        DelayTime,
        TDGrDelayed,
        TDPhDelayed,
        TDGrFast,
        TDPhFast: Double;

        FPresentState,
        FNormalState: pStateArray;

        OperationCount: pIntegerArray;

        LockedOut,
        ArmedForClose, ArmedForOpen,
        PhaseTarget: pBooleanArray;
        GroundTarget: Boolean;
        IdxMultiPh: Integer; // Index used for accessing arrays for multi-phase, ganged operation

        RecloserTarget: pStringArray;

        NormalStateSet,
        SinglePhTrip,
        SinglePhLockout,
        FLocked: Boolean;

        CondOffset: Integer; // Offset for monitored terminal

        cBuffer: pComplexArray;    // Complexarray buffer
        DebugTrace: Boolean;

        procedure InterpretRecloserState(ActorID: Integer; const param: String; const property_name: String);
        function get_States(Idx: Integer): EControlAction;
        procedure set_States(Idx: Integer; const Value: EControlAction);
        function get_NormalStates(Idx: Integer): EControlAction;
        procedure set_NormalStates(Idx: Integer; const Value: EControlAction);

        procedure set_Flocked(const Value: Boolean);

    PUBLIC

        RecloseIntervals: pdoubleArray;
        NumFast,
        NumReclose: Integer;
        MonitoredElementName: String;
        MonitoredElementTerminal: Integer;
        PhaseTrip,
        GroundTrip,
        PhaseInst,
        GroundInst: Double;


        constructor Create(ParClass: TDSSClass; const RecloserName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a Recloser

        procedure Sample(ActorID: Integer); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset(ActorID: Integer); OVERRIDE;  // Reset to initial defined state


        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        property States[Idx: Integer]: EControlAction READ get_States WRITE set_States;
        property NormalStates[Idx: Integer]: EControlAction READ get_NormalStates WRITE set_NormalStates;

        property Locked: Boolean READ Flocked WRITE set_Flocked;

    end;


var
    ActiveRecloserObj: TRecloserObj;
    RecloserClass: TRecloser;


{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    uCmatrix,
    MathUtil;

const

    NumPropsThisClass = 30;

    CURRENT = 0;  {Default}
    VOLTAGE = 1;
    REVPOWER = 3;

var
    TCC_CurveClass: TDSSClass;

{General Module Function}

function GetTccCurve(const CurveName: String): TTCC_CurveObj;

begin

    if lowercase(CurveName) = 'none' then
        Exit;

    Result := TCC_CurveClass.Find(CurveName);
    if Result = nil then
        DoSimpleMsg('TCC Curve object: "' + CurveName + '" not found.', 388);

end;


{--------------------------------------------------------------------------}
constructor TRecloser.Create;  // Creates superstructure for all Recloser objects
begin
    inherited Create;

    Class_name := 'Recloser';
    DSSClassType := DSSClassType + RECLOSER_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(PropertyName, NumProperties);
    CommandList.Abbrev := true;

    TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
    RecloserClass := Self;
end;

{--------------------------------------------------------------------------}
destructor TRecloser.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TRecloser.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName^[1] := 'MonitoredObj';
    PropertyName^[2] := 'MonitoredTerm';
    PropertyName^[3] := 'SwitchedObj';
    PropertyName^[4] := 'SwitchedTerm';
    PropertyName^[5] := 'NumFast';
    PropertyName^[6] := 'PhaseFast';
    PropertyName^[7] := 'PhaseDelayed';
    PropertyName^[8] := 'GroundFast';
    PropertyName^[9] := 'GroundDelayed';
    PropertyName^[10] := 'PhaseTrip';
    PropertyName^[11] := 'GroundTrip';
    PropertyName^[12] := 'PhaseInst';
    PropertyName^[13] := 'GroundInst';
    PropertyName^[14] := 'ResetTime';
    PropertyName^[15] := 'Shots';
    PropertyName^[16] := 'RecloseIntervals';
    PropertyName^[17] := 'Delay';
    PropertyName^[18] := 'Action';
    PropertyName^[19] := 'TDPhFast';
    PropertyName^[20] := 'TDGrFast';
    PropertyName^[21] := 'TDPhDelayed';
    PropertyName^[22] := 'TDGrDelayed';
    PropertyName^[23] := 'Normal';
    PropertyName^[24] := 'State';
    PropertyName^[25] := 'SinglePhTrip';
    PropertyName^[26] := 'SinglePhLockout';
    PropertyName^[27] := 'Lock';
    PropertyName^[28] := 'Reset';
    PropertyName^[29] := 'EventLog';
    PropertyName^[30] := 'DebugTrace';


    PropertyHelp^[1] := 'Full object name of the circuit element, typically a line, transformer, load, or generator, ' +
        'to which the Recloser''s PT and/or CT are connected.' +
        ' This is the "monitored" element. ' +
        'There is no default; must be specified.';
    PropertyHelp^[2] := 'Number of the terminal of the circuit element to which the Recloser is connected. ' +
        '1 or 2, typically.  Default is 1.';
    PropertyHelp^[3] := 'Name of circuit element switch that the Recloser controls. ' +
        'Specify the full object name.' +
        'Defaults to the same as the Monitored element. ' +
        'This is the "controlled" element.';
    PropertyHelp^[4] := 'Number of the terminal of the controlled element in which the switch is controlled by the Recloser. ' +
        '1 or 2, typically.  Default is 1.';
    PropertyHelp^[5] := 'Number of Fast (fuse saving) operations.  Default is 1. (See "Shots")';
    PropertyHelp^[6] := 'Name of the TCC Curve object that determines the Phase Fast trip. Must have been previously defined as a TCC_Curve object or specified as "none" (ignored). ' +
        'Default is "none". ' +
        'Multiplying the current values in the curve by the "phasetrip" value gives the actual current.';
    PropertyHelp^[7] := 'Name of the TCC Curve object that determines the Phase Delayed trip. Must have been previously defined as a TCC_Curve object or specified as "none" (ignored). ' +
        'Default is "none". ' +
        'Multiplying the current values in the curve by the "phasetrip" value gives the actual current.';
    PropertyHelp^[8] := 'Name of the TCC Curve object that determines the Ground Fast trip.  Must have been previously defined as a TCC_Curve object or specified as "none" (ignored). ' +
        'Default is "none". ' +
        'Multiplying the current values in the curve by the "groundtrip" value gives the actual current.';
    PropertyHelp^[9] := 'Name of the TCC Curve object that determines the Ground Delayed trip.  Must have been previously defined as a TCC_Curve object or specified as "none" (ignored). ' +
        'Default is "none". ' +
        'Multiplying the current values in the curve by the "groundtrip" value gives the actual current.';
    PropertyHelp^[10] := 'Multiplier or actual phase amps for the phase TCC curve. Defaults to 1.0.';
    PropertyHelp^[11] := 'Multiplier or actual ground amps (3I0) for the ground TCC curve. Defaults to 1.0.';
    PropertyHelp^[12] := 'Actual amps for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip.';
    PropertyHelp^[13] := 'Actual amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip.';
    PropertyHelp^[14] := 'Reset time in sec for Recloser. Default is 15.';
    PropertyHelp^[15] := 'Total Number of fast and delayed shots to lockout.  Default is 4. This is one more than the number of reclose intervals.';
    PropertyHelp^[16] := 'Array of reclose intervals.  Default for Recloser is (0.5, 2.0, 2.0) seconds. ' +
        'A locked out Recloser must be closed manually (action=close).';
    PropertyHelp^[17] := 'Fixed delay time (sec) added to Recloser trip time. Default is 0.0. Used to represent breaker time or any other delay.';
    PropertyHelp^[18] := 'DEPRECATED. See "State" property';
    PropertyHelp^[19] := 'Time dial for Phase Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.';
    PropertyHelp^[20] := 'Time dial for Ground Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.';
    PropertyHelp^[21] := 'Time dial for Phase Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.';
    PropertyHelp^[22] := 'Time dial for Ground Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.';
    PropertyHelp^[23] := 'ARRAY of strings {Open | Closed} representing the Normal state of the recloser in each phase of the controlled element. ' +
        'The recloser reverts to this state for reset, change of mode, etc. ' +
        'Defaults to "State" if not specifically declared.  Setting this property to {Open | Closed} sets the normal state to the specified value for all phases (ganged operation).';
    PropertyHelp^[24] := 'ARRAY of strings {Open | Closed} representing the Actual state of the recloser in each phase of the controlled element. ' +
        'Upon setting, immediately forces the state of the recloser. Simulates manual control on Recloser. Defaults to Closed for all phases. Setting this property to {Open | Closed} ' +
        'sets the actual state to the specified value for all phases (ganged operation). "Open" causes the controlled element or respective phase to open and lock out. "Closed" causes the ' +
        'controlled element or respective phase to close and the recloser to reset to its first operation.';
    PropertyHelp^[25] := '{Yes | No*} Enables single-phase tripping and reclosing for multi-phase controlled elements. Previously locked out phases do not operate/reclose even considering multi-phase tripping.';
    PropertyHelp^[26] := '{Yes | No*} Enables single-phase lockout for multi-phase controlled elements with single-phase tripping. Does not have impact if single-phase trip is not enabled.';
    PropertyHelp^[27] := '{Yes | No*} Controlled switch is locked in its present open / closed state or unlocked. ' +
        'When locked, the recloser will not respond to either a manual state change issued by the user or a state change issued internally by OpenDSS when reseting the control. ' +
        'Note this locking mechanism is different from the recloser automatic lockout after specifed number of shots.';
    PropertyHelp^[28] := '{Yes | No} If Yes, forces Reset of recloser to Normal state and removes Lock independently of any internal ' +
        'reset command for mode change, etc.';
    PropertyHelp^[29] := '{Yes/True* | No/False} Default is Yes for Recloser. Write trips, reclose and reset events to EventLog.';
    PropertyHelp^[30] := '{Yes/True* | No/False} Default is No for Recloser. Write extra details to Eventlog.';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TRecloser.NewObject(const ObjName: String): Integer;
begin
    // Make a new Recloser and add it to Recloser class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TRecloserObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;
{--------------------------------------------------------------------------}


{--------------------------------------------------------------------------}
function TRecloser.Edit(ActorID: Integer): Integer;
var
    ParamPointer, i: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing WITH contents of Parser
    ActiveRecloserObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveRecloserObj;

    Result := 0;

    with ActiveRecloserObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 390);
                1:
                    MonitoredElementName := lowercase(param);
                2:
                    MonitoredElementTerminal := Parser[ActorID].IntValue;
                3:
                    ElementName := lowercase(param);
                4:
                    ElementTerminal := Parser[ActorID].IntValue;
                5:
                    NumFast := Parser[ActorID].Intvalue;
                6:
                    PhaseFast := GetTccCurve(Param);
                7:
                    PhaseDelayed := GetTCCCurve(Param);
                8:
                    GroundFast := GetTccCurve(Param);
                9:
                    GroundDelayed := GetTCCCurve(Param);
                10:
                    PhaseTrip := Parser[ActorID].Dblvalue;
                11:
                    GroundTrip := Parser[ActorID].Dblvalue;
                12:
                    PhaseInst := Parser[ActorID].Dblvalue;
                13:
                    GroundInst := Parser[ActorID].Dblvalue;
                14:
                    ResetTime := Parser[ActorID].Dblvalue;
                15:
                    NumReclose := Parser[ActorID].Intvalue - 1;   // one less than number of shots
                16:
                    NumReclose := Parser[ActorID].ParseAsVector(4, RecloseIntervals);   // max of 4 allowed
                17:
                    DelayTime := Parser[ActorID].DblValue;
                19:
                    TDPhFast := Parser[ActorID].DblValue;
                20:
                    TDGrFast := Parser[ActorID].DblValue;
                21:
                    TDPhDelayed := Parser[ActorID].DblValue;
                22:
                    TDGrDelayed := Parser[ActorID].DblValue;
                23:
                begin
                    InterpretRecloserState(ActorID, Param, ParamName);   // set normal state
                    if not NormalStateSet then
                        NormalStateSet := true;
                end;
                18, 24:
                    InterpretRecloserState(ActorID, Param, ParamName);    // set state
                25:
                    SinglePhTrip := InterpretYesNo(Param);
                26:
                    SinglePhLockout := InterpretYesNo(Param);
                27:
                    Locked := InterpretYesNo(Param);
                28:
                    if InterpretYesNo(Param) then
                    begin  // force a reset
                        Locked := false;
                        Reset(ActorID);
                        PropertyValue[28] := 'n';
                    end;
                29:
                    ShowEventLog := InterpretYesNo(param);
                30:
                    DebugTrace := InterpretYesNo(Param);

            else
           // Inherited parameters
                ClassEdit(ActiveRecloserObj, ParamPointer - NumPropsthisClass)
            end;

            case ParamPointer of
              {Default the controlled element to the monitored element}
                1:
                    ElementName := MonitoredElementName;
                2:
                    ElementTerminal := MonitoredElementTerminal;
                18, 24:
                begin
                    for i := 1 to FNPhases do
                        if not NormalStateSet then
                            FNormalState^[i] := FPresentState^[i];
                    NormalStateSet := true;   // normal state will default to state only the 1st time state is specified.
                end;

            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
    end;

end;

{--------------------------------------------------------------------------}
function TRecloser.MakeLike(const RecloserName: String): Integer;
var
    OtherRecloser: TRecloserObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Recloser name in the present collection}
    OtherRecloser := Find(RecloserName);
    if OtherRecloser <> nil then
        with ActiveRecloserObj do
        begin

            NPhases := OtherRecloser.Fnphases;
            NConds := OtherRecloser.Fnconds; // Force Reallocation of terminal stuff
            ShowEventLog := OtherRecloser.ShowEventLog; // but leave DebugTrace off

            ElementName := OtherRecloser.ElementName;
            ElementTerminal := OtherRecloser.ElementTerminal;
            ControlledElement := OtherRecloser.ControlledElement;  // Pointer to target circuit element

            MonitoredElement := OtherRecloser.MonitoredElement;  // Pointer to target circuit element
            MonitoredElementName := OtherRecloser.MonitoredElementName;  // Pointer to target circuit element
            MonitoredElementTerminal := OtherRecloser.MonitoredElementTerminal;  // Pointer to target circuit element

            PhaseDelayed := OtherRecloser.PhaseDelayed;
            GroundDelayed := OtherRecloser.GroundDelayed;
            PhaseFast := OtherRecloser.PhaseFast;
            GroundFast := OtherRecloser.GroundFast;
            PhaseTrip := OtherRecloser.PhaseTrip;
            GroundTrip := OtherRecloser.GroundTrip;
            PhaseInst := OtherRecloser.PhaseInst;
            GroundInst := OtherRecloser.GroundInst;
            ResetTime := OtherRecloser.ResetTime;
            NumReclose := OtherRecloser.NumReclose;
            NumFast := OtherRecloser.NumFast;
            SinglePhTrip := OtherRecloser.SinglePhTrip;
            SinglePhLockout := OtherRecloser.SinglePhLockout;

            Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4);      // Always make a max of 4
            for i := 1 to NumReclose do
                RecloseIntervals^[i] := OtherRecloser.RecloseIntervals^[i];

            Locked := OtherRecloser.Locked;

            for i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) do
            begin
                FPresentState^[i] := OtherRecloser.FPresentState^[i];
                FNormalState^[i] := OtherRecloser.FNormalState^[i];
            end;

            CondOffset := OtherRecloser.CondOffset;


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherRecloser.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in Recloser MakeLike: "' + RecloserName + '" Not Found.', 391);

end;


{==========================================================================}
{                    TRecloserObj                                           }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TRecloserObj.Create(ParClass: TDSSClass; const RecloserName: String);
var
    i: Integer;
begin
    inherited Create(ParClass);
    Name := LowerCase(RecloserName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class


    ElementName := '';
    ControlledElement := nil;
    ElementTerminal := 1;

    MonitoredElementName := '';
    MonitoredElementTerminal := 1;
    MonitoredElement := nil;

    PhaseFast := nil;
    PhaseDelayed := nil;
    GroundFast := nil;
    GroundDelayed := nil;

    PhaseTrip := 1.0;
    GroundTrip := 1.0;
    PhaseInst := 0.0;
    GroundInst := 0.0;

    TDGrDelayed := 1.0;
    TDPhDelayed := 1.0;
    TDGrFast := 1.0;
    TDPhFast := 1.0;

    ResetTime := 15.0;
    NumReclose := 3;
    NumFast := 1;

    RecloseIntervals := nil;
    Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4); // fixed allocation of 4
    RecloseIntervals^[1] := 0.5;
    RecloseIntervals^[2] := 2.0;
    RecloseIntervals^[3] := 2.0;

    FPresentState := nil;
    FNormalState := nil;
    LockedOut := nil;
    ArmedForOpen := nil;
    ArmedForClose := nil;
    GroundTarget := false;
    PhaseTarget := nil;
    Operationcount := nil;
    RecloserTarget := nil;
    SinglePhTrip := false;
    SinglePhLockout := false;
    IdxMultiPh := FNPhases + 1;

     // Reallocate arrays  (Must be initialized to nil for first call)
    Reallocmem(FPresentState, Sizeof(FPresentState^[1]) * FNPhases);
    Reallocmem(FNormalState, Sizeof(FNormalState^[1]) * FNPhases);
    Reallocmem(LockedOut, Sizeof(LockedOut^[1]) * IdxMultiPh);
    Reallocmem(ArmedForOpen, Sizeof(ArmedForOpen^[1]) * IdxMultiPh);
    Reallocmem(ArmedForClose, Sizeof(ArmedForClose^[1]) * IdxMultiPh);
    Reallocmem(PhaseTarget, Sizeof(PhaseTarget^[1]) * IdxMultiPh);
    Reallocmem(Operationcount, Sizeof(Operationcount^[1]) * IdxMultiPh);
    RecloserTarget := AllocStringArray(IdxMultiPh);

    for i := 1 to Min(RECLOSERCONTROLMAXDIM, IdxMultiPh) do
    begin

        if i <= FNPhases then
        begin
            FPresentState^[i] := CTRL_CLOSE;
            FNormalState^[i] := CTRL_CLOSE;  // default to present state;
        end;

        LockedOut^[i] := false;
        ArmedForOpen^[i] := false;
        ArmedForClose^[i] := false;
        PhaseTarget^[i] := false;
        Operationcount^[i] := 1;
        RecloserTarget^[i] := '';
    end;

    NormalStateSet := false;
    Locked := false;

    cBuffer := nil; // Complex buffer

    DSSObjType := ParClass.DSSClassType;

    InitPropertyValues(0);

   //  RecalcElementData;

end;

destructor TRecloserObj.Destroy;
begin
    MonitoredElementName := '';
    ReallocMem(RecloseIntervals, 0);
    ReallocMem(cBuffer, 0);
    ReallocMem(FPresentState, 0);
    ReallocMem(FNormalState, 0);
    ReallocMem(LockedOut, 0);
    ReallocMem(ArmedForOpen, 0);
    ReallocMem(ArmedForClose, 0);
    ReallocMem(PhaseTarget, 0);
    ReallocMem(Operationcount, 0);
    FreeStringArray(RecloserTarget, IdxMultiPh);

    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.RecalcElementData(ActorID: Integer);

var
    DevIndex, i: Integer;

begin

    Devindex := GetCktElementIndex(MonitoredElementName); // Global function
    if DevIndex > 0 then
    begin

        MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
        if FNphases > RECLOSERCONTROLMAXDIM then
            DosimpleMsg('Warning: Recloser ' + Self.Name + ': Number of phases > Max Recloser dimension.', 392);
        if MonitoredElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('Recloser: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 392);
        end
        else
        begin
         // Sets name of i-th terminal's connected bus in Recloser's buslist
            Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
         // Allocate a buffer bigenough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
            CondOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
        end;
    end;

{Check for existence of Controlled Element}

   // If previously assigned, reset HasOCPDevice flag in case this is a move
    if Assigned(ControlledElement) then
    begin
        ControlledElement.HasOCPDevice := false;
        ControlledElement.HasAutoOCPDevice := false;
    end;

    Devindex := GetCktElementIndex(ElementName); // Global function
    if DevIndex > 0 then
    begin  // Both CktElement and monitored element must already exist

        ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

       // If the recloser becomes disabled, leave at False
        if Enabled then
        begin
            ControlledElement.HasOCPDevice := true;  // For Reliability calcs
            ControlledElement.HasAutoOCPDevice := true;  // For Reliability calcs
        end;

       // Open/Closed State of controlled element based on state assigned to the control
        for i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) do  // TODO --- evaluate if we need to do anything here....
            if FPresentState^[i] = CTRL_CLOSE then
            begin
                ControlledElement.Closed[i, ActorID] := true;
                LockedOut^[i] := false;
                OperationCount^[i] := 1;
                ArmedForOpen^[i] := false;
            end
            else
            begin
                ControlledElement.Closed[i, ActorID] := false;
                LockedOut^[i] := true;
                OperationCount^[i] := NumReclose + 1;
                ArmedForClose^[i] := false;
            end;


    end
    else
    begin
        ControlledElement := nil;   // element not found
        DoErrorMsg('Recloser: "' + Self.Name + '"', 'CktElement Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 393);
    end;
end;

procedure TRecloserObj.MakePosSequence(ActorID: Integer);
begin
    if MonitoredElement <> nil then
    begin
        Nphases := MonitoredElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
        ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
        CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
    end;
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;
{--------------------------------------------------------------------------}

procedure TRecloserObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer);
var
    i, PhIdx: Integer;
begin

    if SinglePhTrip then
        PhIdx := ProxyHdl
    else
        PhIdx := IdxMultiPh; // Proxy holds phase information for single-phase trip

    with ControlledElement do
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1

        case Code of
            Integer(CTRL_OPEN):
                if SinglePhTrip then
                begin
                    case FPresentState^[PhIdx] of
                        CTRL_CLOSE:
                            if ArmedForOpen^[PhIdx] then
                            begin   // ignore if we became disarmed in meantime

                                ControlledElement.Closed[PhIdx, ActorID] := false;   // Open phase of active terminal
                                FPresentState^[PhIdx] := CTRL_OPEN;

                                if OperationCount^[PhIdx] > NumReclose then
                                begin
                                    LockedOut^[PhIdx] := true;
                                    if SinglePhLockout and ShowEventLog then
                                        AppendtoEventLog('Recloser.' + Self.Name, Format('Phase %d opened on %s (1ph trip) & locked out (1ph lockout)', [PhIdx, RecloserTarget^[PhIdx]]), ActorID)
                                    else
                                    begin
                                        if ShowEventLog then
                                            AppendtoEventLog('Recloser.' + Self.Name, Format('Phase %d opened on %s (1ph trip) & locked out (3ph lockout)', [PhIdx, RecloserTarget^[PhIdx]]), ActorID); // 3-Phase Lockout

                                              // Lockout other phases
                                        for i := 1 to ControlledElement.Nphases do
                                        begin

                                            if (i <> PhIdx) and (not LockedOut^[i]) then  // Check LockedOut^[i] to skip individual phase that were previously locked out
                                            begin
                                                ControlledElement.Closed[i, ActorID] := false;
                                                FPresentState^[i] := CTRL_OPEN;
                                                LockedOut^[i] := true;
                                                if ArmedForOpen^[PhIdx] then
                                                    ArmedForOpen^[PhIdx] := false;
                                                if ShowEventLog then
                                                    AppendtoEventLog('Recloser.' + Self.Name, Format('Phase %d opened on 3ph lockout (1ph trip) & locked out (3ph lockout)', [i]), ActorID);

                                            end;

                                        end;

                                    end;

                                end
                                else
                                if ShowEventLog then
                                    AppendtoEventLog('Recloser.' + Self.Name, Format('Phase %d opened on %s (1ph trip)', [PhIdx, RecloserTarget^[PhIdx]]), ActorID);

                                ArmedForOpen^[PhIdx] := false;
                            end;
                    end;

                end
                else
                begin // 3-Ph Trip

                                // Analyze each phase separately as states may not be the same.
                    for i := 1 to ControlledElement.Nphases do
                    begin
                        case FPresentState^[i] of
                            CTRL_CLOSE:
                                if ArmedForOpen^[PhIdx] then
                                begin   // ignore if we became disarmed in meantime

                                    ControlledElement.Closed[i, ActorID] := false;   // Open phases of active terminal
                                    FPresentState^[i] := CTRL_OPEN;

                                    if OperationCount^[PhIdx] > NumReclose then
                                    begin
                                        LockedOut^[PhIdx] := true;
                                        if ShowEventLog then
                                            AppendtoEventLog('Recloser.' + Self.Name, Format('Phase %d opened on %s (3ph trip) & locked out (3ph lockout)', [i, RecloserTarget^[PhIdx]]), ActorID);
                                    end
                                    else
                                    if ShowEventLog then
                                        AppendtoEventLog('Recloser.' + Self.Name, Format('Phase %d opened on %s (3ph trip)', [i, RecloserTarget^[PhIdx]]), ActorID);

                                end;
                        end;

                    end;

                    ArmedForOpen^[PhIdx] := false;

                end;

            Integer(CTRL_CLOSE):
                if SinglePhTrip then
                begin

                    case FPresentState^[PhIdx] of
                        CTRL_OPEN:
                            if ArmedForClose^[PhIdx] and not LockedOut^[PhIdx] then
                            begin
                                ControlledElement.Closed[PhIdx, ActorID] := true;    // Close phase of active terminal
                                FPresentState^[PhIdx] := CTRL_CLOSE;

                                if ShowEventLog then
                                    AppendtoEventLog('Recloser.' + Self.Name, Format('Phase %d closed (1h reclosing)', [PhIdx]), ActorID);

                                          // Count reclosing operations for each phase on single ph trip
                                Inc(OperationCount^[PhIdx]);
                                ArmedForClose^[PhIdx] := false;
                            end;
                    end;

                end
                else
                begin

                    for i := 1 to ControlledElement.Nphases do
                    begin

                        case FPresentState^[i] of
                                        // Check LockedOut^[i] to skip individual phases that were previously locked out
                            CTRL_OPEN:
                                if ArmedForClose^[PhIdx] and not LockedOut^[i] and not LockedOut^[PhIdx] then
                                begin

                                    ControlledElement.Closed[i, ActorID] := true;    // Close phases of active terminal
                                    FPresentState^[i] := CTRL_CLOSE;
                                    if ShowEventLog then
                                        AppendtoEventLog('Recloser.' + Self.Name, Format('Phase %d closed (3ph reclosing)', [i]), ActorID);
                                end;

                        end;
                    end;

                    ArmedForClose^[PhIdx] := false;
                    Inc(OperationCount^[PhIdx]);

                end;

            Integer(CTRL_RESET):
                if SinglePhTrip then
                begin
                    case FPresentState^[PhIdx] of
                        CTRL_CLOSE:
                            if not ArmedForOpen^[PhIdx] then
                                OperationCount^[PhIdx] := 1;       // Don't reset if we just rearmed
                    end;
                end
                else
                begin
                    for i := 1 to ControlledElement.Nphases do
                    begin
                        case FPresentState^[i] of
                            CTRL_CLOSE:
                            begin
                                if not ArmedForOpen^[PhIdx] then
                                    OperationCount^[PhIdx] := 1;       // Don't reset if we just rearmed
                                if not SinglePhTrip then
                                    Break; // no need to loop at all phases
                            end;

                        else
                        end;
                    end;
                end
        else
        {Do Nothing }
        end;
    end;
end;


{--------------------------------------------------------------------------}


procedure TRecloserObj.InterpretRecloserState(ActorID: Integer; const param: String; const property_name: String);
var
    i: Integer;
    DataStr1, DataStr2: String;
begin

    // Only allowed to change normal state if locked.
    if Locked and ((LowerCase(property_name[1]) = 'a') or (LowerCase(property_name[1]) = 's')) then
        Exit;

    if (LowerCase(property_name[1]) = 'a') then // Interpret ganged specification to state when using action
    begin // action (deprecated) will be removed
        for i := 1 to RECLOSERCONTROLMAXDIM do
        begin
            case LowerCase(param)[1] of
                'o':
                    States[i] := CTRL_OPEN;
                'c':
                    States[i] := CTRL_CLOSE;
            end;

        end;
    end
    else
    begin
        if not Parser[ActorID].WasQuoted then // Interpret ganged specification to state and normal when not quoted
        begin
            for i := 1 to RECLOSERCONTROLMAXDIM do
            begin

                if (LowerCase(property_name[1]) = 's') then
                begin  // state
                    case LowerCase(param)[1] of
                        'o':
                            States[i] := CTRL_OPEN;
                        'c':
                            States[i] := CTRL_CLOSE;
                    end;

                end // 'normal
                else
                begin
                    case LowerCase(param)[1] of
                        'o':
                            NormalStates[i] := CTRL_OPEN;
                        'c':
                            NormalStates[i] := CTRL_CLOSE;
                    end;
                end;
            end;
        end
        else // process phase by phase

            AuxParser[ActorID].CmdString := param;  // Load up Parser

        DataStr1 := AuxParser[ActorID].NextParam;  // ignore
        DataStr2 := AuxParser[ActorID].StrValue;

        i := 1;
        while (Length(DataStr2) > 0) and (i < RECLOSERCONTROLMAXDIM) do
        begin

            if (LowerCase(property_name[1]) = 's') then
            begin  // state
                case LowerCase(DataStr2)[1] of
                    'o':
                        States[i] := CTRL_OPEN;
                    'c':
                        States[i] := CTRL_CLOSE;
                end;
            end
            else // 'normal'
            begin
                case LowerCase(DataStr2)[1] of
                    'o':
                        NormalStates[i] := CTRL_OPEN;
                    'c':
                        NormalStates[i] := CTRL_CLOSE;
                end;
            end;

            DataStr1 := AuxParser[ActorID].NextParam;  // ignore
            DataStr2 := AuxParser[ActorID].StrValue;
            inc(i);
        end;
    end;

end;

{--------------------------------------------------------------------------}
procedure TRecloserObj.Sample(ActorID: Integer);

var
    i: Integer;
    cmag: Double;
    Csum: Complex;
    MaxOperatingCount: Integer;

    GroundCurve, PhaseCurve: TTCC_CurveObj;
    Groundtime, PhaseTime, TripTime, TimeTest: Double;
    TDPhase, TDGround: Double;
    PhaseCurveType, GroundCurveType: String;

begin

    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    MonitoredElement.GetCurrents(cBuffer, ActorID);

    // Check state of phases of active terminal as they could have changed through other mechanisms
    for i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) do
    begin
        if ControlledElement.Closed[i, ActorID] then
            FPresentState^[i] := CTRL_CLOSE
        else
            FPresentState^[i] := CTRL_OPEN;
    end;

    if DebugTrace then
        AppendtoEventLog('Debug Sample: Recloser.' + Self.Name, Format('FPresentState: %s ', [self.GetPropertyValue(24)]), ActorID);

    for i := Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) downto 1 do
    begin
        if FPresentState^[i] = CTRL_CLOSE then
            Break; // Continue sampling if at least one phase is closed.
        if i = 1 then
            Exit;  // Exit sampling if none of the phases is closed.
    end;

     // Identify number of operations to pick appropriate curve.
     // Pending to identify phase to trip when considering single-phase tripping as in modern microprocessed relays.
    if SinglePhTrip then
    begin
        for i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) do
        begin

            if LockedOut^[i] then
                continue; // Skip locked out phases (includes phases that have been manually opened).

            if i = 1 then
                MaxOperatingCount := OperationCount^[i]             // TODO: check if we need to use IdxMultiPh for OperationCount here
            else
                MaxOperatingCount := Max(MaxOperatingCount, OperationCount^[i]);
        end;

    end
    else
        MaxOperatingCount := OperationCount^[IdxMultiPh];

    if MaxOperatingCount > NumFast then
    begin
        GroundCurve := GroundDelayed;
        TDGround := TDGrDelayed;
        GroundCurveType := 'Delayed';
    end
    else
    begin
        GroundCurve := GroundFast;
        TDGround := TDGrFast;
        GroundCurveType := 'Fast';
    end;

    GroundTime := -1.0;

     {Check Ground Trip, if any}
    if GroundCurve <> nil then
    begin
        Csum := CZERO;
        for i := (1 + CondOffset) to (Fnphases + CondOffset) do
            caccum(Csum, cBuffer^[i]);
        Cmag := Cabs(Csum);
        if (GroundInst > 0.0) and (Cmag >= GroundInst) and (MaxOperatingCount = 1) then
        begin
            GroundTime := 0.01 + DelayTime;      // Inst trip on first operation

            if DebugTrace then
                AppendToEventLog('Debug Sample: Recloser.' + Self.Name, Format('Ground Instantaneous Trip: Mag=%.3g, Time=%.3g',
                    [Cmag, GroundTime]), ActorID);
        end
        else
        begin
            GroundTime := TDGround * GroundCurve.GetTCCTime(Cmag / GroundTrip);

            if DebugTrace then
                AppendToEventLog('Debug Sample: Recloser.' + Self.Name, Format('Ground %s Curve Trip: Mag=%.3g, Time=%.3g',
                    [GroundCurveType, Cmag / GroundTrip, GroundTime]), ActorID);
        end;


    end;

    if Groundtime > 0.0 then
        GroundTarget := true;
     // If GroundTime > 0 then we have a ground trip

    if SinglePhTrip then
    begin
        for i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) do
        begin

            if FPresentState^[i] <> CTRL_CLOSE then
                continue;
            if Groundtime > 0.0 then
                TripTime := GroundTime
            else
                TripTime := -1.0;  // initialize trip time for this phase.

            if OperationCount^[i] > NumFast then
            begin
                PhaseCurve := PhaseDelayed;
                TDPhase := TDPhDelayed;
                PhaseCurveType := 'Delayed';
            end
            else
            begin
                PhaseCurve := PhaseFast;
                TDPhase := TDPhFast;
                PhaseCurveType := 'Fast';
            end;

            if FPresentState^[i] = CTRL_CLOSE then
            begin

                PhaseTime := -1.0;  {No trip}

                   {Check Phase Trip, if any} // Check current at i phase of monitored element
                if PhaseCurve <> nil then
                begin

                    Cmag := Cabs(cBuffer^[i + CondOffset]);

                    if (PhaseInst > 0.0) and (Cmag >= PhaseInst) and (OperationCount^[i] = 1) then
                    begin
                        PhaseTime := 0.01 + DelayTime;  // Inst trip on first operation
                        if DebugTrace then
                            AppendToEventLog('Debug Sample: Recloser.' + Self.Name, Format('Ph Instantaneous (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g',
                                [i, Cmag, PhaseTime]), ActorID);

                    end
                    else
                    begin
                        TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip);
                        if TimeTest > 0.0 then
                        begin
                            PhaseTime := TimeTest;

                            if DebugTrace then
                                AppendToEventLog('Debug Sample: Recloser.' + Self.Name, Format('Ph %s (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g',
                                    [PhaseCurveType, i, Cmag / PhaseTrip, PhaseTime]), ActorID);
                        end;

                    end;

                end;

                   // If PhaseTime > 0 then we have a phase trip
                if PhaseTime > 0.0 then
                begin

                    PhaseTarget^[i] := true;
                    if TripTime > 0.0 then
                        TripTime := Min(TripTime, Phasetime)
                    else
                        TripTime := PhaseTime;
                end;

                if TripTime > 0.0 then
                begin
                    if not ArmedForOpen^[i] then
                        with ActiveCircuit[ActorID] do   // Then arm for an open operation
                        begin

                            RecloserTarget^[i] := '';
                            if TripTime = Groundtime then
                            begin
                                if Groundtime = 0.01 + DelayTime then
                                    RecloserTarget^[i] := 'Gnd Instantaneous'
                                else
                                    RecloserTarget^[i] := Format('Ground %s', [GroundCurveType]);
                            end;
                            if TripTime = Phasetime then
                            begin
                                if RecloserTarget^[i] <> '' then
                                    RecloserTarget^[i] := RecloserTarget^[i] + ' + ';

                                if PhaseTime = 0.01 + DelayTime then
                                    RecloserTarget^[i] := 'Ph Instantaneous'
                                else
                                    RecloserTarget^[i] := Format('Ph %s', [PhaseCurveType]);
                            end;

                            ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime, CTRL_OPEN, i, Self, ActorID);
                            if OperationCount^[i] <= NumReclose then
                                ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + DelayTime + RecloseIntervals^[OperationCount^[i]], CTRL_CLOSE, i, Self, ActorID);
                            ArmedForOpen^[i] := true;
                            ArmedForClose^[i] := true;
                        end;
                end
                else
                begin
                    if ArmedForOpen^[i] then
                        with ActiveCircuit[ActorID] do    // If current dropped below pickup, disarm trip and set for reset
                        begin
                            ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, i, Self, ActorID);
                            ArmedForOpen^[i] := false;
                            ArmedForClose^[i] := false;
                            GroundTarget := false;
                            PhaseTarget^[i] := false;
                        end;
                end;
            end;  {IF PresentState=CLOSE}
        end;

    end
    else // 3-Phase Trip
    begin

        if MaxOperatingCount > NumFast then
        begin
            PhaseCurve := PhaseDelayed;
            TDPhase := TDPhDelayed;
            PhaseCurveType := 'Delayed';
        end
        else
        begin
            PhaseCurve := PhaseFast;
            TDPhase := TDPhFast;
            PhaseCurveType := 'Fast';
        end;

        if Groundtime > 0.0 then
            TripTime := GroundTime
        else
            TripTime := -1.0;  // initialize trip time
        PhaseTime := -1.0;

        {Check Phase Trip, if any}
        if PhaseCurve <> nil then
        begin
            for i := (1 + CondOffset) to (Fnphases + CondOffset) do
            begin

                Cmag := Cabs(cBuffer^[i]);

                if (PhaseInst > 0.0) and (Cmag >= PhaseInst) and (OperationCount^[IdxMultiPh] = 1) then
                begin
                    PhaseTime := 0.01 + DelayTime;  // Inst trip on first operation

                    if DebugTrace then
                        AppendToEventLog('Debug Sample: Recloser.' + Self.Name, Format('Ph Instantaneous (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g',
                            [i, Cmag, PhaseTime]), ActorID);

                    Break;  {FOR - if Inst, no sense checking other phases}
                end
                else
                begin
                    TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip);
                    if (TimeTest > 0.0) then
                    begin

                        if DebugTrace then
                            AppendToEventLog('Debug Sample: Recloser.' + Self.Name, Format('Ph %s (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g',
                                [IfThen(PhaseCurve = PhaseFast, 'Fast', 'Delayed'), i, Cmag / PhaseTrip, TimeTest]), ActorID);

                        if Phasetime < 0.0 then
                            PhaseTime := TimeTest
                        else
                            PhaseTime := Min(PhaseTime, TimeTest);
                    end;
                end;

            end;

        end;

        // If PhaseTime > 0 then we have a phase trip
        if PhaseTime > 0.0 then
        begin
            PhaseTarget^[IdxMultiPh] := true;
            if TripTime > 0.0 then
                TripTime := Min(TripTime, Phasetime)
            else
                TripTime := PhaseTime;
        end;

        if TripTime > 0.0 then
        begin
            if not ArmedForOpen^[IdxMultiPh] then
                with ActiveCircuit[ActorID] do   // Then arm for an open operation
                begin

                    RecloserTarget^[IdxMultiPh] := '';
                    if TripTime = Groundtime then
                    begin
                        if Groundtime = 0.01 + DelayTime then
                            RecloserTarget^[IdxMultiPh] := 'Gnd Instantaneous'
                        else
                            RecloserTarget^[IdxMultiPh] := Format('Ground %s', [GroundCurveType]);
                    end;
                    if TripTime = Phasetime then
                    begin
                        if RecloserTarget^[IdxMultiPh] <> '' then
                            RecloserTarget^[IdxMultiPh] := RecloserTarget^[IdxMultiPh] + ' + ';
                        if PhaseTime = 0.01 + DelayTime then
                            RecloserTarget^[IdxMultiPh] := 'Ph Instantaneous'
                        else
                            RecloserTarget^[IdxMultiPh] := Format('Ph %s', [PhaseCurveType]);
                    end;

                    ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime, CTRL_OPEN, 0, Self, ActorID);
                    if MaxOperatingCount <= NumReclose then
                        ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + DelayTime + RecloseIntervals^[MaxOperatingCount], CTRL_CLOSE, 0, Self, ActorID);
                    ArmedForOpen^[IdxMultiPh] := true;
                    ArmedForClose^[IdxMultiPh] := true;
                end;
        end
        else
        begin
            if ArmedForOpen^[IdxMultiPh] then
                with ActiveCircuit[ActorID] do    // If current dropped below pickup, disarm trip and set for reset
                begin
                    ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                    ArmedForOpen^[IdxMultiPh] := false;
                    ArmedForClose^[IdxMultiPh] := false;
                    GroundTarget := false;
                    PhaseTarget^[IdxMultiPh] := false;
                end;
        end;

    end;

end;


{--------------------------------------------------------------------------}
procedure TRecloserObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    if Complete then
    begin
        Writeln(F);
    end;

end;

function TRecloserObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin

    case Index of
        23..24:
            Result := '[';
    else
        Result := '';
    end;

    case Index of
        15:
            Result := Format('%d', [NumReclose + 1]);
        16:
        begin
            Result := '(';
            for i := 1 to NumReclose do
                Result := Result + Format('%-g, ', [RecloseIntervals^[i]]);
            Result := Result + ')';
        end;
        23:
            if ControlledElement <> nil then
            begin
                for i := 1 to ControlledElement.NPhases do
                begin
                    case FNormalState^[i] of
                        CTRL_OPEN:
                            Result := Result + 'open' + ', ';
                    else
                  {CTRL_CLOSE:} Result := Result + 'closed' + ', ';
                    end;
                end;
            end;
        24:
            if ControlledElement <> nil then
            begin
                for i := 1 to ControlledElement.NPhases do
                begin
                    case FPresentState^[i] of
                        CTRL_OPEN:
                            Result := Result + 'open' + ', ';
                    else
                 {CTRL_CLOSE:} Result := Result + 'closed' + ', ';
                    end;
                end;
            end;
        27:
            if Locked then
                Result := 'Yes'
            else
                Result := 'No';
    else
        Result := inherited GetPropertyValue(index);
    end;

    case Index of
        23..24:
            Result := Result + ']';
    else
    end;

end;


procedure TRecloserObj.Reset;
var
    i: Integer;
begin

    if not Locked and (ControlledElement <> nil) then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal

        for i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) do
        begin
            FPresentState^[i] := FNormalState^[i];  // reset to normal state
            ArmedForOpen^[i] := false;
            ArmedForClose^[i] := false;
            GroundTarget := false;
            PhaseTarget^[i] := false;

            case FNormalState[i] of
                CTRL_OPEN:
                begin
                    ControlledElement.Closed[i, ActiveActor] := false;
                    LockedOut^[i] := true;
                    OperationCount^[i] := NumReclose + 1;
                end;

            else
           {CTRL_CLOSE:}
            begin
                ControlledElement.Closed[i, ActiveActor] := true;
                LockedOut^[i] := false;
                OperationCount^[i] := 1;
            end;
            end;
        end;

    end;

end;

procedure TRecloserObj.set_Flocked(const Value: Boolean);
begin
    Flocked := Value;
end;

function TRecloserObj.get_States(Idx: Integer): EControlAction;
begin

    if ControlledElement <> nil then
    begin

        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
        case ControlledElement.Closed[Idx, ActiveActor] of
            false:
                FPresentState^[Idx] := CTRL_OPEN;
        else
        {TRUE:} FPresentState^[Idx] := CTRL_CLOSE;
        end;

    end;

    Result := FPresentState^[Idx];
end;

procedure TRecloserObj.set_States(Idx: Integer; const Value: EControlAction);
begin

    if States[Idx] <> Value then
    begin

        if ControlledElement <> nil then
        begin
            ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal

            case Value of
                CTRL_OPEN:
                begin
                    ControlledElement.Closed[Idx, ActiveActor] := false;
                    LockedOut^[Idx] := true;
                    OperationCount^[Idx] := NumReclose + 1;
                    ArmedForClose^[Idx] := false;
                end

            else
          {CTRL_CLOSE:} begin
                ControlledElement.Closed[Idx, ActiveActor] := true;
                LockedOut^[Idx] := false;
                OperationCount^[Idx] := 1;
                ArmedForOpen^[Idx] := false;
            end

            end;
        end;

        FPresentState^[Idx] := Value;
    end;

end;

function TRecloserObj.get_NormalStates(Idx: Integer): EControlAction;
begin
    Result := FNormalState^[Idx];
end;

procedure TRecloserObj.set_NormalStates(Idx: Integer; const Value: EControlAction);
begin
    if FNormalState^[Idx] <> Value then
    begin
        FNormalState^[Idx] := Value;
    end;
end;


procedure TRecloserObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := '';
    PropertyValue[4] := '1'; //'terminal';
    PropertyValue[5] := IntToStr(NumFast);
    PropertyValue[6] := 'none';
    PropertyValue[7] := 'none';
    PropertyValue[8] := 'none';
    PropertyValue[9] := 'none';
    PropertyValue[10] := '1.0';
    PropertyValue[11] := '1.0';
    PropertyValue[12] := '0';
    PropertyValue[13] := '0';
    PropertyValue[14] := '15';
    PropertyValue[15] := '4';
    PropertyValue[16] := '(0.5, 2.0, 2.0)';
    PropertyValue[17] := '0.0';
    PropertyValue[18] := ''; // action
    PropertyValue[19] := '1.0';
    PropertyValue[20] := '1.0';
    PropertyValue[21] := '1.0';
    PropertyValue[22] := '1.0';
    PropertyValue[23] := '[closed, closed, closed]';  // normal
    PropertyValue[24] := '[closed, closed, closed]';  // state
    PropertyValue[25] := 'No';  // SinglePhTripping
    PropertyValue[26] := 'No';  // SinglePhLockout
    PropertyValue[27] := 'No';  // Lock
    PropertyValue[28] := 'n';  // Reset

    inherited  InitPropertyValues(NumPropsThisClass);

end;

end.
