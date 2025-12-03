unit SwtControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------}

interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    ucomplex,
    Math;

const
    SWTCONTROLMAXDIM = 6;

type

    pStateArray = ^StateArray;
    StateArray = array[1..SWTCONTROLMAXDIM] of EControlAction;  // 0 = open 1 = close


    TSwtControl = class(TControlClass)
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const SwtControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

    TSwtControlObj = class(TControlElem)
    PRIVATE

        FPresentState: pStateArray;
        FNormalState: pStateArray;
//      LockCommand    : EControlAction;
        FLocked: Boolean;

        NormalStateSet: Boolean;

        procedure InterpretSwitchState(ActorID: Integer; const param: String; const property_name: String);
        function get_States(Idx: Integer): EControlAction;
        procedure set_States(Idx: Integer; const Value: EControlAction);
        function get_NormalStates(Idx: Integer): EControlAction;
        procedure set_NormalStates(Idx: Integer; const Value: EControlAction);

        procedure set_Flocked(const Value: Boolean);

    PUBLIC

        RatedCurrent: Double;

        constructor Create(ParClass: TDSSClass; const SwtControlName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a SwtControl

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

        property IsLocked: Boolean READ FLocked;
        property Locked: Boolean READ Flocked WRITE set_Flocked;

    end;

var
    ActiveSwtControlObj: TSwtControlObj;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    Utilities,
    solution;

const

    NumPropsThisClass = 9;

constructor TSwtControl.Create;  // Creates superstructure for all SwtControl objects
begin
    inherited Create;

    Class_name := 'SwtControl';
    DSSClassType := DSSClassType + SWT_CONTROL;

    DefineProperties;

    CommandList := TCommandList.Create(PropertyName, NumProperties);
    CommandList.Abbrev := true;
end;

destructor TSwtControl.Destroy;

begin
    inherited Destroy;
end;

procedure TSwtControl.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count

    AllocatePropertyArrays;   {see DSSClass}

    PropertyName^[1] := 'SwitchedObj';
    PropertyName^[2] := 'SwitchedTerm';
    PropertyName^[3] := 'Action';
    PropertyName^[4] := 'Lock';
    PropertyName^[5] := 'Delay';
    PropertyName^[6] := 'Normal';
    PropertyName^[7] := 'State';
    PropertyName^[8] := 'Reset';
    PropertyName^[9] := 'RatedCurrent';

    PropertyHelp^[1] := 'Name of circuit element switch that the SwtControl operates. ' +
        'Specify the full object class and name.';
    PropertyHelp^[2] := 'Terminal number of the controlled element switch. ' +
        '1 or 2, typically.  Default is 1.';
    PropertyHelp^[3] := 'DEPRECATED. See "State" property.';
    PropertyHelp^[4] := '{Yes | No} Controlled switch is locked in its present open / closed state or unlocked. ' +
        'When locked, the switch will not respond to either a manual state change issued by the user or a state change issued internally by OpenDSS when reseting the control.';
    PropertyHelp^[5] := 'DEPRECATED.';
    PropertyHelp^[6] := 'ARRAY of strings {Open | Closed} representing the Normal state of the switch in each phase of the controlled element. ' +
        'The switch reverts to this state for reset, change of mode, etc. ' +
        'Defaults to "State" if not specifically declared.  Setting this property to {Open | Closed} sets the normal state to the specified value for all phases (ganged operation).';
    PropertyHelp^[7] := 'ARRAY of strings {Open | Closed} representing the Actual state of the switch in each phase of the controlled element. ' +
        'Upon setting, immediately forces the state of the switch(es). Simulates manual control on Switch. Defaults to Closed for all phases. Setting this property to {Open | Closed} ' +
        'sets the actual state to the specified value for all phases (ganged operation).';
    PropertyHelp^[8] := '{Yes | No} If Yes, forces Reset of switch to Normal state and removes Lock independently of any internal ' +
        'reset command for mode change, etc.';
    PropertyHelp^[9] := 'Switch continous rated current in Amps. Defaults to 0. Not used internally for either power flow or reporting.';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

function TSwtControl.NewObject(const ObjName: String): Integer;
begin
    // Make a new SwtControl and add it to SwtControl class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TSwtControlObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

function TSwtControl.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    Devindex, i: Integer;

begin

  // continue parsing WITH contents of Parser
    ActiveSwtControlObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveSwtControlObj;

    Result := 0;

    with ActiveSwtControlObj do
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
           {internal SwtControl Property commands}
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 382);
                1:
                    ElementName := lowercase(Param);
                2:
                    ElementTerminal := Parser[ActorID].IntValue;
                4:
                    Locked := InterpretYesNo(Param);
                6:
                begin    // set the normal state
                    InterpretSwitchState(ActorID, Param, ParamName);
                    if not NormalStateSet then
                        NormalStateSet := true;
                end;
                3, 7:
                begin    // set the present state
                    InterpretSwitchState(ActorID, Param, ParamName);
                end;
                8:
                    if InterpretYesNo(Param) then
                    begin  // force a reset
                        Locked := false;
                        Reset(ActorID);
                        PropertyValue[8] := 'n';
                    end;
                9:
                    RatedCurrent := Parser[ActorID].Dblvalue;

            else
           // Inherited parameters
                ClassEdit(ActiveSwtControlObj, ParamPointer - NumPropsthisClass)
            end;

         {Supplemental Actions}
            case ParamPointer of

                3, 7:
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

function TSwtControl.MakeLike(const SwtControlName: String): Integer;
var
    OtherSwtControl: TSwtControlObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this SwtControl name in the present collection}
    OtherSwtControl := Find(SwtControlName);
    if OtherSwtControl <> nil then
        with ActiveSwtControlObj do
        begin

            NPhases := OtherSwtControl.Fnphases;
            NConds := OtherSwtControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherSwtControl.ElementName;
            ElementTerminal := OtherSwtControl.ElementTerminal;
            ControlledElement := OtherSwtControl.ControlledElement;  // Pointer to target circuit element

            TimeDelay := OtherSwtControl.TimeDelay;
            Locked := OtherSwtControl.Locked;

            for i := 1 to Min(SWTCONTROLMAXDIM, ControlledElement.Nphases) do
            begin
                FPresentState^[i] := OtherSwtControl.FPresentState^[i];
                FNormalState^[i] := OtherSwtControl.FNormalState^[i];
            end;

            RatedCurrent := OtherSwtControl.RatedCurrent;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherSwtControl.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in SwtControl MakeLike: "' + SwtControlName + '" Not Found.', 383);

end;

{==========================================================================}
{                    TSwtControlObj                                           }
{==========================================================================}

constructor TSwtControlObj.Create(ParClass: TDSSClass; const SwtControlName: String);
var
    i: Integer;
begin
    inherited Create(ParClass);
    Name := LowerCase(SwtControlName);
    DSSObjType := ParClass.DSSClassType;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class

    ElementName := '';
    ControlledElement := nil;
    ElementTerminal := 1;

    FPresentState := nil;
    FNormalState := nil;

  // Reallocate arrays  (Must be initialized to nil for first call)
    Reallocmem(FPresentState, Sizeof(FPresentState^[1]) * FNPhases);
    Reallocmem(FNormalState, Sizeof(FNormalState^[1]) * FNPhases);

    for i := 1 to Min(SWTCONTROLMAXDIM, FNPhases) do
    begin
        FPresentState^[i] := CTRL_CLOSE;
        FNormalState^[i] := CTRL_CLOSE;  // default to present state;
    end;

    NormalStateSet := false;

    Locked := false;
    TimeDelay := 120.0; // 2 minutes

    RatedCurrent := 0.0;

    InitPropertyValues(0);
end;

destructor TSwtControlObj.Destroy;
begin

    ReallocMem(FPresentState, 0);
    ReallocMem(FNormalState, 0);

    inherited Destroy;
end;

procedure TSwtControlObj.RecalcElementData(ActorID: Integer);
var
    DevIndex, i: Integer;
begin
    Devindex := GetCktElementIndex(ElementName);
    if DevIndex > 0 then
    begin
        ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        Nphases := ControlledElement.NPhases;
        if FNphases > SWTCONTROLMAXDIM then
            DosimpleMsg('Warning: SwitchControl ' + Self.Name + ': Number of phases > Max SwtControl dimension.', 384);
        if ElementTerminal > ControlledElement.NTerms then
        begin
            DoErrorMsg('SwtControl: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 384);
        end;

        Nconds := FNphases;
        ControlledElement.ActiveTerminalIdx := ElementTerminal;
        ControlledElement.HasSwtControl := true;  // For Reliability calcs

    // Open/Closed State of controlled element based on state assigned to the control
        for i := 1 to Min(SWTCONTROLMAXDIM, ControlledElement.Nphases) do
            if FPresentState^[i] = CTRL_OPEN then
            begin
                ControlledElement.Closed[i, ActorID] := false;
            end
            else
            begin
                ControlledElement.Closed[i, ActorID] := true;
            end;

    // attach controller bus to the switch bus - no space allocated for monitored variables
        Setbus(1, ControlledElement.GetBus(ElementTerminal));

    end
    else
    begin
        ControlledElement := nil;   // element not found
        DoErrorMsg('SwtControl: "' + Self.Name + '"', 'CktElement Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 387);
    end;
end;

procedure TSwtControlObj.MakePosSequence(ActorID: Integer);
begin
    if ControlledElement <> nil then
    begin
        Nphases := ControlledElement.NPhases;
        Nconds := FNphases;
        Setbus(1, ControlledElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TSwtControlObj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil
end;

procedure TSwtControlObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TSwtControlObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TSwtControlObj.DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer);
var
    ctrl_code: EControlAction;
begin

     {
     ctrl_code := EControlAction(Code);  // change type
     ControlledElement.ActiveTerminalIdx := ElementTerminal;
     case Ctrl_Code of
          CTRL_LOCK:    Locked := TRUE;
          CTRL_UNLOCK:  Locked := FALSE;
     end;
     }
end;

procedure TSwtControlObj.InterpretSwitchState(ActorID: Integer; const param: String; const property_name: String);
var
    i: Integer;
    DataStr1, DataStr2: String;
begin

  // Only allowed to change normal state if locked.
    if Locked and ((LowerCase(property_name[1]) = 'a') or (LowerCase(property_name[1]) = 's')) then
        Exit;

    if (LowerCase(property_name[1]) = 'a') then // Interpret ganged specification to state when using action
    begin // action (deprecated) will be removed
        for i := 1 to SWTCONTROLMAXDIM do
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
            for i := 1 to SWTCONTROLMAXDIM do
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
        while (Length(DataStr2) > 0) and (i < SWTCONTROLMAXDIM) do
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

procedure TSwtControlObj.Sample(ActorID: Integer);
begin

  // Removing because action (redirects to state) and lock are instantaenous.
  {
  // push on the Lock command if any at the present time delay
  if LockCommand <> CTRL_NONE then
  With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do begin
       ControlQueue.Push(DynaVars.intHour, Dynavars.t + TimeDelay, LockCommand, 0, Self, ActorID);
       LockCommand := CTRL_NONE;  // reset the lock command for next time
  end;

  if (ActionCommand <> PresentState) and not Armed then   // we need to operate this switch
  With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do begin
       ControlQueue.Push(DynaVars.intHour, Dynavars.t + TimeDelay, ActionCommand, 0, Self, ActorID);
       Armed := TRUE;
  end;
  }

  {ControlledElement.ActiveTerminalIdx := ElementTerminal;
  IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
  THEN PresentState := CTRL_CLOSE
  ELSE PresentState := CTRL_OPEN; }
end;

procedure TSwtControlObj.set_Flocked(const Value: Boolean);
begin
    Flocked := Value;
end;

function TSwtControlObj.get_States(Idx: Integer): EControlAction;
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

procedure TSwtControlObj.set_States(Idx: Integer; const Value: EControlAction);
begin

    if States[Idx] <> Value then
    begin

        if ControlledElement <> nil then
        begin
            ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal
            case Value of
                CTRL_OPEN:
                    ControlledElement.Closed[Idx, ActiveActor] := false;
            else
              {CTRL_CLOSE:} ControlledElement.Closed[Idx, ActiveActor] := true;
            end;
        end;

        FPresentState^[Idx] := Value;
    end;
end;

function TSwtControlObj.get_NormalStates(Idx: Integer): EControlAction;
begin
    Result := FNormalState^[Idx];
end;

procedure TSwtControlObj.set_NormalStates(Idx: Integer; const Value: EControlAction);
begin
    if FNormalState^[Idx] <> Value then
    begin
        FNormalState^[Idx] := Value;
    end;
end;

procedure TSwtControlObj.DumpProperties(var F: TextFile; Complete: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);
    with ParentClass do
        for i := 1 to NumProperties do
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[PropertyIdxMap^[i]]);
    if Complete then
        Writeln(F);
end;

function TSwtControlObj.GetPropertyValue(Index: Integer): String;
var
    i: Integer;
begin

    case Index of
        6..7:
            Result := '[';

    else
        Result := '';
    end;

    case Index of
        1:
            Result := ElementName;
        2:
            Result := Format('%d', [ElementTerminal]);
        4:
            if Locked then
                Result := 'Yes'
            else
                Result := 'No';
        5:
            Result := Format('%-.7g', [TimeDelay]);
        6:
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
        7:
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
        8:
            Result := 'n';  // Always no; yes is executed immediately
        9:
            Result := Format('%-.6g', [RatedCurrent]);
    else
        Result := inherited GetPropertyValue(Index);
    end;

    case Index of
        6..7:
            Result := Result + ']';

    else
    end;

end;

procedure TSwtControlObj.Reset;
var
    i: Integer;
begin

    if not Locked then
    begin

        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal

        for i := 1 to Min(SWTCONTROLMAXDIM, ControlledElement.Nphases) do
        begin
            FPresentState[i] := FNormalState[i];  // reset to normal state

            case FNormalState[i] of
                CTRL_OPEN:
                    ControlledElement.Closed[i, ActiveActor] := false;
            else
       {CTRL_CLOSE:} ControlledElement.Closed[i, ActiveActor] := true;
            end;

        end;

    end;

end;

procedure TSwtControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := '';  // 'action'
    PropertyValue[4] := 'n';
    PropertyValue[5] := '120.0';
    PropertyValue[5] := '';
    PropertyValue[6] := '[closed, closed, closed]';  // normal;
    PropertyValue[7] := '[closed, closed, closed]';  // state;
    PropertyValue[8] := 'n';
    PropertyValue[9] := '0'; // ratedcurrent
    inherited  InitPropertyValues(NumPropsThisClass);
end;

end.
