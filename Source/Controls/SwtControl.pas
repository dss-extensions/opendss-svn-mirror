unit SwtControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------}

interface
USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     Math;

CONST
  SWTCONTROLMAXDIM = 6;

TYPE

   pStateArray = ^StateArray;
   StateArray = Array[1..SWTCONTROLMAXDIM] of EControlAction;  // 0 = open 1 = close


  TSwtControl = class(TControlClass)
    protected
      PROCEDURE DefineProperties;
      FUNCTION MakeLike(const SwtControlName:String):Integer; override;
    public
      constructor Create;
      destructor Destroy; override;

      FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
      FUNCTION NewObject(const ObjName:String):Integer; override;
  end;

  TSwtControlObj = class(TControlElem)
    private

      FPresentState  : pStateArray;
      FNormalState   : pStateArray;
//      LockCommand    : EControlAction;
      FLocked        : Boolean;

      NormalStateSet : Boolean;

      PROCEDURE InterpretSwitchState(ActorID: Integer; const param: string; const property_name: string);
      FUNCTION  get_States(Idx: Integer): EControlAction;
      PROCEDURE set_States(Idx: Integer; const Value: EControlAction);
      FUNCTION  get_NormalStates(Idx: Integer): EControlAction;
      PROCEDURE set_NormalStates(Idx: Integer; const Value: EControlAction);

      procedure set_Flocked(const Value: Boolean);

    public
      constructor Create(ParClass:TDSSClass; const SwtControlName:String);
      destructor Destroy; override;

      PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
      PROCEDURE RecalcElementData(ActorID : Integer); Override;
      PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a SwtControl

      PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
      PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
      PROCEDURE Reset(ActorID : Integer); Override;  // Reset to initial defined state

      PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
      PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

      FUNCTION  GetPropertyValue(Index:Integer):String;Override;
      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

      Property States[Idx:Integer]:EControlAction Read get_States write set_States;
      Property NormalStates[Idx:Integer]:EControlAction Read get_NormalStates write set_NormalStates;

      Property IsLocked: Boolean Read FLocked;
      Property Locked: Boolean   Read Flocked write set_Flocked;

   end;

VAR
    ActiveSwtControlObj:TSwtControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, Sysutils, Utilities, solution;

CONST

    NumPropsThisClass = 8;

constructor TSwtControl.Create;  // Creates superstructure for all SwtControl objects
Begin
     Inherited Create;

     Class_name   := 'SwtControl';
     DSSClassType := DSSClassType + SWT_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(PropertyName, NumProperties);
     CommandList.Abbrev := TRUE;
End;

destructor TSwtControl.Destroy;

Begin
     Inherited Destroy;
End;

PROCEDURE TSwtControl.DefineProperties;
Begin

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

     PropertyHelp^[1] := 'Name of circuit element switch that the SwtControl operates. '+
                        'Specify the full object class and name.';
     PropertyHelp^[2] := 'Terminal number of the controlled element switch. ' +
                        '1 or 2, typically.  Default is 1.';
     PropertyHelp^[3] := 'DEPRECATED. See "State" property.';
     PropertyHelp^[4] := '{Yes | No} Controlled switch is locked in its present open / close state or unlocked. ' +
                         'When locked, the switch will not respond to either a manual state change issued by the user or a state change issues internally by OpenDSS when Reseting the control.';
     PropertyHelp^[5] := 'DEPRECATED.';
     PropertyHelp^[6] := 'ARRAY of strings {Open | Closed} representing the Normal state of the switch in each phase of the controlled element. ' +
                         'The switch reverts to this state for reset, change of mode, etc. ' +
                         'Defaults to "State" if not specifically declared.  Setting this property to {Open | Closed} sets the normal state to the specified value for all phases.';
     PropertyHelp^[7] := 'ARRAY of strings {Open | Closed} representing the Actual state of the switch in each phase of the controlled element. ' +
                         'Upon setting, immediately forces state of switch(es). Simulates manual control on Switch. Defaults to Closed for all phases. Setting this property to {Open | Closed} ' +
                         'sets the actual state to the specified value for all phases.';
     PropertyHelp^[8] := '{Yes | No} If Yes, forces Reset of switch to Normal state and removes Lock independently of any internal '+
                        'reset command for mode change, etc.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

FUNCTION TSwtControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new SwtControl and add it to SwtControl class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TSwtControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

FUNCTION TSwtControl.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer   :Integer;
   ParamName      :String;
   Param          :String;
   Devindex, i    :Integer;

Begin

  // continue parsing WITH contents of Parser
  ActiveSwtControlObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveSwtControlObj;

  Result := 0;

  WITH ActiveSwtControlObj Do Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0
         THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
           {internal SwtControl Property commands}
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 382);
            1: ElementName     := lowercase(Param);
            2: ElementTerminal := Parser[ActorID].IntValue;
            4: Locked := InterpretYesNo (Param);
            6: Begin    // set the normal state
                  InterpretSwitchState(ActorID, Param, ParamName);
                  if not NormalStateSet then NormalStateSet:= TRUE;
               End;
            3, 7: Begin    // set the present state
                  InterpretSwitchState(ActorID, Param, ParamName);
               End;
            8: If InterpretYesNo (Param) Then Begin  // force a reset
                  Locked := FALSE;
                  Reset(ActorID);
                  PropertyValue[8]  := 'n';
               End;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveSwtControlObj, ParamPointer - NumPropsthisClass)
         End;

         {Supplemental Actions}
         case ParamPointer of

//             4: if Locked then LockCommand :=  CTRL_LOCK else LockCommand := CTRL_UNLOCK;

             7: Begin
                  For i := 1 to FNPhases Do If not NormalStateSet then FNormalState^[i] := FPresentState^[i];
                  NormalStateSet := TRUE;   // normal state will default to state only the 1st state is specified.
                End;

         end;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
  End;

End;

FUNCTION TSwtControl.MakeLike(const SwtControlName:String):Integer;
VAR
   OtherSwtControl:TSwtControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this SwtControl name in the present collection}
   OtherSwtControl := Find(SwtControlName);
   IF OtherSwtControl<>Nil THEN
   WITH ActiveSwtControlObj Do
     Begin

        NPhases := OtherSwtControl.Fnphases;
        NConds  := OtherSwtControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherSwtControl.ElementName;
        ElementTerminal   := OtherSwtControl.ElementTerminal;
        ControlledElement := OtherSwtControl.ControlledElement;  // Pointer to target circuit element

        TimeDelay        := OtherSwtControl.TimeDelay;
        Locked           := OtherSwtControl.Locked;

        For i := 1 to Min(SWTCONTROLMAXDIM, ControlledElement.Nphases) Do Begin
          FPresentState^[i] := OtherSwtControl.FPresentState^[i];
          FNormalState^[i]  := OtherSwtControl.FNormalState^[i];
        End;

        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherSwtControl.PropertyValue[i];

     End
   ELSE  DoSimpleMsg('Error in SwtControl MakeLike: "' + SwtControlName + '" Not Found.', 383);

End;

{==========================================================================}
{                    TSwtControlObj                                           }
{==========================================================================}

constructor TSwtControlObj.Create(ParClass:TDSSClass; const SwtControlName:String);
var
  i: Integer;
Begin
  Inherited Create(ParClass);
  Name := LowerCase(SwtControlName);
  DSSObjType := ParClass.DSSClassType;

  NPhases := 3;  // Directly set conds and phases
  Fnconds := 3;
  Nterms := 1;  // this forces allocation of terminals and conductors in base class

  ElementName   := '';
  ControlledElement := NIL;
  ElementTerminal := 1;

  FPresentState := Nil;
  FNormalState  := Nil;

  // Reallocate arrays  (Must be initialized to nil for first call)
  Reallocmem(FPresentState, Sizeof(FPresentState^[1]) * FNPhases);
  Reallocmem(FNormalState,  Sizeof(FNormalState^[1])  * FNPhases);

  For i := 1 to Min(SWTCONTROLMAXDIM, FNPhases) Do Begin
    FPresentState^[i] := CTRL_CLOSE;
    FNormalState^[i]  := CTRL_CLOSE;  // default to present state;
  End;

  NormalStateSet := FALSE;

//  Lockcommand   := CTRL_NONE;
  Locked        := FALSE;
  TimeDelay     := 120.0; // 2 minutes

  InitPropertyValues(0);
End;

destructor TSwtControlObj.Destroy;
Begin

  ReallocMem(FPresentState,0);
  ReallocMem(FNormalState,0);

  Inherited Destroy;
End;

PROCEDURE TSwtControlObj.RecalcElementData(ActorID : Integer);
VAR
  DevIndex, i :Integer;
Begin
  Devindex := GetCktElementIndex(ElementName);
  IF DevIndex>0 THEN Begin
    ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
    Nphases := ControlledElement.NPhases;
    if FNphases > SWTCONTROLMAXDIM Then DosimpleMsg('Warning: SwitchControl '+Self.Name+': Number of phases > Max SwtControl dimension.', 384);
    if ElementTerminal > ControlledElement.NTerms Then
    Begin
      DoErrorMsg('SwtControl: "' + Name + '"',
                 'Terminal no. "' +'" does not exist.',
                 'Re-specify terminal no.', 384);
    End;

    Nconds  := FNphases;
    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    ControlledElement.HasSwtControl := TRUE;  // For Reliability calcs

    // Open/Close State of controlled element based on state assigned to the control
    For i := 1 to Min(SWTCONTROLMAXDIM, ControlledElement.Nphases) Do
      If FPresentState^[i] = CTRL_OPEN Then
        Begin
          ControlledElement.Closed[i,ActorID] := FALSE;
        End
      Else
        Begin
          ControlledElement.Closed[i,ActorID] := TRUE;
        End;

    // attach controller bus to the switch bus - no space allocated for monitored variables
    Setbus (1, ControlledElement.GetBus(ElementTerminal));

  End ELSE Begin
    ControlledElement := nil;   // element not found
    DoErrorMsg('SwtControl: "' + Self.Name + '"', 'CktElement Element "'+ ElementName + '" Not Found.',
             ' Element must be defined previously.', 387);
  End;
End;

procedure TSwtControlObj.MakePosSequence(ActorID : Integer);
begin
  if ControlledElement <> Nil then begin
    Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
    Setbus(1, ControlledElement.GetBus(ElementTerminal));
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TSwtControlObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil
End;

PROCEDURE TSwtControlObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin
  For i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

PROCEDURE TSwtControlObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
  FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

PROCEDURE TSwtControlObj.DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer);
Var ctrl_code : EControlAction;
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

PROCEDURE TSwtControlObj.InterpretSwitchState(ActorID : Integer; const param: String; const property_name: String);
var
  i: Integer;
  DataStr1, DataStr2: String;
Begin

  // Only allowed to change normal state if locked.
  if Locked and ((LowerCase(property_name[1]) = 'a') or (LowerCase(property_name[1]) = 's')) Then Exit;

  if (LowerCase(property_name[1]) = 'a') then // Interpret ganged specification to state and normal when using action
  begin // action (deprecated) will be removed
    for i:= 1 to SWTCONTROLMAXDIM do Begin

      case LowerCase(param)[1] of
        'o': NormalStates[i] := CTRL_OPEN;
        'c': NormalStates[i] := CTRL_CLOSE;
      End;

    End;
  End
  Else Begin

    if not Parser[ActorID].WasQuoted Then // Interpret ganged specification to state and normal when not quoted
    Begin
      for i:= 1 to SWTCONTROLMAXDIM do Begin

        if (LowerCase(property_name[1]) = 's') then begin  // state
          case LowerCase(param)[1] of
            'o': States[i] := CTRL_OPEN;
            'c': States[i] := CTRL_CLOSE;
          end;

        end // 'normal
        else
        begin
          case LowerCase(param)[1] of
            'o': NormalStates[i] := CTRL_OPEN;
            'c': NormalStates[i] := CTRL_CLOSE;
          end;
        End;
      End;
    End
    Else // process phase by phase

      AuxParser[ActorID].CmdString := param;  // Load up Parser

      DataStr1 := AuxParser[ActorID].NextParam;  // ignore
      DataStr2 := AuxParser[ActorID].StrValue;

      i := 1;
      While (Length(DataStr2)>0) and (i<SWTCONTROLMAXDIM) Do Begin

          if (LowerCase(property_name[1]) = 's') then begin  // state
              case LowerCase(DataStr2)[1] of
               'o': States[i] := CTRL_OPEN;
               'c': States[i] := CTRL_CLOSE;
              end;
          end
         else // 'normal'
         begin
             case LowerCase(DataStr2)[1] of
              'o': NormalStates[i] := CTRL_OPEN;
              'c': NormalStates[i] := CTRL_CLOSE;
             end;
         end;

        DataStr1 := AuxParser[ActorID].NextParam;  // ignore
        DataStr2 := AuxParser[ActorID].StrValue;
        inc(i);
      end;
    End;
End;

PROCEDURE TSwtControlObj.Sample(ActorID : Integer);
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

Function TSwtControlObj.get_States(Idx: Integer): EControlAction;
Begin

      IF ControlledElement <> NIL  THEN
      Begin

         ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal
          case ControlledElement.Closed[Idx,ActiveActor] of
            FALSE:  FPresentState^[Idx]:= CTRL_OPEN;
          else
            {TRUE:} FPresentState^[Idx]:= CTRL_CLOSE;
          end;

      End;

      Result := FPresentState^[Idx];
End;

Procedure TSwtControlObj.set_States(Idx: Integer; const Value: EControlAction);
Begin

      If States[Idx] <> Value Then Begin

          IF ControlledElement <> NIL  THEN
          Begin
            ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal
            case Value of
              CTRL_OPEN: ControlledElement.Closed[Idx,ActiveActor] := FALSE;
            else
              {CTRL_CLOSE:} ControlledElement.Closed[Idx,ActiveActor] := TRUE;
            end;
          End;

          FPresentState^[Idx] := Value;
      End;
End;

Function TSwtControlObj.get_NormalStates(Idx: Integer): EControlAction;
Begin
        Result := FNormalState^[Idx];
End;

Procedure TSwtControlObj.set_NormalStates(Idx: Integer; const Value: EControlAction);
Begin
      If FNormalState^[Idx] <> Value Then Begin
          FNormalState^[Idx] := Value;
      End;
End;

PROCEDURE TSwtControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);
VAR
  i:Integer;
Begin
  Inherited DumpProperties(F,Complete);
  WITH ParentClass Do
    For i := 1 to NumProperties Do Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[PropertyIdxMap^[i]]);
  If Complete THEN Writeln(F);
End;

FUNCTION TSwtControlObj.GetPropertyValue(Index: Integer): String;
var i: Integer;
begin

    Case Index of
      6..7: Result := '[';

    Else
        Result := '';
    End;

    case Index of
        1: Result := ElementName;
        2: Result := Format('%d',[ElementTerminal]);
        4: If Locked then Result := 'Yes' else Result := 'No';
        5: Result := Format('%-.7g',[TimeDelay]);
        6: If ControlledElement <> Nil Then
           Begin
            For i := 1 to ControlledElement.NPhases Do
              Begin
                case FNormalState^[i] of
                  CTRL_OPEN: Result := Result + 'open' + ', ';
                else
                  {CTRL_CLOSE:} Result := Result + 'closed' + ', ';
                end;
              End;
           End;
        7: If ControlledElement <> Nil Then
           Begin
             For i := 1 to ControlledElement.NPhases Do
              Begin
                case FPresentState^[i] of
                  CTRL_OPEN: Result := Result + 'open' + ', ';
                else
                  {CTRL_CLOSE:} Result := Result + 'closed' + ', ';
                end;
              End;
           End;
        8: Result := 'n';  // Always no; yes is executed immediately
    else
        Result := Inherited GetPropertyValue(Index);
    end;

    Case Index of
      6..7: Result := Result + ']';

    Else
    End;

end;

Procedure TSwtControlObj.Reset;
var i: Integer;
Begin

  if not Locked then Begin

    ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal

    For i := 1 to Min(SWTCONTROLMAXDIM, ControlledElement.Nphases) Do Begin
      FPresentState[i]   := FNormalState[i];  // reset to normal state

      case FNormalState[i] of
        CTRL_OPEN: ControlledElement.Closed[i,ActiveActor] := FALSE;
      else
       {CTRL_CLOSE:} ControlledElement.Closed[i,ActiveActor] := TRUE;
       end;

    End;

  End;

end;

procedure TSwtControlObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1]  := ''; //'element';
  PropertyValue[2]  := '1'; //'terminal';
  PropertyValue[3]  := '';  // 'action'
  PropertyValue[4]  := 'n';
  PropertyValue[5]  := '120.0';
  PropertyValue[5]  := '';
  PropertyValue[6]  := '[close, close, close]';  // normal;
  PropertyValue[7]  := '[close, close, close]';  // state;
  PropertyValue[8]  := 'n';
  inherited  InitPropertyValues(NumPropsThisClass);
end;

end.
