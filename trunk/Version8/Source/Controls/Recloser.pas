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

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
      utilities, TCC_Curve, Math, StrUtils;

CONST
  RECLOSERCONTROLMAXDIM = 6;

TYPE

  pStateArray = ^StateArray;
  StateArray = Array[1..RECLOSERCONTROLMAXDIM] of EControlAction;  // 0 = open 1 = close

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRecloser = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const RecloserName:String):Integer; override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRecloserObj = class(TControlElem)
     private

            PhSlowCurve,
            GndSlowCurve,
            PhFastCurve,
            GndFastCurve     :TTCC_CurveObj;

            ResetTime,
            MechanicalDelay,
            TDGndSlow,
            TDPhSlow,
            TDGndFast,
            TDPhFast  :Double;

            FPresentState,
            FNormalState    :pStateArray;

            OperationCount  :pIntegerArray;

            LockedOut,
            ArmedForClose, ArmedForOpen,
            PhaseTarget :pBooleanArray;
            GroundTarget: Boolean;
            IdxMultiPh: Integer; // Index used for accessing arrays for multi-phase, ganged operation

            RecloserTarget: pStringArray;

            NormalStateSet,
            SinglePhTrip,
            SinglePhLockout,
            FLocked :Boolean;

            CondOffset     :Integer; // Offset for monitored terminal

            cBuffer :pComplexArray;    // Complexarray buffer
            DebugTrace   :Boolean;

            PROCEDURE InterpretRecloserState(ActorID : Integer;const param:String; const property_name: String);
            FUNCTION  get_States(Idx: Integer): EControlAction;
            PROCEDURE set_States(Idx: Integer; const Value: EControlAction);
            FUNCTION  get_NormalStates(Idx: Integer): EControlAction;
            PROCEDURE set_NormalStates(Idx: Integer; const Value: EControlAction);

            procedure set_Flocked(const Value: Boolean);

     public

        RecloseIntervals : pdoubleArray;
        NumFast,
        NumReclose : Integer;
        MonitoredElementName     :String;
        MonitoredElementTerminal :Integer;
        PhFastPickup,
        GndFastPickup,
        PhSlowPickup,
        GndSlowPickup,
        PhInst,
        GndInst,
        RatedCurrent,
        InterruptingRating : Double;


       constructor Create(ParClass:TDSSClass; const RecloserName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
       PROCEDURE RecalcElementData(ActorID : Integer); Override;
       PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a Recloser

       PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset(ActorID : Integer); Override;  // Reset to initial defined state


       PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

       FUNCTION  GetPropertyValue(Index:Integer):String;Override;
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       Property States[Idx: Integer]:EControlAction Read get_States write set_States;
       Property NormalStates[Idx: Integer]:EControlAction Read get_NormalStates write set_NormalStates;

       Property Locked: Boolean   Read Flocked write set_Flocked;

   end;


VAR
    ActiveRecloserObj : TRecloserObj;
    RecloserClass     : TRecloser;


{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,   Sysutils, uCmatrix, MathUtil;

CONST

    NumPropsThisClass = 42;

    CURRENT = 0;  {Default}
    VOLTAGE = 1;
    REVPOWER = 3;

VAR
   TCC_CurveClass:TDSSClass;

{General Module Function}

Function GetTccCurve(Const CurveName:String):TTCC_CurveObj;

Begin

     Result := NIL;
     if lowercase(CurveName) = 'none' then Exit;

     Result := TCC_CurveClass.Find(CurveName);
     IF Result = NIL
     THEN DoSimpleMsg('TCC Curve object: "'+CurveName+'" not found.', 388);

End;


{--------------------------------------------------------------------------}
constructor TRecloser.Create;  // Creates superstructure for all Recloser objects
Begin
     Inherited Create;

     Class_name   := 'Recloser';
     DSSClassType := DSSClassType + RECLOSER_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(PropertyName, NumProperties);
     CommandList.Abbrev := TRUE;

     TCC_CurveClass := GetDSSClassPtr('TCC_Curve');
     RecloserClass := Self;
End;

{--------------------------------------------------------------------------}
destructor TRecloser.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TRecloser.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName^[1]  := 'MonitoredObj';
     PropertyName^[2]  := 'MonitoredTerm';
     PropertyName^[3]  := 'SwitchedObj';
     PropertyName^[4]  := 'SwitchedTerm';
     PropertyName^[5]  := 'NumFast';
     PropertyName^[6]  := 'PhFastCurve';
     PropertyName^[7]  := 'PhSlowCurve';
     PropertyName^[8]  := 'GndFastCurve';
     PropertyName^[9]  := 'GndSlowCurve';
     PropertyName^[10] := 'PhFastPickup';
     PropertyName^[11] := 'GndFastPickup';
     PropertyName^[12] := 'PhInst';
     PropertyName^[13] := 'GndInst';
     PropertyName^[14] := 'ResetTime';
     PropertyName^[15] := 'Shots';
     PropertyName^[16] := 'RecloseIntervals';
     PropertyName^[17] := 'MechanicalDelay';
     PropertyName^[18] := 'Action';
     PropertyName^[19] := 'TDPhFast';
     PropertyName^[20] := 'TDGndFast';
     PropertyName^[21] := 'TDPhSlow';
     PropertyName^[22] := 'TDGndSlow';
     PropertyName^[23] := 'Normal';
     PropertyName^[24] := 'State';
     PropertyName^[25] := 'SinglePhTrip';
     PropertyName^[26] := 'SinglePhLockout';
     PropertyName^[27] := 'Lock';
     PropertyName^[28] := 'Reset';
     PropertyName^[29] := 'EventLog';
     PropertyName^[30] := 'DebugTrace';
     PropertyName^[31] := 'RatedCurrent';
     PropertyName^[32] := 'InterruptingRating';
     PropertyName^[33] := 'PhaseFast';
     PropertyName^[34] := 'PhaseDelayed';
     PropertyName^[35] := 'GroundFast';
     PropertyName^[36] := 'GroundDelayed';
     PropertyName^[37] := 'PhaseTrip';
     PropertyName^[38] := 'GroundTrip';
     PropertyName^[39] := 'PhSlowPickup';
     PropertyName^[40] := 'GndSlowPickup';
     PropertyName^[41] := 'TDPhDelayed';
     PropertyName^[42] := 'TDGrDelayed';


     PropertyHelp^[1] := 'Full object name of the circuit element, typically a line, transformer, load, or generator, '+
                        'to which the Recloser''s PT and/or CT are connected.' +
                        ' This is the "monitored" element. ' +
                        'There is no default; must be specified.';
     PropertyHelp^[2] := 'Number of the terminal of the circuit element to which the Recloser is connected. '+
                        '1 or 2, typically.  Default is 1.';
     PropertyHelp^[3] := 'Name of circuit element switch that the Recloser controls. '+
                        'Specify the full object name.' +
                        'Defaults to the same as the Monitored element. '+
                        'This is the "controlled" element.';
     PropertyHelp^[4] := 'Number of the terminal of the controlled element in which the switch is controlled by the Recloser. '+
                        '1 or 2, typically.  Default is 1.';
     PropertyHelp^[5] := 'Number of Fast (fuse saving) operations.  Default is 1. (See "Shots")';
     PropertyHelp^[6] := 'Name of the TCC Curve object that determines the Phase Fast trip. Must have been previously defined as a TCC_Curve object or specified as "none" (ignored). '+
                        'Default is "none". ' +
                        'Multiplying the current values in the curve by the "PhFastPickup" value gives the actual current.';
     PropertyHelp^[7] := 'Name of the TCC Curve object that determines the Phase Slow trip. Must have been previously defined as a TCC_Curve object or specified as "none" (ignored). '+
                        'Default is "none". ' +
                        'Multiplying the current values in the curve by the "PhSlowPickup" value gives the actual current.';
     PropertyHelp^[8] := 'Name of the TCC Curve object that determines the Ground Fast trip.  Must have been previously defined as a TCC_Curve object or specified as "none" (ignored). '+
                        'Default is "none". ' +
                        'Multiplying the current values in the curve by the "GndFastPickup" value gives the actual current.';
     PropertyHelp^[9] := 'Name of the TCC Curve object that determines the Ground Slow trip.  Must have been previously defined as a TCC_Curve object or specified as "none" (ignored). '+
                         'Default is "none". ' +
                        'Multiplying the current values in the curve by the "GndSlowPickup" value gives the actual current.';
     PropertyHelp^[10] := 'Multiplier for the phase fast TCC curve. Defaults to 1.0.';
     PropertyHelp^[11] := 'Multiplier for the ground fast TCC curve. Defaults to 1.0.';
     PropertyHelp^[12] := 'Actual amps for instantaneous phase trip which is assumed to happen in 0.01 sec + Mechanical Delay Time. Default is 0.0, which signifies no inst trip.';
     PropertyHelp^[13] := 'Actual amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Mechanical Delay Time. Default is 0.0, which signifies no inst trip.';
     PropertyHelp^[14] := 'Reset time in sec for Recloser. Default is 15.';
     PropertyHelp^[15] := 'Total Number of fast and delayed shots to lockout.  Default is 4. This is one more than the number of reclose intervals.';
     PropertyHelp^[16] := 'Array of reclose intervals.  Default for Recloser is (0.5, 2.0, 2.0) seconds. ' +
                         'A locked out Recloser must be closed manually (action=close).';
     PropertyHelp^[17] := 'Fixed delay time (sec) added to Recloser trip time. Default is 0.0. Used to represent breaker time or any other delay.' ;
     PropertyHelp^[18] := 'DEPRECATED. See "State" property';
     PropertyHelp^[19] := 'Time dial for Phase Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.';
     PropertyHelp^[20] := 'Time dial for Ground Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.';
     PropertyHelp^[21] := 'Time dial for Phase Slow trip curve. Multiplier on time axis of specified curve. Default=1.0.';
     PropertyHelp^[22] := 'Time dial for Ground Slow trip curve. Multiplier on time axis of specified curve. Default=1.0.';
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
     PropertyHelp^[28] := '{Yes | No} If Yes, forces Reset of recloser to Normal state and removes Lock independently of any internal '+
                          'reset command for mode change, etc.';
     PropertyHelp^[29] := '{Yes/True* | No/False} Default is Yes for Recloser. Write trips, reclose and reset events to EventLog.';
     PropertyHelp^[30] := '{Yes/True* | No/False} Default is No for Recloser. Write extra details to Eventlog.';
     PropertyHelp^[31] := 'Recloser continous rated current in Amps. Defaults to 0. Not used internally for either power flow or reporting.';
     PropertyHelp^[32] := 'Recloser rated interrupting current in Amps. Defaults to 0. Not used internally for either power flow or reporting.';
     PropertyHelp^[33] := 'DEPRECATED. See "PhFastCurve" property.';
     PropertyHelp^[34] := 'DEPRECATED. See "PhSlowCurve" property.';
     PropertyHelp^[35] := 'DEPRECATED. See "GndFastCurve" property.';
     PropertyHelp^[36] := 'DEPRECATED. See "GndSlowCurve" property.';
     PropertyHelp^[37] := 'DEPRECATED. Assigned value is specified to "PhPickupFast" and "PhPickupSlow" properties for backwards compatibility. See "PhPickupFast" and "PhPickupSlow" properties.';
     PropertyHelp^[38] := 'DEPRECATED. Assigned value is specified to "GndPickupFast" and "GndPickupSlow" properties for backwards compatibility. See "GndPickupFast" and "GndPickupSlow" properties.';
     PropertyHelp^[39] := 'Multiplier for the phase slow TCC curve. Defaults to 1.0.';
     PropertyHelp^[40] := 'Multiplier for the ground slow TCC curve. Defaults to 1.0.';
     PropertyHelp^[41] := 'DEPRECATED. Assigned value is specified to "TDPhSlow" property for backwards compatibility. See "TDPhSlow" property.';
     PropertyHelp^[42] := 'DEPRECATED. Assigned value is specified to "TDGndSlow" property for backwards compatibility. See "TDGndSlow" property.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TRecloser.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Recloser and add it to Recloser class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TRecloserObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;
{--------------------------------------------------------------------------}


{--------------------------------------------------------------------------}
FUNCTION TRecloser.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer, i :Integer;
   ParamName       :String;
   Param           :String;

Begin

  // continue parsing WITH contents of Parser
  ActiveRecloserObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveRecloserObj;

  Result := 0;

  WITH ActiveRecloserObj Do Begin

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
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 390);
            1: MonitoredElementName     := lowercase(param);
            2: MonitoredElementTerminal := Parser[ActorID].IntValue;
            3: ElementName     := lowercase(param);
            4: ElementTerminal := Parser[ActorID].IntValue;
            5: NumFast   := Parser[ActorID].Intvalue;
	          6, 33: PhFastCurve := GetTccCurve(Param);
            7, 34: PhSlowCurve := GetTCCCurve(Param);
	          8, 35: GndFastCurve  := GetTccCurve(Param);
            9, 36: GndSlowCurve  := GetTCCCurve(Param);
           10: PhFastPickup  := Parser[ActorID].Dblvalue;
           11: GndFastPickup  := Parser[ActorID].Dblvalue;
           12: PhInst      := Parser[ActorID].Dblvalue;
           13: GndInst     := Parser[ActorID].Dblvalue;
           14: ResetTime   := Parser[ActorID].Dblvalue;
           15: NumReclose  := Parser[ActorID].Intvalue -1 ;   // one less than number of shots
           16: NumReclose  := Parser[ActorID].ParseAsVector(4, RecloseIntervals);   // max of 4 allowed
           17: MechanicalDelay   := Parser[ActorID].DblValue;
           19: TDPhFast    := Parser[ActorID].DblValue;
           20: TDGndFast    := Parser[ActorID].DblValue;
           21, 41: TDPhSlow    := Parser[ActorID].DblValue;
           22, 42: TDGndSlow    := Parser[ActorID].DblValue;
           23: Begin
                  InterpretRecloserState(ActorID, Param, ParamName);   // set normal state
                  if not NormalStateSet then NormalStateSet := TRUE;
               End;
       18, 24: InterpretRecloserState(ActorID, Param, ParamName);    // set state
           25: SinglePhTrip := InterpretYesNo (Param);
           26: SinglePhLockout := InterpretYesNo (Param);
           27: Locked := InterpretYesNo (Param);
           28: If InterpretYesNo (Param) Then Begin  // force a reset
                  Locked := FALSE;
                  Reset(ActorID);
                  PropertyValue[28]  := 'n';
               End;
           29: ShowEventLog := InterpretYesNo(param);
           30: DebugTrace   := InterpretYesNo(Param);
           31: RatedCurrent := Parser[ActorID].Dblvalue;
           32: InterruptingRating := Parser[ActorID].Dblvalue;
           37: Begin
                  PhFastPickup := Parser[ActorID].Dblvalue;
                  PhSlowPickup := PhFastPickup;
               End;
           38: Begin
                 GndFastPickup  := Parser[ActorID].Dblvalue;
                 GndSlowPickup  := Parser[ActorID].DblValue;
               End;
           39: PhSlowPickup  := Parser[ActorID].DblValue;
           40: GndSlowPickup := Parser[ActorID].DblValue;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveRecloserObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer OF
              {Default the controlled element to the monitored element}
              1: ElementName     := MonitoredElementName;
              2: ElementTerminal := MonitoredElementTerminal;
              18, 24:
              Begin
                  For i := 1 to FNPhases Do If not NormalStateSet then FNormalState^[i] := FPresentState^[i];
                  NormalStateSet := TRUE;   // normal state will default to state only the 1st time state is specified.
              End;

         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
  End;

End;

{--------------------------------------------------------------------------}
FUNCTION TRecloser.MakeLike(const RecloserName:String):Integer;
VAR
   OtherRecloser:TRecloserObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this Recloser name in the present collection}
   OtherRecloser := Find(RecloserName);
   IF OtherRecloser<>Nil THEN
   WITH ActiveRecloserObj Do Begin

        NPhases := OtherRecloser.Fnphases;
        NConds  := OtherRecloser.Fnconds; // Force Reallocation of terminal stuff
        ShowEventLog := OtherRecloser.ShowEventLog; // but leave DebugTrace off

        ElementName       := OtherRecloser.ElementName;
        ElementTerminal   := OtherRecloser.ElementTerminal;
        ControlledElement := OtherRecloser.ControlledElement;  // Pointer to target circuit element

        MonitoredElement  := OtherRecloser.MonitoredElement;  // Pointer to target circuit element
        MonitoredElementName  := OtherRecloser.MonitoredElementName;  // Pointer to target circuit element
        MonitoredElementTerminal  := OtherRecloser.MonitoredElementTerminal;  // Pointer to target circuit element

        PhSlowCurve               := OtherRecloser.PhSlowCurve;
        GndSlowCurve              := OtherRecloser.GndSlowCurve;
        PhFastCurve               := OtherRecloser.PhFastCurve;
        GndFastCurve              := OtherRecloser.GndFastCurve;
        PhFastPickup              := OtherRecloser.PhFastPickup;
        GndFastPickup             := OtherRecloser.GndFastPickup;
        PhSlowPickup              := OtherRecloser.PhSlowPickup;
        GndSlowPickup             := OtherRecloser.GndSlowPickup;
        PhInst                    := OtherRecloser.PhInst;
        GndInst                   := OtherRecloser.GndInst;
        ResetTime                 := OtherRecloser.ResetTime;
        NumReclose                := OtherRecloser.NumReclose;
	      NumFast                   := OtherRecloser.NumFast;
        SinglePhTrip              := OtherRecloser.SinglePhTrip;
        SinglePhLockout           := OtherRecloser.SinglePhLockout;
        RatedCurrent              := OtherRecloser.RatedCurrent;
        InterruptingRating        := OtherRecloser.InterruptingRating;

        Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4);      // Always make a max of 4
        FOR i := 1 to NumReclose DO RecloseIntervals^[i] :=  OtherRecloser.RecloseIntervals^[i];

        Locked         := OtherRecloser.Locked;

        For i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) Do Begin
          FPresentState^[i]  := OtherRecloser.FPresentState^[i];
          FNormalState^[i]   := OtherRecloser.FNormalState^[i];
        End;

        CondOffset      := OtherRecloser.CondOffset;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherRecloser.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in Recloser MakeLike: "' + RecloserName + '" Not Found.', 391);

End;




{==========================================================================}
{                    TRecloserObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TRecloserObj.Create(ParClass:TDSSClass; const RecloserName:String);
var
  i: Integer;
Begin
     Inherited Create(ParClass);
     Name       := LowerCase(RecloserName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class


     ElementName       := '';
     ControlledElement := NIL;
     ElementTerminal   := 1;

     MonitoredElementName := '';
     MonitoredElementTerminal := 1;
     MonitoredElement := NIL;

     PhFastCurve    := NIL;
     PhSlowCurve    := NIL;
     GndFastCurve   := NIL;
     GndSlowCurve   := NIL;

     PhFastPickup              := 1.0;
     GndFastPickup             := 1.0;
     PhSlowPickup              := 1.0;
     GndSlowPickup             := 1.0;
     PhInst                    := 0.0;
     GndInst                   := 0.0;

     RatedCurrent        := 0.0;
     InterruptingRating  := 0.0;

     TDGndSlow    := 1.0;
     TDPhSlow    := 1.0;
     TDGndFast    := 1.0;
     TDPhFast    := 1.0;

     ResetTime      := 15.0;
     NumReclose     := 3;
     NumFast	     := 1;

     RecloseIntervals := NIL;
     Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4); // fixed allocation of 4
     RecloseIntervals^[1] := 0.5;
     RecloseIntervals^[2] := 2.0;
     RecloseIntervals^[3] := 2.0;

     FPresentState   := Nil;
     FNormalState    := Nil;
     LockedOut       := Nil;
     ArmedForOpen    := Nil;
     ArmedForClose   := Nil;
     GroundTarget    := FALSE;
     PhaseTarget     := Nil;
     Operationcount  := Nil;
     RecloserTarget  := Nil;
     SinglePhTrip    := FALSE;
     SinglePhLockout := FALSE;
     IdxMultiPh      := FNPhases + 1;

     // Reallocate arrays  (Must be initialized to nil for first call)
     Reallocmem(FPresentState, Sizeof(FPresentState^[1]) * FNPhases);
     Reallocmem(FNormalState,  Sizeof(FNormalState^[1])  * FNPhases);
     Reallocmem(LockedOut, Sizeof(LockedOut^[1]) * IdxMultiPh);
     Reallocmem(ArmedForOpen,  Sizeof(ArmedForOpen^[1])  * IdxMultiPh);
     Reallocmem(ArmedForClose, Sizeof(ArmedForClose^[1]) * IdxMultiPh);
     Reallocmem(PhaseTarget, Sizeof(PhaseTarget^[1]) * IdxMultiPh);
     Reallocmem(Operationcount,  Sizeof(Operationcount^[1])  * IdxMultiPh);
     RecloserTarget := AllocStringArray(IdxMultiPh);

     For i := 1 to Min(RECLOSERCONTROLMAXDIM, IdxMultiPh) Do Begin

       if i <= FNPhases then Begin
        FPresentState^[i]  := CTRL_CLOSE;
        FNormalState^[i]   := CTRL_CLOSE;  // default to present state;
       End;

       LockedOut^[i]      := FALSE;
       ArmedForOpen^[i]   := FALSE;
       ArmedForClose^[i]  := FALSE;
       PhaseTarget^[i]    := FALSE;
       Operationcount^[i] := 1;
       RecloserTarget^[i] := '';
     End;

     NormalStateSet := FALSE;
     Locked        := FALSE;

     cBuffer := Nil; // Complex buffer

     DSSObjType := ParClass.DSSClassType;

     InitPropertyValues(0);

   //  RecalcElementData;

End;

destructor TRecloserObj.Destroy;
Begin
  MonitoredElementName := '';
  ReallocMem(RecloseIntervals, 0);
  ReallocMem(cBuffer, 0);
  ReallocMem(FPresentState,0);
  ReallocMem(FNormalState,0);
  ReallocMem(LockedOut,0);
  ReallocMem(ArmedForOpen,0);
  ReallocMem(ArmedForClose,0);
  ReallocMem(PhaseTarget,0);
  ReallocMem(Operationcount,0);
  FreeStringArray(RecloserTarget, IdxMultiPh);

  Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex, i :Integer;

Begin

   Devindex := GetCktElementIndex(MonitoredElementName); // Global function
   IF   DevIndex>0
   THEN Begin

       MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
       Nphases := MonitoredElement.NPhases;       // Force number of phases to be same
       if FNphases > RECLOSERCONTROLMAXDIM Then DosimpleMsg('Warning: Recloser '+Self.Name+': Number of phases > Max Recloser dimension.', 392);
       IF MonitoredElementTerminal > MonitoredElement.Nterms
       THEN Begin
           DoErrorMsg('Recloser: "' + Name + '"',
                           'Terminal no. "' +'" does not exist.',
                           'Re-specify terminal no.', 392);
       End
       ELSE Begin
         // Sets name of i-th terminal's connected bus in Recloser's buslist
           Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
         // Allocate a buffer bigenough to hold everything from the monitored element
           ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
           CondOffset := (MonitoredElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
       End;
   End;

{Check for existence of Controlled Element}

   // If previously assigned, reset HasOCPDevice flag in case this is a move
   If Assigned(ControlledElement) Then Begin
      ControlledElement.HasOCPDevice := FALSE;
      ControlledElement.HasAutoOCPDevice := FALSE;
   End;

   Devindex := GetCktElementIndex(ElementName); // Global function
   IF DevIndex>0   THEN Begin  // Both CktElement and monitored element must already exist

       ControlledElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
       ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

       // If the recloser becomes disabled, leave at False
       If Enabled Then  Begin
           ControlledElement.HasOCPDevice := TRUE;  // For Reliability calcs
           ControlledElement.HasAutoOCPDevice := TRUE;  // For Reliability calcs
       End;

       // Open/Closed State of controlled element based on state assigned to the control
       For i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) Do  // TODO --- evaluate if we need to do anything here....
       If FPresentState^[i] = CTRL_CLOSE Then
       Begin
          ControlledElement.Closed[i,ActorID] := TRUE;
          LockedOut^[i]                       := FALSE;
          OperationCount^[i]                  := 1;
          ArmedForOpen^[i]                    := FALSE;
       End
       Else
       Begin
          ControlledElement.Closed[i,ActorID] := FALSE;
          LockedOut^[i]                       := TRUE;
          OperationCount^[i]                  := NumReclose + 1;
          ArmedForClose^[i]                   := FALSE;
       End;



   End
   ELSE Begin
      ControlledElement := nil;   // element not found
      DoErrorMsg('Recloser: "' + Self.Name + '"', 'CktElement Element "'+ ElementName + '" Not Found.',
                      ' Element must be defined previously.', 393);
   End;
End;

procedure TRecloserObj.MakePosSequence(ActorID : Integer);
begin
  if MonitoredElement <> Nil then begin
    Nphases := MonitoredElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
    ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
    CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;
{--------------------------------------------------------------------------}

PROCEDURE TRecloserObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer);
var i, PhIdx: Integer;
begin

    if SinglePhTrip then PhIdx := ProxyHdl else PhIdx := IdxMultiPh; // Proxy holds phase information for single-phase trip

    WITH   ControlledElement Do
    Begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1

        CASE Code of
            Integer(CTRL_OPEN):
                              if SinglePhTrip then
                              Begin
                                  CASE FPresentState^[PhIdx] of
                                      CTRL_CLOSE:IF ArmedForOpen^[PhIdx] THEN Begin   // ignore if we became disarmed in meantime

                                        ControlledElement.Closed[PhIdx,ActorID] := FALSE;   // Open phase of active terminal
                                        FPresentState^[PhIdx] := CTRL_OPEN;

                                        IF OperationCount^[PhIdx] > NumReclose THEN
                                        Begin
                                            LockedOut^[PhIdx] := TRUE;
                                            if SinglePhLockout and ShowEventLog then AppendtoEventLog('Recloser.'+Self.Name, Format('Phase %d opened on %s (1ph trip) & locked out (1ph lockout)', [PhIdx, RecloserTarget^[PhIdx]]),ActorID)
                                            Else
                                            Begin
                                              if ShowEventLog then AppendtoEventLog('Recloser.'+Self.Name, Format('Phase %d opened on %s (1ph trip) & locked out (3ph lockout)', [PhIdx, RecloserTarget^[PhIdx]]), ActorID); // 3-Phase Lockout

                                              // Lockout other phases
                                              for i := 1 to ControlledElement.Nphases Do
                                              Begin

                                                if (i <> PhIdx) and (Not LockedOut^[i]) then  // Check LockedOut^[i] to skip individual phase that were previously locked out
                                                Begin
                                                  ControlledElement.Closed[i,ActorID] := FALSE;
                                                  FPresentState^[i] := CTRL_OPEN;
                                                  LockedOut^[i] := TRUE;
                                                  IF ArmedForOpen^[i] then ArmedForOpen^[i] := FALSE;
                                                  if ShowEventLog then AppendtoEventLog('Recloser.'+Self.Name, Format('Phase %d opened on 3ph lockout (1ph trip) & locked out (3ph lockout)', [i]), ActorID);

                                                End;

                                              End;

                                            End;

                                        End
                                        ELSE if ShowEventLog then AppendtoEventLog('Recloser.'+Self.Name, Format('Phase %d opened on %s (1ph trip)', [PhIdx, RecloserTarget^[PhIdx]]),ActorID);

                                        ArmedForOpen^[PhIdx] := FALSE;
                                      END;
                                  END;

                              End
                              Else
                              Begin // 3-Ph Trip

                                // Analyze each phase separately as states may not be the same.
                                For i := 1 to ControlledElement.Nphases Do
                                Begin
                                   CASE FPresentState^[i] of
                                      CTRL_CLOSE:IF ArmedForOpen^[PhIdx] THEN Begin   // ignore if we became disarmed in meantime

                                        ControlledElement.Closed[i,ActorID] := FALSE;   // Open phases of active terminal
                                        FPresentState^[i] := CTRL_OPEN;

                                        IF OperationCount^[PhIdx] > NumReclose THEN
                                        Begin
                                            LockedOut^[PhIdx] := TRUE;
                                            if ShowEventLog then AppendtoEventLog('Recloser.'+Self.Name, Format('Phase %d opened on %s (3ph trip) & locked out (3ph lockout)', [i, RecloserTarget^[PhIdx]]), ActorID);
                                        End
                                        ELSE if ShowEventLog then AppendtoEventLog('Recloser.'+Self.Name, Format('Phase %d opened on %s (3ph trip)', [i, RecloserTarget^[PhIdx]]),ActorID);

                                      END;
                                   END;

                                End;

                                ArmedForOpen^[PhIdx] := FALSE;

                              End;

            Integer(CTRL_CLOSE):
                            if SinglePhTrip then
                            Begin

                              CASE FPresentState^[PhIdx] of
                                  CTRL_OPEN: IF ArmedForClose^[PhIdx] and Not LockedOut^[PhIdx] THEN Begin
                                    ControlledElement.Closed[PhIdx,ActorID] := TRUE;    // Close phase of active terminal
                                    FPresentState^[PhIdx] := CTRL_CLOSE;

                                    if ShowEventLog then AppendtoEventLog('Recloser.'+Self.Name, Format('Phase %d closed (1ph reclosing)', [PhIdx]),ActorID);

                                    // Count reclosing operations for each phase on single ph trip
                                    Inc(OperationCount^[PhIdx]);
                                    ArmedForClose^[PhIdx]     := FALSE;
                                  End;
                              End;

                            End
                            Else  // 3-Ph Trip
                            Begin

                                  For i := 1 to ControlledElement.Nphases Do
                                  Begin

                                      CASE FPresentState^[i] of
                                        // Check LockedOut^[i] to skip individual phases that were previously locked out
                                        CTRL_OPEN: IF ArmedForClose^[PhIdx] and Not LockedOut^[i] and Not LockedOut^[PhIdx] THEN Begin

                                          ControlledElement.Closed[i,ActorID] := TRUE;    // Close phases of active terminal
                                          FPresentState^[i] := CTRL_CLOSE;
                                          if ShowEventLog then AppendtoEventLog('Recloser.'+Self.Name, Format('Phase %d closed (3ph reclosing)', [i]),ActorID);
                                        End;

                                      End;
                                  End;

                                  ArmedForClose^[PhIdx]     := FALSE;
                                  Inc(OperationCount^[PhIdx]);

                            End;

            Integer(CTRL_RESET):
                            if SinglePhTrip then
                            Begin
                              CASE FPresentState^[PhIdx] of
                                  CTRL_CLOSE: IF Not ArmedForOpen^[PhIdx] THEN OperationCount^[PhIdx] := 1;       // Don't reset if we just rearmed
                              END;
                            End
                            Else
                            Begin
                              For i := 1 to ControlledElement.Nphases Do
                              Begin
                                CASE FPresentState^[i] of
                                  CTRL_CLOSE:
                                  Begin
                                    IF Not ArmedForOpen^[PhIdx] THEN OperationCount^[PhIdx] := 1;       // Don't reset if we just rearmed
                                    Break; // no need to loop at all closed phases
                                  End;

                                ELSE
                                END;
                              End;
                            End
        ELSE
        {Do Nothing }
        END;
    End;
end;


{--------------------------------------------------------------------------}


PROCEDURE TRecloserObj.InterpretRecloserState(ActorID : Integer; const param:String; const property_name: String);
var
  i: Integer;
  DataStr1, DataStr2: String;
Begin

    // Only allowed to change normal state if locked.
    if Locked and ((LowerCase(property_name[1]) = 'a') or (LowerCase(property_name[1]) = 's')) Then Exit;

    if (LowerCase(property_name[1]) = 'a') then // Interpret ganged specification to state when using action
    begin // action (deprecated) will be removed
      for i:= 1 to RECLOSERCONTROLMAXDIM do Begin
        case LowerCase(param)[1] of
          'o': States[i] := CTRL_OPEN;
          'c': States[i] := CTRL_CLOSE;
        End;

      End;
    End
    Else
    Begin
      if not Parser[ActorID].WasQuoted Then // Interpret ganged specification to state and normal when not quoted
      Begin
        for i:= 1 to RECLOSERCONTROLMAXDIM do Begin

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
        While (Length(DataStr2)>0) and (i<RECLOSERCONTROLMAXDIM) Do Begin

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

{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.Sample(ActorID : Integer);

VAR
   i                 :Integer;
   cmag              :Double;
   Csum              :Complex;
   MaxOperatingCount :Integer;

   GroundCurve, PhaseCurve:  TTCC_CurveObj;
   Groundtime, PhaseTime, TripTime, TimeTest :Double;
   TDPhase, TDGround : Double;
   PhaseCurveType, GroundCurveType: string;
   PhaseCurveMultiplier, GroundCurveMultiplier: Double;

begin

    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    MonitoredElement.GetCurrents(cBuffer, ActorID);

    // Check state of phases of active terminal as they could have changed through other mechanisms
    for i:=1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) Do
    Begin
      IF  ControlledElement.Closed[i,ActorID]
      THEN FPresentState^[i] := CTRL_CLOSE
      ELSE FPresentState^[i] := CTRL_OPEN;
    End;

    if DebugTrace then AppendtoEventLog('Debug Sample: Recloser.'+Self.Name, Format('FPresentState: %s ', [self.GetPropertyValue(24)]), ActorID);

     for i:=Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) downto 1 do
     Begin
        IF FPresentState^[i] = CTRL_CLOSE Then Break; // Continue sampling if at least one phase is closed.
        if i=1 then Exit;  // Exit sampling if none of the phases is closed.
     End;

     // Identify number of operations to pick appropriate curve.
     // Pending to identify phase to trip when considering single-phase tripping as in modern microprocessed relays.
     if SinglePhTrip then
     begin
      for i:=1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) Do
       Begin

          if LockedOut^[i] Then continue; // Skip locked out phases (includes phases that have been manually opened).

          if i = 1 then MaxOperatingCount := OperationCount^[i]             // TODO: check if we need to use IdxMultiPh for OperationCount here
          else MaxOperatingCount := Max(MaxOperatingCount, OperationCount^[i]);
       End;

     end
     else MaxOperatingCount := OperationCount^[IdxMultiPh];

     IF MaxOperatingCount > NumFast THEN
     Begin
      GroundCurve     := GndSlowCurve;
      TDGround        := TDGndSlow;
      GroundCurveType := 'Slow';
      GroundCurveMultiplier := GndSlowPickup;
     End
     ELSE
     Begin
      GroundCurve     := GndFastCurve;
      TDGround        := TDGndFast;
      GroundCurveType := 'Fast';
      GroundCurveMultiplier := GndFastPickup;
     End;

     GroundTime := -1.0;

     {Check Ground Trip, if any}
     IF GroundCurve <> NIL
     THEN Begin
         Csum := CZERO;
         FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
            caccum(Csum, cBuffer^[i]);
         Cmag  :=  Cabs(Csum);
         IF (GndInst>0.0) AND (Cmag>=GndInst) AND (MaxOperatingCount=1)
         THEN
         Begin
            GroundTime := 0.01 + MechanicalDelay;      // Inst trip on first operation

            if DebugTrace then
            AppendToEventLog ('Debug Sample: Recloser.'+Self.Name, Format ('Ground Instantaneous Trip: Mag=%.3g, Time=%.3g',
                             [Cmag, GroundTime]),ActorID);
         End
         ELSE
         Begin
            GroundTime :=  TDGround * GroundCurve.GetTCCTime(Cmag/ GroundCurveMultiplier);

            if DebugTrace then
            AppendToEventLog ('Debug Sample: Recloser.'+Self.Name, Format ('Ground %s Curve Trip: Mag=%.3g, Time=%.3g',
                             [GroundCurveType, Cmag/ GroundCurveMultiplier, GroundTime]),ActorID);
         End;


     End;

     IF Groundtime > 0.0 THEN GroundTarget := TRUE;
     // If GroundTime > 0 then we have a ground trip

     if SinglePhTrip then begin
         For i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) Do
         Begin

             IF FPresentState^[i] <> CTRL_CLOSE Then continue;
             IF Groundtime > 0.0 THEN TripTime := GroundTime else TripTime := -1.0;  // initialize trip time for this phase.

             IF OperationCount^[i] > NumFast THEN Begin
                 PhaseCurve  := PhSlowCurve;
                 TDPhase     :=  TDPhSlow;
                 PhaseCurveType := 'Slow';
                 PhaseCurveMultiplier := PhSlowPickup;
             End
             ELSE Begin
                 PhaseCurve     :=  PhFastCurve;
                 TDPhase        :=  TDPhFast;
                 PhaseCurveType := 'Fast';
                 PhaseCurveMultiplier := PhFastPickup;
             End;

             IF FPresentState^[i] = CTRL_CLOSE
             THEN Begin

                   PhaseTime := -1.0;  {No trip}

                   {Check Phase Trip, if any} // Check current at i phase of monitored element
                   IF PhaseCurve <> NIL
                   Then Begin

                     Cmag :=  Cabs(cBuffer^[i+CondOffset]);

                     IF (PhInst > 0.0) AND (Cmag>=PhInst) AND (OperationCount^[i]=1)
                     THEN
                     Begin
                        PhaseTime := 0.01 + MechanicalDelay;  // Inst trip on first operation
                        if DebugTrace then
                            AppendToEventLog ('Debug Sample: Recloser.'+Self.Name, Format ('Ph Instantaneous (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g',
                            [i, Cmag, PhaseTime]),ActorID);

                     End
                     ELSE Begin
                         TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag/PhaseCurveMultiplier);
                         IF TimeTest > 0.0 then Begin
                            PhaseTime := TimeTest;

                            if DebugTrace then
                            AppendToEventLog ('Debug Sample: Recloser.'+Self.Name, Format ('Ph %s (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g',
                            [PhaseCurveType, i, Cmag/PhaseCurveMultiplier, PhaseTime]),ActorID);
                         End;

                     End;

                   End;

                   // If PhaseTime > 0 then we have a phase trip
                   IF PhaseTime > 0.0
                   THEN Begin

                        PhaseTarget^[i] := TRUE;
                        IF   TripTime > 0.0
                        THEN TripTime := Min(TripTime, Phasetime)
                        ELSE TripTime := PhaseTime;
                   End;

                   IF   TripTime > 0.0
                   THEN Begin
                      IF Not ArmedForOpen^[i]
                      THEN WITH ActiveCircuit[ActorID] Do   // Then arm for an open operation
                      Begin

                          RecloserTarget^[i] := '';
                          If TripTime = Groundtime Then
                          Begin
                            if Groundtime = 0.01 + MechanicalDelay then RecloserTarget^[i] := 'Gnd Instantaneous'
                            else RecloserTarget^[i] := Format('Ground %s', [GroundCurveType]);
                          end;
                          If TripTime = Phasetime Then
                          Begin
                            if RecloserTarget^[i] <> '' then RecloserTarget^[i] := RecloserTarget^[i] + ' + ';

                            if PhaseTime = 0.01 + MechanicalDelay then RecloserTarget^[i] := 'Ph Instantaneous'
                            else RecloserTarget^[i] := Format('Ph %s', [PhaseCurveType]);
                          end;

                          ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + MechanicalDelay, CTRL_OPEN, i, Self, ActorID);
                          IF OperationCount^[i] <= NumReclose THEN ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + MechanicalDelay + RecloseIntervals^[OperationCount^[i]], CTRL_CLOSE, i, Self, ActorID);
                          ArmedForOpen^[i] := TRUE;
                          ArmedForClose^[i] := TRUE;
                      End;
                   End
                   ELSE Begin
                      IF ArmedForOpen^[i]
                      THEN WITH ActiveCircuit[ActorID] Do    // If current dropped below pickup, disarm trip and set for reset
                      Begin
                          ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, i, Self, ActorID);
                          ArmedForOpen^[i] := FALSE;
                          ArmedForClose^[i] := FALSE;
                          GroundTarget := FALSE;
                          PhaseTarget^[i] := FALSE;
                      End;
                   End;
             End;  {IF PresentState=CLOSE}
         End;

     end
     Else // 3-Phase Trip
     Begin

        IF MaxOperatingCount > NumFast THEN
        Begin
          PhaseCurve     := PhSlowCurve;
          TDPhase        := TDPhSlow;
          PhaseCurveType := 'Slow';
          PhaseCurveMultiplier := PhSlowPickup;
        End
        ELSE
        Begin
          PhaseCurve     := PhFastCurve;
          TDPhase        := TDPhFast;
          PhaseCurveType := 'Fast';
          PhaseCurveMultiplier := PhFastPickup;
        End;

        IF Groundtime > 0.0 THEN TripTime := GroundTime else TripTime := -1.0;  // initialize trip time
        PhaseTime := -1.0;

        {Check Phase Trip, if any}
        IF PhaseCurve <> NIL
        Then Begin
          FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
          Begin

             Cmag :=  Cabs(cBuffer^[i]);

             IF (PhInst>0.0) AND (Cmag>=PhInst) AND (OperationCount^[IdxMultiPh]=1)
             THEN Begin
                PhaseTime := 0.01 + MechanicalDelay;  // Inst trip on first operation

                if DebugTrace then
                                AppendToEventLog ('Debug Sample: Recloser.'+Self.Name, Format ('Ph Instantaneous (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g',
                                [i, Cmag, PhaseTime]),ActorID);

                Break;  {FOR - if Inst, no sense checking other phases}
             End
             ELSE Begin
               TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag/PhaseCurveMultiplier);
               IF (TimeTest > 0.0)
               THEN Begin

                  if DebugTrace then
                                AppendToEventLog ('Debug Sample: Recloser.'+Self.Name, Format ('Ph %s (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g',
                                [IfThen(PhaseCurve = PhFastCurve, 'Fast', 'Slow'), i, Cmag/PhaseCurveMultiplier, TimeTest]),ActorID);

                  IF Phasetime<0.0 THEN PhaseTime := TimeTest
                  ELSE PhaseTime := Min(PhaseTime, TimeTest);
               End;
             End;

          End;

        End;

        // If PhaseTime > 0 then we have a phase trip
        IF   PhaseTime > 0.0
        THEN Begin
          PhaseTarget^[IdxMultiPh] := TRUE;
          IF   TripTime > 0.0
          THEN TripTime := Min(TripTime, Phasetime)
          ELSE TripTime := PhaseTime;
        End;

        IF   TripTime > 0.0
         THEN Begin
            IF Not ArmedForOpen^[IdxMultiPh]
            THEN WITH ActiveCircuit[ActorID] Do   // Then arm for an open operation
            Begin

                   RecloserTarget^[IdxMultiPh] := '';
                   If TripTime = Groundtime Then
                   Begin
                     if Groundtime = 0.01 + MechanicalDelay then RecloserTarget^[IdxMultiPh] := 'Gnd Instantaneous'
                     else RecloserTarget^[IdxMultiPh] := Format('Ground %s', [GroundCurveType]);
                   end;
                   If TripTime = Phasetime Then
                   Begin
                     if RecloserTarget^[IdxMultiPh] <> '' then RecloserTarget^[IdxMultiPh] := RecloserTarget^[IdxMultiPh] + ' + ';
                     if PhaseTime = 0.01 + MechanicalDelay then RecloserTarget^[IdxMultiPh] := 'Ph Instantaneous'
                     else RecloserTarget^[IdxMultiPh] := Format('Ph %s', [PhaseCurveType]);
                   end;

                   ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + MechanicalDelay, CTRL_OPEN, 0, Self, ActorID);
                   IF MaxOperatingCount <= NumReclose THEN ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + MechanicalDelay + RecloseIntervals^[MaxOperatingCount], CTRL_CLOSE, 0, Self, ActorID);
                   ArmedForOpen^[IdxMultiPh]  := TRUE;
                   ArmedForClose^[IdxMultiPh] := TRUE;
            End;
         End
         ELSE Begin
             IF ArmedForOpen^[IdxMultiPh]
             THEN  WITH ActiveCircuit[ActorID] Do    // If current dropped below pickup, disarm trip and set for reset
             Begin
                  ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self, ActorID);
                  ArmedForOpen^[IdxMultiPh]  := FALSE;
                  ArmedForClose^[IdxMultiPh] := FALSE;
                  GroundTarget  := FALSE;
                  PhaseTarget^[IdxMultiPh]   := FALSE;
             End;
         End;

     End;

end;


{--------------------------------------------------------------------------}
PROCEDURE TRecloserObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
           Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete THEN
    Begin
      Writeln(F);
    End;

End;

FUNCTION TRecloserObj.GetPropertyValue(Index: Integer): String;
VAR
   i: Integer;
begin

    Case Index of
      23..24: Result := '[';
    Else
      Result := '';
    End;

    CASE Index of

      6, 33: if PhFastCurve <> nil then Result := PhFastCurve.Name else Result := 'none';
      7, 34: if PhSlowCurve <> nil then Result := PhSlowCurve.Name else Result := 'none';
      8, 35: if GndFastCurve <> nil then Result := GndFastCurve.Name else Result := 'none';
      9, 36: if GndSlowCurve <> nil then Result := GndSlowCurve.Name else Result := 'none';
      10, 37: Result:= Format('%.3f',[PhFastPickup]);
      11, 38: Result:= Format('%.3f',[GndFastPickup]);
      15: Result := Format('%d', [NumReclose+1]);
      16: Begin
           Result := '(';
           FOR i := 1 to NumReclose Do Result := Result + Format('%-g, ' , [RecloseIntervals^[i]]);
           Result := Result + ')';
          End;
      21, 41: Result := Format('%.3f',[TDPhSlow]);
      22, 42: Result := Format('%.3f',[TDGndSlow]);
      23: If ControlledElement <> Nil Then
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
      24: If ControlledElement <> Nil Then
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
      27: If Locked then Result := 'Yes' else Result := 'No';
      31: Result := Format('%-.6g',[RatedCurrent]);
      32: Result := Format('%-.6g',[InterruptingRating]);
      39: Result := Format('%.3f',[PhSlowPickup]);
      40: Result := Format('%.3f',[GndSlowPickup]);

    ELSE
      Result := Inherited GetPropertyValue(index);
    END;

    Case Index of
      23..24: Result := Result + ']';
    Else
    End;

end;


Procedure TRecloserObj.Reset;
var i: Integer;
Begin

    if not Locked and (ControlledElement <> NIL) THEN
    Begin
        ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal

        For i := 1 to Min(RECLOSERCONTROLMAXDIM, ControlledElement.Nphases) Do Begin
          FPresentState^[i]  := FNormalState^[i];  // reset to normal state
          ArmedForOpen^[i]   := FALSE;
          ArmedForClose^[i]  := FALSE;
          GroundTarget   := FALSE;
          PhaseTarget^[i]    := FALSE;

          case FNormalState[i] of
            CTRL_OPEN:
                      Begin
                        ControlledElement.Closed[i,ActiveActor] := FALSE;
                        LockedOut^[i] := TRUE;
                        OperationCount^[i] := NumReclose + 1;
                      End;

          else
           {CTRL_CLOSE:}
                      Begin
                        ControlledElement.Closed[i,ActiveActor] := TRUE;
                        LockedOut^[i] := FALSE;
                        OperationCount^[i] := 1;
                      End;
          end;
        End;

    End;

end;

procedure TRecloserObj.set_Flocked(const Value: Boolean);
begin
     Flocked := Value;
end;

Function TRecloserObj.get_States(Idx: Integer): EControlAction;
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

Procedure TRecloserObj.set_States(Idx: Integer; const Value: EControlAction);
Begin

  If States[Idx] <> Value Then Begin

      IF ControlledElement <> NIL  THEN
      Begin
        ControlledElement.ActiveTerminalIdx  := ElementTerminal;  // Set active terminal

        case Value of
           CTRL_OPEN:   Begin
                          ControlledElement.Closed[Idx,ActiveActor] := FALSE;
                          LockedOut^[Idx] := TRUE;
                          OperationCount^[Idx] := NumReclose+1;
                          ArmedForClose^[Idx] := FALSE;
                        End

        else
          {CTRL_CLOSE:} Begin
                          ControlledElement.Closed[Idx,ActiveActor] := TRUE;
                          LockedOut^[Idx] := FALSE;
                          OperationCount^[Idx] := 1;
                          ArmedForOpen^[Idx] := FALSE;
                        end

        end;
      End;

      FPresentState^[Idx] := Value;
  End;

End;

Function TRecloserObj.get_NormalStates(Idx: Integer): EControlAction;
Begin
  Result := FNormalState^[Idx];
End;

Procedure TRecloserObj.set_NormalStates(Idx: Integer; const Value: EControlAction);
Begin
  If FNormalState^[Idx] <> Value Then Begin
      FNormalState^[Idx] := Value;
  End;
End;


procedure TRecloserObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := ''; //'element';
     PropertyValue[2]  := '1'; //'terminal';
     PropertyValue[3]  := '';
     PropertyValue[4]  := '1'; //'terminal';
     PropertyValue[5]  := IntToStr(NumFast);
     PropertyValue[6]  := 'none';
     PropertyValue[7]  := 'none';
     PropertyValue[8]  := 'none';
     PropertyValue[9]  := 'none';
     PropertyValue[10] := '1.0';  // PhFastPickup
     PropertyValue[11] := '1.0';  // GrFastPickup
     PropertyValue[12] := '0';
     PropertyValue[13] := '0';
     PropertyValue[14] := '15';
     PropertyValue[15] := '4';
     PropertyValue[16] := '(0.5, 2.0, 2.0)';
     PropertyValue[17] := '0.0';
     PropertyValue[18] := ''; // action
     PropertyValue[19] := '1.0'; // TDPhFast
     PropertyValue[20] := '1.0'; // TDGrFast
     PropertyValue[21] := '1.0'; // TDPhSlow
     PropertyValue[22] := '1.0'; // TDGrSlow
     PropertyValue[23] := '[closed, closed, closed]';  // normal
     PropertyValue[24] := '[closed, closed, closed]';  // state
     PropertyValue[25] := 'No';  // SinglePhTripping
     PropertyValue[26] := 'No';  // SinglePhLockout
     PropertyValue[27] := 'No';  // Lock
     PropertyValue[28] := 'n';  // Reset
     PropertyValue[31] := '0';  // ratedcurrent
     PropertyValue[32] := '0';  // interruptingRating
     PropertyValue[33] := 'none'; // Deprecated - PhaseFast -> PhFastCurve
     PropertyValue[34] := 'none'; // Deprecated - PhaseDelayed -> PhSlowCurve
     PropertyValue[35] := 'none'; // Deprecated - GroundFast -> GndFastCurve
     PropertyValue[36] := 'none'; // Deprecated - GroundDelayed -> GndSlowCurve
     PropertyValue[37] := '1.0';  // Deprecated - PhaseTrip -> PhFastPickup
     PropertyValue[38] := '1.0';  // Deprecated - GroundTrip -> GndFastPickup
     PropertyValue[39] := '1.0';  // PhSlowPickup
     PropertyValue[40] := '1.0';  // GmdSlowPickup
     PropertyValue[41] := '1.0';  // Deprecated - TDPhDelayed -> TDPhSlow
     PropertyValue[42] := '1.0';  // Deprecated - TDGrDelayed -> TDGndSlow

  inherited  InitPropertyValues(NumPropsThisClass);

end;

end.
