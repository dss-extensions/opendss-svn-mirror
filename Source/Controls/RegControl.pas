unit RegControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   1-28-00 Created
   4-29-00 fixed problem with NumPhases = # phases of controlled element
   12/17/01 Added LDC logic
   12/18/01 Added MaxTapChange property and logic
}

{
  A RegControl is a control element that is connected to a terminal of another
  circuit element that must be a transformer.

  A RegControl is defined by a New command:

  New RegControl.Name=myname Transformer = name Terminal=[1,2,...] Controlledbus=name etc...

  Transformer to be controlled must already exist.
}

interface

USES
     Command, ControlClass, ControlElem, DSSClass, Arraydef, ucomplex,
     Transformer, utilities;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRegControl = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const RegControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TRegControlObj = class(TControlElem)
     private

            Vreg,
            Bandwidth,
            PTRatio,
            CTRating,
            R,
            X,
            revVreg,
            revBandwidth,
            revR,
            revX         :Double;
            RegulatedBus :String;
            IsReversible,
            LDCActive,
            UsingRegulatedBus :Boolean;

            FPendingTapChange,   // amount of tap change pending
            TapDelay :Double;   // delay between taps

            DebugTrace: Boolean;
            Armed:      Boolean;
            Tracefile: TextFile;

            TapLimitPerChange :Integer;
            TapWinding        :Integer;  // Added 7-19-07
            FInversetime      :Boolean;
            Vlimit           :Double;
            VLimitActive      :Boolean;

            VBuffer, CBuffer  :pComplexArray;
            FUNCTION Get_Transformer  :TTransfObj;
            FUNCTION Get_Winding      :Integer;

            PROCEDURE WriteTraceRecord(TapChangeMade:Double);
            procedure set_PendingTapChange(const Value: Double);
            FUNCTION AtLeastOneTap(Const ProposedChange:Double; Increment:Double):Double;
            Function ComputeTimeDelay(Vavg:Double):Double;
     public

       constructor Create(ParClass:TDSSClass; const RegControlName:String);
       destructor Destroy; override;

       PROCEDURE RecalcElementData; Override;
       PROCEDURE CalcYPrim; Override;    // Always Zero for a RegControl

       PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state


       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

       PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       Property Transformer:TTransfObj Read Get_Transformer;  // Pointer to controlled Transformer
       Property TrWinding:Integer Read Get_Winding;  // Report Tapped winding

       Property PendingTapChange: Double  Read FPendingTapChange Write set_PendingTapChange;

   end;


VAR
    ActiveRegControlObj:TRegControlObj;

{--------------------------------------------------------------------------}
implementation

USES

    ParserDel, DSSGlobals, Circuit, CktElement,  Sysutils, uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 21;

Var
    LastChange:Integer;
    
{--------------------------------------------------------------------------}
constructor TRegControl.Create;  // Creates superstructure for all RegControl objects
Begin
     Inherited Create;

     Class_name   := 'RegControl';
     DSSClassType := DSSClassType + REG_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TRegControl.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TRegControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'transformer';
     PropertyName[2] := 'winding';
     PropertyName[3] := 'vreg';
     PropertyName[4] := 'band';
     PropertyName[5] := 'ptratio';
     PropertyName[6] := 'CTprim';
     PropertyName[7] := 'R';
     PropertyName[8] := 'X';
     PropertyName[9] := 'bus';
     PropertyName[10] := 'delay';
     PropertyName[11] := 'reversible';
     PropertyName[12] := 'revvreg';
     PropertyName[13] := 'revband';
     PropertyName[14] := 'revR';
     PropertyName[15] := 'revX';
     PropertyName[16] := 'tapdelay';
     PropertyName[17] := 'debugtrace';
     PropertyName[18] := 'maxtapchange';
     PropertyName[19] := 'inversetime';
     PropertyName[20] := 'tapwinding';
     PropertyName[21] := 'vlimit';

     PropertyHelp[1] := 'Name of Transformer element to which the RegControl is connected. '+
                        'Do not specify the full object name; "Transformer" is assumed for '  +
                        'the object class.  Example:'+CRLF+CRLF+
                        'Transformer=Xfmr1';
     PropertyHelp[2] := 'Number of the winding of the transformer element that the RegControl is monitoring. '+
                        '1 or 2, typically.  Side Effect: Sets TAPWINDING property to the same winding.';
     PropertyHelp[3] := 'Voltage regulator setting, in VOLTS, for the winding being controlled.  Multiplying this '+
                        'value times the ptratio should yield the voltage across the WINDING of the controlled transformer.' +
                        ' Default is 120.0';
     PropertyHelp[4] := 'Bandwidth in VOLTS for the controlled bus (see help for ptratio property).  Default is 3.0';
     PropertyHelp[5] := 'Ratio of the PT that converts the controlled winding voltage to the regulator voltage. '+
                        'Default is 60.  If the winding is Wye, the line-to-neutral voltage is used.  Else, the line-to-line ' +
                        'voltage is used.';
     PropertyHelp[6] := 'Rating, in Amperes, of the primary CT rating for converting the line amps to control amps.'+
                        'The typical default secondary ampere rating is 5 Amps.';
     PropertyHelp[7] := 'R setting on the line drop compensator in the regulator, expressed in VOLTS.';
     PropertyHelp[8] := 'X setting on the line drop compensator in the regulator, expressed in VOLTS.';
     PropertyHelp[9] := 'Name of a bus in the system to use as the controlled bus instead of the bus to which the '+
                        'winding is connected or the R and X line drop compensator settings.  Do not specify this '+
                        'value if you wish to use the line drop compensator settings.  Default is null string. Assumes the base voltage for this '+
                        'bus is the same as the transformer winding base specified above. ' +
                        'Note: This bus WILL BE CREATED by the regulator control upon SOLVE if not defined by some other device.' ;
     PropertyHelp[10] := 'Time delay, in seconds, from when the voltage goes out of band to when the tap changing begins. ' +
                         'This is used to determine which regulator control will act first. Default is 15.  You may specify any '+
                         'floating point number to achieve a model of whatever condition is necessary.';
     PropertyHelp[11] := '{Yes |No} Indicates whether or not the regulator can be switched to regulate in the reverse direction. Default is No.' +
                         'Typically applies only to line regulators and not to LTC on a substation transformer.';
     PropertyHelp[12] := 'Voltage setting in volts for operation in the reverse direction.';
     PropertyHelp[13] := 'Bandwidth for operating in the reverse direction.';
     PropertyHelp[14] := 'R line drop compensator setting for reverse direction.';
     PropertyHelp[15] := 'X line drop compensator setting for reverse direction.';
     PropertyHelp[16] := 'Delay in sec between tap changes. Default is 2. This is how long it takes between changes ' +
                         'after the first change.';
     PropertyHelp[17] := '{Yes | No }  Default is no.  Turn this on to capture the progress of the regulator model ' +
                         'for each control iteration.  Creates a separate file for each RegControl named "REG_name.CSV".' ;
     PropertyHelp[18] := 'Maximum allowable tap change per control iteration in STATIC control mode.  Default is 16. ' + CRLF+ CRLF +
                         'Set this to 1 to better approximate actual control action. ' + CRLF + CRLF +
                         'Set this to 0 to fix the tap in the current position.';
     PropertyHelp[19] := '{Yes | No } Default is no.  The time delay is adjusted inversely proportional to the amount the voltage is outside the band down to 10%.';
     PropertyHelp[20] := 'Winding containing the actual taps, if different than the WINDING property. Defaults to the same winding as specified by the WINDING property.';
     PropertyHelp[21] := 'Voltage Limit for bus to which regulated winding is connected (e.g. first customer). Default is 0.0. ' +
                         'Set to a value greater then zero to activate this function.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TRegControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new RegControl and add it to RegControl class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TRegControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TRegControl.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

   Function Max(a,b:integer):Integer;
   Begin
      If a>=b Then Result := a else Result := b;
   End;
Begin

  // continue parsing WITH contents of Parser
  ActiveRegControlObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveRegControlObj;

  Result := 0;

  WITH ActiveRegControlObj Do
   Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 120);
            1: ElementName := 'Transformer.' + lowercase(param);
            2: ElementTerminal := Parser.IntValue;
            3: Vreg := Parser.DblValue;
            4: Bandwidth := Parser.DblValue;
            5: PTRatio := Parser.DblValue;
            6: CTRating := Parser.DblValue;
            7: R := Parser.DblValue;
            8: X := Parser.DblValue;
            9: RegulatedBus := Param;
            10: TimeDelay := Parser.DblValue;
            11: IsReversible := InterpretYesNo(Param);
            12: revVreg := Parser.DblValue;
            13: revBandwidth := Parser.DblValue;
            14: revR := Parser.DblValue;
            15: revX := Parser.DblValue;
            16: TapDelay := Parser.DblValue;
            17: DebugTrace   := InterpretYesNo(Param);
            18: TapLimitPerChange := max(0, Parser.IntValue);
            19: FInversetime := InterpretYesNo(Param);
            20: TapWinding   := Parser.intValue;
            21: Begin
                  Vlimit      := Parser.DblValue;
                  If VLimit > 0.0 then  VLimitActive := TRUE else VLimitActive := FALSE;
                End;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveRegControlObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer of
            2: Begin
                  Tapwinding := ElementTerminal;  // Resets if property re-assigned
                  PropertyValue[20]:= Param ;
                End;
            17: IF DebugTrace THEN
                 Begin
                   AssignFile(TraceFile,  DSSDataDirectory +'REG_'+Name+'.CSV' );
                   ReWrite(TraceFile);
                   Writeln(TraceFile, 'Hour, Sec, ControlIteration, Iterations, LoadMultiplier, Present Tap, Pending Change, Actual Change, Increment, Min Tap, Max Tap');
                   CloseFile(Tracefile);
                 End;

         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
   End;  {With}

End;



{--------------------------------------------------------------------------}
FUNCTION TRegControl.MakeLike(const RegControlName:String):Integer;
VAR
   OtherRegControl:TRegControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this RegControl name in the present collection}
   OtherRegControl := Find(RegControlName);
   IF OtherRegControl<>Nil THEN
   WITH ActiveRegControlObj Do Begin

        Nphases := OtherRegControl.Fnphases;
        NConds  := OtherRegControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherRegControl.ElementName;
        ControlledElement := OtherRegControl.ControlledElement;  // Pointer to target circuit element
        ElementTerminal   := OtherRegControl.ElementTerminal;
        Vreg         := OtherRegControl.Vreg;
        Bandwidth    := OtherRegControl.Bandwidth;
        PTRatio      := OtherRegControl.PTRatio;
        CTRating     := OtherRegControl.CTRating;
        R            := OtherRegControl.R;
        X            := OtherRegControl.X;
        RegulatedBus := OtherRegControl.RegulatedBus;
        TimeDelay    := OtherRegControl.TimeDelay;
        IsReversible := OtherRegControl.IsReversible;
        revVreg      := OtherRegControl.revVreg;
        revBandwidth := OtherRegControl.revBandwidth;
        revR         := OtherRegControl.revR;
        revX         := OtherRegControl.revX;
        TapDelay     := OtherRegControl.TapDelay;
        TapLimitPerChange := OtherRegControl.TapLimitPerChange;
        TapWinding   := OtherRegControl.TapWinding;
        FInversetime := OtherRegControl.FInversetime;
    //    DebugTrace     := OtherRegControl.DebugTrace;  Always default to NO

        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherRegControl.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in RegControl MakeLike: "' + RegControlName + '" Not Found.',121);

End;




{==========================================================================}
{                    TRegControlObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TRegControlObj.Create(ParClass:TDSSClass; const RegControlName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(RegControlName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class


    Vreg         :=  120.0;
    Bandwidth    :=  3.0;
    PTRatio      :=  60.0;
    CTRating     := 300.0;
    R            :=  0.0;
    X            :=  0.0;
    TimeDelay    :=  15.0;
    revVreg      :=  120.0;
    revBandwidth :=  3.0;
    revR         :=  0.0;
    revX         :=  0.0;

    IsReversible := FALSE;
    LDCActive    := FALSE;
    TapDelay := 2.0;
    TapLimitPerChange := 16;

    DebugTrace := FALSE;
    Armed      := FALSE;

     ElementName := '';
     ControlledElement := nil;
     ElementTerminal := 1;
     TapWinding := ElementTerminal;

     VBuffer := Nil;
     CBuffer := Nil;

     DSSObjType := ParClass.DSSClassType; //REg_CONTROL;

     InitPropertyValues(0);
     FInversetime := FALSE;
     RegulatedBus := '';
     Vlimit := 0.0;
   //  RecalcElementData;

End;

destructor TRegControlObj.Destroy;
Begin
     ElementName := '';
     if Assigned(VBuffer) then ReallocMem (VBuffer, 0);
     if Assigned(CBuffer) then ReallocMem (CBuffer, 0);
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.RecalcElementData;

VAR
   DevIndex :Integer;

Begin
         IF (R<>0.0) or (X<>0.0) Then LDCActive := TRUE else LDCActive := FALSE;
         IF Length(RegulatedBus)=0 Then UsingRegulatedBus := FALSE Else  UsingRegulatedBus := TRUE;

         Devindex := GetCktElementIndex(ElementName); // Global FUNCTION
         IF   DevIndex>0  THEN
          Begin  // RegControled element must already exist
             ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);
             Nphases := ControlledElement.NPhases;
             Nconds := FNphases;
             IF  Comparetext(ControlledElement.DSSClassName, 'transformer') = 0  THEN
                Begin
                   IF ElementTerminal>ControlledElement.Nterms  THEN Begin
                       DoErrorMsg('RegControl: "' + Name + '"', 'Winding no. "' +'" does not exist.',
                          'Respecify Monitored Winding no.', 122);
                    End
                   ELSE Begin
                     // Sets name of i-th terminal's connected bus in RegControl's buslist
                     // This value will be used to set the NodeRef array (see Sample function)
                       IF UsingRegulatedBus
                                            Then Setbus(1, RegulatedBus)   // hopefully this will actually exist
                                            Else Setbus(1, ControlledElement.GetBus(ElementTerminal));
                       ReAllocMem(VBuffer, SizeOF(Vbuffer^[1]) * ControlledElement.NPhases );  // buffer to hold regulator voltages
                       ReAllocMem(CBuffer, SizeOF(CBuffer^[1]) * ControlledElement.Yorder );
                    End;
                End
             ELSE Begin
                  ControlledElement := nil;   // we get here if element not found
                  DoErrorMsg('RegControl: "' + Self.Name + '"', 'Controlled Regulator Element "'+ ElementName + '" Is not a transformer.',
                                  ' Element must be defined previously.', 123);
               End;

         End
         ELSE
           Begin
            ControlledElement := nil;   // element not found
            DoErrorMsg('RegControl: "' + Self.Name + '"', 'Transformer Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 124);
           End;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.CalcYPrim;
Begin
  // leave YPrim as nil and it will be ignored ... zero current source
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;






{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TRegControlObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := cZero;
End;

{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);

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
      Writeln(F,'! Bus =', GetBus(1));
      Writeln(F);
    End;

End;

{--------------------------------------------------------------------------}
FUNCTION TRegControlObj.AtLeastOneTap(Const ProposedChange:Double; Increment:Double):Double;

// Called in STATIC mode
// Changes 70% of the way but at least one tap, subject to maximum allowable tap change
VAR
   NumTaps  :Integer;

Begin

     NumTaps := Trunc(0.7 * Abs(ProposedChange)/Increment);

     IF NumTaps = 0  THEN  NumTaps := 1;

     If NumTaps > TapLimitPerChange Then NumTaps := TapLimitPerChange;

     IF ProposedChange > 0.0    // check sign on change
     THEN  Result := NumTaps * Increment
     ELSE  Result := -NumTaps * Increment;

End;


{--------------------------------------------------------------------------}
FUNCTION OneInDirectionOf(VAR ProposedChange:Double; Increment:Double):Double;

// Computes the amount of one tap change in the direction of the pending tapchange
// Automatically decrements the proposed change by that amound

Begin
    LastChange := 0;
    IF ProposedChange > 0.0
    THEN Begin
         Result := Increment;
         LastChange := 1;
         ProposedChange := ProposedChange - Increment;
    End
    ELSE Begin
         Result := -Increment;
         LastChange := -1;
         ProposedChange := ProposedChange + Increment;
    End;

    IF   Abs(ProposedChange) < 0.9*Increment
    Then ProposedChange := 0.0;

End;

{--------------------------------------------------------------------------}
PROCEDURE TRegControlObj.DoPendingAction;

// 2-23-00 Modified to change one tap at a time

begin
    IF   PendingTapChange = 0.0    {Check to make sure control has not reset}
    THEN Armed := FALSE

    ELSE WITH   TTransfObj(ControlledElement) Do
    Begin

         // Transformer PresentTap property automatically limits tap
         WITH ActiveCircuit, ActiveCircuit.Solution Do
         Begin
             CASE ControlMode of
               STATIC:
                  Begin
                      If (DebugTrace) Then WriteTraceRecord(AtLeastOneTap(PendingTapChange, TapIncrement[TapWinding]));
                      PresentTap[TapWinding] := PresentTap[TapWinding] + AtLeastOneTap(PendingTapChange, TapIncrement[TapWinding]);
                      PendingTapChange := 0.0;  // Reset to no change.  Program will determine if another needed.
                      Armed := FALSE;
                  End;
               EVENTDRIVEN:
                  Begin
                      If (DebugTrace) Then WriteTraceRecord(OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]));
                      PresentTap[TapWinding] := PresentTap[TapWinding] + OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]);
                      IF   PendingTapChange <> 0.0 THEN ControlQueue.Push(intHour, Dynavars.t + TapDelay, 0,Self)
                      ELSE Armed := FALSE;
                  End;
               TIMEDRIVEN:
                  Begin
                      If (DebugTrace) Then WriteTraceRecord(OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]));
                      PresentTap[TapWinding] := PresentTap[TapWinding] + OneInDirectionOf(FPendingTapChange, TapIncrement[TapWinding]);
                      AppendtoEventLog('Regulator.' + ControlledElement.Name, Format(' Changed %d tap to %-.4g.',[Lastchange,PresentTap[TapWinding]]));
                      IF   PendingTapChange <> 0.0 THEN ControlQueue.Push(intHour, DynaVars.t + TapDelay, 0,Self)
                      ELSE Armed := FALSE;
                  End;
            End;
         End;



    End;
end;

PROCEDURE TRegControlObj.Sample;

VAR

   BoostNeeded,
   Increment,
   Vactual,
   Vboost    :Double;
   VlocalBus :Double;
   Vterm,
   VLDC,
   ILDC      :Complex;
   TapChangeIsNeeded :Boolean;
   i,ii      :Integer;
   ControlledTransformer:TTransfObj;
   TransformerConnection:Integer;

begin
       ControlledTransformer := TTransfObj(ControlledElement);

           If UsingRegulatedBus Then Begin
              TransformerConnection := ControlledTransformer.Winding^[ElementTerminal].Connection;
              ComputeVTerminal;   // Computes the voltage at the bus being regulated
              FOR i := 1 to Fnphases Do Begin
                   CASE TransformerConnection OF
                      0:Begin      // Wye
                           VBuffer^[i] := Vterminal^[i];
                        End;
                      1:Begin   // Delta
                          ii := ControlledTransformer.RotatePhases(i);      // Get next phase in sequence using Transformer Obj rotate
                          VBuffer^[i] := CSub(Vterminal^[i], Vterminal^[ii]);
                        End
                   End;
              End;
           End
           Else ControlledTransformer.GetWindingVoltages(ElementTerminal, VBuffer);

           // Changed to first phase only  12-17-01 (way most LTCs work)

           Vterm := CDivReal(VBuffer^[1], PTRatio );

           // Check Vlimit
           If VlimitActive then Begin
             If UsingRegulatedBus then ControlledTransformer.GetWindingVoltages(ElementTerminal, VBuffer);
             Vlocalbus := Cabs(CDivReal(VBuffer^[1], PTRatio ));
           End
           Else Vlocalbus := 0.0; // to get rid of warning message;

           // Check for LDC
           IF NOT UsingRegulatedBus and LDCActive Then
             Begin
                 ControlledElement.GetCurrents(Cbuffer);
                 ILDC  := CDivReal(CBuffer^[ControlledElement.Nconds*(ElementTerminal-1)+1], CTRating);
                 VLDC  := Cmul(Cmplx(R, X), ILDC);
                 Vterm := Cadd(Vterm, VLDC);   // Direction on ILDC is INTO terminal, so this is equivalent to Vterm - (R+jX)*ILDC
             End;

           Vactual := Cabs(Vterm);

     WITH   ControlledTransformer Do
     Begin
         // Check for out of band voltage
         IF (Abs(Vreg - Vactual) > Bandwidth / 2.0) Then TapChangeIsNeeded := TRUE
                                                    Else TapChangeIsNeeded := FALSE;

         If Vlimitactive then If (Vlocalbus > Vlimit) Then TapChangeIsNeeded := TRUE;

         If TapChangeIsNeeded then
           Begin
              // Compute tapchange
              Vboost := (Vreg - Vactual);
              If Vlimitactive then If (Vlocalbus > Vlimit) then Vboost := (Vlimit - Vlocalbus);
              BoostNeeded      := Vboost * PTRatio / BaseVoltage[ElementTerminal];  // per unit Winding boost needed
              Increment        := TapIncrement[TapWinding];
              PendingTapChange := Round(BoostNeeded / Increment) * Increment;  // Make sure it is an even increment

              {If Tap is another winding, it has to move the other way to accomplish the change}
              If TapWinding <> ElementTerminal Then PendingTapChange := -PendingTapChange;

               // Send Initial Tap Change message to control queue
              // Add Delay time to solution control queue
              IF (PendingTapChange <> 0.0) and Not Armed THEN
                Begin
                 // Now see if any tap change is possible in desired direction  Else ignore
                 IF PendingTapChange > 0.0 THEN
                   Begin
                     IF   PresentTap[TapWinding] < MaxTap[TapWinding]  THEN
                     WITH ActiveCircuit Do Begin
                           ControlQueue.Push(Solution.intHour, Solution.DynaVars.t + ComputeTimeDelay(Vactual), 0, Self);
                           Armed := TRUE;  // Armed to change taps
                     End;
                   End
                 ELSE
                   Begin
                     IF   PresentTap[TapWinding] > MinTap[TapWinding]  THEN
                     WITH ActiveCircuit Do Begin
                           ControlQueue.Push(Solution.intHour, Solution.DynaVars.t + ComputeTimeDelay(Vactual),0, Self);
                           Armed := TRUE;  // Armed to change taps
                     End;
                   End;

               End;

           END
         ELSE PendingTapChange := 0.0;
     End;
end;

FUNCTION TRegControlObj.Get_Transformer: TTransfObj;
begin

     Result := TTransfObj(ControlledElement);

end;

FUNCTION TRegControlObj.Get_Winding: Integer;
begin
     Result := TapWinding;
end;

Procedure TRegControlObj.WriteTraceRecord(TapChangeMade:Double);
VAR
   Separator :String;

Begin

      Try
      If (Not InshowResults) Then
          Begin
               Separator := ', ';
               Append(TraceFile);
               WITH TTransfObj(ControlledElement) Do
               Writeln(TraceFile,
                        ActiveCircuit.Solution.intHour:0, Separator,
                        ActiveCircuit.Solution.DynaVars.t:0:3, Separator,
                        ActiveCircuit.Solution.ControlIteration:0, Separator,
                        ActiveCircuit.Solution.Iteration:0, Separator,
                        ActiveCircuit.LoadMultiplier:6:2, Separator,
                        PresentTap[ElementTerminal]:8:5, Separator,
                        PendingTapChange:8:5, Separator,
                        TapChangeMade:8:5, Separator,
                        TapIncrement[ElementTerminal]:8:5, Separator,
                        MinTap[ElementTerminal]:8:5, Separator,
                        MaxTap[ElementTerminal]:8:5 );

               CloseFile(TraceFile);
          End;
      Except
            On E:Exception Do Begin End;

      End;
End;

Procedure TRegControlObj.Reset;
begin
      PendingTapChange := 0.0;

end;

procedure TRegcontrolObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := ''; //'element';
     PropertyValue[2] := '1'; //'terminal';
     PropertyValue[3] := '120';
     PropertyValue[4] := '3';
     PropertyValue[5] := '60';
     PropertyValue[6] := '300';
     PropertyValue[7] := '0';
     PropertyValue[8] := '0';
     PropertyValue[9] := '';
     PropertyValue[10] := '15';
     PropertyValue[11] := 'no';
     PropertyValue[12] := '120';
     PropertyValue[13] := '3';
     PropertyValue[14] := '0';
     PropertyValue[15] := '0';
     PropertyValue[16] := '2';
     PropertyValue[17] := 'no';
     PropertyValue[18] := '16';
     PropertyValue[19] := 'no';
     PropertyValue[20] := '1';
     PropertyValue[21] := '0.0';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TRegControlObj.set_PendingTapChange(const Value: Double);
begin
  FPendingTapChange := Value;
  dblTraceParameter := Value;
end;

procedure TRegControlObj.MakePosSequence;
begin
  // Use only if controlled element is in pos seq equivalent
  Enabled :=   ControlledElement.Enabled;
  inherited;

end;

function TRegControlObj.ComputeTimeDelay(Vavg:Double): Double;
begin

     If Finversetime Then
           Result := TimeDelay / Min(10.0, (2.0*Abs(Vreg-Vavg)/Bandwidth))
     Else
           Result :=  TimeDelay;
end;

INITIALIZATION


end.
