unit CapControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   2-14-00 Created

   3-1-00  Added Voltage override
   5/21/01  Fixed bug with number of phases
   5/30/01  Eliminated extra event queue reports
}

{
  A CapControl is a control element that is connected to a terminal of another
  circuit element and controls a capacitor.  The control is usually placed in the
  terminal of a line or transformer, although a voltage control device could be placed
  in the terminal of the capacitor it controls

  A CapControl is defined by a New command:

  New CapControl.Name=myname Element=devclass.name terminal=[ 1|2|...] Capacitor = name

  Capacitor to be controlled must already exist.
}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     Capacitor, utilities;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TCapControl = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const CapControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TCapControlObj = class(TControlElem)
     private
            ControlType :Integer;

            ON_Value,
            OFF_Value,
            PFON_Value,
            PFOFF_Value,
            CTRatio,
            PTRatio,
            ONDelay,
            OFFDelay,
            DeadTime,
            LastOpenTime   :Double;

            Voverride  :Boolean;
            Vmax,
            Vmin       :Double;

            CapacitorName :String;
            MonitoredElement :TDSSCktElement;
            ControlledCapacitor :TCapacitorObj;
            FPendingChange   :Integer;  // 0 = open 1 = close
            ShouldSwitch     :Boolean;  // True: action is pending
            Armed            :Boolean;  // Control is armed for switching unless reset
            PresentState,             // 0 = open 1 = close
            InitialState,
            ControlActionHandle    :Integer;
            CondOffset             :Integer; // Offset for monitored terminal

            cBuffer :pComplexArray;    // Complexarray buffer
            FUNCTION Get_Capacitor: TCapacitorObj;
            FUNCTION NormalizeToTOD(h:Integer; sec:Double) :Double;
            procedure Set_PendingChange(const Value: Integer);

     public

       constructor Create(ParClass:TDSSClass; const CapControlName:String);
       destructor Destroy; override;

       PROCEDURE RecalcElementData; Override;
       PROCEDURE CalcYPrim; Override;    // Always Zero for a CapControl

       PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state


       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       Property This_Capacitor:TCapacitorObj Read Get_Capacitor;  // Pointer to controlled Capacitor
       Property PendingChange:Integer Read FPendingChange Write Set_PendingChange;

       // for CIM export, which doesn't yet use the delays, CT, PT, and voltage override
       Property CapControlType:Integer Read ControlType;
       Property OnValue:Double Read ON_Value;
       Property OffValue:Double Read OFF_Value;
       Property PFOnValue:Double Read PFON_Value;
       Property PFOffValue:Double Read PFOFF_Value;
   end;


VAR
    ActiveCapControlObj:TCapControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSGlobals, Circuit,   Sysutils, uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 14;

    NONE = -1;
    OPEN = 0;
    CLOSE = 1;

    // TEMc - might move these to the interface
    CURRENTCONTROL = 1;
    VOLTAGECONTROL = 2;
    KVARCONTROL = 3;
    TIMECONTROL = 4;
    PFCONTROL = 5;


{--------------------------------------------------------------------------}
constructor TCapControl.Create;  // Creates superstructure for all CapControl objects
Begin
     Inherited Create;

     Class_name   := 'CapControl';
     DSSClassType := DSSClassType + CAP_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TCapControl.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TCapControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'element';
     PropertyName[2] := 'terminal';
     PropertyName[3] := 'capacitor';
     PropertyName[4] := 'type';
     PropertyName[5] := 'PTratio';
     PropertyName[6] := 'CTratio';
     PropertyName[7] := 'ONsetting';
     PropertyName[8] := 'OFFsetting';
     PropertyName[9] := 'Delay';
     PropertyName[10] := 'VoltOverride';
     PropertyName[11] := 'Vmax';
     PropertyName[12] := 'Vmin';
     PropertyName[13] := 'DelayOFF';
     PropertyName[14] := 'DeadTime';

     PropertyHelp[1] := 'Full object name of the circuit element, typically a line or transformer, '+
                        'to which the capacitor control''s PT and/or CT are connected.' +
                        'There is no default; must be specified.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the CapControl is connected. '+
                        '1 or 2, typically.  Default is 1.';
     PropertyHelp[3] := 'Name of Capacitor element which the CapControl controls. No Default; Must be specified.'+
                        'Do not specify the full object name; "Capacitor" is assumed for '  +
                        'the object class.  Example:'+CRLF+CRLF+
                        'Capacitor=cap1';
     PropertyHelp[4] := '{Current | voltage | kvar | PF | time } Control type.  Specify the ONsetting and OFFsetting ' +
                        'appropriately with the type of control. (See help for ONsetting)';
     PropertyHelp[5] := 'Ratio of the PT that converts the monitored voltage to the control voltage. '+
                        'Default is 60.  If the capacitor is Wye, the 1st phase line-to-neutral voltage is monitored.  Else, the line-to-line ' +
                        'voltage (1st - 2nd phase) is monitored.';
     PropertyHelp[6] := 'Ratio of the CT from line amps to control ampere setting for current and kvar control types. ';
     PropertyHelp[7] := 'Value at which the control arms to switch the capacitor ON (or ratchet up a step).  ' + CRLF+CRLF +
                        'Type of Control:'+CRLF+CRLF+
                        'Current: Line Amps / CTratio'+CRLF+
                        'Voltage: Line-Neutral (or Line-Line for delta) Volts / PTratio' +CRLF+
                        'kvar:    Total kvar, all phases (3-phase for pos seq model). This is directional. ' + CRLF +
                        'PF:      Power Factor, Total power in monitored terminal. Negative for Leading. ' + CRLF +
                        'Time:    Hrs from Midnight as a floating point number (decimal). 7:30am would be entered as 7.5.';
     PropertyHelp[8] := 'Value at which the control arms to switch the capacitor OFF. (See help for ONsetting)' +
                        'For Time control, is OK to have Off time the next day ( < On time)';
     PropertyHelp[9] := 'Time delay, in seconds, from when the control is armed before it sends out the switching ' +
                        'command to turn ON.  The control may reset before the action actually occurs. ' +
                        'This is used to determine which capacity control will act first. Default is 15.  You may specify any '+
                         'floating point number to achieve a model of whatever condition is necessary.';
     PropertyHelp[10] := '{Yes | No}  Default is No.  Switch to indicate whether VOLTAGE OVERRIDE is to be considered. ' +
                         'Vmax and Vmin must be set to reasonable values if this property is Yes.';
     PropertyHelp[11] := 'Maximum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is greater ' +
                         'than this voltage, the capacitor will switch OFF regardless of other control settings. ' +
                         'Default is 126 (goes with a PT ratio of 60 for 12.47 kV system).';
     PropertyHelp[12] := 'Minimum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is less ' +
                         'than this voltage, the capacitor will switch ON regardless of other control settings. '+
                         'Default is 115 (goes with a PT ratio of 60 for 12.47 kV system).';
     PropertyHelp[13] := 'Time delay, in seconds, for control to turn OFF when present state is ON. Default is 15.';
     PropertyHelp[14] := 'Dead time after capacitor is turned OFF before it can be turned back ON. Default is 300 sec.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TCapControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new CapControl and add it to CapControl class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TCapControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TCapControl.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActiveCapControlObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveCapControlObj;

  Result := 0;

  WITH ActiveCapControlObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 352);
            1: ElementName     := lowercase(param);
            2: ElementTerminal := Parser.IntValue;
            3: CapacitorName   := 'capacitor.'+ param;
            4: CASE lowercase(param)[1] of
                    'c': ControlType := CURRENTCONTROL;
                    'v': ControlType := VOLTAGECONTROL;
                    'k': ControlType := KVARCONTROL;
                    't': ControlType := TIMECONTROL;
                    'p': ControlType := PFCONTROL;
               End;
            5: PTRatio := Parser.DblValue;
            6: CTRatio := Parser.DblValue;
            7: ON_Value := Parser.DblValue;
            8: OFF_Value := Parser.DblValue;
            9: ONDelay := Parser.DblValue;
           10: Voverride := InterpretYesNo(param);
           11: Vmax      := Parser.DblValue;
           12: Vmin      := Parser.DblValue;
           13: OFFDelay  := Parser.DblValue;
           14: DeadTime  := Parser.DblValue;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveCapControlObj, ParamPointer - NumPropsthisClass)
         End;


         {PF Controller changes}
         If ControlType=PFCONTROL then
         Case ParamPointer of

            4: Begin
                  PFON_Value := 0.95;     // defaults
                  PFOFF_Value := 1.05;
               End;

            7: Begin
                 If (ON_Value >= -1.0) and (ON_Value <= 1.0) then Begin
                    If ON_Value < 0.0 then PFON_Value := 2.0 + ON_Value else PFON_Value := ON_Value;
                 End Else Begin
                    DoSimpleMsg('Invalid PF ON value for CapControl.'+ActiveCapControlObj.Name, 353);
                 End;
               End;
            8: Begin
                 If (OFF_Value >= -1.0) and (OFF_Value <= 1.0) then Begin
                    If OFF_Value < 0.0 then PFOFF_Value := 2.0 + OFF_Value else PFOFF_Value :=  OFF_Value;
                 End Else Begin
                    DoSimpleMsg('Invalid PF OFF value for CapControl.'+ActiveCapControlObj.Name, 353);
                 End;
               End;
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
  End;

End;

{--------------------------------------------------------------------------}
FUNCTION TCapControl.MakeLike(const CapControlName:String):Integer;
VAR
   OtherCapControl:TCapControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this CapControl name in the present collection}
   OtherCapControl := Find(CapControlName);
   IF OtherCapControl<>Nil THEN
   WITH ActiveCapControlObj Do Begin

        NPhases := OtherCapControl.Fnphases;
        NConds  := OtherCapControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherCapControl.ElementName;
        CapacitorName     := OtherCapControl.CapacitorName;
        ControlledElement := OtherCapControl.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherCapControl.MonitoredElement;  // Pointer to target circuit element

        ElementTerminal   := OtherCapControl.ElementTerminal;
        PTRatio           := OtherCapControl.PTRatio;
        CTRatio           := OtherCapControl.CTRatio;
        ControlType       := OtherCapControl.ControlType;
        PresentState      := OtherCapControl.PresentState;
        ShouldSwitch     := OtherCapControl.ShouldSwitch;
        CondOffset        := OtherCapControl.CondOffset;

        ON_Value          := OtherCapControl.ON_Value;
        OFF_Value         := OtherCapControl.OFF_Value;
        PFON_Value        := OtherCapControl.PFON_Value;
        PFOFF_Value       := OtherCapControl.PFOFF_Value;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherCapControl.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in CapControl MakeLike: "' + CapControlName + '" Not Found.', 360);

End;


{==========================================================================}
{                    TCapControlObj                                        }
{==========================================================================}


{--------------------------------------------------------------------------}
constructor TCapControlObj.Create(ParClass:TDSSClass; const CapControlName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(CapControlName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class


      PTRatio      := 60.0;
      CTRatio      := 60.0;
      ControlType  := 1;       // Current control
      ONDelay    := 15.0;
      OFFDelay  := 15.0;
      DeadTime  := 300.0;
      LastOpenTime := -DeadTime;

      ON_Value    := 300.0;
      OFF_Value   := 200.0;

      PFON_Value    := 0.95;
      PFOFF_Value   := 1.05;

      Voverride  := False;
      Vmax       := 126;
      Vmin       := 115;

     ElementName   := '';
     ControlledElement := nil;
     ElementTerminal := 1;
     CapacitorName := '';
     MonitoredElement := Nil;

     PresentState  := CLOSE;

     ShouldSwitch :=  FALSE;
     Armed        :=  FALSE;
     PendingChange :=  NONE;
     ControlActionHandle := 0;

     cBuffer := Nil; // Complex buffer

     DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

     InitPropertyValues(0);



   //  RecalcElementData;

End;

destructor TCapControlObj.Destroy;
Begin
     ElementName := '';
     CapacitorName := '';
     if Assigned(cBuffer) then ReallocMem (cBuffer, 0);
     Inherited Destroy;
End;


{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.RecalcElementData;

VAR
   DevIndex :Integer;

Begin

{Check for existence of capacitor}

// 5-21-01 RCD moved this section ahead of monitored element so Nphases gets defined first

         Devindex := GetCktElementIndex(CapacitorName); // Global function
         IF   DevIndex>0
         THEN Begin  // Both capacitor and monitored element must already exist
             ControlledElement := ActiveCircuit.CktElements.Get(DevIndex);
             ControlledCapacitor := This_Capacitor;
             Nphases := ControlledElement.NPhases;  // Force number of phases to be same   Added 5/21/01  RCD
             Nconds := FNphases;
             ControlledElement.ActiveTerminalIdx := 1;  // Make the 1 st terminal active
             IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
             THEN PresentState := CLOSE
             ELSE PresentState := OPEN;
         End
         ELSE Begin
            ControlledElement := nil;   // element not found
            DoErrorMsg('CapControl: "' + Self.Name + '"', 'Capacitor Element "'+ CapacitorName + '" Not Found.',
                            ' Element must be defined previously.', 361);
         End;

         InitialState := PresentState;

{Check for existence of monitored element}

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0  THEN Begin
             MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
             IF ElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('CapControl: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 362);
             End
             ELSE Begin
               // Sets name of i-th terminal's connected bus in CapControl's buslist
                 Setbus(1, MonitoredElement.GetBus(ElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
                 ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder );
                 CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
             End;
         End
         ELSE DoSimpleMsg('Monitored Element in CapControl.'+Name+ ' does not exist:"'+ElementName+'"', 363);


End;

{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;



{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TCapControlObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);

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


{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.DoPendingAction;


begin

         ControlledElement.ActiveTerminalIdx := 1;  // Set active terminal of capacitor to terminal 1


         CASE PendingChange of
            OPEN: CASE ControlledCapacitor.NumSteps of
                    1: Begin
                        IF PresentState=CLOSE Then Begin
                          ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                          AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Opened**');
                          PresentState := OPEN;
                          With ActiveCircuit.Solution Do LastOpenTime := DynaVars.t + 3600.0*intHour;
                        End;
                       End;
                    ELSE
                        If PresentState=CLOSE Then Begin      // Do this only if at least one step is closed
                           If NOT ControlledCapacitor.SubtractStep Then Begin
                              PresentState := OPEN;
                              ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                              AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Opened**');
                           End
                           ELSE AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Step Down**');
                        End;
                    END;
            CLOSE: BEGIN
                      If PresentState=OPEN Then Begin
                           ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                           AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Closed**');
                           PresentState := CLOSE;
                           ControlledCapacitor.AddStep;
                       End
                       ELSE Begin
                           IF ControlledCapacitor.AddStep Then
                             AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Step Up**');
                       END;
                   END;
         ELSE
            {Do Nothing for NONE if the control has reset}
         END;

         ShouldSwitch := FALSE;
         Armed        := FALSE;   // reset control
end;

{--------------------------------------------------------------------------}
PROCEDURE TCapControlObj.Sample;

VAR
   i       :Integer;
   Cmax,
   cmag,
   Vavg,
   t_value,
   Q       :Double;
   S       :Complex;
   PF      :Double;
   Sabs    :Double;

   Function PF1to2(Const Spower:Complex):Double;   // return PF in range of 1 to 2
   Begin
       Sabs := Cabs(Spower);
       If Sabs <> 0.0 then Result := abs(Spower.re) / Sabs else Result := 1.0;  // default to unity
       If Spower.im < 0.0 Then Result := 2.0 - Result;
   End;

begin

     ControlledElement.ActiveTerminalIdx := 1;
     IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
     THEN PresentState := CLOSE
     ELSE PresentState := OPEN;


     WITH   MonitoredElement Do
     Begin
         ShouldSwitch := FALSE;

         // First Check voltage override
         IF Voverride THEN
          IF ControlType <> 2 THEN Begin  // Don't bother for voltage control

              MonitoredElement.GetTermVoltages (ElementTerminal, cBuffer);
              //Vavg := 0.0;
              //FOR i := 1 to MonitoredElement.NPhases Do Vavg := Vavg + Cabs(cBuffer^[i]);
              //Vavg := Vavg/MonitoredElement.NPhases/PTRatio;
              {Just use phase 1 because that's what most controls do.}
              // Use L-L aB if capacitor is delta connected!!
              With TCapacitorObj(ControlledElement) Do
              Case Connection of
                1: Vavg := Cabs(Csub(cBuffer^[1], cBuffer^[2]))/PTRatio;   // Delta
              Else
                Vavg := Cabs(cBuffer^[1])/PTRatio;     // Wye - Default
              End;

             CASE PresentState of
               OPEN:
                    IF   Vavg < VMin
                    THEN Begin
                        PendingChange := CLOSE;
                        ShouldSwitch := TRUE;
                    End;
               CLOSE:
                    IF   Vavg > Vmax
                    THEN Begin
                        PendingChange := OPEN;
                        ShouldSwitch := TRUE;
                    End;
             End;
         End;


         IF Not ShouldSwitch THEN   // Else skip other control evaluations
         CASE ControlType of

              CURRENTCONTROL: {Current}
                 Begin

                     // Check largest Current of all phases of monitored element
                     MonitoredElement.GetCurrents(cBuffer);
                     Cmax := 0.0;

                     FOR i := (1 + CondOffset) to (Fnphases + CondOffset) Do
                     Begin
                         cmag := Cabs( cBuffer^[i] );
                         If cmag > Cmax THEN Cmax := cmag;
                     End;
                     Cmax := Cmax / CTRatio;

                     CASE PresentState of
                          OPEN:   IF Cmax > ON_Value
                                  THEN  Begin
                                        PendingChange := CLOSE;
                                        ShouldSwitch := TRUE;
                                  End
                                  ELSE // Reset
                                        PendingChange := NONE;
                          CLOSE:  IF Cmax < OFF_Value
                                  THEN Begin
                                         PendingChange := OPEN;
                                         ShouldSwitch := TRUE;
                                  End
                                  ELSE  If ControlledCapacitor.AvailableSteps >0 Then Begin
                                    IF Cmax > ON_Value THEN  Begin
                                            PendingChange := CLOSE;
                                            ShouldSwitch := TRUE;
                                    End;
                                  End
                                  ELSE // Reset
                                        PendingChange := NONE;
                     End;

                 End;

              VOLTAGECONTROL: {Voltage}
                 Begin
                     MonitoredElement.GetTermVoltages(ElementTerminal, cBuffer);

                    // Average the voltage on all phases
                    //Vavg := 0.0;
                    // FOR i := 1 to MonitoredElement.NPhases Do
                    //Begin
                    //    Vavg := Vavg + Cabs(cBuffer^[i]);
                    // End;
                    // Vavg := Vavg/MonitoredElement.NPhases/PTRatio;
                    // Just use Phase 1 because that is what most controls do
                    
                    With TCapacitorObj(ControlledElement) Do
                    Case Connection of
                      1: Vavg := Cabs(Csub(cBuffer^[1], cBuffer^[2]))/PTRatio;   // Delta
                    Else
                      Vavg := Cabs(cBuffer^[1])/PTRatio;     // Wye - Default
                    End;

                     CASE PresentState of
                          OPEN:   IF Vavg < ON_Value
                                  THEN  Begin
                                        PendingChange := CLOSE;
                                        ShouldSwitch := TRUE;
                                  End
                                  ELSE // Reset
                                        PendingChange := NONE;
                          CLOSE:  IF Vavg > OFF_Value
                                  THEN Begin
                                         PendingChange := OPEN;
                                         ShouldSwitch := TRUE;
                                  End
                                  ELSE  If ControlledCapacitor.AvailableSteps >0 Then Begin
                                   IF Vavg < ON_Value THEN  Begin
                                            PendingChange := CLOSE;
                                            ShouldSwitch := TRUE;
                                    End;
                                  End
                                  ELSE // Reset
                                        PendingChange := NONE;
                     End;

                 End;

              KVARCONTROL: {kvar}
                 Begin
                      //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
                      S := MonitoredElement.Power[ElementTerminal];
                      Q := S.im * 0.001;  // kvar

                      CASE PresentState of
                          OPEN:   IF Q > ON_Value
                                  THEN  Begin
                                        PendingChange := CLOSE;
                                        ShouldSwitch := TRUE;
                                  End
                                  ELSE // Reset
                                        PendingChange := NONE;
                          CLOSE:  IF Q < OFF_Value
                                  THEN Begin
                                         PendingChange := OPEN;
                                         ShouldSwitch := TRUE;
                                  End
                                  ELSE IF ControlledCapacitor.AvailableSteps > 0 Then Begin
                                      IF Q > ON_Value Then Begin
                                        PendingChange := CLOSE;  // We can go some more
                                        ShouldSwitch := TRUE;
                                      End;
                                  End
                                  ELSE // Reset
                                        PendingChange := NONE;
                      End;

                 End;

              TIMECONTROL: {time}
                 Begin
                    WITH ActiveCircuit.Solution Do t_Value := NormalizeToTOD(intHour, DynaVars.t);
                    // 1/28/09 Code modified to accommodate OFF_Value < ON_Value
                    CASE PresentState OF
                          OPEN:   IF OFF_Value > ON_Value Then Begin
                                    IF (t_Value >= ON_Value) and (t_Value < OFF_Value)
                                    THEN  Begin
                                          PendingChange := CLOSE;
                                          ShouldSwitch := TRUE;
                                    End
                                    ELSE // Reset
                                          PendingChange := NONE;
                                  End ELSE Begin    // OFF time is next day
                                    IF (t_Value >= ON_Value) and (t_Value < 24)
                                    THEN  Begin
                                          PendingChange := CLOSE;
                                          ShouldSwitch := TRUE;
                                    End
                                    ELSE // Reset
                                          PendingChange := NONE;
                                  End;

                          CLOSE:  IF OFF_Value > ON_Value Then Begin
                                      IF t_Value >= OFF_Value
                                      THEN Begin
                                             PendingChange := OPEN;
                                             ShouldSwitch := TRUE;
                                      End
                                      ELSE IF ControlledCapacitor.AvailableSteps > 0 Then Begin
                                          IF (t_Value >= ON_Value) and (T_Value < OFF_Value) Then Begin
                                             PendingChange := CLOSE;  // We can go some more
                                             ShouldSwitch := TRUE;
                                          End;
                                      End
                                      ELSE // Reset
                                            PendingChange := NONE;
                                  End ELSE Begin  // OFF time is next day
                                      IF (t_Value >= OFF_Value) and (t_Value < ON_Value)
                                      THEN Begin
                                             PendingChange := OPEN;
                                             ShouldSwitch := TRUE;
                                      End
                                      ELSE IF ControlledCapacitor.AvailableSteps > 0 Then Begin
                                          IF (t_Value >= ON_Value) and (T_Value < 24) Then Begin
                                             PendingChange := CLOSE;  // We can go some more
                                             ShouldSwitch := TRUE;
                                          End;
                                      End
                                      ELSE // Reset
                                            PendingChange := NONE;
                                  End;
                     End;
                 End;

                 PFCONTROL: {PF}
                 Begin
                      //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
                      S := MonitoredElement.Power[ElementTerminal];
                      PF := PF1to2(S);

                      {PF is in range of 0 .. 2;  Leading is 1..2}
                      {When turning on make sure there is at least half the kvar of the bank}

                      CASE PresentState of
                          OPEN:   IF (PF < PFON_Value) and (S.im * 0.001 > ControlledCapacitor.Totalkvar * 0.5) // make sure we don't go too far leading
                                  THEN  Begin
                                        PendingChange := CLOSE;
                                        ShouldSwitch := TRUE;
                                  End
                                  ELSE // Reset
                                        PendingChange := NONE;
                          CLOSE:  IF PF > PFOFF_Value
                                  THEN Begin
                                         PendingChange := OPEN;
                                         ShouldSwitch := TRUE;
                                  End
                                  ELSE IF ControlledCapacitor.AvailableSteps > 0 Then Begin
                                      IF (PF < PFON_Value) and (S.im * 0.001 > ControlledCapacitor.Totalkvar/ControlledCapacitor.Numsteps * 0.5) Then Begin
                                        PendingChange := CLOSE;  // We can go some more
                                        ShouldSwitch := TRUE;
                                      End;
                                  End
                                  ELSE // Reset
                                        PendingChange := NONE;
                      End;

                 End;

         End;
     End;
     WITH ActiveCircuit Do
      Begin
         IF   ShouldSwitch and Not Armed THEN
           Begin
            If PendingChange = CLOSE Then Begin
               If (Solution.DynaVars.t + Solution.intHour*3600.0 - LastOpenTime)<DeadTime Then // delay the close operation
                    {2-6-09 Added ONDelay to Deadtime so that all caps do not close back in at same time}
                    TimeDelay := Max(ONDelay , (Deadtime + ONDelay) - (Solution.DynaVars.t + Solution.intHour*3600.0-LastOpenTime))
               Else TimeDelay := ONDelay;
            End Else TimeDelay := OFFDelay;
            ControlActionHandle := ControlQueue.Push(Solution.intHour, Solution.DynaVars.t + TimeDelay, PendingChange, Self);
            Armed := TRUE;
            AppendtoEventLog('Capacitor.' + ControlledElement.Name, Format('**Armed**, Delay= %.5g sec', [TimeDelay]));
           End;

        IF Armed and (PendingChange = NONE) Then
          Begin
              ControlQueue.Delete(ControlActionHandle);
              Armed := FALSE;
              AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Reset**');
          End;
      End;  {With}
end;

FUNCTION TCapControlObj.Get_Capacitor: TCapacitorObj;
begin

     Result := ControlledElement as TCapacitorObj;

end;

FUNCTION TCapControlObj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day if Hour > 24
// time should be 0 to 24.
VAR
    HourOfDay :Integer;

Begin

   IF    h > 23
   THEN  HourOfDay := (h - (h div 24)*24)
   ELSE  HourOfDay := h;

   Result := HourOfDay + sec/3600.0;

   If   Result > 24.0
   THEN Result := Result - 24.0;   // Wrap around

End;

Procedure TCapControlObj.Reset;
begin
      PendingChange := NONE;
      ControlledElement.ActiveTerminalIdx := 1;
      CASE InitialState of
            OPEN:   ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
            CLOSE:  ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
      END;
      ShouldSwitch := FALSE;
      LastOpenTime := -DeadTime;
      PresentState := InitialState;
end;

procedure TCapControlObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '';   //'element';
     PropertyValue[2]  := '1';   //'terminal';
     PropertyValue[3]  := '';
     PropertyValue[4]  := 'current';
     PropertyValue[5]  := '60';
     PropertyValue[6]  := '60';
     PropertyValue[7]  := '300';
     PropertyValue[8]  := '200';
     PropertyValue[9]  := '15';
     PropertyValue[10] := 'NO';
     PropertyValue[11] := '126';
     PropertyValue[12] := '115';
     PropertyValue[13] := '15';
     PropertyValue[14] := '300';


  inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TCapControlObj.Set_PendingChange(const Value: Integer);
begin
  FPendingChange := Value;
  DblTraceParameter := Value;
end;

INITIALIZATION


end.
