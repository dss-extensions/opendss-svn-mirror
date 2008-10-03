unit ExecOptions;

interface

Uses Command;

CONST
        NumExecOptions = 68;

VAR
         ExecOption,
         OptionHelp :Array[1..NumExecOptions] of String;
         OptionList  :TCommandList;

FUNCTION DoGetCmd:Integer;
FUNCTION DoSetCmd(SolveOption:Integer):Integer;
FUNCTION DoSetCmd_NoCircuit:Boolean;  // Set Commands that do not require a circuit


implementation

Uses DSSGlobals, ParserDel, Math, Executive, ExecHelper,
     LoadShape, Utilities, sysutils, solution, energymeter;




PROCEDURE DefineOptions;

Begin

     ExecOption[1]  := 'type';
     ExecOption[2]  := 'element';
     ExecOption[3]  := 'hour';
     ExecOption[4]  := 'sec';
     ExecOption[5]  := 'year';
     ExecOption[6]  := 'frequency';
     ExecOption[7]  := 'stepsize';
     ExecOption[8]  := 'mode';
     ExecOption[9]  := 'random';
     ExecOption[10] := 'number';
     ExecOption[11] := 'time';
     ExecOption[12] := 'class';
     ExecOption[13] := 'object';
     ExecOption[14] := 'circuit';
     ExecOption[15] := 'editor';
     ExecOption[16] := 'tolerance';
     ExecOption[17] := 'maxiter';
     ExecOption[18] := 'h';
     ExecOption[19] := 'loadmodel';
     ExecOption[20] := 'loadmult';
     ExecOption[21] := 'normvminpu';
     ExecOption[22] := 'normvmaxpu';
     ExecOption[23] := 'emergvminpu';
     ExecOption[24] := 'emergvmaxpu';
     ExecOption[25] := '%mean';
     ExecOption[26] := '%stddev';
     ExecOption[27] := 'LDCurve';  // Load Duration Curve
     ExecOption[28] := '%growth';  // default growth rate
     ExecOption[29] := 'genkw';
     ExecOption[30] := 'genpf';
     ExecOption[31] := 'capkVAR';
     ExecOption[32] := 'addtype';
     ExecOption[33] := 'allowduplicates';
     ExecOption[34] := 'zonelock';
     ExecOption[35] := 'ueweight';
     ExecOption[36] := 'lossweight';
     ExecOption[37] := 'ueregs';
     ExecOption[38] := 'lossregs';
     ExecOption[39] := 'voltagebases';  //  changes the default voltage base rules
     ExecOption[40] := 'algorithm';  //  changes the default voltage base rules
     ExecOption[41] := 'trapezoidal';
     ExecOption[42] := 'autobuslist';  // array of bus names to include in auto add solutions
     ExecOption[43] := 'controlmode';
     ExecOption[44] := 'tracecontrol';
     ExecOption[45] := 'genmult';
     ExecOption[46] := 'defaultdaily';
     ExecOption[47] := 'defaultyearly';
     ExecOption[48] := 'allocationfactors';
     ExecOption[49] := 'cktmodel';
     ExecOption[50] := 'pricesignal';
     ExecOption[51] := 'pricecurve';
     ExecOption[52] := 'terminal';
     ExecOption[53] := 'basefrequency';
     ExecOption[54] := 'harmonics';
     ExecOption[55] := 'maxcontroliter';
     ExecOption[56] := 'bus';
     ExecOption[57] := 'datapath';
     ExecOption[58] := 'KeepList';
     ExecOption[59] := 'ReduceOption';
     ExecOption[60] := 'DemandInterval';
     ExecOption[61] := '%Normal';
     ExecOption[62] := 'DIVerbose';
     ExecOption[63] := 'casename';
     ExecOption[64] := 'markercode';
     ExecOption[65] := 'nodewidth';
     ExecOption[66] := 'log';
     ExecOption[67] := 'recorder';
     ExecOption[68] := 'overloadreport';


     OptionHelp[1]  := 'Sets the active DSS class type.  Same as Class=...';
     OptionHelp[2]  := 'Sets the active DSS element by name. You can use '+
                        'the complete object spec (class.name) or just the '+
                        'name.  if full name is specifed, class becomes the active '+
                        'class, also.';
     OptionHelp[3]  := 'Sets the hour used for the start time of the solution.';
     OptionHelp[4]  := 'Sets the seconds from the hour for the start time of the solution.';
     OptionHelp[5]  := 'Sets the Year (integer number) to be used for the solution. '+
                        'for certain solution types, this determines the growth multiplier.';
     OptionHelp[6]  := 'Sets the frequency for the solution of the active circuit.';
     OptionHelp[7]  := 'Sets the time step in sec for the active circuit.  Nominally for dynamics solution.';
     OptionHelp[8]  := 'Set the solution Mode: One of'+
                    CRLF+'  Snapshot,'+
                    CRLF+'  Daily,'+
                    CRLF+'  DIrect,'+
                    CRLF+'  DUtycycle,'+
                    CRLF+'  DYnamic,'+
                    CRLF+'  Harmonic,'+
                    CRLF+'  M1 (Monte Carlo 1),'+
                    CRLF+'  M2 (Monte Carlo 2),'+
                    CRLF+'  M3 (Monte Carlo 3),'+
                    CRLF+'  Faultstudy,'+
                    CRLF+'  Yearly (follow Yearly curve),'+
                    CRLF+'  MF (monte carlo fault study)'+
                    CRLF+'  Peakday,'+
                    CRLF+'  LD1 (load-duration 1)'+
                    CRLF+'  LD2 (load-duration 2)'+
                    CRLF+'  AutoAdd (see AddType)' +CRLF +CRLF+
                    'Side effect: setting the Mode propergy resets all monitors and energy meters. It also ' +
                    'resets the time step, etc. to defaults for each mode.  After the initial reset, the user ' +
                    'must explicitly reset the monitors and/or meters until another Set Mode= command.';
     OptionHelp[9]  := 'One of [Uniform | Gaussian | Lognormal | None ] for Monte Carlo Variables.';
     OptionHelp[10] := 'Number of solutions to perform for Monte Carlo or dutycycle solutions.';
     OptionHelp[11] := 'Specify the solution start time as an array:'+CRLF+
                        'time=(hour, secs)';
     OptionHelp[12] := 'Synonym for Type=. (See above)';
     OptionHelp[13] := 'Synonym for Element=. (See above)';
     OptionHelp[14] := 'Set the active circuit by name.';
     OptionHelp[15] := 'Set the command string required to start up the editor preferred by the user. Does not require a circuit defined.';
     OptionHelp[16] := 'Sets the solution tolerance.  Default is 0.0001.';
     OptionHelp[17] := 'Sets the maximum allowable iterations for power flow solutions. Default is 15.';
     OptionHelp[18] := 'Alternate name for time step size.';
     OptionHelp[19] := '{Powerflow | Admittance} depending on the type of solution you wish to perform. '+
                        'If admittance, a non-iterative, direct solution is done with all loads and generators modeled by their '+
                        'equivalent admittance.';
     OptionHelp[20] := 'Global load multiplier for this circuit.  Does not affect loads '+
                        'designated to be "fixed".  All other base kW values are multiplied by this number. '+
                        'Defaults to 1.0 when the circuit is created. As with other values, it always stays '+
                        'at the last value to which it was set until changed again.';
     OptionHelp[21] := 'Minimum permissible per unit voltage for normal conditions. Default is 0.95.';
     OptionHelp[22] := 'Maximum permissible per unit voltage for normal conditions. Default is 1.05.';
     OptionHelp[23] := 'Minimum permissible per unit voltage for emergency (contingency) conditions. Default is 0.90.';
     OptionHelp[24] := 'Maximum permissible per unit voltage for emergency (contingency) conditions. Default is 1.08.';
     OptionHelp[25] := 'Percent mean to use for global load multiplier. Default is 65%.';
     OptionHelp[26] := 'Percent Standard deviation to use for global load multiplier. Default is 9%.';
     OptionHelp[27] := 'Set Load-Duration Curve. Global load multiplier is defined by this curve for LD1 and LD2 solution modes. Default is Nil.';
     OptionHelp[28] := 'Set default annual growth rate, percent, for loads with no growth curve specified. Default is 2.5.';
     OptionHelp[29] := 'Size of generator, kW, to automatically add to system. Default is 1000.0';
     OptionHelp[30] := 'Power factor of generator to assume for automatic addition. Default is 1.0.';
     OptionHelp[31] := 'Size of capacitor, kVAR, to automatically add to system.  Default is 600.0.';
     OptionHelp[32] := '{Generator | Capacitor} Default is Generator. Type of device for AutoAdd Mode.';
     OptionHelp[33] := '{YES/TRUE | NO/FALSE}   Default is No. Flag to indicate if it is OK to have devices of same name in the same class. ' +
                         'If No, then a New command is treated as an Edit command. '+
                         'If Yes, then a New command will always result in a device being added.';
     OptionHelp[34] := '{YES/TRUE | NO/FALSE}  Default is No. if No, then meter zones are recomputed each time there is a change in the circuit. '+
                        'If Yes, then meter zones are not recomputed unless they have not yet been computed. '+
                        'Meter zones are normally recomputed on Solve command following a circuit change.';
     OptionHelp[35] := 'Weighting factor for UE/EEN in AutoAdd functions.  Defaults to 1.0.' + CRLF + CRLF +
                        'Autoadd mode minimizes'  + CRLF + CRLF +
                        '(Lossweight * Losses + UEweight * UE). ' + CRLF + CRLF +
                        'If you wish to ignore UE, set to 0. ' +
                        'This applies only when there are EnergyMeter objects. ' +
                        'Otherwise, AutoAdd mode minimizes total system losses.';
     OptionHelp[36] := 'Weighting factor for Losses in AutoAdd functions.  Defaults to 1.0.' + CRLF+CRLF+
                        'Autoadd mode minimizes'  + CRLF+CRLF+
                        '(Lossweight * Losses + UEweight * UE). ' + CRLF + CRLF +
                        'If you wish to ignore Losses, set to 0. '+
                        'This applies only when there are EnergyMeter objects. ' +
                        'Otherwise, AutoAdd mode minimizes total system losses.';
     OptionHelp[37] := 'Which EnergyMeter register(s) to use for UE in AutoAdd Mode. ' +
                        'May be one or more registers.  if more than one, register values are summed together. ' +
                        'Array of integer values > 0.  Defaults to 11 (for Load EEN). ' + CRLF+CRLF+
                        'for a list of EnergyMeter register numbers, do the "Show Meters" command after defining a circuit.';
     OptionHelp[38] := 'Which EnergyMeter register(s) to use for Losses in AutoAdd Mode. ' +
                        'May be one or more registers.  if more than one, register values are summed together. ' +
                        'Array of integer values > 0.  Defaults to 13 (for Zone kWh Losses). ' +  CRLF+CRLF+
                        'for a list of EnergyMeter register numbers, do the "Show Meters" command after defining a circuit.';
     OptionHelp[39] := 'Define legal bus voltage bases for this circuit.  Enter an array '+
                        'of the legal voltage bases, in phase-to-phase voltages, for example:' +CRLF+CRLF+
                        'set voltagebases=".208, .480, 12.47, 24.9, 34.5, 115.0, 230.0" '+CRLF+CRLF+
                        'When the CalcVoltageBases command is issued, a snapshot solution is performed '+
                        'with no load injections and the bus base voltage is set to the nearest legal voltage base. '+
                        'The defaults are as shown in the example above.';
     OptionHelp[40] := '{Normal | Newton}  Solution algorithm type.  Normal is a fixed point iteration ' +
                        'that is a little quicker than the Newton iteration.  Normal is adequate for most radial '+
                        'distribution circuits.  Newton is more robust for circuits that are difficult to solve.';
     OptionHelp[41] := '{YES/TRUE | NO/FALSE}  Default is "No". Specifies whether to use trapezoidal integration for accumulating energy meter registers. ' +
                        'Applies to EnergyMeter and Generator objects.  Default method simply multiplies the ' +
                        'present value of the registers times the width of the interval. ' +
                        'Trapezoidal is more accurate when there are sharp changes in a load shape or unequal intervals. ' +
                        'Trapezoidal is automatically used for ' +
                        'some load-duration curve simulations where the interval size varies considerably. ' +
                        'Keep in mind that for Trapezoidal, you have to solve one more point than the number of intervals. ' +
                        'That is, to do a Daily simulation on a 24-hr load shape, you would set Number=25 to force a solution ' +
                        'at the first point again to establish the last (24th) interval.';
     OptionHelp[42] := 'Array of bus names to include in AutoAdd searches. Or, you can specify a text file holding the names, one to a line, ' +
                        'by using the syntax (file=filename) instead of the actual array elements. '  +
                        'Default is null, which results in the program ' +
                        'using either the buses in the EnergyMeter object zones or, if no EnergyMeters, all the buses, which can ' +
                        'make for lengthy solution times. ' +Crlf+Crlf+
                        'Examples:'+Crlf+CRlf+
                        'Set autobuslist=(bus1, bus2, bus3, ... )' +CRLF+
                        'Set autobuslist=(file=buslist.txt)';
     OptionHelp[43] := '{OFF | STATIC |EVENT | TIME}  Default is "STATIC".  Control mode for the solution. ' +
                        'Set to OFF to prevent controls from changing.' + CRLF +
                        'STATIC = Time does not advance.  Control actions are executed in order of shortest time to act ' +
                        'until all actions are cleared from the control queue.  Use this mode for power flow solutions which may require several ' +
                        'regulator tap changes per solution.' + CRLF+CRLF+
                        'EVENT = solution is event driven.  Only the control actions nearest in time ' +
                        'are executed and the time is advanced automatically to the time of the event. ' + crlf +crlf+
                        'TIME = solution is time driven.  Control actions are executed when the time for the pending ' +
                        'action is reached or surpassed.' + CRLF + CRLF +
                        'Controls may reset and may choose not to act when it comes their time. ' +CRLF+
                        'Use TIME mode when modeling a control externally to the DSS and a solution mode such as ' +
                        'DAILY or DUTYCYCLE that advances time, or set the time (hour and sec) explicitly from the external program. ';
     OptionHelp[44] := '{YES/TRUE | NO/FALSE}  Set to YES to trace the actions taken in the control queue.  '  +
                        'Creates a file named TRACE_CONTROLQUEUE.CSV in the default directory. ' +
                        'The names of all circuit elements taking an action are logged.';
     OptionHelp[45] := 'Global multiplier for the kW output of every generator in the circuit. Default is 1.0. ' +
                        'Applies to all but Autoadd solution modes. ' +
                        'Ignored for generators designated as Status=Fixed.';
     OptionHelp[46] := 'Default daily load shape name. Default value is "default", which is a 24-hour curve defined when the DSS is started.';
     OptionHelp[47] := 'Default yearly load shape name. Default value is "default", which is a 24-hour curve defined when the DSS is started.';
     OptionHelp[48] := 'Sets all allocation factors for all loads in the active circuit to the value given.';
     OptionHelp[49] := '{Multiphase | Positive}  Default = Multiphase.  Designates whether circuit model is to interpreted as a normal multi-phase '+
                        'model or a positive-sequence only model';
     OptionHelp[50] := 'Sets the price signal ($/MWh) for the circuit.  Initial value is 25.';
     OptionHelp[51] := 'Sets the curve to use to obtain for price signal. Default is none (null string). If none, ' +
                        'price signal either remains constant or is set by an external process. ' +
                        'Curve is defined as a loadshape (not normalized) and should correspond to ' +
                        'the type of analysis being performed (daily, yearly, load-duration, etc.).';
     OptionHelp[52] := 'Set the active terminal of the active circuit element. May also be done with Select command.';
     OptionHelp[53] := 'Default = 60. Set the fundamental frequency for harmonic solution and the default base frequency for all impedance quantities. ' +
                        'Side effect: also changes the value of the solution frequency.';
     OptionHelp[54] := '{ALL | (list of harmonics) }  Default = ALL. Array of harmonics for which to perform a solution in Harmonics mode. ' +
                        'If ALL, then solution is performed for all harmonics defined in spectra currently being used. ' +
                        'Otherwise, specify a more limited list such as: ' +CRLF+CRLF+
                        '   Set Harmonics=(1 5 7 11 13)';
     OptionHelp[55] := 'Max control iterations per solution.  Default is 10.';
     OptionHelp[56] := 'Set Active Bus by name.  Can also be done with Select and SetkVBase commands and the "Set Terminal="  option. ' +
                        'The bus connected to the active terminal becomes the active bus. See Zsc and Zsc012 commands.';
     OptionHelp[57] := 'Set the data path for files written or read by the DSS.  Defaults to the startup path. May be Null.  Executes a CHDIR to this path if non-null. Does not require a circuit defined.';
     OptionHelp[58] := 'Array of bus names to keep when performing circuit reductions. You can specify a text file holding the names, one to a line, ' +
                        'by using the syntax (file=filename) instead of the actual array elements. '  +
                        'Command is cumulative (reset keeplist first). ' +
                        'Reduction algorithm may keep other buses automatically. ' +Crlf+Crlf+
                        'Examples:'+Crlf+CRlf+
                        'Reset Keeplist (sets all buses to FALSE (no keep))' +CRLF+
                        'Set KeepList=(bus1, bus2, bus3, ... )' +CRLF+
                        'Set KeepList=(file=buslist.txt)';
     OptionHelp[59] := '{ Default or [null] | Stubs [Zmag=nnn] | MergeParallel | BreakLoops | Switches | TapEnds [maxangle=nnn] | Ends}  Strategy for reducing feeders. ' +
                       'Default is to eliminate all dangling end buses and buses without load, caps, or taps. ' +  CRLF +
                       '"Stubs [Zmag=0.02]" merges short branches with impedance less than Zmag (default = 0.02 ohms) ' + CRLF +
                       '"MergeParallel" merges lines that have been found to be in parallel ' +CRLF+
                       '"Breakloops" disables one of the lines at the head of a loop. ' +CRLF+
                       '"Tapends [maxangle=15]" eliminates all buses except those at the feeder ends, at tap points and where the feeder turns by greater than maxangle degrees. ' + CRLF+
                       '"Ends" eliminates dangling ends only.'+CRLF+
                       '"Switches" merges switches with downline lines and eliminates dangling switches.'+CRLF+
                       'Marking buses with "Keeplist" will prevent their elimination.';
     OptionHelp[60] := '{YES/TRUE | NO/FALSE} Default = no. Set for keeping demand interval data for daily, yearly, etc, simulations. '+
                       'Side Effect:  Resets all meters!!!';
     OptionHelp[61] := 'Sets the Normal rating of all lines to a specified percent of the emergency rating.  Note: This action takes place immediately. '+
                        'Only the in-memory value is changed for the duration of the run.';
     OptionHelp[62] := '{YES/TRUE | NO/FALSE} Default = FALSE.  Set to Yes/True if you wish a separate demand interval (DI) file written ' +
                       'for each meter.  Otherwise, only the totalizing meters are written.';
     OptionHelp[63] := 'Name of case for yearly simulations with demand interval data. '+
                       'Becomes the name of the subdirectory under which all the year data are stored. '+
                       'Default = circuit name '+CRLF+CRLF+
                       'Side Effect: Sets the prefix for output files';
     OptionHelp[64] := 'Number code for node marker on circuit plots (SDL MarkAt options).';
     OptionHelp[65] := 'Width of node marker. Default=1.';
     OptionHelp[66] := '{YES/TRUE | NO/FALSE} Default = FALSE.  Significant solution events are added to the Event Log, primarily for debugging.';
     OptionHelp[67] := '{YES/TRUE | NO/FALSE} Default = FALSE. Opens DSSRecorder.DSS in DSS install folder and enables recording of all commands that come through ' +
                       'the text command interface. Closed by either setting to NO/FALSE or exiting the program. ' +
                       'When closed by this command, the file name can be found in the Result. Does not require a circuit defined.';
     OptionHelp[68] := '{YES/TRUE | NO/FALSE} Default = FALSE. For yearly solution mode, sets overload reports on/off. DemandInterval must be set to true for this to have effect.';
End;
//----------------------------------------------------------------------------
FUNCTION DoSetCmd_NoCircuit:Boolean;  // Set Commands that do not require a circuit
//----------------------------------------------------------------------------

// This is for setting global options that do not require an active circuit

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

     Result := TRUE;
     // Continue parsing command line
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := OptionList.GetCommand(ParamName);

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Set Command ', 130);
           15: DefaultEditor := Param;     // 'Editor='
           57: SetDataPath(Param);  // Set a legal data path
           67: DSSExecutive.RecorderOn := InterpretYesNo(Param);
         ELSE
            Begin
                DoSimpleMsg('You must create a new circuit object first: "new circuit.mycktname" to execute this Set command.', 301);
                Result := FALSE;  // Indicate that we could not process all set command
                Exit;
           End;
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End; {WHILE}

END;

//----------------------------------------------------------------------------
FUNCTION DoSetCmd(SolveOption:Integer):Integer;
//----------------------------------------------------------------------------

// Set DSS Options
// Solve Command is re-routed here first to set options beFORe solving

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   TestLoadShapeObj :TLoadShapeObj;


Begin

     Result := 0;
     // Continue parsing command line
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := OptionList.GetCommand(ParamName);

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Set Command ', 130);
            1,12: SetObjectClass(Param);
            2,13: SetObject(Param);
            3: ActiveCircuit.solution.Hour := Parser.IntValue;
            4: ActiveCircuit.solution.DynaVars.t    := Parser.DblValue;
            5: WITH ActiveCircuit Do Begin
                  Solution.Year := Parser.IntValue;
                  DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
               End;
            6: ActiveCircuit.solution.Frequency        := Parser.DblValue;
            7,18: ActiveCircuit.solution.DynaVars.h          := Parser.DblValue;
            8: ActiveCircuit.solution.Mode          := InterpretSolveMode(Param);  // see DSSGlobals
            9: ActiveCircuit.solution.RandomType    := InterpretRandom(Param);
           10: ActiveCircuit.solution.NumberOfTimes := Parser.IntValue;
           11: Set_Time;
           14: SetActiveCircuit(Param);
           15: DefaultEditor := Param;     // 'Editor='
           16: ActiveCircuit.solution.ConvergenceTolerance := Parser.DblValue;
           17: ActiveCircuit.solution.MaxIterations        := Parser.IntValue;
           19: WITH ActiveCircuit.solution DO Begin
                    DefaultLoadModel := InterpretLoadModel(Param); // for reverting to last on specified
                    LoadModel := DefaultLoadModel;
               End;
           20: ActiveCircuit.LoadMultiplier   := Parser.DblValue;  // Set using LoadMultiplier property
           21: ActiveCircuit.NormalMinVolts := Parser.DblValue;
           22: ActiveCircuit.NormalMaxVolts := Parser.DblValue;
           23: ActiveCircuit.EmergMinVolts  := Parser.DblValue;
           24: ActiveCircuit.EmergMaxVolts  := Parser.DblValue;
           25: ActiveCircuit.DefaultDailyShapeObj.Mean    := Parser.DblValue / 100.0;
           26: ActiveCircuit.DefaultDailyShapeObj.StdDev  := Parser.DblValue / 100.0;
           27: WITH ActiveCircuit DO Begin
                  LoadDurCurve    := Param;
                  LoadDurCurveObj := LoadShapeClass.Find(Param);
                  IF LoadDurCurveObj=nil THEN
                   DoSimpleMsg('Load-Duration Curve not found.', 131);
               End;
           28: WITH ActiveCircuit Do Begin
                    DefaultGrowthRate := 1.0 + Parser.DblValue/100.0;
                    DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
               End;
           29: ActiveCircuit.AutoAddObj.GenkW    := Parser.DblValue;
           30: ActiveCircuit.AutoAddObj.GenPF    := Parser.DblValue;
           31: ActiveCircuit.AutoAddObj.CapkVAR  := Parser.DblValue;
           32: ActiveCircuit.AutoAddObj.AddType  := InterpretAddType(Param);
           33: ActiveCircuit.DuplicatesAllowed := InterpretYesNo(Param);
           34: ActiveCircuit.ZonesLocked := InterpretYesNo(Param);
           35: ActiveCircuit.UEWeight    := Parser.DblValue;
           36: ActiveCircuit.LossWeight  := Parser.DblValue;
           37: ParseIntArray(ActiveCircuit.UERegs, ActiveCircuit.NumUEregs, Param);
           38: ParseIntArray(ActiveCircuit.LossRegs, ActiveCircuit.NumLossregs, Param);
           39: DoLegalVoltageBases;
           40: ActiveCircuit.Solution.Algorithm := InterpretSolveAlg(Param);
           41: ActiveCircuit.TrapezoidalIntegration := InterpretYesNo(Param);
           42: DoAutoAddBusList(Param);
           43: WITH ActiveCircuit.Solution Do Begin
                    ControlMode := InterpretControlMode(Param);
                    DefaultControlMode := ControlMode;  // always revert to last one specified in a script
               END;
           44: ActiveCircuit.ControlQueue.TraceLog := InterpretYesNo(Param);
           45: ActiveCircuit.GenMultiplier := Parser.DblValue ;
           46: Begin
                 TestLoadShapeObj := LoadShapeClass.Find(Param);
                 IF TestLoadShapeObj <> NIL THEN ActiveCircuit.DefaultDailyShapeObj  := TestLoadShapeObj;
               END;
           47: Begin
                 TestLoadShapeObj := LoadShapeClass.Find(Param);
                 IF TestLoadShapeObj <> NIL THEN ActiveCircuit.DefaultYearlyShapeObj  := TestLoadShapeObj;
               END;
           48: DoSetAllocationFactors(Parser.DblValue);
           49: ActiveCircuit.PositiveSequence := InterpretCktModel(Param);
           50: ActiveCircuit.PriceSignal := Parser.DblValue ;
           51: WITH ActiveCircuit DO  Begin
                  PriceCurve    := Param;
                  PriceCurveObj := LoadShapeClass.Find(Param);
                  IF PriceCurveObj=nil THEN
                   DoSimpleMsg('Price Curve: "' +param+ '" not found.', 132);
               End;
           52: With ActiveCircuit DO IF ActiveCktElement<> NIL THEN With ActiveCktElement Do
                Begin
                   ActiveTerminalIdx := Parser.IntValue;
                   SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));   // bus connected to terminal
                End;
           53: ActiveCircuit.Fundamental := Parser.DblValue;     // Set Base Frequency for system (used henceforth)
           54: DoHarmonicsList(Param);
           55: ActiveCircuit.Solution.MaxControlIterations := Parser.IntValue;
           56: Result := SetActiveBus(Param);   // See DSSGlobals
           57: SetDataPath(Param);  // Set a legal data path
           58: DoKeeperBusList(Param);
           59: DoSetReduceStrategy(param);
           60: EnergyMeterClass.SaveDemandInterval := InterpretYesNo(Param);
           61: Begin
                 ActiveCircuit.PctNormalFactor := Parser.DblValue;
                 DoSetNormal(ActiveCircuit.PctNormalFactor);
               End;
           62: EnergyMeterClass.DI_Verbose   := InterpretYesNo(Param);
           63: ActiveCircuit.CaseName        := Parser.StrValue;
           64: ActiveCircuit.NodeMarkerCode  := Parser.IntValue;
           65: ActiveCircuit.NodeMarkerWidth := Parser.IntValue;
           66: ActiveCircuit.LogEvents       := InterpretYesNo(Param);
           67: DSSExecutive.RecorderOn       := InterpretYesNo(Param);
           68: EnergyMeterClass.Do_OverloadReport := InterpretYesNo(Param);
         ELSE
           // Ignore excess parameters
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End; {WHILE}

     IF SolveOption = 1 THEN  DoSolveCmd;

End;


//----------------------------------------------------------------------------
FUNCTION DoGetCmd:Integer;

// Get DSS Options Reguaeste and put it in Global Result string
// may be retrieved by Result property of the DSSText interface

VAR
   ParamPointer, i:Integer;
   ParamName:String;
   Param:String;

Begin

  Result := 0;
  Try

     GlobalResult := '';  //initialize for appending

     // Continue parsing command line
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     // there will be no named paramters in this command and the params
     // themselves will be the parameter name to return
     WHILE Length(Param)>0 DO
     Begin
         ParamPointer := OptionList.GetCommand(Param);

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Get Command ', 133);
            1,12: AppendGlobalResult(ActiveCircuit.ActiveCktElement.DSSClassName );
            2,13: AppendGlobalResult(ActiveCircuit.ActiveCktElement.Name);
            3: AppendGlobalResult(IntToStr(ActiveCircuit.solution.Hour));
            4: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.solution.DynaVars.t]));
            5: AppendGlobalResult(IntToStr(ActiveCircuit.solution.Year));
            6: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.solution.Frequency]));
           7,18: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.solution.DynaVars.h]));
            8: AppendGlobalResult(GetSolutionModeID);
            9: AppendGlobalResult(GetRandomModeID);
           10: AppendGlobalResult(IntToStr(ActiveCircuit.solution.NumberOfTimes));
           11: Begin
                   AppendGlobalResult(IntToStr(ActiveCircuit.solution.Hour));
                   AppendGlobalResult(FORmat('%-g' ,[ActiveCircuit.solution.DynaVars.t]));
               End;
           14: AppendGlobalResult(ActiveCircuit.name);
           15: AppendGlobalResult(DefaultEditor);
           16: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.solution.ConvergenceTolerance]));
           17: AppendGlobalResult(IntToStr(ActiveCircuit.solution.MaxIterations));
           19: AppendGlobalResult(GetLoadModel);
           20: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.LoadMultiplier]));
           21: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.NormalMinVolts]));
           22: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.NormalMaxVolts]));
           23: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.EmergMinVolts]));
           24: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.EmergMaxVolts]));
           25: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.DefaultDailyShapeObj.Mean*100.0]));
           26: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.DefaultDailyShapeObj.StdDev*100.0]));
           27: AppendGlobalResult(ActiveCircuit.LoadDurCurve);
           28: AppendGlobalResult(Format('%-g' ,[(ActiveCircuit.DefaultGrowthRate-1.0)*100.0]));
           29: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.AutoAddObj.GenkW]));
           30: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.AutoAddObj.GenPF]));
           31: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.AutoAddObj.CapkVAR]));
           32: CASE ActiveCircuit.AutoAddObj.Addtype of
                  GENADD:AppendGlobalResult('generator');
                  CAPADD:AppendGlobalResult('capacitor');
               End;
           33: IF ActiveCircuit.DuplicatesAllowed THEN AppendGlobalResult('Yes') ELSE AppendGlobalResult('No');
           34: IF ActiveCircuit.ZonesLocked THEN AppendGlobalResult('Yes') ELSE AppendGlobalResult('No');
           35: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.UEWeight]));
           36: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.LossWeight]));
           37: AppendGlobalResult(IntArrayToString(ActiveCircuit.UERegs,   ActiveCircuit.NumUEregs));
           38: AppendGlobalResult(IntArrayToString(ActiveCircuit.LossRegs, ActiveCircuit.NumLossRegs));
           39: WITH ActiveCircuit Do
               Begin
                 i:=1;
                 GlobalResult := '(';
                 WHILE LegalVoltageBases^[i] > 0.0 Do
                 Begin
                     GlobalResult := GlobalResult + Format('%-g, ' , [LegalVoltageBases^[i]]);
                     inc(i);
                 End;
                 GlobalResult := GlobalResult + ')';
               End;
           40: CASE ActiveCircuit.Solution.Algorithm  of
                 NORMALSOLVE: AppendGlobalResult('normal');
                 NEWTONSOLVE: AppendGlobalResult('newton');
               End;
           41: IF ActiveCircuit.TrapezoidalIntegration  THEN AppendGlobalResult('Yes') ELSE AppendGlobalResult('No');
           42: WITH ActiveCircuit.AutoAddBusList Do
               FOR i := 1 to ListSize Do AppendGlobalResult(Get(i));
           43: AppendGlobalResult(GetControlModeID);
           44: IF ActiveCircuit.ControlQueue.traceLog  THEN AppendGlobalResult('Yes') ELSE AppendGlobalResult('No');
           45: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.GenMultiplier]));
           46: AppendGlobalResult(ActiveCircuit.DefaultDailyShapeObj.Name);
           47: AppendGlobalResult(ActiveCircuit.DefaultYearlyShapeObj.Name);
           48: AppendGlobalResult('Get function not applicable.');
           49: IF ActiveCircuit.positiveSequence THEN AppendGlobalResult('positive') Else AppendGlobalResult('multiphase') ;
           50: AppendGlobalResult(Format('%-g', [ActiveCircuit.PriceSignal]));
           51: AppendGlobalResult(ActiveCircuit.PriceCurve);
           52: AppendGlobalResult(Format('%d' ,[ActiveCircuit.ActiveCktElement.ActiveTerminalIdx]));
           53: AppendGlobalResult(Format('%-g' ,[ActiveCircuit.Fundamental]));
           54: WITH ActiveCircuit.Solution DO
               IF DoALLHarmonics THEN AppendGlobalResult('ALL')
               ELSE  Begin
                        FOR i := 1 to HarmonicListSize Do AppendGlobalResult(Format('%-g' ,[HarmonicList^[i]]));
                     End;
           55: AppendGlobalResult(IntToStr(ActiveCircuit.solution.MaxControlIterations));
           56: AppendGlobalResult(ActiveCircuit.BusList.Get(ActiveCircuit.ActiveBusIndex));
           57: AppendGlobalResult(DSSDataDirectory);
           58: With ActiveCircuit Do For i := 1 to NumBuses Do If Buses^[i].Keep Then AppendGlobalResult(BusList.Get(i));
           59: AppendGlobalResult(ActiveCircuit.ReductionStrategyString );
           60: If EnergyMeterClass.SaveDemandInterval Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');
           61: AppendGlobalResult(Format('%-.g', [ActiveCircuit.PctNormalFactor]));
           62: If EnergyMeterClass.DI_Verbose Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');
           63: AppendGlobalResult(ActiveCircuit.CaseName);
           64: AppendGlobalResult(Format('%d' ,[ActiveCircuit.NodeMarkerCode]));
           65: AppendGlobalResult(Format('%d' ,[ActiveCircuit.NodeMarkerWidth]));
           66: If ActiveCircuit.LogEvents Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');
           67: If DSSExecutive.RecorderON Then  AppendGlobalResult('Yes') else AppendGlobalResult('No');

         ELSE
           // Ignore excess parameters
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End; {WHILE}

  Except
      AppendGlobalResult('***Error***');
  End;

End;

Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumExecOptions Do Begin
       ExecOption[i] := '';
       OptionHelp[i] := '';
   End;

End;


Initialization

    DefineOptions;

Finalization

    DisposeStrings;


end.
