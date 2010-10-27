unit PlotOptions;

interface

Uses Command;



CONST
        NumPlotOptions = 18;

FUNCTION DoPlotCmd:Integer;

Var

         PlotOption,
         PlotHelp :Array[1..NumPlotOptions] of String;
         PlotCommands:TCommandList;


implementation

Uses DSSPlot, DSSGlobals, SysUtils, ParserDel, Utilities;



PROCEDURE DefineOptions;

Begin


      PlotOption[ 1] := 'type';
      PlotOption[ 2] := 'quantity';
      PlotOption[ 3] := 'max';
      PlotOption[ 4] := 'dots';
      PlotOption[ 5] := 'labels';
      PlotOption[ 6] := 'object';
      PlotOption[ 7] := 'showloops';
      PlotOption[ 8] := 'r3';
      PlotOption[ 9] := 'r2';
      PlotOption[10] := 'c1';
      PlotOption[11] := 'c2';
      PlotOption[12] := 'c3';
      PlotOption[13] := 'channels';
      PlotOption[14] := 'bases';
      PlotOption[15] := 'subs';
      PlotOption[16] := 'thickness';
      PlotOption[17] := 'buslist';
      PlotOption[18] := 'min';


      PlotHelp[ 1] := 'One of {Circuit | Monitor | Daisy | Zones | AutoAdd | General (bus data) | Loadshape } ' +
                      'A "Daisy" plot is a special circuit plot that places a marker at each Generator location ' +
                      'or at buses in the BusList property, if defined. ' +
                      'A Zones plot shows the meter zones (see help on Object). ' +
                      'Autoadd shows the autoadded generators. General plot shows quantities associated with buses ' +
                      'using gradient colors between C1 and C2. Values are read from a file (see Object). ' +
                      'Loadshape plots the specified loadshape. Examples:'+CRLF+CRLF+
                      'Plot type=circuit quantity=power' +CRLF+
                      'Plot Circuit Losses' +CRLF+
                      'Plot Circuit quantity=3 object=mybranchdata.csv' +CRLF+
                      'Plot daisy power max=5000 dots=N Buslist=[file=MyBusList.txt]' +CRLF+
                      'Plot General quantity=1 object=mybusdata.csv' +CRLF+
                      'Plot Loadshape object=myloadshape' ;
      PlotHelp[ 2] := 'One of {Voltage | Current | Power | Losses | Capacity | (Value Index for General, AutoAdd, or Circuit[w/ file]) }';
      PlotHelp[ 3] := 'Enter 0 (the default value) or the value corresponding to max scale or line thickness in the circuit plots. '+
                      'Power and Losses in kW. Also, use this to specify the max value corresponding to color C2 in General plots.';
      PlotHelp[ 4] := 'Yes or No*. Places a marker on the circuit plot at the bus location. See Set Markercode under options.';
      PlotHelp[ 5] := 'Yes or No*. If yes, bus labels (abbreviated) are printed on the circuit plot.';
      PlotHelp[ 6] := 'Object to be plotted. One of [Meter Name (zones plot) | Monitor Name | LoadShape Name | File Name for General bus data | File Name Circuit branch data]';
      PlotHelp[ 7] := '{Yes | No*} Shows loops on Circuit plot. Requires an EnergyMeter to be defined.';
      PlotHelp[ 8] := 'pu value for tri-color plot max range [default=.85 of max scale]. Corresponds to color C3.';
      PlotHelp[ 9] := 'pu value for tri-color plot mid range [default=.50 of max scale]. Corresponds to color C2.';
      PlotHelp[10] := 'RGB color number for color C1. This is the default color for circuit plots. Default is blue. See options in the Plot menu.';
      PlotHelp[11] := 'RGB color number for color C2. Used for gradients and tricolor plots such as circuit voltage.';
      PlotHelp[12] := 'RGB color number for color C3. Used for gradients and tricolor plots such a circuit voltage.';
      PlotHelp[13] := 'Array of channel numbers for monitor plot. Example' +CRLF+CRLF+
                      'Plot Type=Monitor Object=MyMonitor Channels=[1, 3, 5]'+CRLF+CRLF+
                      'Do "Show Monitor MyMonitor" to see channel definitions.';
      PlotHelp[14] := 'Array of base values for each channel for monitor plot. Useful for creating per unit plots. Default is 1.0 for each channel.  Set Base= property after defining channels.'+CRLF+CRLF+
                      'Plot Type=Monitor Object=MyMonitor Channels=[1, 3, 5] Bases=[2400 2400 2400]'+CRLF+CRLF+
                      'Do "Show Monitor MyMonitor" to see channel range and definitions.';;
      PlotHelp[15] := '{Yes | No*} Displays a marker at each transformer declared to be a substation. ' +
                      'At least one bus coordinate must be defined for the transformer. '+
                      'See MarkTransformer and TransMarkerCode options.';
      PlotHelp[16] := 'Max thickness allowed for lines in circuit plots (default=7).';
      PlotHelp[17] := '{Array of Bus Names | File=filename } This is for the Daisy plot. '+CRLF+CRLF+
                      'Plot daisy power max=5000 dots=N Buslist=[file=MyBusList.txt]' +CRLF+CRLF+
                      'A "daisy" marker is plotted for ' +
                      'each bus in the list. Bus name may be repeated, which results in multiple markers distributed around the bus location. ' +
                      'This gives the appearance of a daisy if there are several symbols at a bus. Not needed for plotting active generators.';
      PlotHelp[18] := 'Enter 0 (the default value) or the value corresponding to min value corresponding to color C1 in General bus data plots.';



End;


//----------------------------------------------------------------------------
FUNCTION DoPlotCmd:Integer;

{
  Produce a plot with the DSSGraphX object
}

Var

   ParamName, Param:String;
   ParamPointer, i:Integer;
   DblBuffer:Array[0..50] of Double;
   NumChannels:Integer;

Begin
    Result := 0;

    If NoFormsAllowed Then Begin Result :=1; Exit; End;

    If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;

    DSSPlotObj.SetDefaults;

    {Get next parameter on command line}
    ParamPointer := 0;
    ParamName := Uppercase(Parser.NextParam);
    Param := Uppercase(Parser.StrValue);
    While Length(Param) > 0 Do
     Begin
      {Interpret Parameter}
       IF   (Length(ParamName) = 0)  THEN Inc(ParamPointer)
       ELSE  ParamPointer := PlotCommands.Getcommand (ParamName);

       With DSSPlotObj Do
       Case ParamPointer of

         1: Case Param[1] of
               'A': Begin
                     PlotType   := ptAutoAddLogPlot;
                     ObjectName := CircuitName_ + 'AutoAddLog.CSV';
                     ValueIndex := 2;
                    End;
               'C': PlotType := ptCircuitplot;
               'G': PlotType := ptGeneralDataPlot;
               'L': PlotType := ptLoadshape;
               'M': PlotType := ptMonitorplot;
               'D': Begin
                      PlotType := ptDaisyplot;
                      DaisyBusList.Clear;
                    End;
               'Z': PlotType := ptMeterZones;
            Else
            End;
         2: Case Param[1] of
               'V': Quantity := pqVoltage;
               'C': Case Param[2] of
                    'A': Quantity := pqcapacity;
                    'U': Quantity := pqcurrent;
                    End;
               'P': Quantity := pqpower;
               'L': Quantity := pqlosses;
             Else
               Quantity := pqNone;
               Valueindex := Parser.IntValue;
             End;
         3:  Begin
                 MaxScale := Parser.DblValue;
                 If MaxScale>0.0 Then MaxScaleIsSpecified := TRUE;    // Indicate the user wants a particular value
             End;
         4:  Dots := InterpretYesNo(Param);
         5:  Labels := InterpretYesNo(Param);
         6:  ObjectName := Parser.StrValue;
         7:   Begin
                ShowLoops := InterpretYesNo(Param);
                If ShowLoops then PlotType := ptMeterzones;
              End;
         8:   TriColorMax := Parser.DblValue;
         9:   TriColorMid := Parser.DblValue;
         10:  Color1 := Parser.IntValue;
         11:  Color2 := Parser.IntValue;
         12:  Color3 := Parser.IntValue;
         13: Begin    {Channel definitions for Plot Monitor}
               NumChannels := Parser.ParseAsVector(51, @DblBuffer);  // allow up to 50 channels
               If NumChannels>0 Then Begin   // Else take the defaults
                 SetLength(Channels, NumChannels);
                 For i := 0 to NumChannels-1 Do Channels[i] := Round(DblBuffer[i]);
                 SetLength(Bases, NumChannels);
                 For i := 0 to NumChannels-1 Do Bases[i] :=1.0;
               End;
             End;
         14: Begin
               NumChannels  := Parser.ParseAsVector(51, @DblBuffer);  // allow up to 50 channels
               If NumChannels>0 Then Begin
                  SetLength(Bases, NumChannels);
                 For i := 0 to NumChannels-1 Do Bases[i] := DblBuffer[i];
               End;
             End;
         15: ShowSubs := InterpretYesNo(Param);
         16: MaxLineThickness := Parser.IntValue ;
         17: InterpretTStringListArray(Param,  DaisyBusList);  {read in Bus list}
         18: Begin
                 MinScale := Parser.DblValue;
                 If MinScale>0.0 Then MinScaleIsSpecified := TRUE;    // Indicate the user wants a particular value
             End;

       Else
       End;


      ParamName := Uppercase(Parser.NextParam);
      Param := Uppercase(Parser.StrValue);
     End;

     If Not ActiveCircuit.Issolved Then DSSPlotObj.Quantity := pqNone;

     With DSSPlotObj Do Begin

        Execute;   // makes a new plot based on these options

     End;

End;


Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumPlotOptions Do Begin
       PlotOption[i] := '';
       PlotHelp[i]   := '';
   End;

End;


Initialization

    DefineOptions;

    PlotCommands := TCommandList.Create(PlotOption);
    PlotCommands.Abbrev := True;

Finalization

    DisposeStrings;
    PlotCommands.Free;
end.
