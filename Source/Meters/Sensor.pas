unit Sensor;

{
   Change Log
   8-24-2007 Created from Sensor Object
}

{
 
}

interface

USES
     Command, MeterClass, Meterelement, DSSClass, Arraydef, ucomplex, utilities, Classes;

TYPE
    TSensorStrBuffer = Array[1..256] of Char;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TSensor = class(TMeterClass)
     private

     protected
        Procedure DefineProperties;
        Function MakeLike(const SensorName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       Procedure ResetAll; Override;
       Procedure SampleAll; Override;  // Force all Sensors to take a sample
       Procedure SaveAll;  Override;   // Force all Sensors to save their buffers to disk

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TSensorObj = class(TMeterElement)
     private
       BaseFrequency:Double;
       ValidSensor:Boolean;

//    function Get_FileName: String;
    procedure Set_SensedPhases(const Value: String);
    procedure Set_SensorModes(const Value: String);
    procedure Set_Conn(const Value: String);
    procedure Set_Action(const Value: String);

     public
       MeaskV  :Double;
       MeasAmps:Double;
       MeaskW  :Double;
       Measkvar:Double;

       pctError     :Double;

       constructor Create(ParClass:TDSSClass; const SensorName:String);
       destructor Destroy; override;

       Procedure RecalcElementData; Override;
       Procedure CalcYPrim; Override;    // Always Zero for a Sensor
       Procedure TakeSample; Override; // Go add a sample to the buffer
       Procedure ResetIt;
       Procedure Save;  // Saves present buffer to file

       Procedure GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       Procedure GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;


       {Properties to interpret input to the sensor}

       Property Sensormodes:String  Write Set_SensorModes;
       Property SensedPhases:String Write Set_SensedPhases;
       Property Conn:String         Write Set_Conn;
       Property Action:String       Write Set_Action;


   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

VAR
    ActiveSensorObj:TSensorObj;

{--------------------------------------------------------------------------}
implementation

USES

    ParserDel, DSSGlobals, Circuit, CktElement,Transformer, PCElement,
    Sysutils, ucmatrix, showresults, mathUtil, PointerList, TOPExport, Dynamics;

CONST

    NumPropsThisClass = 11;

{--------------------------------------------------------------------------}
constructor TSensor.Create;  // Creates superstructure for all Sensor objects
Begin
     Inherited Create;

     Class_name   := 'Sensor';
     DSSClassType := DSSClassType + SENSOR_ELEMENT;

     DefineProperties;


     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TSensor.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TSensor.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'element';
     PropertyName[2] := 'terminal';
     PropertyName[3] := 'modes';
     PropertyName[4] := 'v';
     PropertyName[5] := 'i';
     PropertyName[6] := 'p';
     PropertyName[7] := 'q';
     PropertyName[8] := 'Phases';  // Monitored phases
     PropertyName[9] := 'conn';  //  Sensor connection
     PropertyName[10] := '%Error';  //  %Error of sensor
     PropertyName[11] := 'action';  //  %Error of sensor

     PropertyHelp[1] := 'Name (Full Object name) of element to which the Sensor is connected.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the Sensor is connected. '+
                    '1 or 2, typically. For Sensoring states, attach Sensor to terminal 1.';
     PropertyHelp[3] := 'Array of any of { Voltage | Current | kW | kvar } in any order. Default is Voltage only. ' + CRLF +
                        ' Quantities being sensed. Scalar magnitudes only. ' + CRLF +
                        ' If quantity is not specified here, it ';
     PropertyHelp[4] := 'Array of Voltages (kV) measured by the voltage sensor.';
     PropertyHelp[5] := 'Array of Currents (amps) measured by the current sensor.';
     PropertyHelp[6] := 'Array of Active power (kW) measurements at the sensor.';
     PropertyHelp[7] := 'Array of Reactive power (kvar) measurements at the sensor.';
     PropertyHelp[8] := 'Array of phases being monitored by this sensor. [1, 2, 3] or [2 3 1] or [1], etc.  '+CRLF+
                        'Corresponds to the order that the measurement arrays will be supplied. ' +
                        'Defaults to same number of quantities as phases in the monitored element.';
     PropertyHelp[9] := 'Connection: { wye | delta | LN | LL }.  Default is wye. Applies to voltage measurement. '+CRLF+
                        'If wye or LN, voltage is assumed measured line-neutral; otherwise, line-line.';
     PropertyHelp[10] := 'Assumed percent error in the measurement. Default is 1.';
     PropertyHelp[11] := 'Action options: '+CRLF+'SQERROR: Show square error of the present value of the monitored terminal  '+CRLF+
                        'quantity vs the sensor value. Actual values - convert to per unit in calling program.  '+CRLF+
                        'Value reported in result window/result variable.';


     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
Function TSensor.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Sensor and add it to Sensor class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TSensorObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
Function TSensor.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing with contents of Parser
  // continue parsing with contents of Parser
  ActiveSensorObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveSensorObj;

  Result := 0;

  WITH ActiveSensorObj DO Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 661);
            1: ElementName := lowercase(param);
            2: MeteredTerminal := Parser.IntValue;
            3: Sensormodes := Param;
            4: MeaskV      := Parser.dblValue;
            5: MeasAmps    := Parser.dblValue;
            6: MeaskW      := Parser.dblValue;
            7: Measkvar    := Parser.dblValue;
            8: SensedPhases := Param;
            9: Conn         := Param;
           10: pctError     := Parser.dblValue;
           11: Action       := Param;  // Put sq error in Global Result
         ELSE
           // Inherited parameters
           ClassEdit( ActiveSensorObj, ParamPointer - NumPropsthisClass)
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
  End;

End;

{--------------------------------------------------------------------------}
Procedure TSensor.ResetAll;  // Force all Sensors in the circuit to reset

//VAR
//   pSensor  :TSensorObj;

Begin
{
      pSensor := ActiveCircuit.Sensors.First;
      WHILE pSensor<>Nil DO
      Begin
          If pSensor.enabled Then pSensor.ResetIt;
          pSensor := ActiveCircuit.Sensors.Next;
      End;
 }
End;

{--------------------------------------------------------------------------}
Procedure TSensor.SampleAll;  // Force all Sensors in the circuit to take a sample

//VAR
//   Mon:TSensorObj;

Begin

{
      Mon := ActiveCircuit.Sensors.First;
      WHILE Mon<>Nil DO
      Begin
          If Mon.enabled Then Mon.TakeSample;
          Mon := ActiveCircuit.Sensors.Next;
      End;
}
End;

{--------------------------------------------------------------------------}
Procedure TSensor.SaveAll;     // Force all Sensors in the circuit to save their buffers to disk

//VAR
//   Mon:TSensorObj;

Begin
{
   Mon := ActiveCircuit.Sensors.First;
   WHILE Mon<>Nil DO
   Begin
       If Mon.Enabled Then Mon.Save;
       Mon := ActiveCircuit.Sensors.Next;
   End;
}
End;

{--------------------------------------------------------------------------}
Function TSensor.MakeLike(const SensorName:String):Integer;
VAR
   OtherSensor:TSensorObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this Sensor name in the present collection}
   OtherSensor := Find(SensorName);
   IF OtherSensor<>Nil THEN
   WITH ActiveSensorObj DO Begin

       NPhases := OtherSensor.Fnphases;
       NConds  := OtherSensor.Fnconds; // Force Reallocation of terminal stuff

       ElementName:= OtherSensor.ElementName;
       MeteredElement:= OtherSensor.MeteredElement;  // Pointer to target circuit element
       MeteredTerminal:= OtherSensor.MeteredTerminal;
 {****Do This      SensorType := OtherSensor.SensorType;}

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherSensor.PropertyValue[i];

       BaseFrequency:= OtherSensor.BaseFrequency;

   End
   ELSE  DoSimpleMsg('Error in Sensor MakeLike: "' + SensorName + '" Not Found.', 662);

End;

{--------------------------------------------------------------------------}
Function TSensor.Init(Handle:Integer):Integer;
VAR
   Mon:TSensorObj;

Begin
      Result := 0;

      IF Handle>0  THEN Begin
         Mon := ElementList.Get(Handle);
         Mon.ResetIt;
      End
      ELSE Begin  // Do 'em all
        Mon := ElementList.First;
        WHILE Mon<>Nil DO Begin
            Mon.ResetIt;
            Mon := ElementList.Next;
        End;
      End;

End;


{==========================================================================}
{                    TSensorObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TSensorObj.Create(ParClass:TDSSClass; const SensorName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(SensorName);

     Nphases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class



     Basefrequency := 60.0;

     DSSObjType := ParClass.DSSClassType; //SENSOR_ELEMENT;

     InitPropertyValues(0);

   //  RecalcElementData;

End;

destructor TSensorObj.Destroy;
Begin
     ElementName := '';
 Inherited Destroy;
End;




{--------------------------------------------------------------------------}
Procedure TSensorObj.RecalcElementData;

VAR
   DevIndex :Integer;

Begin
         ValidSensor := FALSE;
         Devindex := GetCktElementIndex(ElementName); // Global function
         IF DevIndex>0 THEN Begin  // Sensored element must already exist
             MeteredElement := ActiveCircuit.CktElements.Get(DevIndex);

             IF MeteredTerminal>MeteredElement.Nterms THEN Begin
                 DoErrorMsg('Sensor: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Respecify terminal no.', 665);
             End
             ELSE Begin
                 Nphases := MeteredElement.NPhases;
                 Nconds  := MeteredElement.NConds;

               // Sets name of i-th terminal's connected bus in Sensor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
                 Setbus(1, MeteredElement.GetBus(MeteredTerminal));

                 ValidSensor := TRUE;

             End;

         End
         ELSE Begin
            MeteredElement := nil;   // element not found
            DoErrorMsg('Sensor: "' + Self.Name + '"', 'Circuit Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 666);
         End;
End;

{--------------------------------------------------------------------------}
Procedure TSensorObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
Procedure TSensorObj.ResetIt;

{What does it mean to reset a sensor?}
Begin

{     ClearSensorStream;}

End;

{--------------------------------------------------------------------------}
Procedure TSensorObj.TakeSample;
Begin
   If Not (ValidSensor and Enabled) Then Exit;
End;



{--------------------------------------------------------------------------}
Procedure TSensorObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr for reports
VAR
   i:Integer;
Begin

{
  Revised 12-7-99 to return Zero current instead of Sensored element current because
 it was messing up Newton iteration.
}

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

{--------------------------------------------------------------------------}
Procedure TSensorObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds DO Curr^[i] := cZero;
End;

{--------------------------------------------------------------------------}
Procedure TSensorObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    With ParentClass Do
     For i := 1 to NumProperties Do  Begin
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete Then Begin
      Writeln(F);
    End;

End;

procedure TSensorObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := ''; //'element';
     PropertyValue[2] := '1'; //'terminal';
     PropertyValue[3] := '[v]'; //'modes';
     PropertyValue[4] := '7.2'; // matches default of most other quantities
     PropertyValue[5] := '0.0';
     PropertyValue[6] := '0.0';
     PropertyValue[7] := '0.0';
     PropertyValue[8] := '[1 2 3]';
     PropertyValue[9] := 'wye';
     PropertyValue[10] := '1';
     PropertyValue[11] := '';

  inherited  InitPropertyValues(NumPropsThisClass);

end;


{--------------------------------------------------------------------------}

{ - function is not actually used
function TSensorObj.Get_FileName: String;
begin
        Result := DSSDataDirectory +  CircuitName_ + 'Sensor_' + Name + '.csv'
end;
}

procedure TSensorObj.Save;
begin

end;

procedure TSensorObj.Set_SensedPhases(const Value: String);

{Interpret which phases are being sensed}

begin

end;

procedure TSensorObj.Set_SensorModes(const Value: String);

{ Interpret which modes }

begin

end;

procedure TSensorObj.Set_Conn(const Value: String);
{Interpret the Connection}
begin

end;

procedure TSensorObj.Set_Action(const Value: String);
{Interpret Action Property}
begin

end;

initialization
  //WriteDLLDebugFile('Sensor');

end.
