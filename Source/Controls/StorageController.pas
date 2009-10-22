unit StorageController;
{
  ----------------------------------------------------------
  Copyright (c) 2009, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A StorageController is a control element that is connected to a terminal of another
  circuit element and sends dispatch  signals to a set of energy storage it controls

  A StorageController is defined by a New command:

  New StorageController.Name=myname Element=devclass.name terminal=[ 1|2|...] Elementlist = (elem1  elem2 ...)

  or ... ElementList = [File=filename] where storage class elements are listed one to a line
  If omitted, all storage elements are included by default and controlled as a fleet.

}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     utilities, PointerList, Classes, Loadshape;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageController = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const StorageControllerName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageControllerObj = class(TControlElem)
     private

            FkWTarget,
            FpctkWBand,
            HalfkWBand,
            FPFTarget,
            TotalWeight   :Double;
            HalfPFBand    :Double;
            FPFBand       :Double;
            FleetSize     :Integer;

            FStorageNameList  :TStringList;
            FleetPointerList    :PointerList.TPointerList;
            FWeights          :pDoubleArray;

            DischargeMode         :Integer;
            ChargeMode            :Integer;
            DischargeTriggerTime  :Double;
            ChargeTriggerTime     :Double;
            pctKWRate             :Double;
            pctkvarRate           :Double;
            pctChargeRate         :Double;
            pctFleetReserve       :Double;
            FleetListChanged      :Boolean;
            ChargingAllowed       :Boolean;

            YearlyShape     :String;  // ='fixed' means no variation  on all the time
            YearlyShapeObj  :TLoadShapeObj;  // Shape for this Storage element
            DailyShape      :String;  // Daily (24 HR) Storage element shape
            DailyShapeObj   :TLoadShapeObj;  // Daily Storage element Shape for this load
            DutyShape       :String;  // Duty cycle load shape for changes typically less than one hour
            DutyShapeObj    :TLoadShapeObj;  // Shape for this Storage element

            LoadShapeMult   :Double;

            MonitoredElement :TDSSCktElement;

            PROCEDURE SetPctReserve;
            PROCEDURE SetAllFleetValues;
            PROCEDURE SetFleetkWRate;
            PROCEDURE SetFleetkvarRate;
            PROCEDURE SetFleetChargeRate;
            PROCEDURE SetFleetToCharge;
            PROCEDURE SetFleetToDisCharge;
            PROCEDURE SetFleetToIdle;
            FUNCTION  InterpretMode(Opt :Integer; Const S:String):Integer;
            FUNCTION  GetModeString(Opt, Mode :Integer):String;
            FUNCTION  GetkWTotal:String;
            FUNCTION  GetkWhTotal:String;
            FUNCTION  GetkWhActual:String;
            FUNCTION  GetkWActual:String;

            FUNCTION  ReturnElementsList:String;
            FUNCTION  ReturnWeightsList:String;

            FUNCTION MakeFleetList:Boolean;
            PROCEDURE DoLoadFollowMode;
            PROCEDURE DoLoadShapeMode;
            PROCEDURE DoTimeMode (Opt:Integer);
            FUNCTION NormalizeToTOD(h: Integer; sec: Double): Double;


     public

       constructor Create(ParClass:TDSSClass; const StorageControllerName:String);
       destructor Destroy; override;

       PROCEDURE RecalcElementData; Override;
       PROCEDURE CalcYPrim; Override;    // Always Zero for a StorageController

       PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state

       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;


   end;


VAR
    ActiveStorageControllerObj:TStorageControllerObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,  Storage, Sysutils, uCmatrix, MathUtil, Math;

CONST

    propELEMENT       = 1;
    propTERMINAL      = 2;
    propKWTARGET      = 3;
    propKWBAND        = 4;
    propPFTARGET      = 5;
    propPFBAND        = 6;
    propELEMENTLIST   = 7;
    propWEIGHTS       = 8;
    propMODEDISCHARGE = 9;
    propMODECHARGE    = 10;
    propTIMEDISCHARGETRIGGER = 11;
    propTIMECHARGETRIGGER    = 12;
    propRATEKW        = 13;
    propRATEKVAR      = 14;
    propRATECHARGE    = 15;
    propRESERVE       = 16;
    propKWHTOTAL      = 17;
    propKWTOTAL       = 18;
    propKWHACTUAL     = 19;
    propKWACTUAL      = 20;
    propKWNEED        = 21;
    propPARTICIPATION = 22;
    propYEARLY        = 23;
    propDAILY         = 24;
    propDUTY          = 25;


    NumPropsThisClass = 25;

//= = = = = = = = = = = = = = DEFINE CONTROL MODES = = = = = = = = = = = = = = = = = = = = = = = = =

    MODEFOLLOW      = 1;
    MODELOADSHAPE   = 2;
    MODESUPPORT     = 3;
    MODETIME        = 4;



{--------------------------------------------------------------------------}
constructor TStorageController.Create;  // Creates superstructure for all StorageController objects
Begin
     Inherited Create;

     Class_name   := 'StorageController';
     DSSClassType := DSSClassType + STORAGE_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TStorageController.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorageController.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names

     PropertyName[propELEMENT]                := 'Element';
     PropertyName[propTERMINAL]               := 'Terminal';
     PropertyName[propKWTARGET]               := 'kWTarget';
     PropertyName[propKWBAND]                 := '%kWBand';
     PropertyName[propPFTARGET]               := 'PFTarget';
     PropertyName[propPFBAND]                 := 'PFBand';
     PropertyName[propELEMENTLIST]            := 'ElementList';
     PropertyName[propWEIGHTS]                := 'Weights';
     PropertyName[propMODEDISCHARGE]          := 'ModeDischarge';
     PropertyName[propMODECHARGE]             := 'ModeCharge';
     PropertyName[propTIMEDISCHARGETRIGGER]   := 'TimeDisChargeTrigger';
     PropertyName[propTIMECHARGETRIGGER]      := 'TimeChargeTrigger';
     PropertyName[propRATEKW]                 := '%RatekW';
     PropertyName[propRATEKVAR]               := '%Ratekvar';
     PropertyName[propRATECHARGE]             := '%RateCharge';
     PropertyName[propRESERVE]                := '%Reserve';
     PropertyName[propKWHTOTAL]               := 'kWhTotal';
     PropertyName[propKWTOTAL]                := 'kWTotal';
     PropertyName[propKWHACTUAL]              := 'kWhActual';
     PropertyName[propKWACTUAL]               := 'kWActual';
     PropertyName[propKWNEED]                 := 'kWneed';
     PropertyName[propPARTICIPATION]          := '%Participation';
     PropertyName[propYEARLY]                 := 'Yearly';
     PropertyName[propDAILY]                  := 'Daily';
     PropertyName[propDUTY]                   := 'Duty';


    PropertyHelp[propELEMENT]             :=
      'Full object name of the circuit element, typically a line or transformer, '+
      'which the control is monitoring. There is no default; must be specified.';
    PropertyHelp[propTERMINAL]            :=
      'Number of the terminal of the circuit element to which the StorageController control is connected. '+
      '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
    PropertyHelp[propKWTARGET]            :=
      'kW target for Discharging. The storage element fleet is dispatched to try to hold the power in band '+
      'at least until the storage is depleted.';
    PropertyHelp[propKWBAND]              :=
      'Bandwidth (% of Target kW) of the dead band around the kW target value. Default is 2% (+/-1%).' +
      'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
    PropertyHelp[propPFTARGET]          :=
      'Power Factor target for dispatching the reactive power. The reactive power of the storage element fleet is dispatched to try to hold the power factor in band. '+
      'It is assumed that the storage element inverter can produce kvar up to its kVA limit regardless of storage level.';
    PropertyHelp[propPFBAND]            :=
      'Bandwidth of the Target power factor of the monitored element. of the dead band around the kvar target value. Default is 0.02 (+/- 0.01%).' +
      'No dispatch changes of the kvar are attempted if the power factor of the monitored terminal stays within this band.';
    PropertyHelp[propELEMENTLIST]         :=
      'Array list of Storage elements to be controlled.  If not specified, all storage elements in the circuit are assumed dispatched by this controller.';
    PropertyHelp[propWEIGHTS]             := 
     'Array of proportional weights corresponding to each storage element in the ElementList. ' +
     'The needed kW or kvar to get back to center band is dispatched to each storage element according to these weights. ' +
     'Default is to set all weights to 1.0.';
    PropertyHelp[propMODEDISCHARGE]       :=
     '{Follow | Support | Loadshape | Time} Mode of operation for the DISCHARGE function of this controller. ' +
     'In Follow mode, the control attempts to discharge storage to keep power in the monitored element below the kWTarget. ' +
     'In Support mode, the control operates oppositely of Follow mode: storage is discharged to keep kW power output up near the target. ' +
     'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
     'Storage is discharged when the loadshape value is positive. ' +
     'In Time mode, the storage discharge is turned on at the specified %RatekW and %Ratekvar at the specified discharge trigger time in fractional hours.';
    PropertyHelp[propMODECHARGE]          :=
     '{Loadshape | Time} Mode of operation for the CHARGE function of this controller. ' +
     'In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. ' +
     'Storage is charged when the loadshape value is negative. ' +
     'In Time mode, the storage charging function is triggered at the specified %RateCharge at the specified sharge trigger time in fractional hours.';
    PropertyHelp[propTIMEDISCHARGETRIGGER]:=
     'Default time of day (hr) for initiating Discharging of the fleet. During Follow or Support modes discharging at a fixed time will occur ' +
     'each day at this hour. Set this to a negative value to ignore. Default is -1 (ignored). ';
    PropertyHelp[propTIMECHARGETRIGGER]   :=
     'Default time of day (hr) for initiating charging in Time control mode. Set this to a negative value to ignore. Default is 2.0.  (0200).' +
     'When this value is >0 the storage fleet is set to charging at this time regardless of other control criteria to make sure storage is ' +
     'topped off for the next discharge cycle.';
    PropertyHelp[propRATEKW]              :=
      'Sets the kW discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode or anytime discharging is triggered ' +
      'by time.';
    PropertyHelp[propRATEKVAR]            :=
      'Sets the kvar discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode or anytime discharging is triggered ' +
      'by time.' ;
    PropertyHelp[propRATECHARGE]          :=
      'Sets the kW charging rate in % of rated capacity for each element of the fleet. Applies to TIME control mode and anytime charging mode is ' +
      'entered due to a time trigger.';
    PropertyHelp[propRESERVE]             :=
       'Use this property to change the % reserve for each storage element under control of this controller. This might be used, for example, to ' +
       'allow deeper discharges of storage or in case of emergency operation to use the remainder of the storage element.';
    PropertyHelp[propKWHTOTAL]            :=
      '(Read only). Total rated kWh energy storage capacity of storage elements controlled by this controller.';
    PropertyHelp[propKWTOTAL]             :=
      '(Read only). Total rated kW power capacity of storage elements controlled by this controller.';
    PropertyHelp[propKWHACTUAL]            :=
      '(Read only). Actual kWh output of all controlled storage elements. ';
    PropertyHelp[propKWACTUAL]            :=
      '(Read only). Actual kW output of all controlled storage elements. ';
    PropertyHelp[propKWNEED]              :=
      '(Read only). KW needed to meet target.';
    PropertyHelp[propPARTICIPATION]       :=
      'Participation factor, %. Default = 100.';
    PropertyHelp[propYEARLY]              :=
      'Dispatch loadshape object, if any, for Yearly solution Mode.';
    PropertyHelp[propDAILY]               :=
      'Dispatch loadshape object, if any, for Daily solution mode.';
    PropertyHelp[propDUTY]                :=
      'Dispatch loadshape object, if any, for Dutycycle solution mode.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.NewObject(const ObjName:String):Integer;
Begin
    // Make a new StorageController and add it to StorageController class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TStorageControllerObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   i:Integer;

Begin

  // continue parsing with contents of Parser
  ActiveStorageControllerObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveStorageControllerObj;

  Result := 0;

  WITH ActiveStorageControllerObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer <= NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 364);
            propELEMENT:  ElementName     := lowercase(param);
            propTERMINAL: ElementTerminal := Parser.IntValue;
            propKWTARGET: FkWTarget        := Parser.DblValue;
            propKWBAND:   FpctkWBand       := Parser.DblValue;
            propPFTARGET: FPFTarget        := Parser.DblValue;
            propPFBAND:   FPFBand          := Parser.DblValue;
            propELEMENTLIST: InterpretTStringListArray(Param, FStorageNameList);
            propWEIGHTS:  Begin
                           FleetSize := FStorageNameList.count;
                           IF FleetSize>0 Then Begin
                           Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
                           InterpretDblArray(Param, FleetSize, FWeights);
                           End;
                         End;
            propMODEDISCHARGE: DisChargeMode := InterpretMode(propMODEDISCHARGE, Param);
            propMODECHARGE:    ChargeMode    := InterpretMode(propMODECHARGE, Param);
            propTIMEDISCHARGETRIGGER: DischargeTriggerTime := Parser.DblValue;
            propTIMECHARGETRIGGER:    ChargeTriggerTime    := Parser.DblValue;
            propRATEKW:      pctkWRate      := Parser.DblValue;
            propRATEKVAR:    pctkvarRate    := Parser.DblValue;
            propRATECHARGE:  pctChargeRate  := Parser.DblValue;
            propRESERVE:     pctFleetReserve:= Parser.DblValue;
            propKWHTOTAL:  ;  // Do nothing (Read ONly)
            propKWTOTAL:   ;  // Do nothing (Read ONly)
            propKWHACTUAL:  ;  // Do nothing (Read ONly)
            propKWACTUAL:  ;  // Do nothing (Read ONly)
            propKWNEED:    ;  // Do nothing (Read ONly)
            propPARTICIPATION: ;
            propYEARLY:  YearlyShape  := Param;
            propDAILY:   DailyShape   := Param;
            propDUTY:    DutyShape    := Param;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveStorageControllerObj, ParamPointer - NumPropsthisClass)
         End;

         // Side effects of setting properties above

         CASE ParamPointer OF
            propKWTARGET,
            propKWBAND: HalfkWBand := FpctkWBand / 2.0 * FkWTarget;
            propPFTARGET,
            propPFBAND: HalfPFBand := FPFBand / 2.0 * FPFTarget;
            propELEMENTLIST:
                   Begin   // levelize the list
                       FleetPointerList.Clear;  // clear this for resetting on first sample
                       FleetListChanged := TRUE;
                       FleetSize := FStorageNameList.count;
                       Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
                       For i := 1 to FleetSize Do FWeights^[i] := 1.0;
                   End;
            propYEARLY:
                   Begin
                       YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                       if YearlyShapeObj = nil then  DoSimpleMsg('Yearly loadshape "' + YearlyShape + '" not found.', 14404);
                   End;
            propDAILY:
                   Begin
                       DailyShapeObj  := LoadShapeClass.Find(DailyShape);
                       if DailyShapeObj = nil then  DoSimpleMsg('Daily loadshape "' + DailyShape + '" not found.', 14405);
                   End;
            propDUTY:
                   Begin
                       DutyShapeObj   := LoadShapeClass.Find(DutyShape);
                       if DutyShapeObj = nil then  DoSimpleMsg('Dutycycle loadshape "' + DutyShape + '" not found.', 14406);
                   End

         ELSE

         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TStorageController.MakeLike(const StorageControllerName:String):Integer;
VAR
   OtherStorageController:TStorageControllerObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this StorageController name in the present collection}
   OtherStorageController := Find(StorageControllerName);
   IF OtherStorageController<>Nil THEN
   WITH ActiveStorageControllerObj Do Begin

        NPhases := OtherStorageController.Fnphases;
        NConds  := OtherStorageController.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherStorageController.ElementName;
        ControlledElement := OtherStorageController.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherStorageController.MonitoredElement;  // Pointer to target circuit element

        ElementTerminal   := OtherStorageController.ElementTerminal;


//**** fill in private properties


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherStorageController.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in StorageController MakeLike: "' + StorageControllerName + '" Not Found.', 370);

End;




{==========================================================================}
{                    TStorageControllerObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TStorageControllerObj.Create(ParClass:TDSSClass; const StorageControllerName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(StorageControllerName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class



     ElementName       := '';
     ControlledElement := nil;  // not used in this control
     ElementTerminal   := 1;
     MonitoredElement  := Nil;

     FStorageNameList := TSTringList.Create;
     FWeights         := Nil;
     FleetPointerList   := PointerList.TPointerList.Create(20);  // Default size and increment
     FleetSize        := 0;
     FkWTarget        := 8000.0;
     FpctkWBand       := 2.0;
     TotalWeight      := 1.0;
     HalfkWBand       := FpctkWBand/2.0 * FkWTarget;
     FPFTarget        := 0.98;
     FPFBand          := 0.02;


     DischargeMode := 1;
     ChargeMode    := 4;

     DischargeTriggerTime := -1.0;  // disabled
     ChargeTriggerTime    := 2.0;   // 2 AM
     FleetListChanged     := FALSE;
     pctkWRate            := 20.0;
     pctkvarRate          := 20.0;
     pctChargeRate        := 20.0;

     InitPropertyValues(0);


   //  RecalcElementData;

End;

destructor TStorageControllerObj.Destroy;
Begin
     ElementName := '';
     YearlyShape := '';
     DailyShape  := '';
     DutyShape   := '';
     YearlyShapeObj.Free;
     DailyShapeObj.Free;
     DutyShapeObj.Free;

     FleetPointerList.Free;
     FStorageNameList.Free;

     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.RecalcElementData;

VAR
   DevIndex :Integer;

Begin


{Check for existence of monitored element}

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0  THEN Begin
             MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
             IF ElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('StorageController: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 371);
             End
             ELSE Begin
               // Sets name of i-th terminal's connected bus in StorageController's buslist
                 Setbus(1, MonitoredElement.GetBus(ElementTerminal));
             End;
         End
         ELSE DoSimpleMsg('Monitored Element in StorageController.'+Name+ ' does not exist:"'+ElementName+'"', 372);

       if FleetListChanged then MakeFleetList; // Need error message ??

       if FleetSize > 0 Then SetAllFleetValues;

End;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);


End;


{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TStorageControllerObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

function TStorageControllerObj.GetkWActual: String;
Var
    pStorage:TStorageObj;
    i       :Integer;
    Sum     :Double;
begin
      Sum := 0.0;
      for I := 1 to FleetPointerList.ListSize do Begin
          pStorage :=  FleetPointerList.Get(i);
          sum := sum + pStorage.PresentkW;
      End;
      Result := Format('%-.8g',[sum]);
end;

function TStorageControllerObj.GetkWhActual: String;
Var
    pStorage:TStorageObj;
    i       :Integer;
    Sum     :Double;
begin
      Sum := 0.0;
      for I := 1 to FleetPointerList.ListSize do Begin
          pStorage :=  FleetPointerList.Get(i);
          sum := sum + pStorage.kWhStored;
      End;
      Result := Format('%-.8g',[sum]);
end;

function TStorageControllerObj.GetkWhTotal: String;
Var
    pStorage:TStorageObj;
    i       :Integer;
    Sum     :Double;
begin
      Sum := 0.0;
      for I := 1 to FleetPointerList.ListSize do Begin
          pStorage :=  FleetPointerList.Get(i);
          sum := sum + pStorage.kWhRating;
      End;
      Result := Format('%-.8g',[sum]);
end;

function TStorageControllerObj.GetkWTotal: String;
Var
    pStorage:TStorageObj;
    i       :Integer;
    Sum     :Double;

begin
      Sum := 0.0;
      for I := 1 to FleetPointerList.ListSize do Begin
          pStorage :=  FleetPointerList.Get(i);
          sum := sum + pStorage.kWRating;
      End;
      Result := Format('%-.8g',[sum]);
end;

function TStorageControllerObj.GetModeString(Opt, Mode: Integer): String;
begin
      Result := '';
      CASE Opt of
          propMODEDISCHARGE:
               CASE Mode of
                    MODEFOLLOW:    Result := 'Follow';
                    MODELOADSHAPE: Result := 'Loadshape';
                    MODESUPPORT:   Result := 'Support';
                    MODETIME:      Result := 'Time';
               ELSE
                   Result := 'UNKNOWN'
               END;
          propMODECHARGE:
               CASE Mode of
                   // 1: Result := 'Follow';
                    MODELOADSHAPE: Result := 'Loadshape';
                  //  3: Result := 'Support';
                    MODETIME: Result := 'Time';
               ELSE
                   Result := 'UNKNOWN'
               END;
      ELSE
           DoSimpleMsg('Unknown Charge/Discharge designation', 14401);
      END;
end;

function TStorageControllerObj.GetPropertyValue(Index: Integer): String;
begin
     Result := '';
     CASE Index of

          propKWTARGET             : Result := Format('%-.6g',[FkWTarget]);
          propKWBAND               : Result := Format('%-.6g',[FpctkWBand]);
          propPFTARGET             : Result := Format('%-.6g',[FPFTarget]);
          propPFBAND               : Result := Format('%-.6g',[FPFBand]);
          propELEMENTLIST          : Result := ReturnElementsList;
          propWEIGHTS              : Result := ReturnWeightsList;
          propMODEDISCHARGE        : Result := GetModeString(propMODEDISCHARGE, DischargeMode);
          propMODECHARGE           : Result := GetModeString(propMODECHARGE,    ChargeMode);
          propTIMEDISCHARGETRIGGER : Result := PropertyValue[Index];
          propTIMECHARGETRIGGER    : Result := PropertyValue[Index];
          propRATEKW               : Result := Format('%-8g',[pctkWRate]);
          propRATEKVAR             : Result := Format('%-8g',[pctkvarRate]);
          propRATECHARGE           : Result := Format('%-8g',[pctChargeRate]);
          propRESERVE              : Result := Format('%-8g',[pctFleetReserve]);
          propKWHTOTAL             : Result := GetkWhTotal;
          propKWTOTAL              : Result := GetkWTotal;
          propKWHACTUAL            : Result := GetkWhActual;
          propKWACTUAL             : Result := GetkWActual;
          propKWNEED               : Result := PropertyValue[Index];
          propPARTICIPATION        : Result := PropertyValue[Index];
          propYEARLY               : Result := PropertyValue[Index];
          propDAILY                : Result := PropertyValue[Index];
          propDUTY                 : Result := PropertyValue[Index];

     ELSE  // take the generic handler
           Result := Inherited GetPropertyValue(index);

     END;
end;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DumpProperties(Var F:TextFile; Complete:Boolean);

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
PROCEDURE TStorageControllerObj.DoPendingAction;
begin

        {Do Nothing}
end;

procedure TStorageControllerObj.DoTimeMode(Opt: Integer);
begin

      CASE Opt of

          1:Begin
             if DisChargeTriggerTime > 0.0 then
               WITH ActiveCircuit.Solution Do Begin
               if abs(NormalizeToTOD(intHour, DynaVars.t) - DisChargeTriggerTime) < DynaVars.h/3600.0 then SetFleetToDischarge;
               End;
            End; // Discharge mode
          2:Begin
            if ChargeTriggerTime > 0.0 then
               WITH ActiveCircuit.Solution Do Begin
               if abs(NormalizeToTOD(intHour, DynaVars.t) - ChargeTriggerTime) < DynaVars.h/3600.0 then SetFleetToCharge;
               End;
            End; //Charge mode
      END;
end;

//----------------------------------------------------------------------------
FUNCTION TStorageControllerObj.NormalizeToTOD(h: Integer; sec: Double): Double;
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

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.Sample;

Begin
       ChargingAllowed := FALSE;

       CASE DischargeMode of
            MODEFOLLOW:    DoLoadFollowMode;
            MODELOADSHAPE: DoLoadShapeMode;
            MODESUPPORT:   DoLoadFollowMode;
            MODETIME:      DoTimeMode(1);
       ELSE
           DoSimpleMsg(Format('Invalid DisCharging Mode: %d',[DisChargeMode]));
       END;

       If ChargingAllowed then
       CASE ChargeMode of
          //  MODELOADSHAPE:DoLoadShapeMode;
            MODETIME:DoTimeMode(2);
       ELSE
           DoSimpleMsg(Format('Invalid Charging Mode: %d',[ChargeMode]));
       END;


End;
{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DoLoadFollowMode;

VAR
   i           :Integer;
   PDiff,
   QDiff       :Double;
   S           :Complex ;
   StorageObj         :TSTorageObj;
   StorekWChanged, StorekvarChanged: Boolean;
   DispatchkW, Dispatchkvar :Double;


begin
     // If list is not defined, go make one from all storage elements in circuit
     IF FleetPointerList.ListSize=0 Then  MakeFleetList;

     If FleetSize>0 Then
     Begin

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
       S := MonitoredElement.Power[ElementTerminal];  // Power in active terminal
       case  DischargeMode of
             // Following Load; try to keep load below kW Target
             MODEFOLLOW: Begin
                              PDiff := S.re * 0.001 - FkWTarget;
                              QDiff := S.re/Cabs(S) - FPFTarget;
                         End;
             // supporting DG; Try to keep load above kW target
             MODESUPPORT:Begin
                              PDiff := -S.re * 0.001 + FkWTarget;
                              QDiff := -S.re/Cabs(S) + FPFTarget;
                         End;
       end;


       StorekWChanged := FALSE;
       StorekvarChanged := FALSE;

       // kw dispatch
       If Abs(PDiff) > HalfkWBand Then
         Begin // Redispatch Storage
              // PDiff is kW needed to get back into band
              For i := 1 to FleetSize Do
              Begin
                    StorageObj := FleetPointerList.Get(i);
                    // compute new dispatch value for this storage element ...
                    DispatchkW := Max(0.0, (StorageObj.PresentkW + PDiff *(FWeights^[i]/TotalWeight)));
                    If DispatchkW <> StorageObj.PresentkW Then
                    Begin
                          StorageObj.pctkWout   := DispatchkW/StorageObj.kWrating * 100.0;  //****  Will this work?  Maybe
                          StorekWChanged := TRUE;
                    End;
              End;
         End;

       // kvar dispatch
       If Abs(QDiff) > HalfkWBand Then
         Begin // Redispatch Storage elents
              // QDiff is kvar needed to get back into band
              For i := 1 to FleetSize Do
              Begin
                    StorageObj := FleetPointerList.Get(i);
                    // compute new dispatch value for this generator ...
                    Dispatchkvar := Max(0.0, (StorageObj.Presentkvar + QDiff *(FWeights^[i]/TotalWeight)));
                    If Dispatchkvar <> StorageObj.Presentkvar Then
                    Begin
                          StorageObj.Presentkvar := Dispatchkvar;
                          StorekvarChanged := TRUE;
                    End;
              End;
         End;

       If StorekWChanged or StorekvarChanged Then  // Only push onto controlqueue if there has been a change
          With ActiveCircuit, ActiveCircuit.Solution Do Begin
            LoadsNeedUpdating := TRUE; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
            ControlQueue.Push(intHour, DynaVars.t, 0, 0, Self);
          End;
       

       {Else just continue}
    End;


end;


procedure TStorageControllerObj.DoLoadShapeMode;
begin

    // Get multiplier

    if LoadShapeMult < 0.0 then ChargingAllowed := TRUE;
    

end;

procedure TStorageControllerObj.SetAllFleetValues;
Var
      i   :Integer;
begin
      for i := 1 to FleetPointerList.ListSize do
           WITH TStorageObj(FleetPointerList.Get(i)) Do
             Begin
                pctkWin := pctChargeRate;
                pctkvarout := pctkWRate;
                pctkWout := pctkvarRate;
                pctReserve := pctFleetReserve;
             End;
end;

procedure TStorageControllerObj.SetFleetChargeRate;
Var
      i   :Integer;
begin
      for i := 1 to FleetPointerList.ListSize do
            TStorageObj(FleetPointerList.Get(i)).pctkWin := pctChargeRate;
end;

procedure TStorageControllerObj.SetFleetkvarRate;
Var
      i   :Integer;
begin
      for i := 1 to FleetPointerList.ListSize do
            TStorageObj(FleetPointerList.Get(i)).pctkvarout := pctkWRate;
end;

procedure TStorageControllerObj.SetFleetkWRate;
Var
      i   :Integer;
begin
      for i := 1 to FleetPointerList.ListSize do
            TStorageObj(FleetPointerList.Get(i)).pctkWout := pctkvarRate;
end;

procedure TStorageControllerObj.SetFleetToCharge;
Var
      i   :Integer;
begin
      for i := 1 to FleetPointerList.ListSize do
            TStorageObj(FleetPointerList.Get(i)).StorageState := STATE_CHARGING;
end;

procedure TStorageControllerObj.SetFleetToDisCharge;
Var
      i   :Integer;
begin
      for i := 1 to FleetPointerList.ListSize do
            TStorageObj(FleetPointerList.Get(i)).StorageState := STATE_DISCHARGING;
end;

procedure TStorageControllerObj.SetFleetToIdle;
Var
      i   :Integer;
begin
      for i := 1 to FleetPointerList.ListSize do
            TStorageObj(FleetPointerList.Get(i)).StorageState := STATE_IDLING;
end;

procedure TStorageControllerObj.SetPctReserve;
Var
      i   :Integer;
begin
      for i := 1 to FleetPointerList.ListSize do
            TStorageObj(FleetPointerList.Get(i)).pctReserve := pctFleetReserve;
end;

procedure TStorageControllerObj.InitPropertyValues(ArrayOffset: Integer);
begin


     PropertyValue[propELEMENT]              :='';
     PropertyValue[propTERMINAL]             :='1';
     PropertyValue[propKWTARGET]             :='8000';
     PropertyValue[propKWBAND]               :='2';
     PropertyValue[propPFTARGET]             :='.98';
     PropertyValue[propPFBAND]               :='.02';
     PropertyValue[propELEMENTLIST]          :='';
     PropertyValue[propWEIGHTS]              :='';
     PropertyValue[propMODEDISCHARGE]        :='Follow';
     PropertyValue[propMODECHARGE]           :='Time';
     PropertyValue[propTIMEDISCHARGETRIGGER] :='-1';
     PropertyValue[propTIMECHARGETRIGGER]    :='2';
     PropertyValue[propRATEKW]               :='20';
     PropertyValue[propRATEKVAR]             :='20';
     PropertyValue[propRATECHARGE]           :='20';
     PropertyValue[propRESERVE]              :='25';
     PropertyValue[propKWHTOTAL]             :='';
     PropertyValue[propKWTOTAL]              :='';
     PropertyValue[propKWACTUAL]             :='';
     PropertyValue[propKWNEED]               :='';
     PropertyValue[propPARTICIPATION]        :='';
     PropertyValue[propYEARLY]               :='';
     PropertyValue[propDAILY]                :='';
     PropertyValue[propDUTY]                 :='';


  inherited  InitPropertyValues(NumPropsThisClass);

end;

function TStorageControllerObj.InterpretMode(Opt: Integer;
  const S: String): Integer;
begin

   Result := -1;  // Unknown: error
   CASE Opt of
        propMODEDISCHARGE:
              CASE LowerCase(S)[1] of
                  'f': Result := 1;
                  'l': Result := 2;
                  's': Result := 3;
                  't': Result := 4;
              ELSE
                  DoSimpleMsg('Discharge Mode "' + S + '" not recognized.', 14402);
              END;
        propMODECHARGE:
              CASE LowerCase(S)[1] of
                 // 'f': Result := 1;
                  'l': Result := 2;
                 // 's': Result := 3;
                  't': Result := 4;
              ELSE
                  DoSimpleMsg('Discharge Mode "' + S + '" not recognized.', 14402);
              END;
   ELSE
   END;
end;

Function TStorageControllerObj.MakeFleetList:Boolean;

VAR
   StorageObj:TStorageObj;
   i:Integer;

begin

   Result := FALSE;

   If FleetSize>0 Then Begin    // Name list is defined - Use it
     FleetPointerList.Clear;
     For i := 1 to FleetSize Do
       Begin
             StorageObj := StorageClass.Find(FStorageNameList.Strings[i-1]);
             If Assigned(StorageObj) Then Begin
                If StorageObj.Enabled Then FleetPointerList.New := StorageObj;
             End Else Begin
               DoSimpleMsg('Error: Storage Element "' + FStorageNameList.Strings[i-1] + '" not found.', 14403);
               Exit;
             End;
       End;

   End
   Else Begin
     {Search through the entire circuit for enabled Storage Elements and add them to the list}
     
     For i := 1 to StorageClass.ElementCount Do Begin
        StorageObj :=  StorageClass.ElementList.Get(i);
        If StorageObj.Enabled Then FleetPointerList.New := StorageObj;
     End;

     {Allocate uniform weights}
     FleetSize := FleetPointerList.ListSize;
     Reallocmem(FWeights, Sizeof(FWeights^[1])*FleetSize);
     For i := 1 to FleetSize Do FWeights^[i] := 1.0;

   End;

   // Add up total weights
   TotalWeight := 0.0;
   For i := 1 to FleetSize Do  TotalWeight := TotalWeight + FWeights^[i];

   If FleetPointerList.ListSize>0 Then Result := TRUE;
end;



procedure TStorageControllerObj.Reset;
begin
  // inherited;

end;



function TStorageControllerObj.ReturnElementsList: String;
Var
     i :Integer;
begin
     if FleetSize=0 then
       Begin
            Result := '';
            Exit;
       End;

     Result := '['+ FStorageNameList.Strings[0];
     For i := 2 to FleetSize Do
       Begin
             Result := Result + ', ' + FStorageNameList.Strings[i-1];
       End;
     Result := Result + ']';  // terminate the array

end;

function TStorageControllerObj.ReturnWeightsList: String;
Var
     i :Integer;
begin
     if FleetSize=0 then
       Begin
            Result := '';
            Exit;
       End;

     Result := '['+ Format('%-.6g',[FWeights^[1]]);
     For i := 2 to FleetSize Do
       Begin
             Result := Result  + Format(', %-.6g',[FWeights^[i]]);
       End;
     Result := Result + ']';  // terminate the array
end;

INITIALIZATION




end.
