unit EnergyMeter;
{
     This class of device accumulates the energy of the voltage and current in the
     terminal of the device to which it is connected.

     It is an intelligent energy meter capable of measuring losses of all
     devices within its "zone".

     The Zone is determined automatically after a circuit change.  The Zone starts on the
     opposite side of the branch on which the meter is located and continues in the same
     direction through the network until
       a) an open point is encountered
       b) an open terminal or switch is encountered
       c) another energy meter is encountered
       d) a branch that is already included in a zone is encountered

     It keeps track of kwh, kvarh, UE,  EEN, Losses, etc., having registers FOR each
     of these quantities.

     In EEN/UE calculations, line overload takes precedence.

     If the Max Zone kW limits are specified, then these replace the line overload UE/EEN numbers.
     These limits were added so that the user can override line limits in cases
     such as networks where it is difficult to judge the UE from the individual
     line limits.

     Only the maximum |kVA| overload is accumulated, not all.  Loads downline from
     an overload are marked WITH a factor representing the degree of overload.  This
     is used to compute EEN/UE FOR loads.

     FOR low voltages, the full kW FOR loads below the emergency min voltage are counted.
     The EEN is proportioned based on how low the voltage is.

     Emergency min voltage must be less than normal min voltage.

}

{                 CHANGE LOG

8-3-99  Added Option property
        Revised EEN/UE computation to do either total or excess
8-4-99 Save always rewrites file now and returns file name.

11-11-99 Fixed bug in Take sample to use the maxvalue of the overload_EEN

1-4-99  Modified tree checking to avoid picking up the same load more than once
        Fixed bugs in sampling load EEN/UE
        Modified Overload UE; added kwnormal, kwemerg properties for whole zone

1-28-00 Changed to derived from Meter Element
2-2-00  Trapezoidal Integration option
4-14-00 Added load allocation algorithm
4-17-00 Removed shunt capacitors from meter zones
5-3-00  Corrected Zone kW, kvar accumulation to be actual power not target power
5-29-00 Fixed problem with Nphases not being set right for 1-phase devices.
6-15-01 Added Zonelist and LocalOnly options
7/6/01  Added Voltage Only option for Load UE calcs.
7/19/01 Added Totalizer Function for meterclass
7/24/01 Added Generator registers and code for adding generators to zone lists.
        Changed to use zone loads and gens even if local only. If you only want the local
        measurements, specify a null zone manually.
8/2/01  Fixed hole in Local only options.
4/29/03 Added ReduceZone Function
2/7/07  Fixed overload formulas
9/18/08 Added 4 load loss and no load loss registers
}

{$WARN UNIT_PLATFORM OFF}

interface

Uses DSSClass, MeterClass, MeterElement, CktElement, PDElement, arrayDef,
     PointerList, CktTree, ucomplex, Feeder,
     Load, Generator, Command;

Const  NumEMRegisters = 24;    // Number of energy meter registers

     Reg_kWh               = 1;
     Reg_kvarh             = 2;
     Reg_MaxkW             = 3;
     Reg_MaxkVA            = 4;
     Reg_ZonekWh           = 5;
     Reg_Zonekvarh         = 6;
     Reg_ZoneMaxkW         = 7;
     Reg_ZoneMaxkVA        = 8;
     Reg_OverloadkWhNorm   = 9;    // Max overload
     Reg_OverloadkWhEmerg  = 10;
     Reg_LoadEEN           = 11;
     Reg_LoadUE            = 12;  // Energy served below normal voltage
     Reg_LosseskWh         = 13;
     Reg_Losseskvarh       = 14;
     Reg_LossesMaxkW       = 15;
     Reg_LossesMaxkvar     = 16;
     Reg_GenkWh            = 17;
     Reg_Genkvarh          = 18;
     Reg_GenMaxkW          = 19;
     Reg_GenMaxkVA         = 20;
     Reg_LoadLosseskWh     = 21;
     Reg_NoLoadLosseskWh   = 22;
     Reg_MaxLoadLosses     = 23;
     Reg_MaxNoLoadLosses   = 24;


Type
   TRegisterArray = Array[1..NumEMregisters] of Double;

   TSystemMeter = Class(Tobject)
     private
        kWh, dkWh,
        kvarh, dkvarh,
        peakkW,
        peakkVA,
        Losseskwh,  dLosseskWh,
        Losseskvarh, dlosseskvarh,
        PeakLosseskW            :Double;
        FirstSampleAfterReset,
        This_Meter_DIFileIsOpen :Boolean;
        SystemDIFile            :TextFile;
        cPower, cLosses         :Complex;

        Procedure Clear;
        Procedure Integrate(Var Reg:Double; Value:Double; Var Deriv:Double);
        Procedure WriteRegisters(Var F:TextFile);
        Procedure WriteRegisterNames(Var F:TextFile);

     protected
        Procedure OpenDemandIntervalFile;
        Procedure WriteDemandIntervalData;
        Procedure CloseDemandIntervalFile;
        Procedure AppendDemandIntervalFile;

     public

       Procedure TakeSample;
       Procedure Reset;
       Procedure Save;

       constructor Create;
       destructor Destroy; override;

   end;


   TEnergyMeter = class(TMeterClass)    // derive strait from base class
     private
        GeneratorClass:TGenerator;
        FSaveDemandInterval: Boolean;
        FDI_Verbose: Boolean;
        PROCEDURE ProcessOptions(Const Opts:String);
        procedure Set_SaveDemandInterval(const Value: Boolean);
        Procedure CreateMeterTotals;
        Procedure CreateFDI_Totals;
        Procedure ClearDI_Totals;
        Procedure WriteTotalsFile;
        Procedure InterpretRegisterMaskArray(Var Mask:TRegisterArray);
        procedure Set_DI_Verbose(const Value: Boolean);
     Protected
        Procedure DefineProperties;
        Function MakeLike(Const EnergyMeterName:String):Integer;   Override;
        procedure  SetHasMeterFlag;
     public
       RegisterNames      :Array[1..NumEMregisters] of String;
       DI_RegisterTotals  :TRegisterArray;

       DI_Dir        :String;
       FDI_Totals    :TextFile;
       FMeterTotals  :TextFile;
       SystemMeter   :TSystemMeter;

       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       Procedure ResetMeterZonesAll;
       Procedure ResetAll;  Override;  // Reset all meters in active circuit to zero
       Procedure SampleAll; Override;   // Force all meters in active circuit to sample
       Procedure SaveAll;   Override;

       Procedure AppendAllDIFiles;
       Procedure OpenAllDIFiles;
       Procedure CloseAllDIFiles;

       Property SaveDemandInterval:Boolean Read FSaveDemandInterval Write Set_SaveDemandInterval;
       Property DI_Verbose:Boolean Read FDI_Verbose Write Set_DI_Verbose;

   end;

   TEnergyMeterObj = class(TMeterElement)
      Private
       FirstSampleAfterReset :Boolean;
       ExcessFlag            :Boolean;
       ZoneIsRadial          :Boolean;
       VoltageUEOnly         :Boolean;
       LocalOnly             :Boolean;
       HasFeeder             :Boolean;
       MeteredElementChanged :Boolean;
       FeederObj             :TFeederObj;
       DefinedZoneList       :pStringArray;
       DefinedZoneListSize   :Integer;

       {Limits on the entire load in the zone for networks where UE cannot be determined
        by the individual branches}
       MaxZonekVA_Norm   :Double;
       MaxZonekVA_Emerg  :Double;

       PeakCurrent            :pDoubleArray;
       PhaseAllocationFactor  :pDoubleArray;
       MeteredCurrent         :pComplexArray;

       {Demand Interval File variables}
       DI_File                 :TextFile;
       This_Meter_DIFileIsOpen :Boolean;

       Procedure Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);
       Procedure SetDragHandRegister( Reg:Integer; const Value:Double);
       Procedure Integrate_Load(pLoad:TLoadObj; var TotalZonekW, TotalZonekvar:Double);
       Procedure Integrate_Gen(pGen:TGeneratorObj; var TotalZonekW, TotalZonekvar:Double);
       Procedure CalcBusCoordinates(StartBranch:TCktTreeNode; FirstCoordRef, SecondCoordRef, LineCount:Integer);

       Function MakeDIFileName:String;

       Procedure MakeFeederObj;
       Procedure RemoveFeederObj;

      Protected

        Procedure OpenDemandIntervalFile;
        Procedure WriteDemandIntervalData;
        Procedure CloseDemandIntervalFile;
        Procedure AppendDemandIntervalFile;

      Public

        BranchList     :TCktTree;      // Pointers to all circuit elements in meter's zone

        Registers      :TRegisterArray;
        Derivatives    :TRegisterArray;
        TotalsMask     :TRegisterArray;

        constructor Create(ParClass:TDSSClass; const EnergyMeterName:String);
        destructor Destroy; override;

        Procedure RecalcElementData;Override;
        Procedure CalcYPrim;Override;
        Procedure GetCurrents(Curr: pComplexArray); Override; //Get present value of terminal Curr
        Procedure GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

        Procedure ResetRegisters;
        Procedure TakeSample;Override;
        Procedure SaveRegisters;
        Procedure MakeMeterZoneLists;
        Procedure ZoneDump;
        Procedure InterpolateCoordinates;
        Procedure EnableFeeder;

        Procedure AllocateLoad;
        Procedure ReduceZone;  // Reduce Zone by eliminating buses and merging lines
        Procedure SaveZone(const dirname:String);

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

VAR
   ActiveEnergyMeterObj  :TEnergyMeterObj;
  { RegisterNameList      :TCommandList; }


implementation
USES  ParserDel, DSSGlobals, Bus, Sysutils, Math, MathUtil,  UCMatrix,
      Utilities, PCElement,  StackDef, Circuit, Line,
      Classes, FileCtrl, ReduceAlgs, Windows;


Const NumPropsThisClass = 11;

VAR

   NumPDElements  :Integer;
   Delta_Hrs      :Double;
   PCElementList  :TList;
   PDElementList  :TList;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TEnergyMeter.Create;  // Creates superstructure FOR all EnergyMeter objects

Begin
     Inherited Create;
     Class_Name := 'EnergyMeter';
     DSSClassType := DSSClassType + ENERGY_METER;

     ActiveElement := 0;

     FSaveDemandInterval := FALSE;
     FDI_Verbose := FALSE;
     
     DI_Dir := '';
     
     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

     // Set Register names
     RegisterNames[1]  := 'kWh';
     RegisterNames[2]  := 'kvarh';
     RegisterNames[3]  := 'Max kW';
     RegisterNames[4]  := 'Max kVA';
     RegisterNames[5]  := 'Zone kWh';
     RegisterNames[6]  := 'Zone kvarh';
     RegisterNames[7]  := 'Zone Max kW';
     RegisterNames[8]  := 'Zone Max kVA';
     RegisterNames[9]  := 'Overload kWh Normal';
     RegisterNames[10] := 'Overload kWh Emerg';
     RegisterNames[11] := 'Load EEN';
     RegisterNames[12] := 'Load UE';
     RegisterNames[13] := 'Zone Losses kWh';
     RegisterNames[14] := 'Zone Losses kvarh';
     RegisterNames[15] := 'Zone Max kW Losses';
     RegisterNames[16] := 'Zone Max kvar Losses';
     RegisterNames[17] := 'Gen kWh';
     RegisterNames[18] := 'Gen kvarh';
     RegisterNames[19] := 'Gen Max kW';
     RegisterNames[20] := 'Gen Max kVA';
     RegisterNames[21] := 'Load Losses kWh';
     RegisterNames[22] := 'No Load Losses kWh';
     RegisterNames[23] := 'Max kW Load Losses';
     RegisterNames[24] := 'Max kW No Load Losses';

     GeneratorClass := DSSClassList.Get(ClassNames.Find('generator'));

     SystemMeter := TSystemMeter.Create;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TEnergyMeter.Destroy;

Begin
    SystemMeter.Free;
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeter.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName^[1] := 'element';
     PropertyName^[2] := 'terminal';
     PropertyName^[3] := 'action';
     PropertyName^[4] := 'option';
     PropertyName^[5] := 'kVAnormal';
     PropertyName^[6] := 'kVAemerg';
     PropertyName^[7] := 'peakcurrent';
     PropertyName^[8] := 'Zonelist';
     PropertyName^[9] := 'LocalOnly';
     PropertyName^[10] := 'Mask';
     PropertyName^[11] := 'Feeder';

     PropertyHelp[1] := 'Name (Full Object name) of element to which the monitor is connected.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the monitor is connected. '+
                    '1 or 2, typically.';
     PropertyHelp[3] := '{Clear (reset) | Save | Take | Zonedump | Allocate | Reduce} ' + CRLF + CRLF +
                    '(A)llocate = Allocate loads on the meter zone to match PeakCurrent.' + CRLF +
                    '(C)lear = reset all registers to zero' + CRLF +
                    '(R)educe = reduces zone by merging lines (see Set Keeplist & ReduceOption)' + CRLF +
                    '(S)ave = saves the current register values to a file.' + CRLF +
                    '   File name is "MTR_metername.CSV".' +CRLF +
                    '(T)ake = Takes a sample at present solution' + CRLF +
                    '(Z)onedump = Dump names of elements in meter zone to a file' + CRLF +
                    '   File name is "Zone_metername.CSV".';
      PropertyHelp[4] := 'Enter a string ARRAY of any combination of the following. Options processed left-to-right:' + CRLF + CRLF +
                    '(E)xcess : (default) UE/EEN is estimate of energy over capacity ' + CRLF +
                    '(T)otal : UE/EEN is total energy after capacity exceeded'+ CRLF +
                    '(R)adial : (default) Treats zone as a radial circuit'+ CRLF +
                    '(M)esh : Treats zone as meshed network (not radial).' +CRLF+
                    '(C)ombined : (default) Load UE/EEN computed from combination of overload and undervoltage.'+ CRLF +
                    '(V)oltage : Load UE/EEN computed based on voltage only.'+CRLF+CRLF+
                    'Example: option=(E, R)';
      PropertyHelp[5] := 'Upper limit on kVA load in the zone, Normal configuration. Default is 0.0 (ignored). ' +
                         'Overrides limits on individual lines for overload EEN. ' +
                         'With "LocalOnly=Yes" option, uses only load in metered branch.';
      PropertyHelp[6] := 'Upper limit on kVA load in the zone, Emergency configuration. Default is 0.0 (ignored). ' +
                         'Overrides limits on individual lines for overload UE. ' +
                         'With "LocalOnly=Yes" option, uses only load in metered branch.';
      PropertyHelp[7] := 'ARRAY of current magnitudes representing the peak currents measured at this location ' +
                         'for the load allocation function.  Default is (400, 400, 400). Enter one current for each phase';
      PropertyHelp[8] := 'ARRAY of full element names for this meter''s zone.  Default is for meter to find it''s own zone. ' +
                         'If specified, DSS uses this list instead.  Can access the names in a single-column text file.  Examples: ' + crlf + crlf+
                         'zonelist=[line.L1, transformer.T1, Line.L3] ' + CRLF +
                         'zonelist=(file=branchlist.txt)';
      PropertyHelp[9] := '{Yes | No}  Default is NO.  If Yes, meter considers only the monitored element ' +
                         'for EEN and UE calcs.  Uses whole zone for losses.';
      PropertyHelp[10]:= 'Mask for adding registers whenever all meters are totalized.  Array of floating point numbers ' +
                         'representing the multiplier to be used for summing each register from this meter. ' +
                         'Default = (1, 1, 1, 1, ... ).  You only have to enter as many as are changed (positional). ' +
                         'Useful when two meters monitor same energy, etc.';
      PropertyHelp[11]:= '{Yes/True | No/False}  Default is NO. If set to Yes, a Feeder object is created corresponding to ' +
                         'the energymeter.  Feeder is enabled if Radial=Yes; diabled if Radial=No.  Feeder is ' +
                         'synched automatically with the meter zone.  Do not create feeders for zones in meshed transmission systems.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEnergyMeter.NewObject(const ObjName:String):Integer;
Begin
   // create a new object of this class and add to list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TEnergyMeterObj.Create(Self, ObjName);
      Result           := AddObjectToList(ActiveDSSObject);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEnergyMeter.Edit:Integer;

VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

   DoRecalc :Boolean;

Begin

  // continue parsing WITH contents of Parser
  // continue parsing WITH contents of Parser
  ActiveEnergyMeterObj           := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveEnergyMeterObj;

  Result   := 0;

  DoRecalc := FALSE;

  WITH ActiveEnergyMeterObj DO
  Begin

     MeteredElementChanged := FALSE;
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         IF (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 520);
            1: ElementName := lowercase(param);
            2: MeteredTerminal := Parser.IntValue;
            3: Begin  {Actions}
                  param := lowercase(param);
                  CASE param[1] of
                    'a': AllocateLoad;
                    'c': ResetRegisters;
                    'r': ReduceZone;
                    's': SaveRegisters;
                    't': TakeSample;
                    'z': ZoneDump;
                  End;
               End;
            4: ProcessOptions(Param);
            5: MaxZonekVA_Norm  := Parser.DblValue;
            6: MaxZonekVA_Emerg := Parser.DblValue;
            7: parser.ParseAsVector(Fnphases, PeakCurrent);
            8: InterpretAndAllocStrArray(Param, DefinedZoneListSize, DefinedZoneList);
            9: LocalOnly := InterpretYesNo(Param);
           10: InterpretRegisterMaskArray(TotalsMask);
           11: HasFeeder := InterpretYesNo(Param);
         ELSE
           ClassEdit(ActiveEnergyMeterObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer OF
             1,2: Begin
                     MeteredElementChanged := TRUE;
                     DoRecalc := TRUE;
                  End;
             11: If HasFeeder Then DoRecalc := True Else RemoveFeederObj;
         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     If DoRecalc Then RecalcElementData;   // When some basic data have changed
  End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEnergyMeter.MakeLike(Const EnergyMeterName:String):Integer;
VAR
   OtherEnergyMeter:TEnergyMeterObj;
   i:Integer;
Begin
   Result := 0;
   {See IF we can find this EnergyMeter name in the present collection}
   OtherEnergyMeter := Find(EnergyMeterName);
   IF OtherEnergyMeter<>NIL Then
   WITH ActiveEnergyMeterObj DO
   Begin

       NPhases       := OtherEnergyMeter.Fnphases;
       NConds  := OtherEnergyMeter.Fnconds; // Force Reallocation of terminal stuff

       ElementName     := OtherEnergyMeter.ElementName;
       MeteredElement  := OtherEnergyMeter.MeteredElement;  // Pointer to target circuit element
       MeteredTerminal        := OtherEnergyMeter.MeteredTerminal;
       ExcessFlag      := OtherEnergyMeter.ExcessFlag;

       MaxZonekVA_Norm  := OtherEnergyMeter.MaxZonekVA_Norm;
       MaxZonekVA_Emerg := OtherEnergyMeter.MaxZonekVA_emerg;

       FreeStringArray(DefinedZoneList, DefinedZoneListSize);
       DefinedZoneListSize    := OtherEnergyMeter.DefinedZoneListSize;
       DefinedZoneList        := AllocStringArray(DefinedZoneListSize);
       // Copy Strings over (actually incr ref count on string)
       For i := 1 to DefinedZoneListSize Do  DefinedZoneList^[i] := OtherEnergyMeter.DefinedZoneList^[i];

       LocalOnly       := OtherEnergyMeter.LocalOnly;
       VoltageUEOnly   := OtherEnergyMeter.VoltageUEOnly;

       FOR i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherEnergyMeter.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in EnergyMeter MakeLike: "' + EnergyMeterName + '" Not Found.', 521);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEnergyMeter.Init(Handle:Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TEnergyMeter.Init', -1);
   Result := 0;
End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeter.ResetMeterZonesAll;  // Force all EnergyMeters in the circuit to reset their meter zones

VAR
   mtr :TEnergyMeterObj;
   pCktElement  :TCktElement;
   i :Integer;

Begin
// Have to use For.. loop here to get pointers because this list gets scanned
// inside each element



     WITH ActiveCircuit Do
     Begin
         If Energymeters.ListSize=0 Then Exit;  // Do not do anything
         NumPDElements := PDelements.ListSize;

         // initialize the Checked Flag FOR all circuit Elements
         pCktElement := CktElements.First;
         WHILE  (pCktElement <> NIL) Do
         Begin
             With pCktElement Do
             Begin
               Checked := False;
               IsIsolated := TRUE;
               For i := 1 to NTerms Do Terminals^[i].Checked := FALSE;
             End;
             pCktElement := CktElements.Next;
         End;

         {Set up the PCElementList and PDElementList for faster searching}
         PCElementList.Clear;
         PCElementList.Capacity := PCElements.ListSize ;
         pCktElement := PCElements.First;
         While pCktElement<> Nil Do Begin
           If pCktElement.Enabled Then PCelementList.Add(pCktElement);
           pcktElement := PCElements.Next;
         End;

         PDElementList.Clear;
         PDElementList.Capacity := PDElements.ListSize ;
         pCktElement := PDElements.First;
         {Put only eligible PDElements in the list}
         While pCktElement<> Nil Do
          Begin
           If pCktElement.Enabled Then
            If  IsShuntElement(pCktElement) Then  Begin
               PCelementList.Add(pCktElement); // Put shunt elements in PCelement list
            End
            Else Begin
               IF AllTerminalsClosed(pCktElement) THEN PDelementList.Add(pCktElement);
            End;
           pcktElement := PDElements.Next;
          End;

         {Set Hasmeter flag for all cktelements}
         SetHasMeterFlag;

         // initialize the Checked Flag for all Buses
         FOR i := 1 to NumBuses Do   Buses^[i].BusChecked := False;

         FOR i := 1 TO EnergyMeters.ListSize DO Begin
            mtr :=  EnergyMeters.Get(i);
            IF Mtr.Enabled Then mtr.MakeMeterZoneLists;
         END;

         PCElementList.Clear;
         PDElementList.Clear;

      End;
End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeter.ResetAll;  // Force all EnergyMeters in the circuit to reset

VAR
   mtr:TEnergyMeterObj;

Begin

      If DIFilesAreOpen Then CloseAllDIFiles;

      If FSaveDemandInterval Then   Begin

          {Make directories to save data}

            If not DirectoryExists(ActiveCircuit.CaseName) Then Begin
              Try
                 mkDir(ActiveCircuit.CaseName);
              Except
                 On E:Exception Do DoSimpleMsg('Error making  Directory: "'+ActiveCircuit.CaseName+'". ' + E.Message, 522);
              End;
            End;
            DI_Dir  := ActiveCircuit.CaseName+'\DI_yr_' + Trim( IntToStr(ActiveCircuit.Solution.Year));
            If not DirectoryExists(DI_Dir) Then Begin
              Try
                 mkDir(DI_Dir);
              Except
                 On E:Exception Do DoSimpleMsg('Error making Demand Interval Directory: "'+DI_Dir+'". ' + E.Message, 523);
              End;
            End;


            CreateFDI_Totals;
            CloseFile(FDI_Totals);

      End;

      mtr := ActiveCircuit.EnergyMeters.First;
      WHILE mtr<>NIL DO
      Begin
          mtr.ResetRegisters;
          mtr := ActiveCircuit.EnergyMeters.Next;
      End;

      SystemMeter.Reset;


      // Reset Generator Objects, too
      GeneratorClass.ResetRegistersAll;


End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeter.SampleAll;  // Force all EnergyMeters in the circuit to take a sample

VAR
   mtr:TEnergyMeterObj;
   i:Integer;

Begin

      mtr := ActiveCircuit.EnergyMeters.First;
      WHILE mtr<>NIL DO Begin
          IF mtr.enabled Then mtr.TakeSample;
          mtr := ActiveCircuit.EnergyMeters.Next;
      End;

      SystemMeter.TakeSample;

      If FSaveDemandInterval Then Begin  {Write Totals Demand interval file}
        With ActiveCircuit.Solution  Do Write(FDI_Totals, Format('%-.6g, ',[(Hour + dynavars.t/3600.0)]));
        For i := 1 to NumEMRegisters Do Write(FDI_Totals, Format('%-.6g, ',[DI_RegisterTotals[i]]));
        Writeln(FDI_Totals);
        ClearDI_Totals;
      End;

      // Sample Generator Objects, too
      GeneratorClass.SampleAll;

End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeter.SaveAll;  // Force all EnergyMeters in the circuit to take a sample

VAR
   mtr:TEnergyMeterObj;

Begin
    mtr := ActiveCircuit.EnergyMeters.First;
    WHILE mtr<>NIL DO
    Begin
        IF mtr.enabled Then mtr.SaveRegisters;
        mtr := ActiveCircuit.EnergyMeters.Next;
    End;

    SystemMeter.Save;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TEnergyMeter Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEnergyMeterObj.Create(ParClass:TDSSClass; const EnergyMeterName:String);

VAR
   i :Integer;

Begin
     Inherited Create(ParClass);
     Name       := LowerCase(EnergyMeterName);
     DSSObjType := ParClass.DSSClassType; //ENERGY_METER;

     NPhases      := 3;  // Directly set conds and phases
     Fnconds       := 3;
     Nterms := 1;  // this forces allocation of terminals and conductors
                         // in base class
     ExcessFlag := True;  // Default to Excess energy FOR UE

     ElementName := 'Vsource.'+TCktElement(ActiveCircuit.CktElements.Get(1)).Name; // Default to first circuit element (source)

     MeteredElement := NIL;
     BranchList     := NIL;  // initialize to NIL, set later when inited

     This_Meter_DIFileIsOpen := FALSE;

     InitPropertyValues(0);

     // Max zone kW limits ignored unless the user provides a rating
     MaxZonekVA_Norm := 0.0;
     MaxZonekVA_Emerg := 0.0;

     ZoneIsRadial    := True;
     HasFeeder := FALSE;
     FeederObj := Nil;  // initialize to not assigned

     PeakCurrent :=NIL;
     PhaseAllocationFactor := NIL;
     MeteredCurrent := NIL;

     DefinedZoneList := NIL;
     DefinedZoneListSize := 0;

     LocalOnly := FALSE;
     VoltageUEOnly := FALSE;

     ResetRegisters;
     For i := 1 to NumEMRegisters Do TotalsMask[i] := 1.0;

     ReAllocMem(PeakCurrent, Sizeof(PeakCurrent^[1])* Fnphases);
     FOR i := 1 to Fnphases Do PeakCurrent^[i] := 400.0;
     ReAllocMem(PhaseAllocationFactor, Sizeof(PhaseAllocationFactor^[1])* Fnphases);



    // RecalcElementData;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TEnergyMeterObj.Destroy;
Begin
    BranchList.Free;
    Reallocmem(PeakCurrent, 0);
    Reallocmem(MeteredCurrent, 0);
    Reallocmem(PhaseAllocationFactor, 0);
    FreeStringArray(DefinedZoneList, DefinedZoneListSize);
    Inherited destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.RecalcElementData;

VAR
   DevIndex :Integer;


Begin
         Devindex := GetCktElementIndex(ElementName);   // Global function
         IF DevIndex>0 Then Begin  // Monitored element must already exist
             MeteredElement := ActiveCircuit.CktElements.Get(DevIndex); // Get pointer to metered element
             {MeteredElement must be a PDElement}
             If NOT (MeteredElement is TPDElement) Then Begin
                MeteredElement := NIL;   // element not found
                DoErrorMsg('EnergyMeter: "' + Self.Name + '"', 'Circuit Element "'+ ElementName + '" is not a Power Delivery (PD) element.',
                            ' Element must be a PD element.', 525);
                Exit;
             End;


             IF MeteredTerminal>MeteredElement.Nterms  Then Begin
                 DoErrorMsg('EnergyMeter: "' + Name + '"',
                                 'Terminal no. "' + IntToStr(MeteredTerminal)+'" does not exist.',
                                 'Respecify terminal no.', 524);
             END
             ELSE Begin

                 If MeteredElementChanged Then Begin
                   // Sets name of i-th terminal's connected bus in monitor's buslist
                   // This value will be used to set the NodeRef array (see TakeSample)
                     Setbus(1, MeteredElement.GetBus(MeteredTerminal));
                     Nphases := MeteredElement.NPhases;
                     Nconds   := MeteredElement.Nconds;
                     ReallocMem(MeteredCurrent, Sizeof(MeteredCurrent^[1])*MeteredElement.Yorder);
                     ReAllocMem(PeakCurrent, Sizeof(PeakCurrent^[1])* Fnphases);
                     ReAllocMem(PhaseAllocationFactor, Sizeof(PhaseAllocationFactor^[1])* Fnphases);

                     // If we come through here, throw branchlist away
                     IF BranchList <> NIL Then BranchList.Free;
                     BranchList := Nil;
                 End;

                 If HasFeeder Then MakeFeederObj;  // OK to call multiple times

             END;
         END
         ELSE Begin
            MeteredElement := NIL;   // element not found
            DoErrorMsg('EnergyMeter: "' + Self.Name + '"', 'Circuit Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 525);
         END;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.ResetRegisters;

VAR
   i : Integer;

Begin
   FOR i := 1 to NumEMregisters Do Registers[i]   := 0.0;
   FOR i := 1 to NumEMregisters Do Derivatives[i] := 0.0;
   {Initialize DragHand registers to some big negative number}
   Registers[Reg_MaxkW]           := -1.0e50;
   Registers[Reg_MaxkVA]          := -1.0e50;
   Registers[Reg_ZoneMaxkW]       := -1.0e50;
   Registers[Reg_ZoneMaxkVA]      := -1.0e50;
   Registers[Reg_MaxLoadLosses]   := -1.0e50;
   Registers[Reg_MaxNoLoadLosses] := -1.0e50;
   Registers[Reg_LossesMaxkW]     := -1.0e50;
   Registers[Reg_LossesMaxkvar]   := -1.0e50;

   Registers[Reg_GenMaxkW]        := -1.0e50;
   Registers[Reg_GenMaxkVA]       := -1.0e50;

   FirstSampleAfterReset := True;  // initialize for trapezoidal integration
   // Removed .. open in solution loop See Solve Yearly If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.CalcYPrim;

Begin

 // YPrim is all zeros.  Just leave as NIL so it is ignored.

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.SaveRegisters;

VAR
  CSVName :String;
  F       :TextFile;
  i       :Integer;

Begin

  Try
       CSVName := 'MTR_' + Name + '.CSV';
       AssignFile(F, DSSDataDirectory + CSVName);
       Rewrite(F);
       GlobalResult := CSVName;
  Except
      On E: Exception DO
      Begin
       DoSimpleMsg('Error opening Meter File "' + CRLF + CSVName + '": ' + E.Message, 526);
       Exit;
      End
  End;

 Try
//       Writeln(F,'**** NEW RECORD ****');
       Writeln(F, 'Year, ', ActiveCircuit.Solution.Year:0,',');
       FOR i := 1 to NumEMregisters Do
         Writeln(F, '"',EnergyMeterClass.RegisterNames[i], '",', Registers[i]:0:0);
 Finally
       CloseFile(F);
 End;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);

Begin
     IF ActiveCircuit.TrapezoidalIntegration THEN Begin
        {Trapezoidal Rule Integration}
        If Not FirstSampleAfterReset Then Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
      End
     ELSE   {Plain Euler integration}
         Registers[Reg] := Registers[Reg] + Interval * Deriv;

     Derivatives[Reg] := Deriv;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.TakeSample;
// Update registers from metered zone
// Assumes one time period has taken place since last sample.

VAR
   S_Local,
   S_Totallosses,
   S_LoadLosses,
   S_NoLoadLosses,
   TotalLoadLosses,
   TotalNoLoadLosses,
   TotalLosses :Complex;
   CktElem,
   ParenElem :TPDElement;
   PCelem    :TPCElement;
   pLoad     :TLoadobj;
   pGen      :TGeneratorObj;
   // doubles
   MaxExcesskWNorm ,
   MaxExcesskWEmerg,
   EEN,
   UE,
   ZonekW,
   TotalZonekw,
   TotalZonekvar,
   TotalGenkw,
   TotalGenkvar,
   LoadkVA,
   GenkVA,
   S_Local_kVA :Double;

Begin

// Compute energy in branch  to which meter is connected

     MeteredElement.ActiveTerminalIdx := MeteredTerminal;
     S_Local     := CmulReal(MeteredElement.Power, 0.001);
     S_Local_kVA := Cabs(S_Local);
     Delta_Hrs   := ActiveCircuit.Solution.IntervalHrs;
     Integrate(Reg_kWh,   S_Local.re, Delta_Hrs);   // Accumulate the power
     Integrate(Reg_kvarh, S_Local.im, Delta_Hrs);
     SetDragHandRegister( Reg_MaxkW,  S_Local.re);   // 3-10-04 removed abs()
     SetDragHandRegister( Reg_MaxkVA, cabs(S_Local));

// Compute Maximum overload energy in all branches in zone
// and mark all load downline from an overloaded branch as unserved
// If localonly, check only metered element

     TotalLosses       := CZERO;     // Initialize loss accumulators
     TotalLoadLosses   := CZERO;
     TotalNoLoadLosses := CZERO;
     CktElem     := BranchList.First;
     MaxExcesskWNorm  := 0.0;
     MaxExcesskWEmerg := 0.0;

     IF LocalOnly THEN Begin
           CktElem :=  MeteredElement as TPDElement;
           MaxExcesskWNorm   := Abs(CktElem.ExcesskVANorm.re);
           MaxExcesskWEmerg  := Abs(CktElem.ExcesskVAEmerg.re);
     End
     ELSE WHILE CktElem <> NIL Do  Begin       // loop thru all ckt element on zone

         CktElem.ActiveTerminalIdx := BranchList.Presentbranch.FromTerminal;
       // Invoking this property sets the Overload_UE flag in the PD Element
         EEN  := Abs(CktElem.ExcesskVANorm.re);
         UE   := Abs(CktElem.ExcesskVAEmerg.re);

         {For radial circuits just keep the maximum overload; for mesh, add 'em up}
         IF   (ZoneIsRadial)  THEN Begin
             IF UE  > MaxExcesskWEmerg Then MaxExcesskWEmerg := UE;
             IF EEN > MaxExcesskWNorm  Then MaxExcesskWNorm  := EEN;
         End
         ELSE Begin
              MaxExcesskWEmerg := MaxExcesskWEmerg + UE;
              MaxExcesskWNorm  := MaxExcesskWNorm + EEN;
         End;

         // Even if this branch is not overloaded, if the parent element is overloaded
         // mark load on this branch as unserved also
         // Use the larger of the two factors
         ParenElem := BranchList.Parent;
         IF   (ParenElem <> NIL)   Then Begin
              CktElem.OverLoad_EEN := Maxvalue([CktElem.Overload_EEN, ParenElem.Overload_EEN]);
              CktElem.OverLoad_UE  := Maxvalue([CktElem.OverLoad_UE,  ParenElem.OverLoad_UE]);
         End;

         // Mark loads (not generators) by the degree of overload if the meter's zone is to be considered radial
         // This overrides and supercedes the load's own determination of unserved based on voltage
         // If voltage only is to be used for Load UE/EEN, don't mark (set to 0.0 and load will calc UE based on voltage)
         PCElem := Branchlist.FirstObject;
         WHILE (PCElem <> NIL) Do  Begin
             IF (PCElem.DSSObjType and CLASSMASK) = LOAD_ELEMENT Then Begin
                 pLoad := PCElem as TLoadObj;
                 IF   (CktElem.Overload_EEN > 0.0) And (ZoneIsRadial) and Not (VoltageUEOnly)
                 Then pLoad.EEN_Factor := CktElem.Overload_EEN
                 Else pLoad.EEN_Factor := 0.0;

                 IF   (CktElem.Overload_UE  > 0.0) And (ZoneIsRadial) and Not (VoltageUEOnly)
                 Then pLoad.UE_Factor  := CktElem.Overload_UE
                 Else pLoad.UE_Factor  := 0.0;
             End;
             PCElem := BranchList.NextObject
         End;

     CktElem := BranchList.GoForward;
     End;


     // Get the Losses, and unserved bus energies
     TotalZonekw   := 0.0;
     TotalZonekvar := 0.0;
     TotalGenkw    := 0.0;
     TotalGenkvar  := 0.0;

     CktElem := BranchList.First;
     WHILE (CktElem <> NIL) Do
     Begin
         PCElem := Branchlist.FirstObject;
         WHILE (PCElem <> NIL) Do  Begin
             CASE (PCElem.DSSObjType and CLASSMASK) OF
                LOAD_ELEMENT: If Not LocalOnly Then Begin   // Dont check for load EEN/UE if Local only
                               pLoad := PCElem as TLoadObj;
                               Integrate_Load(pLoad, TotalZonekW, TotalZonekvar);
                              END;
                GEN_ELEMENT:  Begin
                               pGen := PCElem as TGeneratorObj;
                               Integrate_Gen(pGen, TotalGenkW, TotalGenkvar);
                              End;
             ELSE
                {Ignore other types of PC Elements}
             END;
          PCElem := BranchList.NextObject
         End;

         {Get losses from the present circuit element}
         CktElem.GetLosses(S_TotalLosses, S_LoadLosses, S_NoLoadLosses);  // returns watts, vars
         {Add losses into appropriate registers; convert to kW, kvar}
         Integrate(Reg_LosseskWh,         S_TotalLosses.re  * 0.001, Delta_Hrs);
         Integrate(Reg_Losseskvarh,       S_TotalLosses.im  * 0.001, Delta_Hrs);
         Integrate(Reg_LoadLosseskWh,     S_LoadLosses.re   * 0.001, Delta_Hrs);
         Integrate(Reg_NoLoadLosseskWh,   S_NoLoadLosses.re * 0.001, Delta_Hrs);
         {Update accumulators}
         Caccum(TotalLosses,       S_TotalLosses); // Accumulate total losses in meter zone
         Caccum(TotalLoadLosses,   S_LoadLosses);  // Accumulate total load losses in meter zone
         Caccum(TotalNoLoadLosses, S_NoLoadLosses); // Accumulate total no load losses in meter zone

     CktElem := BranchList.GoForward;
     End;

     {So that Demand interval Data will be stored OK}
     Derivatives[Reg_LosseskWh]       := TotalLosses.Re;
     Derivatives[Reg_Losseskvarh]     := TotalLosses.im;
     Derivatives[Reg_LoadLosseskWh]   := TotalLoadLosses.Re;
     Derivatives[Reg_NoLoadLosseskWh] := TotalNoLoadLosses.Re;

     SetDragHandRegister(Reg_LossesMaxkW,    Abs(TotalLosses.Re));
     SetDragHandRegister(Reg_LossesMaxkvar,  Abs(TotalLosses.im));
     SetDragHandRegister(Reg_MaxLoadLosses,  Abs(TotalLoadLosses.Re));
     SetDragHandRegister(Reg_MaxNoLoadLosses,Abs(TotalNoLoadLosses.Re));
     SetDragHandRegister(Reg_ZoneMaxkW,      TotalZonekW ); // Removed abs()  3-10-04

     Derivatives[Reg_GenkWh]          := TotalGenkW;
     GenkVA := Sqrt(Sqr(TotalGenkvar) + Sqr(TotalGenkW));
     Derivatives[Reg_Genkvarh]        := TotalGenkvar;

     LoadkVA :=  Sqrt(Sqr(TotalZonekvar) + Sqr(TotalZonekW));
     SetDragHandRegister(Reg_ZoneMaxkVA ,    LoadkVA  );

     Integrate(Reg_ZonekWh,   TotalZonekW,   Delta_Hrs);
     Integrate(Reg_Zonekvarh, TotalZonekvar, Delta_Hrs);

     {Max total generator registers}
     SetDragHandRegister(Reg_GenMaxkW,   TotalGenkW); // Removed abs()  3-10-04
     SetDragHandRegister(Reg_GenMaxkVA , GenkVA  );

     {Overload energy for the entire zone}
     If LocalOnly Then ZonekW := S_Local.Re Else ZonekW := TotalZonekW;

     {Either the max excess kW of any PD element or the excess over zone limits}

     {Fixed these formulas 2-7-07 per discussions with Daniel Brooks }
     If  (MaxZonekVA_Norm > 0.0) Then Begin
          IF (S_Local_KVA =0.0) Then S_Local_KVA := MaxZonekVA_Norm;
          Integrate(Reg_OverloadkWhNorm, Maxvalue([0.0, (ZonekW * (1.0 -  MaxZonekVA_Norm / S_Local_KVA))]), Delta_Hrs);
     End Else Integrate(Reg_OverloadkWhNorm, MaxExcesskWNorm,                            Delta_Hrs);

     If  (MaxZonekVA_Emerg > 0.0)Then Begin
          IF (S_Local_KVA =0.0) Then S_Local_KVA := MaxZonekVA_Emerg;
          Integrate(Reg_OverloadkWhEmerg, Maxvalue([0.0, (ZonekW * (1.0 - MaxZonekVA_Emerg/ S_Local_KVA))]), Delta_Hrs);
     End Else Integrate(Reg_OverloadkWhEmerg, MaxExcesskWEmerg,                            Delta_Hrs);


    FirstSampleAfterReset := False;
    IF EnergyMeterClass.SaveDemandInterval Then WriteDemandIntervalData;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeter.SetHasMeterFlag;
// Set the HasMeter Flag for all cktElement;
VAR
   i:Integer;
   ThisMeter:TEnergyMeterObj;
   CktElem:TCktElement;

Begin
   {Initialize all to FALSE}
   With  ActiveCircuit Do Begin

     CktElem := PDElements.First;
     While CktElem <> Nil Do Begin
        CktElem.HasMeter := FALSE;
        CktElem := PDElements.Next;
     End;  {WHILE}

   End; {WITH}

   FOR i := 1 to ActiveCircuit.EnergyMeters.ListSize DO Begin
       ThisMeter := ActiveCircuit.EnergyMeters.Get(i);
       With ThisMeter Do If MeteredElement <> Nil Then MeteredElement.HasMeter := TRUE;
   End;   {FOR}
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.MakeMeterZoneLists;

// This gets fired off whenever the buslists are rebuilt
// Must be updated whenever there is a change in the circuit

Var

   TestBusNum,
   ZoneListCounter :Integer ;
   j,
   iTerm,iPC, iPD      :Integer;
   TestBranch: TCktElement;
   TestElement:TPDElement;
   PC         :TPCElement;
   IsFeederEnd:Boolean;

Begin

     ZoneListCounter := 0;

     IF BranchList <> NIL Then BranchList.Free;
     BranchList := TCktTree.Create;     {Instantiates ZoneEndsList, too}


    // Get Started
       If Assigned(MeteredElement) Then BranchList.New := MeteredElement
       Else Begin   // oops
           DoSimpleMsg('Metered Element for EnergyMeter '+Name+' not defined.', 527);
           Exit;
       End;

       MeteredElement.Terminals^[MeteredTerminal].Checked := TRUE;
       With BranchList.PresentBranch Do Begin
          // This bus is the head of the feeder; do not mark as radial bus
          FromBusReference := MeteredElement.Terminals^[MeteredTerminal].BusRef;
          FromTerminal := MeteredTerminal;
          If MeteredElement is TPDElement Then TPDElement(MeteredElement).FromTerminal := MeteredTerminal;
       End;

       // Check off this element so we don't use it  again
       With MeteredElement Do Begin
         Checked := True;
         IsIsolated := FALSE;
       End;


    // Now start looking for other branches
    // Finds any branch connected to the TestBranch and adds it to the list
    // Goes until end of circuit, another energy meter, an open terminal, or disabled device.
       TestBranch := MeteredElement;
       WHILE TestBranch <> NIL DO
       Begin
         BranchList.PresentBranch.IsLoopedHere := FALSE;
         BranchList.PresentBranch.IsParallel := FALSE;
         BranchList.PresentBranch.IsDangling := TRUE;  // Unless we find something connected to it

         FOR iTerm := 1 to TestBranch.Nterms Do  Begin
          IF NOT TestBranch.Terminals^[iTerm].Checked Then
          WITH ActiveCircuit Do
          Begin

           // Now find all loads and generators connected to the bus on this end of branch
           // attach them as generic objects to cktTree node.

           TestBusNum := TestBranch.Terminals^[iTerm].BusRef;
           BranchList.PresentBranch.ToBusReference := TestBusNum;   // Add this as a "to" bus reference


           iPC :=0;
           While iPC < PCElementList.Count Do
           Begin
              pC := PCElementList.List^[iPC];
              // IF pC.Enabled Then Begin   onlye enabled elements in the search list
              //  IF (Not pC.Checked) Then// Skip if we've already done this PC device
               IF (pC.Terminals^[1].BusRef = TestBusNum) Then  Begin // ?Connected to this bus ?
                 BranchList.PresentBranch.IsDangling := FALSE;   // Something is connected here
               // Is this a load or a generator or a Capacitor or reactor??
                 IF ((pC.DSSObjType and CLASSMASK) = LOAD_ELEMENT)
                 OR ((pC.DSSObjType and CLASSMASK) = GEN_ELEMENT)
                 OR ((pC.DSSObjType and CLASSMASK) = CAP_ELEMENT)
                 OR ((pC.DSSObjType and CLASSMASK) = REACTOR_ELEMENT)
                   Then  Begin
                      BranchList.NewObject := pC;
                      pC.Checked := True;  // So we don't pick this element up again
                      pC.IsIsolated := FALSE;
                      PCElementList.Delete(iPC);  // make the search list shorter
                      Dec(iPC);  // Back the pointer up
                   End; {IF}
              End; {IF}
              Inc(iPC);
           End;

           // Now find all branches connected to this bus that we havent found already
           // Do not include in this zone if branch has open terminals or has another meter

            IF DefinedZoneListSize = 0 THEN Begin  // Search tree for connected branches (default)
                 IsFeederEnd := TRUE;
                 iPD := 0;
                 WHILE iPD < PDElementList.Count Do
                 Begin
                   TestElement := PDElementList.List^[iPD];  // Only enabled objects are in thie list

                   // **** See ResetMeterZonesAll

                   IF Not (TestElement=TestBranch) Then  // Skip self
                   IF Not TestElement.HasMeter THEN Begin  // Stop at other meters  so zones don't interfere
                      FOR j := 1 to TestElement.Nterms Do Begin     // Check each terminal
                         IF TestBusNum = TestElement.Terminals^[j].BusRef THEN  Begin
                             BranchList.PresentBranch.IsDangling := FALSE; // We found something it was connected to
                             {Check for loops and parallel branches and mark them}
                             IF   (TestElement.Checked) Then Begin      {This branch is on some meter's list already }
                                 BranchList.PresentBranch.IsLoopedHere := TRUE; {It's a loop}
                                 BranchList.PresentBranch.LoopLineObj := TestElement;
                                 If IsLineElement(TestBranch) and IsLineElement(TestElement) Then
                                   If CheckParallel(TestBranch, TestElement) Then BranchList.PresentBranch.IsParallel := TRUE; {It's paralleled with another line}
                              End
                             Else
                              Begin
                                IsFeederEnd := FALSE;  // for interpolation
                                BranchList.AddNewChild( TestElement, TestBusNum, j);
                                With TestElement Do Begin
                                  Terminals^[j].Checked := TRUE;
                                  FromTerminal := j;
                                  Checked := TRUE;
                                  IsIsolated := FALSE;
                                End;
                                Break;
                              End;
                         END; {IF TestBusNum}
                      END;  {FOR}
                   END; {ELSE IsFeederEnd := FALSE;}  // So we don't accidentally reduce this out

                 Inc(iPD);
                 End; {WHILE}

                 If IsFeederEnd then With BranchList Do ZoneEndsList.Add(PresentBranch, TestBusNum);
                   {This is an end of the feeder and testbusnum is the end bus}
            END
            ELSE Begin   // Zone is manually specified; Just add next element in list as a child
                 Inc(ZoneListCounter);
                 WHILE ZoneListCounter <= DefinedZoneListSize Do Begin
                     IF SetElementActive(DefinedZoneList^[ZoneListCounter]) = 0 THEN
                          Inc(ZoneListCounter) // Not Found. Let's search for another
                     ELSE Begin
                          TestElement := ActiveCktElement as TPDElement;
                          IF Not TestElement.Enabled THEN Inc(ZoneListCounter)  // Lets ignore disabled devices
                          ELSE Begin
                             IF (TestElement.DSSObjType and BaseClassMask) <> PD_ELEMENT
                             THEN Inc(ZoneListCounter)  // Lets ignore non-PD elements
                             ELSE BranchList.AddNewChild(TestElement, 0, 0); // add it as a child to the previous element
                             Break;                                  // Can't do reductions if manually spec'd
                          END;
                     END;
                 END;
            END;
            End;  {WITH Active Circuit}
          End;   {FOR iTerm}
          TestBranch := BranchList.GoForward;   // Sets PresentNode
       End;

       If HasFeeder Then FeederObj.InitializeFeeder(BranchList);   // Synchronize the feeder definition
End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeterObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr FOR reports

Var i:Integer;

Begin
     FOR i := 1 to Fnconds DO Curr^[i] := CZERO;
END;

{--------------------------------------------------------------------------}
Procedure TEnergyMeterObj.GetInjCurrents(Curr: pComplexArray);

Var i:Integer;

Begin
     FOR i := 1 to Fnconds DO Curr^[i] := CZERO;
END;

{--------------------------------------------------------------------------}

Procedure TEnergyMeterObj.ZoneDump;

Var
    CSVName :String;
    F       :TextFile;
    pdelem  :TPDelement;
    LoadElem:TCktElement;

Begin

     TRY

       CSVName := 'Zone_' + Name + '.CSV';
       AssignFile(F, DSSDataDirectory + CSVName);
       Rewrite(F);

       GlobalResult := CSVName;

     EXCEPT

       On E: Exception DO Begin
         DoSimpleMsg('Error opening File "' + CSVName + '": ' + E.Message, 528);
         Exit;
       End;

     END;

     TRY
         IF BranchList<>NIL
         Then Begin
           PDElem := BranchList.First;
           WHILE PDElem <> NIL Do
           Begin
               Writeln(F, BranchList.Level,', ', PDelem.Name);
               LoadElem := Branchlist.FirstObject;
               WHILE LoadElem <> NIL Do
               Begin
                     Writeln(F, '-1, [', LoadElem.ParentClass.Name, '], ', LoadElem.Name);
                     LoadElem := BranchList.NextObject
               End;
           PDElem := BranchList.GoForward;
           End;
         End;

     FINALLY
         Closefile(F);
     END;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;
   pdelem:TPDelement;
   LoadElem:TCktElement;


Begin
    Inherited DumpProperties(F, complete);

    WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Case i of
         4: Begin     // option
                Write(F,'~ ',PropertyName^[i],'=(');
                IF ExcessFlag Then Write(F, 'E,') Else Write(F, 'T,');
                IF ZoneIsRadial Then Write(F, ' R,') Else Write(F, ' M,');
                IF VoltageUEOnly Then Write(F, ' V') Else Write(F, ' C');
                Writeln(F,')');
            End;
         7: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[i],')');
     Else
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    IF complete Then
     Begin

       Writeln(F, 'Registers');
       FOR i := 1 to NumEMregisters Do
       Begin
            Writeln(F, '"',EnergyMeterClass.RegisterNames[i],'" = ', Registers[i]:0:0);
       End;
       Writeln(F);

       Writeln(F, 'Branch List:');
       IF BranchList<>NIL
       Then Begin
         PDElem := BranchList.First;
         WHILE PDElem <> NIL Do
         Begin
             Writeln(F, 'Circuit Element = ', PDelem.Name);
             LoadElem := Branchlist.FirstObject;
             WHILE LoadElem <> NIL Do
             Begin
                   Writeln(F, '   Shunt Element = ', LoadElem.ParentClass.name, '.', LoadElem.Name);
                   LoadElem := BranchList.NextObject
             End;
             PDElem := BranchList.GoForward;
         End;
       End;

    End;

End;

PROCEDURE TEnergyMeter.ProcessOptions(const Opts: String);

VAR
   S1, S2 :String;
begin

    AuxParser.CmdString := Opts;  // Load up aux Parser

    {Loop until no more options found}
    WITH ActiveEnergymeterObj DO
    REPEAT
         S1 := AuxParser.NextParam; // ignore any parameter name  not expecting any
         S2 := lowercase(AuxParser.StrValue);
         IF Length(S2)>0 THEN
         CASE s2[1] of
              'e': ExcessFlag    := TRUE;
              't': ExcessFlag    := FALSE;
              'r': ZoneIsRadial  := TRUE;
              'm': ZoneIsRadial  := FALSE;
              'c': VoltageUEOnly := FALSE;
              'v': VoltageUEOnly := TRUE;
         End;
    UNTIL Length(S2)=0;

end;

PROCEDURE TEnergyMeterObj.AllocateLoad;

VAR
   iOffset, i,
   ConnectedPhase  :Integer;
   Mag,
   AvgFactor :Double;
   CktElem   :TPDElement;
   LoadElem  :TLoadobj;


begin

    MeteredElement.GetCurrents(MeteredCurrent);

    // The Phase Allocation Factor is the amount that the load must change to match the measured peak
    iOffset := (MeteredTerminal-1) * MeteredElement.NConds;
    AvgFactor := 0.0;
    FOR i := 1 to Fnphases Do
       Begin
         Mag := Cabs(MeteredCurrent^[i + iOffset]);
         IF   Mag > 0.0
         THEN PhaseAllocationFactor^[i] := PeakCurrent^[i] / Mag
         ELSE PhaseAllocationFactor^[i] := 1.0; // No change
         AvgFactor := AvgFactor + PhaseAllocationFactor^[i];
       End;
    AvgFactor := AvgFactor / Fnphases;   // Factor for 2- and 3-phase loads

    // Now go through the zone and adjust the loads.

     CktElem     := BranchList.First;
     WHILE CktElem <> NIL Do Begin
         // This overrides and supercedes the load's own determination of unserved based on voltage
         LoadElem := Branchlist.FirstObject;
         WHILE (LoadElem <> NIL) Do Begin
             If (LoadElem.DSSObjType and CLASSMASK) = LOAD_ELEMENT  Then  // only for loads not other shunts
             CASE LoadElem.NPhases of
                 {For Single phase loads, allocate based on phase factor, else average factor}
                  1: WITH LoadElem Do Begin
                          ConnectedPhase := ActiveCircuit.MapNodeToBus^[NodeRef^[1]].NodeNum;
                          IF  (ConnectedPhase > 0) and (ConnectedPhase < 4)   // Restrict to phases 1..3
                          THEN AllocationFactor := AllocationFactor * PhaseAllocationFactor^[ConnectedPhase];
                     End;
             ELSE
                  WITH LoadElem Do AllocationFactor := AllocationFactor * AvgFactor;
             End;  {CASE}
             LoadElem := BranchList.NextObject
         End;   {While Loadelem}
     CktElem := BranchList.GoForward;
     End;  {While CktElem}

end;

procedure TEnergyMeterObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := ''; //'element';
     PropertyValue[2] := '1'; //'terminal';
     PropertyValue[3] := 'clear'; //'action';
     PropertyValue[4] := '(E, R, C)'; //'Option';
     PropertyValue[5] := '0.0'; //'kWnormal';
     PropertyValue[6] := '0.0'; //'kwEmerg';
     PropertyValue[7] := '(400, 400, 400)'; //'PeakCurrent';
     PropertyValue[8] := ''; // ZoneList
     PropertyValue[9] := 'No';
     PropertyValue[10] := '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)';
     PropertyValue[11] := 'No';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

PROCEDURE TEnergyMeterObj.Integrate_Gen;
Var
   S:Complex;
begin

     S := Cnegate(CmulReal(pGen.Power, 0.001));
     TotalZonekw   := TotalZonekW   + S.re;
     TotalZonekvar := TotalZonekvar + S.im;
     Integrate(Reg_Genkwh, S.re , Delta_Hrs);
     Integrate(Reg_Genkvarh, S.im , Delta_Hrs);

end;

procedure TEnergyMeterObj.Integrate_Load;
Var
   S:Complex;
   kW_Load : Double;
begin
  WITH   pLoad  Do
   Begin
       S := CmulReal(pLoad.Power, 0.001);
       kW_Load := S.re;
       TotalZonekw   := TotalZonekW   + kW_Load;
       TotalZonekvar := TotalZonekvar + S.im;

       {always integrate even if the value is 0.0
        otherwise the Integrate function is not correct}
       {Invoking the ExceedsNormal and Unserved Properties causes the factors to be computed}
       IF ExcessFlag THEN  Begin   // Return Excess load as EEN/UE
           If   (ExceedsNormal)
           Then Integrate(Reg_LoadEEN, kW_Load * EEN_Factor, Delta_Hrs)
           Else Integrate(Reg_LoadEEN, 0.0,                  Delta_Hrs);

           If   (Unserved)
           Then Integrate(Reg_LoadUE,  kW_Load * UE_Factor,  Delta_Hrs)
           Else Integrate(Reg_LoadUE,  0.0,                  Delta_Hrs);
        End
       ELSE  Begin    // Return total load as EEN/UE
           IF   (ExceedsNormal)
           Then Integrate(Reg_LoadEEN, kW_Load,  Delta_Hrs)
           Else Integrate(Reg_LoadEEN, 0.0,       Delta_Hrs);

           IF   (Unserved)
           Then Integrate(Reg_LoadUE,  kW_Load,  Delta_Hrs)
           Else Integrate(Reg_LoadUE,  0.0,      Delta_Hrs);
        End;
   END; {WITH}
end;





PROCEDURE TEnergyMeterObj.ReduceZone;

{Reduce the zone by merging lines}

begin
 // Make  sure zone list is built
    If not assigned(BranchList) Then MakeMeterZoneLists;
     
    Case ActiveCircuit.ReductionStrategy of

         rsStubs:         DoReduceStubs(BranchList);    {See ReduceAlgs.Pas}
         rsTapEnds:       DoReduceTapEnds (BranchList);
         rsMergeParallel: DoMergeParallelLines(BranchList);
         rsDangling:      DoReduceDangling(BranchList);
         rsBreakLoop:     DoBreakLoops(BranchList);
         rsSwitches:      DoReduceSwitches(BranchList);
    Else
       {Default}
       DoReduceDefault(BranchList);
    End;

    // Resynchronize with Feeders
    If HasFeeder Then FeederObj.InitializeFeeder (Branchlist);

end;

procedure TEnergyMeterObj.InterpolateCoordinates;
{Start at the ends of the zone and work toward the start
 interpolating between known coordinates}
Var
  i, BusRef,
  FirstCoordRef, SecondCoordRef,
  Linecount:integer;
  PresentNode, StartNode: TCktTreeNode;
  CktElem:TCktElement;

begin
  If Not Assigned(BranchList) Then
  Begin
      DoSimpleMsg('Meter Zone Lists need to be built. Do Solve or Makebuslist first!', 529);
      Exit;
  End;
  With ActiveCircuit Do
   Begin

     For i := 1 to Branchlist.ZoneEndsList.NumEnds Do
     Begin
       Busref := Branchlist.ZoneEndsList.Get(i, PresentNode);

       FirstCoordRef := BusRef;
       SecondCoordRef :=   FirstCoordRef;  {so compiler won't issue stupid warning}
       {Find a bus with a coordinate}
       If Not Buses^[BusRef].CoordDefined Then
       Begin
          While Not Buses^[PresentNode.FromBusReference].CoordDefined Do
          Begin
             PresentNode := PresentNode.ParentBranch;
             If PresentNode=Nil Then Break;
          End;
          If PresentNode<> Nil then  FirstCoordRef := PresentNode.FromBusReference;
       End;

       While PresentNode <> Nil Do
       Begin
          {Back up until we find another Coord defined}
          LineCount := 0;
          StartNode := PresentNode;
          CktElem := PresentNode.CktObject;
          If FirstCoordRef <> PresentNode.FromBusReference then
          Begin   {Handle special case for end branch}
              If  Buses^[PresentNode.FromBusReference].CoordDefined Then
                  FirstCoordRef := PresentNode.FromBusReference
              Else Inc(LineCount);
          End;

          Repeat
              CktElem.Checked := True;
              PresentNode := PresentNode.ParentBranch;
              If PresentNode=Nil Then Break;
              CktElem := PresentNode.CktObject;
              SecondCoordRef := PresentNode.FromBusReference;
              Inc(LineCount);
          Until Buses^[SecondCoordRef].CoordDefined or CktElem.Checked;

          If (PresentNode<>Nil) and (LineCount>1) Then
           IF Buses^[SecondCoordRef].CoordDefined Then
             Begin
               CalcBusCoordinates(StartNode,  FirstCoordRef, SecondCoordRef, LineCount);
             End
            Else Break; {While - went as far as we could go this way}

          FirstCoordRef := SecondCoordRef;
       End;

     End; {For}

  End; {With}

end;

procedure TEnergyMeterObj.CalcBusCoordinates(StartBranch:TCktTreeNode;
  FirstCoordRef,  SecondCoordref, LineCount: Integer);

Var
   X, Y, Xinc, Yinc:Double;
begin

     If LineCount = 1 then Exit;  {Nothing to do!}

     With ActiveCircuit Do
     Begin
       Xinc := (Buses^[FirstCoordref].X - Buses^[SecondCoordRef].X) / LineCount;
       Yinc := (Buses^[FirstCoordref].Y - Buses^[SecondCoordRef].Y) / LineCount;

       X := Buses^[FirstCoordref].X;
       Y := Buses^[FirstCoordref].Y;

       {***Debug}
   (*    If ((X<10.0) and (y<10.0)) or
          ((Buses^[SecondCoordRef].X<10.0) and (Buses^[SecondCoordRef].Y<10.0)) Then
       Begin
          X := y;  // Stopping point
       End;
     *)
     
     {Either start with the "to" end of StartNode or the "from" end;}
       If FirstCoordRef <> StartBranch.FromBusReference Then
       Begin  // Start with "to" end
          X:= X- Xinc;
          Y:= Y- Yinc;
          Buses^[StartBranch.FromBusReference].X := X;
          Buses^[StartBranch.FromBusReference].Y := Y;
          Buses^[StartBranch.FromBusReference].CoordDefined := True;
          Dec(LineCount);
       End;

       While LineCount>1 Do
       Begin
          X:= X- Xinc;
          Y:= Y- Yinc;
          StartBranch := StartBranch.ParentBranch; // back up the tree
          Buses^[StartBranch.FromBusReference].X := X;
          Buses^[StartBranch.FromBusReference].Y := Y;
          Buses^[StartBranch.FromBusReference].CoordDefined := True;
          Dec(LineCount);
       End;

     End;
end;

function TEnergyMeterObj.GetPropertyValue(Index: Integer): String;
begin
        Case Index of
          4,7: Result := '(';
        Else
            Result := '';
        End;

        CASE Index of
           4: Begin     // option
                IF ExcessFlag Then Result := Result +'E,' Else Result := Result +'T,';
                IF ZoneIsRadial Then Result := Result +' R,' Else Result := Result +' M,';
                IF VoltageUEOnly then Result := Result +' V' Else Result := Result +' C';
              End;
        ELSE
           Result := Result + Inherited GetPropertyValue(index);
        END;

        Case Index of
          4,7: Result := Result + ')';
        Else
        End;
end;

procedure TEnergyMeterObj.SaveZone(const dirname:String);

Var cktElem, shuntElement:TCktElement;
    LoadElement:TLoadObj;
    FBranches, FShunts, FLoads, FGens, FCaps: TextFile;
    NBranches, NShunts, Nloads, NGens, NCaps: Integer;


begin
 {We are in the directory indicated by dirname}

{Run down the zone and write each element into a file}

   IF BranchList<>NIL Then
   Begin
    {Open some files:}

     Try
         AssignFile(FBranches, 'Branches.dss');     // Both lines and transformers
         Rewrite(FBranches);
         NBranches := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Branches.dss for Energymeter: ' + Self.Name+'. '+E.Message , 530);
             Exit;
         End;
     End;

     Try
         AssignFile(FShunts, 'Shunts.dss');
         Rewrite(FShunts);
         NShunts := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Shunts.dss for Energymeter: ' + Self.Name+'. '+E.Message , 531);
             CloseFile(FShunts);
             Exit;
         End;
     End;

     Try
         AssignFile(FLoads, 'Loads.dss');
         Rewrite(FLoads);
         Nloads := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Loads.dss for Energymeter: ' + Self.Name+'. '+E.Message , 532);
             CloseFile(FBranches);
             Exit;
         End;
     End;

     Try
         AssignFile(FGens, 'Generators.dss');
         Rewrite(FGens);
         NGens := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Generators.dss for Energymeter: ' + Self.Name+'. '+E.Message , 533);
             CloseFile(FGens);
             Exit;
         End;
     End;

     Try
         AssignFile(FCaps, 'Capacitors.dss');
         Rewrite(FCaps);
         Ncaps := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Generators.dss for Energymeter: ' + Self.Name+'. '+E.Message, 534 );
             CloseFile(FCaps);
             Exit;
         End;
     End;


     cktElem := BranchList.First;
     With ActiveCircuit Do
     WHILE cktElem <> NIL Do
       Begin
         If CktElem.Enabled Then Begin
           ActiveCktElement := cktElem;
           Inc(NBranches);
           WriteActiveDSSObject(FBranches, 'New');     // sets HasBeenSaved := TRUE
           If ActiveCktElement.HasControl Then Begin
              ActiveCktElement := ActiveCktElement.ControlElement;
              WriteActiveDSSObject(FBranches, 'New');  //  regulator control ...Also, relays, switch controls
           End;

           shuntElement := Branchlist.FirstObject;
           While shuntElement <> Nil Do
             Begin
                 ActiveCktElement := shuntElement;
                 If (shuntElement.DSSObjType and Classmask)=LOAD_ELEMENT Then  Begin
                     LoadElement := TLoadObj(shuntElement);
                     If LoadElement.HasBeenAllocated Then Begin
                       {Manually set the allocation factor so it shows up}
                       Parser.CmdString := 'allocationfactor='+Format('%-.4g',[LoadElement.AllocationFactor]);
                       LoadElement.Edit;
                     End;
                     ActiveCktElement := shuntElement; // reset in case Edit mangles it
                     Inc(NLoads);
                     WriteActiveDSSObject(FLoads, 'New');
                 End Else If (shuntElement.DSSObjType and Classmask)=GEN_ELEMENT Then Begin
                     Inc(NGens);
                     WriteActiveDSSObject(FGens, 'New');
                     If ActiveCktElement.HasControl Then Begin
                        ActiveCktElement := ActiveCktElement.ControlElement;
                        WriteActiveDSSObject(FGens, 'New');
                     End;
                 End
                 Else If (shuntElement.DSSObjType and Classmask)=CAP_ELEMENT Then Begin
                     Inc(NCaps);
                     WriteActiveDSSObject(FCaps, 'New');
                     If ActiveCktElement.HasControl Then Begin
                        ActiveCktElement := ActiveCktElement.ControlElement;
                        WriteActiveDSSObject(FCaps, 'New');
                     End;
                 End Else Begin
                   Inc(NShunts);
                   WriteActiveDSSObject(Fshunts, 'New');
                 End;
               shuntElement := BranchList.NextObject
             End;
          End; {if enabled}

        cktElem := BranchList.GoForward;
       End;{WHILE}

     CloseFile(FBranches);
     CloseFile(Fshunts);
     CloseFile(FLoads);
     CloseFile(FGens);
     CloseFile(FCaps);

     {If any records were written to the file, record their relative names}
     If NBranches>0  Then SavedFileList.Add (dirname + '\Branches.dss') else DeleteFile('Branches.dss');
     If NShunts>0 Then SavedFileList.Add (dirname + '\Shunts.dss') else DeleteFile('Shunts.dss');
     If NLoads>0  Then SavedFileList.Add (dirname + '\Loads.dss') else DeleteFile('Loads.dss');
     If NGens>0   Then SavedFileList.Add (dirname + '\Generators.dss') else DeleteFile('Generators.dss');
     If NCaps>0   Then SavedFileList.Add (dirname + '\Capacitors.dss') else DeleteFile('Capacitors.dss');

   End; {IF}

end;


procedure TEnergyMeterObj.SetDragHandRegister(Reg: Integer;  const Value: Double);
begin
    If  Value > Registers[reg] Then Begin
       Registers[reg]   := Value;
       Derivatives[reg] := Value;  // Use this for   demand interval data;
    End;
end;

procedure TEnergyMeterObj.CloseDemandIntervalFile;
Var i:integer;
begin

  Try
     IF This_Meter_DIFileIsOpen Then Begin
       CloseFile(DI_File);
       This_Meter_DIFileIsOpen := FALSE;
     End;
  Except
     ON E:Exception Do DoSimpleMsg('Error Closing Demand Interval file for Meter "'+Name+'"', 534   );
  End;

  
     {Write Registers to Totals File}
     Write(energyMeterClass.FMeterTotals, '"', Name,'"');
     For i := 1 to NumEMregisters Do Write(energyMeterClass.FMeterTotals, Format(', %-g', [Registers[i]]));
     Writeln(energyMeterClass.FMeterTotals);
end;

procedure TEnergyMeterObj.OpenDemandIntervalFile;
Var i:Integer;
begin

  Try
      IF This_Meter_DIFileIsOpen Then CloseDemandIntervalFile;

      If EnergyMeterClass.FDI_Verbose Then Begin
          AssignFile(DI_File, MakeDIFileName);
          Rewrite(DI_File);
          This_Meter_DIFileIsOpen := TRUE;
          Write(DI_File,'"Hour"');
          For i := 1 to NumEMRegisters Do Write(DI_File,', "',TEnergyMeter(ParentClass).RegisterNames[i], '"');
          Writeln(DI_File);
      End;
  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file "' + Name + '.CSV' +' for writing.'+CRLF+E.Message, 535);
  End;

end;

procedure TEnergyMeterObj.WriteDemandIntervalData;
Var i:Integer;

begin
      If EnergyMeterClass.FDI_Verbose and This_Meter_DIFileIsOpen Then Begin
        With ActiveCircuit.Solution Do Write(DI_File, Format('%-.6g, ',[(Hour + dynavars.t/3600.0)]));
        For i := 1 to NumEMRegisters Do Write(DI_File, Format('%-.6g, ',[Derivatives[i]]));
        Writeln(DI_File);
      End;

      {Add to Class demand interval registers}
      With EnergyMeterClass Do For i := 1 to NumEMRegisters Do DI_RegisterTotals[i] := DI_RegisterTotals[i] + Derivatives[i]*TotalsMask[i];

end;

procedure TEnergyMeter.CloseAllDIFiles;
VAR
   mtr:TEnergyMeterObj;

Begin
      If FSaveDemandInterval Then Begin
        {While closing DI files, write all meter registers to one file}
        Try
            CreateMeterTotals;
        Except
            On E:Exception Do DoSimpleMsg('Error on Rewrite of totals file: '+E.Message, 536);
        End;

        mtr := ActiveCircuit.EnergyMeters.First;
        WHILE mtr<>NIL DO
        Begin
            IF mtr.enabled Then mtr.CloseDemandIntervalFile;
            mtr := ActiveCircuit.EnergyMeters.Next;
        End;
        WriteTotalsFile;  // Sum all energymeter registers to "Totals.CSV"
        SystemMeter.CloseDemandIntervalFile;
        SystemMeter.Save;
        CloseFile(FMeterTotals);
        CloseFile(FDI_Totals);
        DIFilesAreOpen := FALSE;
      End;
end;

procedure TEnergyMeterObj.AppendDemandIntervalFile;

Var
    FileNm:String;
begin

  {Only called if "SaveDemandInterval"}

  If This_Meter_DIFileIsOpen Then Exit;

  Try
      If Energymeterclass.FDI_Verbose Then Begin
        FileNm := MakeDIFileName;   // Creates directory if it doesn't exist
        AssignFile(DI_File, FileNm );
        {File Must Exist}
        If FileExists(FileNm) Then Append(DI_File) Else Rewrite(DI_File);
        This_Meter_DIFileIsOpen := TRUE;
      End;
  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file "'+Name+'.CSV' +' for appending.'+CRLF+E.Message, 537);
  End;
end;

procedure TEnergyMeter.AppendAllDIFiles;
VAR
   mtr:TEnergyMeterObj;
   Filenm:String;

Begin
      If FSaveDemandInterval Then  Begin

          ClearDI_Totals;  // clears accumulator arrays

          mtr := ActiveCircuit.EnergyMeters.First;
          WHILE mtr<>NIL DO Begin
              IF mtr.enabled Then mtr.AppendDemandIntervalFile;
              mtr := ActiveCircuit.EnergyMeters.Next;
          End;

          SystemMeter.AppendDemandIntervalFile;

          {Open FDI_Totals}
          Try
              FileNm :=  DI_Dir+'\DI_Totals.CSV';
              {File Must Exist}
              If FileExists(FileNm) Then  Begin
                AssignFile(FDI_Totals, FileNm );    // re-establishes connection to file
                Append(FDI_Totals) ;
              End
              Else CreateFDI_Totals;
          Except
              On E:Exception Do DosimpleMsg('Error opening demand interval file "'+Name+'.CSV' +' for appending.'+CRLF+E.Message, 538);
          End;

          DIFilesAreOpen := TRUE;

      End;{IF}
end;

function TEnergyMeterObj.MakeDIFileName: String;
begin
    Result := EnergyMeterClass.DI_Dir + '\' + Name + '.CSV';
end;

procedure TEnergyMeter.Set_SaveDemandInterval(const Value: Boolean);
begin
  FSaveDemandInterval := Value;
  ResetAll;
end;

procedure TEnergyMeter.ClearDI_Totals;
Var i:integer;
begin
   For i := 1 to NumEMRegisters Do DI_RegisterTotals[i] := 0.0;
end;

procedure TEnergyMeter.CreateFDI_Totals;
Var i:Integer;
begin
 Try
    AssignFile(FDI_Totals, DI_Dir+'\DI_Totals.CSV');
    Rewrite(FDI_Totals);
    Write(FDI_Totals,'Time');
    For i := 1 to NumEMRegisters Do Write(FDI_Totals,', "', RegisterNames[i],'"');
    Writeln(FDI_Totals);
 Except
    On E:Exception Do DoSimpleMsg('Error creating: "'+DI_Dir+'\DI_Totals.CSV": '+E.Message, 539)
 End;
end;

{ TSystemMeter }

procedure TSystemMeter.AppendDemandIntervalFile;
Var
    FileNm:String;
begin

  {Only called if "SaveDemandInterval"}

  If This_Meter_DIFileIsOpen Then Exit;

  Try
      FileNm := EnergyMeterClass.Di_Dir + '\DI_SystemMeter.CSV';
      AssignFile(SystemDIFile, FileNm );
      {File Must Exist}
      If FileExists(FileNm) Then Append(SystemDIFile) Else Rewrite(SystemDIFile);
      This_Meter_DIFileIsOpen := TRUE;
  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file "'+FileNm +' for appending.'+CRLF+E.Message, 540);
  End;

end;

procedure TSystemMeter.Clear;
begin
        kWh := 0.0;
        kvarh := 0.0;
        peakkW := 0.0;
        peakkVA := 0.0;
        Losseskwh := 0.0;
        Losseskvarh := 0.0;
        PeakLosseskW := 0.0;
        dkWh := 0.0;
        dkvarh := 0.0;
        dLosseskwh := 0.0;
        dLosseskvarh := 0.0;
        FirstSampleAfterReset := TRUE;
end;

procedure TSystemMeter.CloseDemandIntervalFile;
begin
     IF This_Meter_DIFileIsOpen Then Begin
       CloseFile(SystemDIFile);
       This_Meter_DIFileIsOpen := FALSE;
     End;
end;

constructor TSystemMeter.Create;
begin
     Clear;
     This_Meter_DIFileIsOpen := FALSE;
end;

destructor TSystemMeter.Destroy;
begin
  inherited;

end;

procedure TSystemMeter.Integrate(Var Reg:Double; Value:Double; Var Deriv:Double);
begin
     IF ActiveCircuit.TrapezoidalIntegration THEN
      Begin
        {Trapezoidal Rule Integration}
        If Not FirstSampleAfterReset Then Reg := Reg + 0.5 * ActiveCircuit.Solution.IntervalHrs * (Value + Deriv);
      End
     ELSE   {Plain Euler integration}
         Reg := Reg + ActiveCircuit.Solution.IntervalHrs * Value;

     Deriv := Value;

end;

procedure TSystemMeter.OpenDemandIntervalFile;
begin

  Try
      IF This_Meter_DIFileIsOpen Then CloseFile(SystemDIFile);

      AssignFile(SystemDIFile, EnergyMeterClass.DI_Dir+'\DI_SystemMeter.CSV');
      Rewrite(SystemDIFile);
      This_Meter_DIFileIsOpen := TRUE;
      Write(SystemDIFile,'"Hour", ');
      WriteRegisterNames(SystemDIfile);
      Writeln(SystemDIFile);
  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file "DI_SystemMeter.CSV"  for writing.'+CRLF+E.Message, 541);
  End;


end;

procedure TSystemMeter.Reset;
begin
    Clear;
   // removed - open in solution If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;
end;

procedure TSystemMeter.Save;
Var  F:Textfile;
     CSVName, Folder:String;
begin
 Try
       CSVName := 'SystemMeter.CSV';
       {If we are doing a simulation and saving interval data, create this in the
        same directortory as the demand interval data}
       If  energyMeterClass.SaveDemandInterval Then
          Folder := energyMeterClass.DI_DIR + '\'
       Else
          Folder := DSSDataDirectory;
       AssignFile(F, Folder + CSVName);
       Rewrite(F);
       GlobalResult := CSVName;
  Except
      On E: Exception DO
      Begin
       DoSimpleMsg('Error opening System Meter File "' + CRLF + CSVName + '": ' + E.Message, 542);
       Exit;
      End
  End;

 Try
       Write(F, 'Year, ');
       WriteRegisterNames(F);
       Writeln(F);
       
       Write(F, ActiveCircuit.Solution.Year:0);
       WriteRegisters(F);
       Writeln(F);

 Finally
       CloseFile(F);
 End;
end;

procedure TSystemMeter.TakeSample;

begin

  {Get total system energy out of the sources}

  cPower := CmulReal(GetTotalPowerFromSources, 0.001);  // convert to kW

  Integrate(kWh, cPower.re, dkwh);
  Integrate(kvarh, cPower.im, dkvarh);

  PeakkW := Max(cPower.re, PeakkW);
  Peakkva := Max(Cabs(cPower), Peakkva);

  {Get total circuit losses}
   cLosses := ActiveCircuit.Losses;
   cLosses := CmulReal(cLosses, 0.001);  // convert to kW

   Integrate(Losseskwh, cLosses.re, dLosseskwh);
   Integrate(Losseskvarh, cLosses.im, dLosseskvarh);
   
  PeakLosseskW := Max(cLosses.re, PeakLosseskW);

  FirstSampleAfterReset := FALSE;
  IF This_Meter_DIFileIsOpen then WriteDemandIntervalData;

end;

procedure TEnergyMeter.CreateMeterTotals;
Var i:Integer;
begin
    AssignFile(FMeterTotals, DI_Dir+'\EnergyMeterTotals.CSV');
    Rewrite(FMeterTotals);
    Write(FMeterTotals,'Name');
    For i := 1 to NumEMRegisters Do Write(FMeterTotals,', "', RegisterNames[i],'"');
    Writeln(FMeterTotals);
end;

procedure TSystemMeter.WriteDemandIntervalData;
begin
   With ActiveCircuit.Solution Do Write(SystemDIFile, Format('%-.6g',[(Hour + dynavars.t/3600.0)]));
   Write(SystemDIFile, Format(', %-g', [cPower.re]));
   Write(SystemDIFile, Format(', %-g', [cPower.im]));
   Write(SystemDIFile, Format(', %-g',[peakkW]));
   Write(SystemDIFile, Format(', %-g',[peakkVA]));

   Write(SystemDIFile, Format(', %-g', [cLosses.re]));
   Write(SystemDIFile, Format(', %-g', [cLosses.im]));
   Write(SystemDIFile, Format(', %-g', [PeakLosseskW]));
   Writeln(SystemDIFile);

end;

procedure TSystemMeter.WriteRegisterNames(var F: TextFile);
begin
  Write(F, 'kWh, kvarh, "Peak kW", "peak kVA", "Losses kWh", "Losses kvarh", "Peak Losses kW"');
end;

procedure TSystemMeter.WriteRegisters(var F: TextFile);
begin
     Write(F, Format(', %-g',[kWh]));
     Write(F, Format(', %-g',[kvarh]));
     Write(F, Format(', %-g',[peakkW]));
     Write(F, Format(', %-g',[peakkVA]));
     Write(F, Format(', %-g',[Losseskwh]));
     Write(F, Format(', %-g',[Losseskvarh]));
     Write(F, Format(', %-g',[PeakLosseskW]));
end;

procedure TEnergyMeter.Set_DI_Verbose(const Value: Boolean);
begin
  FDI_Verbose := Value;
  ResetAll;
end;

procedure TEnergyMeter.WriteTotalsFile;
Var
   mtr:TEnergyMeterObj;
   Regsum:TRegisterArray;
   i :Integer;
   F:Textfile;

begin
  {Sum up all registers of all meters and write to Totals.CSV}
  For i := 1 to NumEMRegisters Do RegSum[i] := 0.0;

  mtr := ActiveCircuit.EnergyMeters.First;
  WHILE mtr<>NIL DO Begin
      IF mtr.enabled Then With Mtr Do
         For i := 1 to NumEMRegisters Do
            Regsum[i] := Regsum[i] + Registers[i] * TotalsMask[i] ;

      mtr := ActiveCircuit.EnergyMeters.Next;
  End;

  Try     // Write the file

        AssignFile(F, DI_Dir + '\Totals.CSV' );
        Rewrite(F);
        Write(F,'Year');
        For i := 1 to NumEMRegisters Do Write(F,', "', RegisterNames[i],'"');
        Writeln(F);
        Write(F, ActiveCircuit.Solution.Year:0);
        For i := 1 to NumEMRegisters Do Write(F,Format(', %-g ', [RegSum[i]]));
        Writeln(F);
        CloseFile(F);
  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file Totals.CSV.'+CRLF+E.Message, 543);
  End;
  
end;

procedure TEnergyMeter.InterpretRegisterMaskArray(Var Mask: TRegisterArray);

Var i,n:integer;
begin
     n := Parser.ParseAsVector(NumEMRegisters, @Mask);
     For i := n+1 to NumEMRegisters Do Mask[i] := 1.0;  // Set the rest to 1
end;

procedure TEnergyMeterObj.MakeFeederObj;
begin
  If Assigned(MeteredElement) Then Begin
    FeederClass.NewObject(Name);  // NewObject creates only if not existent. Else Inits  and desynchs
    FeederObj := ActiveCircuit.ActiveCktElement as TFeederObj;
    FeederObj.SetBus (1, MeteredElement.GetBus(MeteredTerminal));
    FeederObj.Nphases := MeteredElement.NPhases;
    FeederObj.Nconds  := MeteredElement.Nconds;
    // FeederObj.Enabled := ActiveCircuit.RadialSolution;
  End
  Else DoSimpleMsg('Error: Attempted to make Feeder Obj without instantiating Metered Element in Energymeter.'+name,544);
end;

procedure TEnergyMeterObj.RemoveFeederObj;
begin
    If Assigned(FeederObj) Then Begin
       FeederObj.Enabled := FALSE;
       FeederObj.SetCktElementFeederFlags (FALSE);
    End;
end;

procedure TEnergyMeterObj.EnableFeeder;
// HasFeeder has to be true before feederObj will be re-enabled.
begin
    If HasFeeder Then Begin
        If Not Assigned(FeederObj) Then MakeFeederObj
        Else FeederObj.Enabled := TRUE;
        FeederObj.SetCktElementFeederFlags (TRUE);
    End;
end;

procedure TEnergyMeter.OpenAllDIFiles;
{Similar to Append, by creates the files.}

VAR
   mtr:TEnergyMeterObj;
  // Filenm:String;
begin

      If FSaveDemandInterval Then  Begin

          ClearDI_Totals;  // clears accumulator arrays

          mtr := ActiveCircuit.EnergyMeters.First;
          WHILE mtr<>NIL DO Begin
              IF mtr.enabled Then mtr.OpenDemandIntervalFile;
              mtr := ActiveCircuit.EnergyMeters.Next;
          End;

          SystemMeter.OpenDemandIntervalFile;

          {Open FDI_Totals}
          Try
             CreateFDI_Totals;

          Except
              On E:Exception Do DosimpleMsg('Error opening demand interval file "'+Name+'.CSV' +' for appending.'+CRLF+E.Message, 538);
          End;

          DIFilesAreOpen := TRUE;

      End;{IF}


end;

initialization

  PCElementList := TList.Create;
  PDElementList := TList.Create;

  {RegisterNameList := TCommandList.Create(['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Zone kWh',
  'Zone kvarh', 'Zone Max kW','Zone Max kVA','Overload kWh Normal','Overload kWh Emerg','Load EEN',
  'Load UE', 'Zone Losses kWh', 'Zone Losses kvarh', 'Zone Max kW Losses', 'Zone Max kvar Losses',
  'Gen kWh', 'Gen kvarh', 'Gen Max kW', 'Gen Max kVA']); }


Finalization

  PCElementList.Free;
  PDElementList.Free;
  
end.
