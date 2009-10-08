unit Storage;

{
  ----------------------------------------------------------
  Copyright (c) 2009, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    10/04/2009 Created from Generator Model
    

  To Do:
    Make connection to User model
    Yprim for various modes
    Update storage when sampled
    Implement StorageController
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
}
{
  The storage element is essentially a generator that can be dispatched
  to either produce power or consume power commensurate with rating and
  amount of stored energy.
}

//  The Storage element is assumed balanced over the no. of phases defined


interface

USES  StoreUserModel, DSSClass,  PCClass, PCElement, ucmatrix, ucomplex, LoadShape, Spectrum, ArrayDef, Dynamics;

Const  NumStorageRegisters = 6;    // Number of energy meter registers
       NumStorageVariables = 4;    // No state variables that need integrating.

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorage = CLASS(TPCClass)
     private

       Procedure InterpretConnection(const S:String);
       Procedure SetNcondsForConnection;
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherStorageObjName:STring):Integer;Override;
     public
       RegisterNames:Array[1..NumStorageRegisters] of String;

       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       Procedure ResetRegistersAll;
       Procedure SampleAll;
       Procedure UpdateAll;

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageObj = class(TPCElement)
      Private
        Yeq             :Complex;   // at nominal
        Yeq95           :Complex;   // at 95%
        Yeq105          :Complex;   // at 105%
        YeqIdling       :Complex;   // in shunt representing idle impedance

        DebugTrace      :Boolean;
        DispatchMode    :Integer;
        FState          :Integer;
        FStateChanged   :Boolean;
        FirstSampleAfterReset  :Boolean;
        StorageSolutionCount   :Integer;
        StorageFundamental     :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        StorageObjSwitchOpen   :Boolean;

        kVANotSet       :Boolean;
        kVArating       :Double;
        kVStorageBase   :Double;
        kWrating        :double;
        kWhRating       :Double;
        kWhStored       :Double;
        kWhReserve      :Double;
        pctReserve      :Double;
        pctIdlekW       :Double;
        pctIdlekvar     :Double;
        pctChargeEff    :Double;
        pctDischargeEff :Double;
        ChargeEff       :Double;
        DischargeEff    :Double;
        pctkWout        :Double;   // percent of kW rated output currently dispatched
        pctkWin         :Double;
        DischargeTrigger:Double;
        ChargeTrigger   :Double;


        pctR            :Double;
        pctX            :Double;

        OpenStorageSolutionCount :Integer;
        Pnominalperphase :Double;
        Qnominalperphase :Double;
        RandomMult      :Double;

        Reg_Hours       :Integer;
        Reg_kvarh       :Integer;
        Reg_kWh         :Integer;
        Reg_MaxkVA      :Integer;
        Reg_MaxkW       :Integer;
        Reg_Price       :Integer;
        ShapeFactor     :Complex;
        Thetaharm       :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        Tracefile       :TextFile;
        UserModel       :TStoreUserModel;   {User-Written Models}

        varBase         :Double; // Base vars per phase
        VBase           :Double;  // Base volts suitable for computing currents
        VBase105        :Double;
        VBase95         :Double;
        Vmaxpu          :Double;
        Vminpu          :Double;
        Vthevharm       :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        YPrimOpenCond   :TCmatrix;
        RThev           :Double;
        XThev           :Double;


        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);
        Procedure CalcStorageModelContribution;
        Procedure CalcInjCurrentArray;
        (*Procedure CalcVterminal;*)
        Procedure CalcVTerminalPhase;
        PROCEDURE CalcYearlyMult(Hr:double);
        Procedure CalcYPrimMatrix(Ymatrix:TcMatrix);

        Procedure DoConstantPQStorageObj;
        Procedure DoConstantZStorageObj;
        PROCEDURE DoDynamicMode;
        PROCEDURE DoHarmonicMode;
        Procedure DoUserModel;

        Procedure Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);
        Procedure SetDragHandRegister(Reg:Integer; const Value:Double);
        Procedure StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);

        Procedure WriteTraceRecord(const s:string);

        procedure SyncUpPowerQuantities;
        Procedure SetKWandKvarOut;
        Procedure CheckStateTriggerLevel(Level:Double);
        Procedure UpdateStorage;    // Update Storage elements based on present kW and IntervalHrs variable

        Function InterpretState(const S:String):Integer;
        Function DecodeState:String;

        Function Get_PresentkW:Double;
        Function Get_Presentkvar:Double;
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double);
        procedure Set_Presentkvar(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);

      Protected
        PROCEDURE Set_ConductorClosed(Index:Integer; Value:Boolean); Override;
        Procedure GetTerminalCurrents(Curr:pComplexArray); Override ;

      public

        Connection      :Integer;  {0 = line-neutral; 1=Delta}
        DailyShape      :String;  // Daily (24 HR) Generator shape
        DailyShapeObj   :TLoadShapeObj;  // Daily Generator Shape for this load
        DutyShape       :String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj    :TLoadShapeObj;  // Shape for this generator
        StorageClass    :Integer;
        VoltageModel    :Integer;   // Variation with voltage
        kvar_out        :Double;
        kW_out          :Double;
        PFNominal       :Double;
        YearlyShape     :String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj  :TLoadShapeObj;  // Shape for this Generator

        Registers,  Derivatives         :Array[1..NumStorageRegisters] of Double;

        constructor Create(ParClass :TDSSClass; const SourceName :String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;
        Procedure CalcYPrim; Override;

        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;
        Function  NumVariables:Integer;Override;
        Procedure GetAllVariables(States:pDoubleArray);Override;
        Function  Get_Variable(i: Integer): Double; Override;
        procedure Set_Variable(i: Integer; Value: Double);  Override;
        Function  VariableName(i:Integer):String ;Override;

        Procedure SetNominalStorageOuput;
        Procedure Randomize(Opt:Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        Procedure ResetRegisters;
        Procedure TakeSample;

        // Support for Dynamics Mode
        Procedure InitStateVars; Override;
        Procedure IntegrateStates;Override;

        // Support for Harmonics Mode
        Procedure InitHarmonics; Override;

       PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;

       Property PresentkW    :Double  Read Get_PresentkW   Write Set_PresentkW;
       Property Presentkvar  :Double  Read Get_Presentkvar Write Set_Presentkvar;
       Property PresentkV    :Double  Read Get_PresentkV   Write Set_PresentkV;
       Property PowerFactor  :Double  Read PFNominal       Write Set_PowerFactor;

   End;

VAR
    ActiveStorageObj:TStorageObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit,  Sysutils, Command, Math, MathUtil, DSSClassDefs, DSSGlobals, Utilities;

Const

{  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit function
    3) add a statement(s) to InitPropertyValues function to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, if needed
 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =}

  propKV         =  3;
  propKW         =  4;
  propPF         =  5;
  propMODEL      =  6;
  propYEARLY     =  7;
  propDAILY      =  8;
  propDUTY       =  9;
  propDISPMODE   = 10;
  propIDLEKVAR   = 11;
  propCONNECTION = 12;
  propKVAR       = 13;
  propPCTR       = 14;
  propPCTX       = 15;
  propIDLEKW     = 16;
  propCLASS      = 17;
  propDISPOUTTRIG= 18;
  propDISPINTRIG = 19;
  propCHARGEEFF  = 20;
  propDISCHARGEEFF = 21;
  propPCTKWOUT   = 22;
  propVMINPU     = 23;
  propVMAXPU     = 24;
  propSTATE      = 25;
  propKVA        = 26;
  propKWRATED    = 27;
  propKWHRATED   = 28;
  propKWHSTORED  = 29;
  propPCTRESERVE = 30;      
  propUSERMODEL  = 31;
  propUSERDATA   = 32;
  propDEBUGTRACE = 33;
  propPCTKWIN    =34;

  NumPropsThisClass = 34; // Make this agree with the last property constant

//= = = = = = = = = = = = = = DEFINE STATES = = = = = = = = = = = = = = = = = = = = = = = = =

  STATE_CHARGING    = -1;
  STATE_IDLING      =  0;
  STATE_DISCHARGING =  1;

//= = = = = = = = = = = = = = DEFINE DISPATCH MODES = = = = = = = = = = = = = = = = = = = = = = = = =

      DEFAULT = 0;
      LOADMODE = 1;
      PRICEMODE = 2;

Var cBuffer:Array[1..24] of Complex;  // Temp buffer for calcs  24-phase generator?
    CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TStorage.Create;  // Creates superstructure for all Storage elements
Begin
     Inherited Create;
     Class_Name := 'Storage';
     DSSClassType := DSSClassType + STORAGE_ELEMENT;  // In both PCelement and Storage element list

     ActiveElement := 0;

     // Set Register names
     RegisterNames[1]  := 'kWh';
     RegisterNames[2]  := 'kvarh';
     RegisterNames[3]  := 'Max kW';
     RegisterNames[4]  := 'Max kVA';
     RegisterNames[5]  := 'Hours';
     RegisterNames[6]  := '$';

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TStorage.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TStorage.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;   {see DSSClass}

     // Define Property names
     {
      Using the AddProperty function, you can list the properties here in the order you want
      them to appear when properties are accessed sequentially without tags.   Syntax:

      AddProperty( <name of property>, <index in the EDIT Case statement>, <help text>);

     }
     AddProperty('phases',    1,
                              'Number of Phases, this Storage element.  Power is evenly divided among phases.');
     AddProperty('bus1',      2,
                              'Bus to which the Storage element is connected.  May include specific node specification.');
     AddProperty('kv',        propKV,
                              'Nominal rated (1.0 per unit) voltage, kV, for Storage element. For 2- and 3-phase Storage elements, specify phase-phase kV. '+
                              'Otherwise, specify actual kV across each branch of the Storage element. '+
                              'If wye (star), specify phase-neutral kV. '+
                              'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
     AddProperty('kW',        propKW,
                              'Get/set the present kW value.  A positive value denotes power coming OUT of the element, '+CRLF+
                              'which is the opposite of a Load element. A negative value indicates the Storage element is in Charging state. ' +
                              'This value is modified internally depending on the dispatch mode. ' );
     AddProperty('pf',        propPF,
                              'Nominally, the power factor for discharging (acting as a generator). Default is 1.0. ' +
                              'Setting this property will also set the kvar property.' +
                              'Enter negative for leading powerfactor '+
                              '(when kW and kvar have opposite signs.)'+CRLF+
                              'A positive power factor for a generator signifies that the Storage element produces vars ' + CRLF +
                              'as is typical for a generator.  ');
     AddProperty('conn',      propCONNECTION,
                              '={wye|LN|delta|LL}.  Default is wye.');
     AddProperty('kvar',      propKVAR,
                              'Get/set the present kW value.  Alternative to specifying the power factor.  Side effect: '+
                              ' the power factor value is altered to agree based on present value of kW.');
     AddProperty('kVA',       propKVA,
                              'kVA rating of power output. Defaults to rated kW. Used as the base for Dynamics mode and Harmonics mode values.');
     AddProperty('kWrated',   propKWRATED,
                              'kW rating of power output. Side effect: Set KVA property.');

     AddProperty('kWhrated',  propKWHRATED,
                              'Rated storage capacity in kWh. Default is 50.');
     AddProperty('kWhstored', propKWHSTORED,
                              'Present amount of energy stored, kWh. Default is 50.');
     AddProperty('%reserve',  propPCTRESERVE,
                              'Percent of rated kWh storage capacity to be held in reserve for normal operation. Default = 20. ' + CRLF +
                              'This is treated as the minimum energy discharge level unless there is an emergency. For emergency operation ' +
                              'set this property lower. Cannot be less than zero.');
     AddProperty('State',     propSTATE,
                              '{IDLING | CHARGING | DISCHARGING}  Get/Set present operational state. In DISCHARGING mode, the Storage element ' +
                              'acts as a generator and the kW property is positive. The element continues discharging at the scheduled output power level ' +
                              'until the storage reaches the reserve value. Then the state reverts to IDLING. ' +
                              'In the CHARGING state, the Storage element behaves like a Load and the kW property is negative. ' +
                              'The element continues to charge until the max storage kWh is reached and then switches to IDLING state. ' +
                              'In IDLING state, the kW property shows zero. However, the resistive and reactive loss elements remain in the circuit ' +
                              'and the power flow report will show power being consumed.');
     AddProperty('%Discharge',  propPCTKWOUT,
                              'Discharge rate (output power) in Percent of rated kW. Default = 100.');
     AddProperty('%Charge',  propPCTKWIN,
                              'Charging rate (input power) in Percent of rated kW. Default = 100.');
     AddProperty('%EffCharge',propCHARGEEFF,
                              'Percent efficiency for CHARGING the storage element. Default = 90.');
     AddProperty('%EffDischarge',propDISCHARGEEFF,
                              'Percent efficiency for DISCHARGING the storage element. Default = 90.' +
                              'Idling losses are handled by %IdlingkW property and are in addition to the charging and discharging efficiency losses ' +
                              'in the power conversion process inside the unit.');
     AddProperty('%IdlingkW', propIDLEKW,
                              'Percent of rated kW consumed while idling. Default = 1.');
     AddProperty('%Idlingkvar', propIDLEKVAR,
                              'Percent of rated kW consumed as reactive power (kvar) while idling. Default = 0.');
     AddProperty('%R',        propPCTR,
                              'Equivalent percent internal resistance, ohms. Default is 0. Placed in series with internal voltage source' +
                              ' for harmonics and dynamics modes. Use a combination of %IdlekW and %EffCharge and %EffDischarge to account for ' +
                              'losses in power flow modes.');
     AddProperty('%X',        propPCTX,
                              'Equivalent percent internal reactance, ohms. Default is 50%. Placed in series with internal voltage source' +
                              ' for harmonics and dynamics modes. (Limits fault current to 2 pu.) ' +
                              'Use %Idlekvar and kvar properties to account for any reactive power during power flow solutions.');
     AddProperty('model',     propMODEL,
                              'Integer code for the model to use for powet output variation with voltage. '+
                              'Valid values are:' +CRLF+CRLF+
                              '1:Storage element injects a CONSTANT kW at specified power factor.'+CRLF+
                              '2:Storage element is modeled as a CONSTANT ADMITTANCE.'  +CRLF+
                              '3:Compute load injection from User-written Model.');

     AddProperty('Vminpu',       propVMINPU,
                                 'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
                                 'Below this value, the load model reverts to a constant impedance model.');
     AddProperty('Vmaxpu',       propVMAXPU,
                                 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
                                 'Above this value, the load model reverts to a constant impedance model.');
     AddProperty('yearly',       propYEARLY,
                                 'Dispatch shape to use for yearly simulations.  Must be previously defined '+
                                 'as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated '+
                                 'during Yearly solution modes. In the default dispatch mode, ' +
                                 'the Storage element uses this loadshape to trigger State changes.');
     AddProperty('daily',        propDAILY,
                                 'Dispatch shape to use for daily simulations.  Must be previously defined '+
                                 'as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, '+
                                 'the Storage element uses this loadshape to trigger State changes.'); // daily dispatch (hourly)
     AddProperty('duty',          propDUTY,
                                 'Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
                                 'Must be previously defined as a Loadshape object. '+
                                 'Typically would have time intervals of 1-5 seconds. '+
                                 'Designate the number of points to solve using the Set Number=xxxx command. '+
                                 'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation
     AddProperty('dispmode',     propDISPMODE,
                                 '{Default | Loadlevel | Price } Default = "Default". Dispatch mode. '+
                                 'In Default mode, Storage element state is triggered by the loadshape curve corresponding to the solution mode. '+
                                 'For the other two dispatch modes, the Storage element state is controlled by either the global default Loadlevel value or the price level. ');
     AddProperty('dischargetrigger', propDISPOUTTRIG,
                                 'Dispatch trigger value for discharging the storage. '+CRLF+
                                 'If = 0.0 the Storage element state is changed by the State command or by a StorageController object. ' +CRLF+
                                 'If <> 0  the Storage element state is set to DISCHARGING when this trigger level is EXCEEDED by either the specified ' +
                                 'Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.');
     AddProperty('Chargetrigger', propDISPINTRIG,
                                 'Dispatch trigger value for charging the storage. '+CRLF+
                                 'If = 0.0 the Storage element state is changed by the State command or StorageController object.  ' +CRLF+
                                 'If <> 0  the Storage element state is set to CHARGING when this trigger level is GREATER than either the specified ' +
                                 'Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.');

     AddProperty('class',       propCLASS,
                                'An arbitrary integer number representing the class of Generator so that Generator values may '+
                                'be segregated by class.'); // integer

     AddProperty('UserModel',   propUSERMODEL,
                                'Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, ' +
                                'overriding the default model.  Set to "none" to negate previous setting.');
     AddProperty('UserData',    propUSERDATA,
                                'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
     AddProperty('debugtrace',  propDEBUGTRACE,
                                '{Yes | No }  Default is no.  Turn this on to capture the progress of the generator model ' +
                                'for each iteration.  Creates a separate file for each Storage element named "STORAGE_name.CSV".' );



     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
     PropertyHelp[NumPropsThisClass +1] := 'Name of harmonic voltage or current spectrum for this Storage element. ' +
                         'Current injection is assumed for inverter. ' +
                         'Default value is "default", which is defined when the DSS starts.';

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TStorage.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Generator and add it to Generator class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TStorageObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TStorage.SetNcondsForConnection;

Begin
      With ActiveStorageObj Do
      Begin
           CASE Connection OF
             0: NConds := Fnphases +1;
             1: CASE Fnphases OF
                    1,2: NConds := Fnphases +1; // L-L and Open-delta
                ELSE
                    NConds := Fnphases;
                END;
           END;
      End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage.UpdateAll;
Var
     i :Integer;
begin
     For i := 1 to ElementList.ListSize  Do
        With TStorageObj(ElementList.Get(i)) Do
          if Enabled then UpdateStorage;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TStorage.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
VAR
     TestS:String;

Begin                       
      With ActiveStorageObj Do Begin
          TestS := lowercase(S);
          CASE TestS[1] OF
            'y','w': Connection := 0;  {Wye}
            'd': Connection := 1;  {Delta or line-Line}
            'l': CASE Tests[2] OF
                 'n': Connection := 0;
                 'l': Connection := 1;
                 End;
          End;

          SetNCondsForConnection;

          {VBase is always L-N voltage unless 1-phase device or more than 3 phases}

          Case Fnphases Of
               2,3: VBase := kVStorageBase * InvSQRT3x1000;    // L-N Volts
          Else
               VBase := kVStorageBase * 1000.0 ;   // Just use what is supplied
          End;

          VBase95  := Vminpu * VBase;
          VBase105 := Vmaxpu * VBase;

          Yorder := Fnconds * Fnterms;
          YPrimInvalid := True;
      End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION InterpretDispMode(const S:String):Integer;
BEGIN
        CASE lowercase(S)[1] of
             'l': Result := LOADMODE;
             'p': Result := PRICEMODE;
        ELSE
             Result := DEFAULT;
        END;
End;


//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -

Function TStorage.Edit:Integer;
VAR
       i, iCase,
       ParamPointer:Integer;
       ParamName:String;
       Param:String;

Begin

  // continue parsing with contents of Parser
  ActiveStorageObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveStorageObj;

  Result := 0;

  With ActiveStorageObj Do
  Begin

     ParamPointer := 0;
     ParamName    := Parser.NextParam;  // Parse next property off the command line
     Param        := Parser.StrValue;   // Put the string value of the property value in local memory for faster access
     While Length(Param)>0 Do
     Begin

         If  (Length(ParamName) = 0) Then Inc(ParamPointer)       // If it is not a named property, assume the next property
         ELSE ParamPointer := CommandList.GetCommand(ParamName);  // Look up the name in the list for this class

         If  (ParamPointer>0) and (ParamPointer<=NumProperties)
         Then PropertyValue[PropertyIdxMap[ParamPointer]] := Param   // Update the string value of the property
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for Generator "'+Name+'"', 560);

         If ParamPointer > 0 Then Begin
         iCase := PropertyIdxMap[ParamPointer];
         CASE iCASE OF
            0               : DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
            1               : NPhases    := Parser.Intvalue; // num phases
            2               : SetBus(1, param);
           propKV           : PresentkV    := Parser.DblValue;
           propKW           : kW_out       := Parser.DblValue;
           propPF           : PFNominal    := Parser.DblValue;
           propMODEL        : VoltageModel := Parser.IntValue;
           propYEARLY       : YearlyShape  := Param;
           propDAILY        : DailyShape  := Param;
           propDUTY         : DutyShape     := Param;
           propDISPMODE     : DispatchMode  := InterpretDispMode(Param);
           propIDLEKVAR     : pctIdlekvar   := Parser.DblValue;
           propCONNECTION   : InterpretConnection(Param);
           propKVAR         : Presentkvar   := Parser.DblValue;
           propPCTR         : pctR          := Parser.DblValue;
           propPCTX         : pctX          := Parser.DblValue;
           propIDLEKW       : pctIdlekW     := Parser.DblValue;
           propCLASS        : StorageClass  := Parser.IntValue;
           propDISPOUTTRIG  : DischargeTrigger := Parser.DblValue;
           propDISPINTRIG   : ChargeTrigger    := Parser.DblValue;
           propCHARGEEFF    : pctChargeEff     := Parser.DblValue;
           propDISCHARGEEFF : pctDischargeEff   := Parser.DblValue;
           propPCTKWOUT     : pctkWout     := Parser.DblValue;
           propVMINPU       : VMinPu       := Parser.DblValue;
           propVMAXPU       : VMaxPu       := Parser.DblValue;
           propSTATE        : FState       := InterpretState(Param); //****
           propKVA          : kVArating    := Parser.DblValue;
           propKWRATED      : kwrating      := Parser.DblValue ;
           propKWHRATED     : kWhrating    := Parser.DblValue;
           propKWHSTORED    : kWhstored    := Parser.DblValue;
           propPCTRESERVE   : pctReserve    := Parser.DblValue;
           propUSERMODEL    : UserModel.Name := Parser.StrValue;  // Connect to user written models
           propUSERDATA     : UserModel.Edit := Parser.StrValue;  // Send edit string to user model
           propDEBUGTRACE   : DebugTrace   := InterpretYesNo(Param);
           propPCTKWIN      : pctkWIn     := Parser.DblValue;


         ELSE
           // Inherited parameters
             ClassEdit(ActiveStorageObj, ParamPointer - NumPropsThisClass)
         End;

         CASE iCase OF
            1: SetNcondsForConnection;  // Force Reallocation of terminal info
            propKW,propPF: SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF

    {Set loadshape objects;  returns nil if not valid}
            propYEARLY: YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
            propDAILY:  DailyShapeObj := LoadShapeClass.Find(DailyShape);
            propDUTY:   DutyShapeObj := LoadShapeClass.Find(DutyShape);
            propKWRATED:  kVArating := kWrating;

            propDEBUGTRACE: IF DebugTrace THEN Begin   // Init trace file
                   AssignFile(TraceFile, DSSDataDirectory + 'STOR_'+Name+'.CSV');
                   ReWrite(TraceFile);
                   Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, StorageModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                   For i := 1 to nphases Do Write(Tracefile,  ', |Iinj'+IntToStr(i)+'|');
                   For i := 1 to nphases Do Write(Tracefile,  ', |Iterm'+IntToStr(i)+'|');
                   For i := 1 to nphases Do Write(Tracefile,  ', |Vterm'+IntToStr(i)+'|');
                   Write(TraceFile, ',Vthev, Theta');
                   Writeln(TraceFile);
                   CloseFile(Tracefile);
             End;

            propKVA: kVANotSet := FALSE;
         End;
         End;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
Function TStorage.MakeLike(Const OtherStorageObjName:String):Integer;

// Copy over essential properties from other object

VAR
     OtherStorageObj:TStorageObj;
     i:Integer;
Begin
     Result := 0;
     {See if we can find this line name in the present collection}
     OtherStorageObj := Find(OtherStorageObjName);
     If   (OtherStorageObj <> Nil)
     Then With ActiveStorageObj Do
     Begin
         If (Fnphases <> OtherStorageObj.Fnphases) Then Begin
           Nphases := OtherStorageObj.Fnphases;
           NConds := Fnphases;  // Forces reallocation of terminal stuff
           Yorder := Fnconds*Fnterms;
           YPrimInvalid := True;
         End;

         kVStorageBase := OtherStorageObj.kVStorageBase;
         Vbase          := OtherStorageObj.Vbase;
         Vminpu         := OtherStorageObj.Vminpu;
         Vmaxpu         := OtherStorageObj.Vmaxpu;
         Vbase95        := OtherStorageObj.Vbase95;
         Vbase105       := OtherStorageObj.Vbase105;
         kW_out         := OtherStorageObj.kW_out;
         kvar_out       := OtherStorageObj.kvar_out;
         Pnominalperphase   := OtherStorageObj.Pnominalperphase;
         PFNominal      := OtherStorageObj.PFNominal;
         Qnominalperphase   := OtherStorageObj.Qnominalperphase;
         Connection     := OtherStorageObj.Connection;
         YearlyShape    := OtherStorageObj.YearlyShape;
         YearlyShapeObj := OtherStorageObj.YearlyShapeObj;
         DailyShape     := OtherStorageObj.DailyShape;
         DailyShapeObj  := OtherStorageObj.DailyShapeObj;
         DutyShape      := OtherStorageObj.DutyShape;
         DutyShapeObj   := OtherStorageObj.DutyShapeObj;
         DispatchMode   := OtherStorageObj.DispatchMode;
         StorageClass   := OtherStorageObj.StorageClass;
         VoltageModel   := OtherStorageObj.VoltageModel;

         Fstate         := OtherStorageObj.Fstate;
         FstateChanged  := OtherStorageObj.FstateChanged;
         kVANotSet      := OtherStorageObj.kVANotSet;

         kVArating      := OtherStorageObj.kVArating;

         kWRating        := OtherStorageObj.kWRating;
         kWhRating       := OtherStorageObj.kWhRating;
         kWhStored       := OtherStorageObj.kWhStored;
         kWhReserve      := OtherStorageObj.kWhReserve;
         pctReserve      := OtherStorageObj.pctReserve;
         DischargeTrigger := OtherStorageObj.DischargeTrigger;
         ChargeTrigger   := OtherStorageObj.ChargeTrigger;
         pctChargeEff    := OtherStorageObj.pctChargeEff;
         pctDischargeEff := OtherStorageObj.pctDischargeEff;
         pctkWout        := OtherStorageObj.pctkWout;
         pctkWin         := OtherStorageObj.pctkWin;

         pctR            := OtherStorageObj.pctR;
         pctX            := OtherStorageObj.pctX;

         RandomMult      :=  OtherStorageObj.RandomMult;

         UserModel.Name   := OtherStorageObj.UserModel.Name;  // Connect to user written models

         ClassMakeLike(OtherStorageObj);

         For i := 1 to ParentClass.NumProperties Do
             FPropertyValue^[i] := OtherStorageObj.FPropertyValue^[i];

         Result := 1;
     End
     ELSE  DoSimpleMsg('Error in Load MakeLike: "' + OtherStorageObjName + '" Not Found.', 562);

End;

//----------------------------------------------------------------------------
Function TStorage.Init(Handle:Integer):Integer;
VAR
   p:TStorageObj;

Begin
     If (Handle = 0) THEN
       Begin  // init all
             p := elementList.First;
             WHILE (p <> nil) Do
             Begin
                  p.Randomize(0);
                  p := elementlist.Next;
             End;
       End
     ELSE
       Begin
             Active := Handle;
             p := GetActiveObj;
             p.Randomize(0);
       End;

     DoSimpleMsg('Need to implement TStorage.Init', -1);
     Result := 0;
End;

{--------------------------------------------------------------------------}
Procedure TStorage.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

VAR
      idx  :Integer;

Begin
      idx := First;
      WHILE idx > 0 Do
      Begin
           TStorageObj(GetActiveObj).ResetRegisters;
           idx := Next;
      End;
End;

{--------------------------------------------------------------------------}
Procedure TStorage.SampleAll;  // Force all EnergyMeters in the circuit to take a sample

Var
      i :Integer;
begin
      For i := 1 to ElementList.ListSize  Do
        With TStorageObj(ElementList.Get(i)) Do
          if Enabled then TakeSample;
End;

//----------------------------------------------------------------------------
Constructor TStorageObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin

     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType ; // + STORAGE_ELEMENT;  // In both PCelement and Genelement list

     Nphases    := 3;
     Fnconds    := 4;  // defaults to wye
     Yorder     := 0;  // To trigger an initial allocation
     Nterms     := 1;  // forces allocations

     YearlyShape       := '';
     YearlyShapeObj    := nil;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
     DailyShape        := '';
     DailyShapeObj     := nil;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
     DutyShape         := '';
     DutyShapeObj      := nil;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
     Connection        := 0;    // Wye (star)
     VoltageModel      := 1;  {Typical fixed kW negative load}
     StorageClass      := 1;

     StorageSolutionCount     := -1;  // For keep track of the present solution in Injcurrent calcs
     OpenStorageSolutionCount := -1;
     YPrimOpenCond            := nil;

     kVStorageBase    := 12.47;
     VBase            := 7200.0;
     Vminpu           := 0.90;
     Vmaxpu           := 1.10;
     VBase95          := Vminpu * Vbase;
     VBase105         := Vmaxpu * Vbase;
     Yorder           := Fnterms * Fnconds;
     RandomMult       := 1.0 ;

      {Output rating stuff}
     kW_out       := 25.0;
     kvar_out     := 0.0;
     PFNominal    := 1.0;
     kWRating     := 25.0;
     kVArating    := kWRating *1.0;

     FState           := STATE_IDLING;  // Idling and fully charged
     FStateChanged    := TRUE;  // Force building of YPrim
     kWhRating       := 50;
     kWhStored       := 50;
     pctReserve      := 20.0;  // per cent of kWhRating
     kWhReserve      := kWhRating * pctReserve /100.0;
     pctR            := 0.0;;
     pctX            := 50.0;

     DischargeTrigger := 0.0;
     ChargeTrigger    := 0.0;
     pctChargeEff     := 90.0;
     pctDischargeEff  := 90.0;
     pctkWout         := 100.0;
     pctkWin          := 100.0;

     kVANotSet    := TRUE;  // Flag to set the default value for kVA

     UserModel  := TStoreUserModel.Create;

     Reg_kWh    := 1;
     Reg_kvarh  := 2;
     Reg_MaxkW  := 3;
     Reg_MaxkVA := 4;
     Reg_Hours  := 5;
     Reg_Price  := 6;

     DebugTrace := FALSE;
     StorageObjSwitchOpen := FALSE;
     Spectrum := '';  // override base class
     SpectrumObj := nil;
     
     InitPropertyValues(0);
     RecalcElementData;

End;


//----------------------------------------------------------------------------
function TStorageObj.DecodeState: String;
begin
     CASE Fstate of
         STATE_CHARGING :    Result := 'CHARGING';
         STATE_DISCHARGING : Result := 'DISCHARGING';
     ELSE
         Result := 'IDLING';
     END;
end;

//----------------------------------------------------------------------------
Destructor TStorageObj.Destroy;
Begin
      YPrimOpenCond.Free;
      UserModel.Free;
      Inherited Destroy;
End;

//----------------------------------------------------------------------------
Procedure TStorageObj.Randomize(Opt:Integer);
Begin

   CASE Opt OF
       0:         RandomMult := 1.0;
       GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:   RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   END;

End;

//----------------------------------------------------------------------------
Procedure TStorageObj.CalcDailyMult(Hr:Double);

Begin
     If (DailyShapeObj <> Nil) Then
       Begin
            ShapeFactor := DailyShapeObj.GetMult(Hr);
       End
     ELSE ShapeFactor := CDOUBLEONE;  // Default to no  variation

     CheckStateTriggerLevel(ShapeFactor.re);   // last recourse
End;


//----------------------------------------------------------------------------
Procedure TStorageObj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil Then
       Begin
             ShapeFactor := DutyShapeObj.GetMult(Hr);
             CheckStateTriggerLevel(ShapeFactor.re);
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
End;

//----------------------------------------------------------------------------
Procedure TStorageObj.CalcYearlyMult(Hr:Double);

Begin
     If YearlyShapeObj<>Nil Then
       Begin
            ShapeFactor := YearlyShapeObj.GetMult(Hr) ;
            CheckStateTriggerLevel(ShapeFactor.re);
       End
     ELSE CalcDailyMult(Hr);  // Defaults to Daily curve
End;

//----------------------------------------------------------------------------
Procedure TStorageObj.SetNominalStorageOuput;

Begin

   ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    // Check to make sure the generation is ON
   With ActiveCircuit, ActiveCircuit.Solution Do
   Begin
    IF NOT (IsDynamicModel or IsHarmonicModel) THEN     // Leave generator in whatever state it was prior to entering Dynamic mode
    Begin
          CASE DispatchMode of

                LOADMODE: CheckStateTriggerLevel(GeneratorDispatchReference);
                PRICEMODE:CheckStateTriggerLevel(PriceSignal);

          ELSE // dispatch off element's loadshapes, if any

           With Solution Do
            CASE Mode OF
                SNAPSHOT:     {Just solve for the present kW, kvar};  // don't check for state change
                DAILYMODE:    CalcDailyMult(dblHour); // Daily dispatch curve
                YEARLYMODE:   CalcYearlyMult(dblHour);
             (*
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,  *)

                DYNAMICMODE:   {do nothing};

                // Assume Daily curve, if any, for the following
                MONTECARLO2,
                MONTECARLO3,
                LOADDURATION1,
                LOADDURATION2: CalcDailyMult(dblHour);
                PEAKDAY:       CalcDailyMult(dblHour);

                DUTYCYCLE:     CalcDutyMult(dblHour) ;
                AUTOADDFLAG:  ;
            End;

          END;


          SetKWandKvarOut;   // Based on State and amount of energy left in storage

          IF Fstate = STATE_IDLING  THEN
            Begin
                  //  YeqIdle will be in the Yprim Matrix   so set this to zero
                  Pnominalperphase   := 0.0; // -0.1 * kWRating / Fnphases;     // watts
                  Qnominalperphase   := 0.0;
                  Yeq    := Cmplx(0.0,0.0);
                  Yeq95  := Yeq;
                  Yeq105 := Yeq;
            End
          ELSE
            Begin
                  Pnominalperphase   := 1000.0 * kW_out    / Fnphases;
                  Qnominalperphase   := 1000.0 * kvar_out  / Fnphases;

                  CASE VoltageModel  of
        //****  Fix this when user model gets connected in
                       3: // Yeq := Cinv(cmplx(0.0, -StoreVars.Xd))  ;  // Gets negated in CalcYPrim
                  ELSE
                      Yeq  := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
                      If   (Vminpu <> 0.0) Then Yeq95 := CDivReal(Yeq, sqr(Vminpu))  // at 95% voltage
                                           Else Yeq95 := Yeq; // Always a constant Z model

                      If   (Vmaxpu <> 0.0) Then  Yeq105 := CDivReal(Yeq, Sqr(Vmaxpu))   // at 105% voltage
                                           Else  Yeq105 := Yeq;
                  END;
             End;
              { When we leave here, all the Yeq's are in L-N values}

     End;  {If  NOT (IsDynamicModel or IsHarmonicModel)}
   End;  {With ActiveCircuit}

   // If Storage element state changes, force re-calc of Y matrix
   If FStateChanged Then  Begin
      YPrimInvalid := True;
      FStateChanged := FALSE;  // reset the flag
   End;

End;

//----------------------------------------------------------------------------
Procedure TStorageObj.RecalcElementData;

Begin

    VBase95  := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase := 1000.0 * kvar_out / Fnphases;

    // values in ohms for thevenin equivalents
    RThev := pctR * 0.01 * SQR(PresentkV)/kVARating * 1000.0;
    XThev := pctX * 0.01 * SQR(PresentkV)/kVARating * 1000.0;

    // efficiencies
    ChargeEff    := pctChargeEff    * 0.01;
    DisChargeEff := pctDisChargeEff * 0.01;

    YeqIdling := CmulReal(Cmplx(pctIdlekW, pctIdlekvar), (kWrating*10.0/SQR(vbase)/FNPhases));  // 10.0 = 1000/100 = kW->W/pct

    SetNominalStorageOuput;

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    If YearlyShapeObj=Nil Then
      If Length(YearlyShape)>0 Then DoSimpleMsg('WARNING! Yearly load shape: "'+ YearlyShape +'" Not Found.', 563);
    If DailyShapeObj=Nil Then
      If Length(DailyShape)>0 Then DoSimpleMsg('WARNING! Daily load shape: "'+ DailyShape +'" Not Found.', 564);
    If DutyShapeObj=Nil Then
      If Length(DutyShape)>0 Then DoSimpleMsg('WARNING! Duty load shape: "'+ DutyShape +'" Not Found.', 565);

    if Length(Spectrum)> 0 then Begin
          SpectrumObj := SpectrumClass.Find(Spectrum);
          If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 566);
    End
    Else SpectrumObj := Nil;

    // Initialize to Zero - defaults to PQ generator
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    {Update any user-written models}
    If Usermodel.Exists  Then UserModel.FUpdateModel;

End;

//----------------------------------------------------------------------------
Procedure TStorageObj.CalcYPrimMatrix(Ymatrix:TcMatrix);

Var
       Y , Yij  :Complex;
       i, j     :Integer;
       FreqMultiplier :Double;

Begin

   FYprimFreq := ActiveCircuit.Solution.Frequency  ;
   FreqMultiplier := FYprimFreq / BaseFrequency;

   With  ActiveCircuit.solution  Do
   IF {IsDynamicModel or} IsHarmonicModel Then
     Begin
       {Yeq is computed from %R and %X -- inverse of Rthev + j Xthev}
           CASE Fstate of
               STATE_IDLING: Y := YeqIdling;
               STATE_DISCHARGING: Y := Cadd(cnegate(Yeq), YeqIdling);
           ELSE
               Y  := Yeq   // L-N value computed in initialization routines
           END;

           IF Connection=1 Then Y := CDivReal(Y, 3.0); // Convert to delta impedance
           Y.im := Y.im / FreqMultiplier;
           Yij := Cnegate(Y);
           FOR i := 1 to Fnphases Do
             Begin
                   CASE Connection of
                     0: Begin
                             Ymatrix.SetElement(i, i, Y);
                             Ymatrix.AddElement(Fnconds, Fnconds, Y);
                             Ymatrix.SetElemsym(i, Fnconds, Yij);
                        End;
                     1: Begin   {Delta connection}
                             Ymatrix.SetElement(i, i, Y);
                             Ymatrix.AddElement(i, i, Y);  // put it in again
                             For j := 1 to i-1 Do Ymatrix.SetElemsym(i, j, Yij);
                        End;
                   END;
             End;
     End

   ELSE
     Begin  //  Regular power flow generator model

       {Yeq is always expected as the equivalent line-neutral admittance}


       CASE Fstate of
           STATE_IDLING: Y := YeqIdling;
       ELSE
           Y := Cadd(cnegate(Yeq), YeqIdling);   // negate for generation    Yeq is L-N quantity
       END;

       // ****** Need to modify the base admittance for real harmonics calcs
       Y.im           := Y.im / FreqMultiplier;

         CASE Connection OF

           0: With YMatrix Do
              Begin // WYE
                     Yij := Cnegate(Y);
                     FOR i := 1 to Fnphases Do
                     Begin
                          SetElement(i, i, Y);
                          AddElement(Fnconds, Fnconds, Y);
                          SetElemsym(i, Fnconds, Yij);
                     End;
              End;

           1: With YMatrix Do
              Begin  // Delta  or L-L
                    Y    := CDivReal(Y, 3.0); // Convert to delta impedance
                    Yij  := Cnegate(Y);
                    FOR i := 1 to Fnphases Do
                    Begin
                         j := i+1;
                         If j>Fnconds Then j := 1;  // wrap around for closed connections
                         AddElement(i,i, Y);
                         AddElement(j,j, Y);
                         AddElemSym(i,j, Yij);
                    End;
              End;

         END;
     End;  {ELSE IF Solution.mode}

End;

//----------------------------------------------------------------------------
procedure TStorageObj.CheckStateTriggerLevel(Level: Double);
{This is where we set the state of the Storage element}

Var
     OldState :Integer;

begin
     FStateChanged := FALSE;
     if (ChargeTrigger=0.0) and (DischargeTrigger=0.0) then   Exit;

     OldState := Fstate;
  // First see if we want to turn off Charging or Discharging State
     CASE Fstate of
         STATE_CHARGING:    if (ChargeTrigger    <> 0.0) Then if (ChargeTrigger    < Level) or (kWhStored >= kWHRating)  then Fstate := STATE_IDLING;
         STATE_DISCHARGING: if (DischargeTrigger <> 0.0) Then if (DischargeTrigger > Level) or (kWhStored <= kWHReserve) then Fstate := STATE_IDLING;
     END;

  // Now check to see if we want to turn on the opposite state
     CASE Fstate of
         STATE_IDLING: if      (DischargeTrigger <> 0.0) and (DischargeTrigger < Level) and (kWhStored > kWHReserve) then FState := STATE_DISCHARGING
                       else if (ChargeTrigger    <> 0.0) and (ChargeTrigger    > Level) and (kWhStored < kWHRating)  then Fstate := STATE_CHARGING;
     END;

     if OldState <> Fstate then FstateChanged := TRUE;
end;

//----------------------------------------------------------------------------
Procedure TStorageObj.CalcYPrim;

Var
        i:integer;
        
Begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV does not fail
     If YPrimInvalid
     Then  Begin
         If YPrim_Shunt<>nil Then YPrim_Shunt.Free;
         YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
         IF YPrim_Series <> nil THEN Yprim_Series.Free;
         YPrim_Series := TcMatrix.CreateMatrix(Yorder);
          If YPrim <> nil Then  YPrim.Free;
         YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Shunt.Clear;
          YPrim_Series.Clear;
          YPrim.Clear;
     End;

     SetNominalStorageOuput;
     CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));
     
     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
     Inherited CalcYPrim;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);
 {Add the current into the proper location according to connection}

 {Reverse of similar routine in load  (Cnegates are switched)}

VAR j :Integer;

Begin
    CASE Connection OF
         0: Begin  //Wye
                 Caccum(TermArray^[i], Curr );
                 Caccum(TermArray^[Fnconds], Cnegate(Curr) ); // Neutral
            End;
         1: Begin //DELTA
                 Caccum(TermArray^[i], Curr );
                 j := i + 1;
                 If j > Fnconds Then j := 1;
                 Caccum(TermArray^[j], Cnegate(Curr) );
            End;
    End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.WriteTraceRecord(const s:string);

Var i:Integer;

Begin

      Try
      If (Not InshowResults) Then
      Begin
           Append(TraceFile);
           Write(TraceFile,Format('%-.g, %d, %-.g, ',
                    [ActiveCircuit.Solution.DynaVars.t,
                    ActiveCircuit.Solution.Iteration,
                    ActiveCircuit.LoadMultiplier]),
                    GetSolutionModeID,', ',
                    GetLoadModel,', ',
                    VoltageModel:0,', ',
                   (Qnominalperphase*3.0/1.0e6):8:2,', ',
                   (Pnominalperphase*3.0/1.0e6):8:2,', ',
                   s,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(InjCurrent^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(ITerminal^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(Vterminal^[i])):8:1 ,', ');
   //****        Write(TraceFile,VThevMag:8:1 ,', ', StoreVars.Theta*180.0/PI);
           Writeln(TRacefile);
           CloseFile(TraceFile);
      End;
      Except
            On E:Exception Do Begin End;

      End;
End;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.DoConstantPQStorageObj;

{Compute total terminal current for Constant PQ}

VAR
   i:Integer;
   Curr, V:Complex;
   Vmag: Double;

Begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;


        CalcVTerminalPhase; // get actual voltage across each phase of the load
        FOR i := 1 to Fnphases Do Begin
            V    := Vterminal^[i];
            VMag := Cabs(V);

            CASE Connection of

             0: Begin  {Wye}
                  IF   VMag <= VBase95
                  THEN Curr := Cmul(Yeq95, V)  // Below 95% use an impedance model
                  ELSE If VMag > VBase105
                  THEN Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
                  ELSE Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
                End;

              1: Begin  {Delta}
                  VMag := VMag/SQRT3;  // L-N magnitude
                  IF   VMag <= VBase95
                  THEN Curr := Cmul(CdivReal(Yeq95, 3.0), V)  // Below 95% use an impedance model
                  ELSE If VMag > VBase105
                  THEN Curr := Cmul(CdivReal(Yeq105, 3.0), V)  // above 105% use an impedance model
                  ELSE  Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
                End;

             END;

            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            IterminalUpdated := TRUE;
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.DoConstantZStorageObj;

{constant Z model}
VAR
   i    :Integer;
   Curr,
   Yeq2 :Complex;

Begin

// Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;
    If Connection=0 Then Yeq2 := Yeq Else Yeq2 := CdivReal(Yeq, 3.0);

     FOR i := 1 to Fnphases Do Begin

        Curr := Cmul(Yeq2, Vterminal^[i]);   // Yeq is always line to neutral
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

     End;

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DoUserModel;
{Compute total terminal Current from User-written model}
Var
   i:Integer;

Begin

   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   If UserModel.Exists Then    // Check automatically selects the usermodel if true
     Begin
         UserModel.FCalc (Vterminal, Iterminal);
         IterminalUpdated := TRUE;
         With ActiveCircuit.Solution Do  Begin          // Negate currents from user model for power flow generator model
               FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
         End;
     End
   Else
     Begin
        DoSimpleMsg('Generator.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
     End;

End;



// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DoDynamicMode;

{Compute Total Current and add into InjTemp}
{
   For now, just assume the storage element is constant power
   for the duration of the dynamic simulation.
}


Begin

  DoConstantPQStorageObj;

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TStorageObj.DoHarmonicMode;

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

Var
   i     :Integer;
   E     :Complex;
   StorageHarmonic :double;

Begin

   ComputeVterminal;

   WITH ActiveCircuit.Solution Do
     Begin
        StorageHarmonic := Frequency/StorageFundamental;
        if SpectrumObj <> Nil then         
             E := CmulReal(SpectrumObj.GetMult(StorageHarmonic), VThevHarm) // Get base harmonic magnitude
        Else E := CZERO;

        RotatePhasorRad(E, StorageHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift
        FOR i := 1 to Fnphases DO Begin
           cBuffer[i] := E;
           If i < Fnphases Then RotatePhasorDeg(E, StorageHarmonic, -120.0);  // Assume 3-phase generator
        End;
     END;

   {Handle Wye Connection}
   IF Connection=0 THEN cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
   YPrim.MVMult(InjCurrent,@cBuffer);

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.CalcVTerminalPhase;

VAR i,j:Integer;

Begin

{ Establish phase voltages and stick in Vterminal}
   Case Connection OF

     0:Begin
         With ActiveCircuit.Solution Do
           FOR i := 1 to Fnphases Do Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
       End;

     1:Begin
         With ActiveCircuit.Solution Do
          FOR i := 1 to Fnphases Do  Begin
             j := i + 1;
             If j > Fnconds Then j := 1;
             Vterminal^[i] := VDiff( NodeRef^[i] , NodeRef^[j]);
          End;
       End;

   End;

   StorageSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
(*
Procedure TStorageObj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   StorageSolutionCount := ActiveCircuit.Solution.SolutionCount;
End;
*)


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.CalcStorageModelContribution;

// Calculates generator current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

Begin
     IterminalUpdated := FALSE;
     WITH  ActiveCircuit, ActiveCircuit.Solution DO
     Begin
          IF      IsDynamicModel THEN  DoDynamicMode
          ELSE IF IsHarmonicModel and (Frequency <> Fundamental) THEN  DoHarmonicMode
          ELSE
            Begin
               //  compute currents and put into InjTemp array;
                 CASE VoltageModel OF
                      1: DoConstantPQStorageObj;
                      2: DoConstantZStorageObj;
                      3: DoUserModel;
                 ELSE
                      DoConstantPQStorageObj;  // for now, until we implement the other models.
                 End;
            End; {ELSE}
     END; {WITH}

   {When this is done, ITerminal is up to date}

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.CalcInjCurrentArray;
// Difference between currents in YPrim and total current
Begin
      // Now Get Injection Currents
       If StorageObjSwitchOpen Then ZeroInjCurrent
       Else CalcStorageModelContribution;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.GetTerminalCurrents(Curr:pComplexArray);

// Compute total Currents

Begin
   WITH ActiveCircuit.Solution  DO
     Begin
        If IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount Then Begin     // recalc the contribution
          IF Not StorageObjSwitchOpen Then CalcStorageModelContribution;  // Adds totals in Iterminal as a side effect
        End;
        Inherited GetTerminalCurrents(Curr);
     End;

   If (DebugTrace) Then WriteTraceRecord('TotalCurrent');

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Function TStorageObj.InjCurrents:Integer;

Begin
     With ActiveCircuit.Solution Do
      Begin
         If LoadsNeedUpdating Then SetNominalStorageOuput; // Set the nominal kW, etc for the type of solution being done

         CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

         If (DebugTrace) Then WriteTraceRecord('Injection');

         // Add into System Injection Current Array

         Result := Inherited InjCurrents;
      End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.GetInjCurrents(Curr:pComplexArray);

// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

VAR
   i:Integer;

Begin

   CalcInjCurrentArray;  // Difference between currents in YPrim and total current

   TRY
   // Copy into buffer array
     FOR i := 1 TO Yorder Do Curr^[i] := InjCurrent^[i];

   EXCEPT
     ON E: Exception Do
        DoErrorMsg('Storage Object: "' + Name + '" in GetInjCurrents function.',
                    E.Message,
                   'Current buffer not big enough.', 568);
   End;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TStorageObj.ResetRegisters;

VAR
   i : Integer;

Begin
     For i := 1 to NumStorageRegisters Do Registers[i]   := 0.0;
     For i := 1 to NumStorageRegisters Do Derivatives[i] := 0.0;
     FirstSampleAfterReset := True;  // initialize for trapezoidal integration
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TStorageObj.Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);

Begin
     IF ActiveCircuit.TrapezoidalIntegration THEN
       Begin
        {Trapezoidal Rule Integration}
        If Not FirstSampleAfterReset Then Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
       End
     ELSE   {Plain Euler integration}
         Registers[Reg] := Registers[Reg] + Interval * Deriv;

     Derivatives[Reg] := Deriv;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TStorageObj.TakeSample;
// Update Energy from metered zone

VAR
     S         :Complex;
     Smag      :double;
     HourValue :Double;

Begin

// Compute energy in Generator branch
     IF  Enabled  THEN Begin

     // Only tabulate discharge hours
       IF FSTate = STATE_DISCHARGING then   Begin
          S := cmplx(Get_PresentkW, Get_Presentkvar);
          Smag := Cabs(S);
          HourValue := 1.0;
       End Else Begin
          S := CZERO;
          Smag := 0.0;
          HourValue := 0.0;
       End;

        IF (FState = STATE_DISCHARGING) or ActiveCircuit.TrapezoidalIntegration THEN
        {Make sure we always integrate for Trapezoidal case
         Don't need to for Gen Off and normal integration}
        WITH ActiveCircuit.Solution Do
          Begin
             IF ActiveCircuit.PositiveSequence THEN Begin
                S    := CmulReal(S, 3.0);
                Smag := 3.0*Smag;
             End;
             Integrate            (Reg_kWh,   S.re, IntervalHrs);   // Accumulate the power
             Integrate            (Reg_kvarh, S.im, IntervalHrs);
             SetDragHandRegister  (Reg_MaxkW, abs(S.re));
             SetDragHandRegister  (Reg_MaxkVA, Smag);
             Integrate            (Reg_Hours, HourValue, IntervalHrs);  // Accumulate Hours in operation
             Integrate            (Reg_Price, S.re*ActiveCircuit.PriceSignal , IntervalHrs);  // Accumulate Hours in operation
             FirstSampleAfterReset := False;
          End;
     End;
End;

//----------------------------------------------------------------------------
procedure TStorageObj.UpdateStorage;
{Update Storage levels}
begin

    With ActiveCircuit.Solution Do
    Case FState of

        STATE_DISCHARGING: Begin
                               kWhStored := kWhStored - PresentkW * IntervalHrs / DischargeEff;
                               if kWhStored < kWhReserve then Begin
                                  kWhStored := kWhReserve;
                                  Fstate := STATE_IDLING;  // It's empty Turn it off
                                  FstateChanged := TRUE;
                               End;
                           End;

        STATE_CHARGING:    Begin
                               kWhStored := kWhStored - PresentkW * IntervalHrs * ChargeEff;
                               if kWhStored > kWhRating then Begin
                                  kWhStored := kWhRating;
                                  Fstate := STATE_IDLING;  // It's full Turn it off
                                  FstateChanged := TRUE;
                               End;
                           End;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Function TStorageObj.Get_PresentkW:Double;
Begin
     Result := Pnominalperphase * 0.001 * Fnphases;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TStorageObj.Get_PresentkV: Double;
begin
     Result := kVStorageBase;
end;

Function TStorageObj.Get_Presentkvar:Double;
Begin
     Result := Qnominalperphase * 0.001 * Fnphases;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.DumpProperties(Var F:TextFile; Complete:Boolean);

Var
   i, idx :Integer;

Begin
      Inherited DumpProperties(F, Complete);

      With ParentClass Do
       For i := 1 to NumProperties Do
       Begin
            idx := PropertyIdxMap[i] ;
            Case idx of
                propUSERDATA: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[idx],')')
            Else
                Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[idx]);
            End;
       End;

      Writeln(F);
End;

      
//----------------------------------------------------------------------------
Procedure TStorageObj.InitHarmonics;

// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X

Var
  E, Va:complex;

begin
     YPrimInvalid       := TRUE;  // Force rebuild of YPrims
     StorageFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.

     Yeq := Cinv(Cmplx(RThev, XThev));      // used for current calcs  Always L-N

     {Compute reference Thevinen voltage from phase 1 current}

     IF FState = STATE_DISCHARGING Then
       Begin
           ComputeIterminal;  // Get present value of current

           With ActiveCircuit.solution Do
           Case Connection of
             0: Begin {wye - neutral is explicit}
                     Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]]);
                End;
             1: Begin  {delta -- assume neutral is at zero}
                     Va := NodeV^[NodeRef^[1]];
                End;
           End;

           E := Csub(Va, Cmul(Iterminal^[1], cmplx(Rthev, Xthev)));
           Vthevharm := Cabs(E);   // establish base mag and angle
           ThetaHarm := Cang(E);
       End
     ELSE
       Begin
           Vthevharm := 0.0;
           ThetaHarm := 0.0;
       End;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.InitPropertyValues(ArrayOffset: Integer);

// Define default values for the properties

begin

     PropertyValue[1]      := '3';         //'phases';
     PropertyValue[2]      := Getbus(1);   //'bus1';

     PropertyValue[propKV]      := Format('%-g', [kVStorageBase]);
     PropertyValue[propKW]      := Format('%-g', [kW_out]);
     PropertyValue[propPF]      := Format('%-g', [PFNominal]);
     PropertyValue[propMODEL]     := '1';
     PropertyValue[propYEARLY]    := '';
     PropertyValue[propDAILY]     := '';
     PropertyValue[propDUTY]      := '';
     PropertyValue[propDISPMODE]  := 'Default';
     PropertyValue[propIDLEKVAR]  := '0';
     PropertyValue[propCONNECTION]:= 'wye';
     PropertyValue[propKVAR]      := Format('%-g', [Presentkvar]);

     PropertyValue[propPCTR]      := Format('%-g', [pctR]);
     PropertyValue[propPCTX]      := Format('%-g', [pctX]);

     PropertyValue[propIDLEKW]    := '1';       // PERCENT
     PropertyValue[propCLASS]     := '1'; //'class'
     PropertyValue[propDISPOUTTRIG]    := '0';   // 0 MEANS NO TRIGGER LEVEL
     PropertyValue[propDISPINTRIG]:= '0';
     PropertyValue[propCHARGEEFF] := '90';
     PropertyValue[propDISCHARGEEFF]  := '90';
     PropertyValue[propPCTKWOUT]  := '100';
     PropertyValue[propPCTKWIN]   := '100';

     PropertyValue[propVMINPU]    := '0.90';
     PropertyValue[propVMAXPU]    := '1.10';
     PropertyValue[propSTATE]     := 'IDLING';
     PropertyValue[propKVA]       := Format('%-g', [kVARating]);
     PropertyValue[propKWRATED]   := Format('%-g', [kWRating]);
     PropertyValue[propKWHRATED]  := Format('%-g', [kWhRating]);
     PropertyValue[propKWHSTORED] := Format('%-g', [kWhStored]);
     PropertyValue[propPCTRESERVE]:= Format('%-g', [pctReserve]);

     PropertyValue[propUSERMODEL] := '';  // Usermodel
     PropertyValue[propUSERDATA]  := '';  // Userdata
     PropertyValue[propDEBUGTRACE]:= 'NO';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

//----------------------------------------------------------------------------
PROCEDURE TStorageObj.InitStateVars;

// for going into dynamics mode
(*
Var
    VNeut,
    Edp   :Complex;
    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;
*)
begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims

(*

//****     Yeq := Cinv(Cmplx(0.0, Xdp));

     {Compute nominal Positive sequence voltage behind transient reactance}

     IF FState = STATE_DISCHARGING Then With ActiveCircuit.Solution Do
       Begin
             ComputeIterminal;
             Phase2SymComp(ITerminal, @I012);
             // Voltage behind Xdp  (transient reactance), volts
             Case Connection of
                0: Vneut :=  NodeV^[NodeRef^[Fnconds]]
             Else
                Vneut :=  CZERO;
             End;

             For i := 1 to FNphases Do Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
             Phase2SymComp(@Vabc, @V012);
    //****         Edp      := Csub( V012[1] , Cmul(I012[1], cmplx(0.0, Xdp)));    // Pos sequence
             VThevMag := Cabs(Edp);


              // Init User-written models
             //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
             With ActiveCircuit.Solution Do If VoltageModel=6 then Begin
               If UserModel.Exists Then UserModel.FInit( Vterminal, Iterminal);
             End;
       End
     ELSE  Begin
         Vthev  := cZERO;
//****  ANY OTHERS
     End;
*)
end;

//----------------------------------------------------------------------------
procedure TStorageObj.IntegrateStates;

// dynamics mode integration routine

(****
Var
    TracePower:Complex;

****)
begin
   // Compute Derivatives and then integrate

(*    For now, do nothing; no state vars to integrate

   ComputeIterminal;

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)
    With ActiveCircuit.Solution Do  Begin

      With DynaVars Do
      If (IterationFlag = 0) Then Begin {First iteration of new time step}
          ThetaHistory := Theta + 0.5*h*dTheta;
          SpeedHistory := Speed + 0.5*h*dSpeed;
      End;


      // Compute shaft dynamics
      TracePower := TerminalPowerIn(Vterminal,Iterminal,FnPhases) ;
      dSpeed := (Pshaft + TracePower.re - D*Speed) / Mmass;
       dTheta  := Speed ;

     // Trapezoidal method
      With DynaVars Do Begin
        Speed := SpeedHistory + 0.5*h*dSpeed;
        Theta := ThetaHistory + 0.5*h*dTheta;
      End;

      // Write Dynamics Trace Record
        IF DebugTrace Then
          Begin
             Append(TraceFile);
             Write(TraceFile,Format('t=%-.5g ',[Dynavars.t]));
             Write(TraceFile,Format(' Flag=%d ',[Dynavars.Iterationflag]));
             Write(TraceFile,Format(' Speed=%-.5g ',[Speed]));
             Write(TraceFile,Format(' dSpeed=%-.5g ',[dSpeed]));
             Write(TraceFile,Format(' Pshaft=%-.5g ',[PShaft]));
             Write(TraceFile,Format(' P=%-.5g Q= %-.5g',[TracePower.Re, TracePower.im]));
             Write(TraceFile,Format(' M=%-.5g ',[Mmass]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;

       If VoltageModel=6 then Begin
         If UserModel.Exists    Then UserModel.Integrate;
       End;


   End;
*)

end;

//----------------------------------------------------------------------------
function TStorageObj.InterpretState(const S: String): Integer;
begin
     CASE LowerCase(S)[1] of
         'c' : Result := STATE_CHARGING;
         'd' : Result := STATE_DISCHARGING;
     ELSE
         Result := STATE_IDLING;
     END;
end;

//----------------------------------------------------------------------------
function TStorageObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

Var
      N, k:Integer;

begin
    Result := -9999.99;  // error return value; no state fars
    If i < 1 Then Exit;
// for now, report kWhstored and mode
    Case i of
       1: Result := kWhStored;
       2: Result := FState;
       3: Result := pctkWout;
       4: Result := pctkWin;
     Else
        Begin
             If UserModel.Exists Then Begin
                N := UserModel.FNumVars;
                k := (i-NumStorageVariables);
                If k <= N Then Begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                End;
             End;
        End;
     End;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_Variable(i: Integer;  Value: Double);
var N, k:Integer;

begin
  If i<1 Then Exit;  // No variables to set

    Case i of
       1: kWhStored := Value;
       2: Fstate    := Trunc(Value);
       3: pctkWout  := Value;
       4: pctkWin   := Value;
     Else
       Begin
         If UserModel.Exists Then Begin
              N := UserModel.FNumVars;
              k := (i-NumStorageVariables) ;
              If  k<= N Then Begin
                  UserModel.FSetVariable( k, Value );
                  Exit;
              End;
          End;
       End;
     End;

end;

//----------------------------------------------------------------------------
procedure TStorageObj.GetAllVariables(States: pDoubleArray);

Var  i{, N}:Integer;
begin
     For i := 1 to NumStorageVariables Do States^[i] := Variable[i];

     If UserModel.Exists Then Begin
        {N := UserModel.FNumVars;}
        UserModel.FGetAllVars(@States^[NumStorageVariables+1]);
     End;

end;

//----------------------------------------------------------------------------
function TStorageObj.NumVariables: Integer;
begin
     Result  := NumStorageVariables;
     If UserModel.Exists    then Result := Result + UserModel.FNumVars;
end;

//----------------------------------------------------------------------------
Function TStorageObj.VariableName(i: Integer):String;

Const
    BuffSize = 255;
Var
    n,
    i2    :integer;
    Buff  :Array[0..BuffSize] of Char;
    pName :pchar;

begin
      If i<1 Then Exit;  // Someone goofed

      Case i of
          1:Result := 'kWh Stored';
          2:Result := 'Storage State Flag';
          3:Result := '% discharge level';
          4:Result := '% charge level';
      Else
          Begin
            If UserModel.Exists Then
            Begin
                  pName := @Buff;
                  n := UserModel.FNumVars;
                  i2 := i-NumStorageVariables;
                  If i2 <= n Then
                  Begin
                       UserModel.FGetVarName(i2, pName, BuffSize);
                       Result := pName;
                       Exit;
                  End;
            End;
          End;
      End;

end;


//----------------------------------------------------------------------------
function TStorageObj.GetPropertyValue(Index: Integer): String;

begin

      Result := '';
      CASE Index of
        // Special handlers
           propKV:  Result := Format('%.6g', [kVStorageBase]);
           propKW:  Result := Format('%.6g', [kW_out]);
           propPF:  Result := Format('%.6g', [PFNominal]);
           propKVAR: Result := Format('%.6g', [kvar_out]);
           propKVA: Result := Format('%.6g', [kVArating]);
           propKWRATED: Result := Format('%.6g', [kWrating]);
           propUSERDATA: Begin
                      Result := '(' + inherited GetPropertyValue(index) + ')';
                  End;
           propSTATE: Result := DecodeState;
      ELSE  // take the generic handler
           Result := Inherited GetPropertyValue(index);
      END;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.MakePosSequence;

Var
    S :String;
    V :Double;

begin

  S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0)
    Then V :=  kVStorageBase/SQRT3
    Else V :=  kVStorageBase;

  S := S + Format(' kV=%-.5g',[V]);

  If Fnphases>1 Then
  Begin
       S := S + Format(' kWrating=%-.5g  PF=%-.5g',[kWrating/Fnphases, PFNominal]);
  End;

  inherited;   // write out other properties

  Parser.CmdString := S;
  Edit;

end;

procedure TStorageObj.Set_ConductorClosed(Index: Integer;
  Value: Boolean);
begin
   inherited;

 // Just turn storage element on or off;

   If Value Then StorageObjSwitchOpen := FALSE Else StorageObjSwitchOpen := TRUE;

end;



//----------------------------------------------------------------------------
procedure TStorageObj.Set_PowerFactor(const Value: Double);
begin
     PFNominal := Value;
     SyncUpPowerQuantities;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_PresentkV(const Value: Double);
begin
      kVStorageBase := Value ;
      CASE FNphases Of
           2,3: VBase := kVStorageBase * InvSQRT3x1000;
      ELSE
           VBase := kVStorageBase * 1000.0 ;
      END;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_Presentkvar(const Value: Double);
Var
     kVA_Gen :Double;
begin
     kvar_out := Value;
     Qnominalperphase := 1000.0 * kvar_out  / Fnphases; // init to something reasonable
     kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out)) ;
     IF kVA_Gen <> 0.0 THEN PFNominal := kW_out / kVA_Gen ELSE PFNominal := 1.0;
     If (kW_out*kvar_out) < 0.0 Then PFNominal := -PFNominal;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_PresentkW(const Value: Double);
begin
     kW_out := Value;
     SyncUpPowerQuantities;
End;

//----------------------------------------------------------------------------
procedure TStorageObj.SyncUpPowerQuantities;
Begin
     // keep kvar nominal up to date with kW and PF
     If (PFNominal <> 0.0)  Then
     Begin
          kvar_out := kW_out* sqrt(1.0/Sqr(PFNominal) - 1.0);
          Qnominalperphase := 1000.0* kvar_out / Fnphases;
          If PFNominal<0.0 Then kvar_out := -kvar_out;
          If kVANotSet Then kVARating := kWrating;
     End;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    If Value>Registers[reg] Then Registers[Reg] := Value;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.SetKWandKvarOut;
begin
    CASE FState of

       STATE_CHARGING: Begin
                            if kWhStored < kWhRating then
                            Begin
                                 kW_out := -kWRating * pctkWin / 100.0;
                                 if PFNominal = 1.0 then   kvar_out := 0.0
                                 else kvar_out := kW_out * (1.0/sqrt(SQR(PFNominal) - 1.0) );
                            End
                            Else Fstate := STATE_IDLING;
                       End;


       STATE_DISCHARGING: Begin
                                if kWhStored > kWhReserve then
                                Begin
                                     kW_out := kWRating * pctkWout / 100.0;
                                     if PFNominal = 1.0 then   kvar_out := 0.0
                                     else kvar_out := kW_out * (1.0/sqrt(SQR(PFNominal) - 1.0) );
                                End
                                Else Fstate := STATE_IDLING;

                          End;

    END;
end;

//----------------------------------------------------------------------------

initialization

   CDOUBLEONE := CMPLX(1.0, 1.0);

end.
