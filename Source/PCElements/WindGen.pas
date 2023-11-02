unit WindGen;
{
  ----------------------------------------------------------
  Copyright (c) 2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

   2/26/21 Created from   Generator.pas
   3/25/21 Removed Generator-related properties  (e.g., Fuel variables)
   5/25/22 Dynamic expression compatibility added.

}
{
  In power flow modes, the WindGen element is essentially a negative load that can be dispatched.
}

//  The WindGen is assumed balanced over the no. of phases defined

// If you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation (i.e. multiplier = 1.0 always)
//    Daily:   Defaults to No variation
//    Dutycycle: Defaults to Daily shape

interface

uses
    WindGenVars,
    WindGenUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    LoadShape,
    GrowthShape,
    Spectrum,
    ArrayDef,
    Dynamics;

const
    NumGenRegisters = 6;    // Number of energy meter registers
    NumGenVariables = 6;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TWindGen = class(TPCClass)
    PRIVATE

        procedure InterpretConnection(const S: String);
        procedure SetNcondsForConnection;
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherWindGenName: String): Integer; OVERRIDE;
    PUBLIC
        RegisterNames: array[1..NumGenregisters] of String;

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        procedure ResetRegistersAll(ActorID: Integer);
        procedure SampleAll(ActorID: Integer);

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TWindGenObj = class(TPCElement)
    PRIVATE
// Moved to WindGenVars        Zthev           :Complex;
        Yeq: Complex;   // at nominal
        Yeq95: Complex;   // at 95%
        Yeq105: Complex;   // at 105%

        Edp: Complex;
        PhaseCurrentLimit: Complex;
        Model7MaxPhaseCurr: Double;
        Model7LastAngle: Double;
        DebugTrace: Boolean;
        DeltaQMax: Double;  // Max allowable var change on Model=3 per iteration

        DQDV: Double;
        DQDVSaved: Double;
        FForcedON: Boolean;
        FirstSampleAfterReset: Boolean;
        IsFixed: Boolean;   // if Fixed, always at base value
        WindGenSolutionCount: Integer;
        GenFundamental: Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        GenON: Boolean;           {Indicates whether WindGen is currently on}
        GenSwitchOpen: Boolean;
        kVANotSet: Boolean;
        LastGrowthFactor: Double;
        LastYear: Integer;   // added for speedup so we don't have to search for growth factor a lot
        OpenWindGenSolutionCount: Integer;
        PVFactor: Double;  // deceleration Factor for computing vars for PV WindGens
        RandomMult: Double;
        Reg_Hours: Integer;
        Reg_kvarh: Integer;
        Reg_kWh: Integer;
        Reg_MaxkVA: Integer;
        Reg_MaxkW: Integer;
        Reg_Price: Integer;
        ShapeFactor: Complex;
// moved to WindGenVars        Thetaharm       :Double;  {Thevinen equivalent voltage angle reference for Harmonic model}
        Tracefile: TextFile;
        UserModel, ShaftModel: TWindGenUserModel;   {User-Written Models}
        V_Avg: Double;
        V_Remembered: Double;
        var_Remembered: Double;
        varBase: Double; // Base vars per phase
        varMax: Double;
        varMin: Double;
        VBase: Double;  // Base volts suitable for computing currents
        VBase105: Double;
        VBase95: Double;
        Vthev: Complex;  {Thevinen equivalent voltage (complex) for dynamic model}
// moved to WindGenVars        Vthevharm       :Double;  {Thevinen equivalent voltage mag reference for Harmonic model}
// moved to WindGenVars        VthevMag        :Double;    {Thevinen equivalent voltage for dynamic model}
        YPrimOpenCond: TCmatrix;  // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
        YQFixed: Double;  // Fixed value of y for type 7 load
        ShapeIsActual: Boolean;
        ForceBalanced: Boolean;

        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);  // now incorporates DutyStart offset
        procedure CalcGenModelContribution(ActorID: Integer);
        procedure CalcInjCurrentArray(ActorID: Integer);
        procedure CalcVterminal(ActorID: Integer);
        procedure CalcVTerminalPhase(ActorID: Integer);
        procedure CalcVthev_Dyn;      // 3-phase Voltage behind transient reactance
        procedure CalcVthev_Dyn_Mod7(const V: Complex);
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix; ActorID: Integer);

        procedure DoConstantPQGen(ActorID: Integer);
        procedure DoConstantZGen(ActorID: Integer);
        procedure DoCurrentLimitedPQ(ActorID: Integer);
        procedure DoDynamicMode(ActorID: Integer);
        procedure DoFixedQGen(ActorID: Integer);
        procedure DoFixedQZGen(ActorID: Integer);
        procedure DoHarmonicMode(ActorID: Integer);
        procedure DoPVTypeGen(ActorID: Integer);
        procedure DoUserModel(ActorID: Integer);

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double; ActorID: Integer);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String; ActorID: Integer);

        procedure SyncUpPowerQuantities;


        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double);
        procedure Set_Presentkvar(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);

        procedure SetkWkvar(const PkW, Qkvar: Double);

    PROTECTED
        procedure Set_ConductorClosed(Index: Integer; ActorID: Integer; Value: Boolean); OVERRIDE;
        procedure GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;

    PUBLIC

        Connection: Integer;  {0 = line-neutral; 1=Delta}
        DailyDispShape: String;  // Daily (24 HR) WindGen shape
        DailyDispShapeObj: TLoadShapeObj;  // Daily WindGen Shape for this load
        DutyShape: String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // Shape for this WindGen
        DutyStart: Double; // starting time offset into the DutyShape [hrs] for this WindGen
        GenClass: Integer;
        GenModel: Integer;   // Variation with voltage
        WindGenVars: TWindGenVars; {State Variables}
        kvarBase: Double;
        kvarMax: Double;
        kvarMin: Double;
        kWBase: Double;
        PFNominal: Double;
        Vpu: Double;   // per unit Target voltage for WindGen with voltage control
        Vmaxpu: Double;
        Vminpu: Double;

        GenActive: Boolean;
// Fuel variables from Generator model removed

// moved to WindGenVars        VTarget         :Double;  // Target voltage for WindGen with voltage control
        YearlyShape: String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj: TLoadShapeObj;  // Shape for this WindGen

        Registers, Derivatives: array[1..NumGenregisters] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;

        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure SetNominalGeneration(ActorID: Integer);
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        procedure ResetRegisters;
        procedure TakeSample(ActorID: Integer);

        // Procedures for setting the DQDV used by the Solution Object
        procedure InitDQDVCalc;
        procedure BumpUpQ;
        procedure RememberQV(ActorID: Integer);
        procedure CalcDQDV(ActorID: Integer);
        procedure ResetStartPoint;

        // Support for Dynamics Mode
        procedure InitStateVars(ActorID: Integer); OVERRIDE;
        procedure IntegrateStates(ActorID: Integer); OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics(ActorID: Integer); OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        function CheckIfDynVar(myVar: String; ActorID: Integer): Integer;    // for Dynamic expressions
        procedure SetDynOutput(myVar: String);                               // for Dynamic expressions
        function GetDynOutputStr(): String;                                    // for Dynamic expressions

        property PresentkW: Double READ Get_PresentkW WRITE Set_PresentkW;
        property Presentkvar: Double READ Get_Presentkvar WRITE Set_Presentkvar;
        property ForcedON: Boolean READ FForcedON WRITE FForcedON;
        property PresentkV: Double READ Get_PresentkV WRITE Set_PresentkV;
        property PowerFactor: Double READ PFNominal WRITE Set_PowerFactor;

    end;

var
    ActiveWindGenObj: TWindGenObj;
    WinGenClass: TWindGen;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


uses
    ParserDel,
    Circuit,
    Sysutils,
    Command,
    Math,
    MathUtil,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Classes;

const
    NumPropsThisClass = 41;  // removed Fuel variables
  // Dispatch modes
    DEFAULT = 0;
    LOADMODE = 1;

var
    cBuffer: array[1..24] of Complex;  // Temp buffer for calcs  24-phase WindGen?

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TWindGen.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'WindGen';
    DSSClassType := DSSClassType + WINDGEN_ELEMENT;  // In both PCelement and Genelement list

    ActiveElement := 0;

     // Set Register names
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Hours';
    RegisterNames[6] := '$';

    DefineProperties;

    CommandList := TCommandList.Create(PropertyName, NumProperties);
    CommandList.Abbrev := true;

    WinGenClass := Self;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TWindGen.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TWindGen.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;   {see DSSClass}

     // Define Property names
    PropertyName^[1] := 'phases';
    PropertyHelp^[1] := 'Number of Phases, this WindGen.  Power is evenly divided among phases.';

    PropertyName^[2] := 'bus1';
    PropertyHelp^[2] := 'Bus to which the WindGen is connected.  May include specific node specification.';

    PropertyName^[3] := 'kv';
    PropertyHelp^[3] := 'Nominal rated (1.0 per unit) voltage, kV, for WindGen. For 2- and 3-phase WindGens, specify phase-phase kV. ' +
        'Otherwise, for phases=1 or phases>3, specify actual kV across each branch of the WindGen. ' +
        'If wye (star), specify phase-neutral kV. ' +
        'If delta or phase-phase connected, specify phase-phase kV.';

    PropertyName^[4] := 'kW';
    PropertyHelp^[4] := 'Total base kW for the WindGen.  A positive value denotes power coming OUT of the element, ' + CRLF +
        'which is the opposite of a load. This value is modified depending on the dispatch mode. ' +
        'Unaffected by the global load multiplier and growth curves. ' +
        'If you want there to be more generation, you must add more WindGens or change this value.';

    PropertyName^[5] := 'PF';
    PropertyHelp^[5] := 'WindGen power factor. Default is 0.80. Enter negative for leading powerfactor ' +
        '(when kW and kvar have opposite signs.)' + CRLF +
        'A positive power factor for a WindGen signifies that the WindGen produces vars ' + CRLF +
        'as is typical for a synchronous WindGen.  Induction machines would be ' + CRLF +
        'generally specified with a negative power factor.';

    PropertyName^[6] := 'model';
    PropertyHelp^[6] := 'Integer code for the model to use for generation variation with voltage. ' +
        'Valid values are:' + CRLF + CRLF +
        '1:WindGen injects a constant kW at specified power factor.' + CRLF +
        '2:WindGen is modeled as a constant admittance.' + CRLF +
        '3:Const kW, constant kV.  Voltage-regulated model.' + CRLF +
        '4:Const kW, Fixed Q (Q never varies)' + CRLF +
        '5:Const kW, Fixed Q(as a constant reactance)' + CRLF +
        '6:Compute load injection from User-written Model.(see usage of Xd, Xdp)';


    PropertyName^[7] := 'yearly';
    PropertyHelp^[7] := 'Dispatch shape to use for yearly-mode simulations.  Must be previously defined ' +
        'as a Loadshape object. If this is not specified, a constant value is assumed (no variation). ' +
        'Set to NONE to reset to no loadahape. ' +
        'Nominally for 8760 simulations.  If there are fewer points in the designated shape than ' +
        'the number of points in the solution, the curve is repeated.';

    PropertyName^[8] := 'daily';
    PropertyHelp^[8] := 'Dispatch shape to use for daily-mode simulations.  Must be previously defined ' +
        'as a Loadshape object of 24 hrs, typically.' +
        'Set to NONE to reset to no loadahape. '; // daily dispatch (hourly)

    PropertyName^[9] := 'duty';
    PropertyHelp^[9] := 'Load shape to use for duty cycle dispatch simulations such as for wind or solar generation. ' +
        'Must be previously defined as a Loadshape object. ' +
        'Typically would have time intervals less than 1 hr -- perhaps, in seconds. ' +
        'Set to NONE to reset to no loadahape. ' +
        'Designate the number of points to solve using the Set Number=xxxx command. ' +
        'If there are fewer points in the actual shape, the shape is assumed to repeat.';  // as for wind generation

    PropertyName^[10] := ' ';
    PropertyHelp^[10] := ' '; // = 0 | >0

    PropertyName^[11] := ' ';
    PropertyHelp^[11] := ' ';  // = 0 | >0

    PropertyName^[12] := 'conn';
    PropertyHelp^[12] := '={wye|LN|delta|LL}.  Default is wye.';

    PropertyName^[13] := 'kvar';
    PropertyHelp^[13] := 'Specify the base kvar.  Alternative to specifying the power factor.  Side effect: ' +
        ' the power factor value is altered to agree based on present value of kW.';

    PropertyName^[16] := 'status';
    PropertyHelp^[16] := '={Fixed | Variable*}.  If Fixed, then dispatch multipliers do not apply. ' +
        'The WindGen is always at full power when it is ON. ' +
        ' Default is Variable  (follows curves or windspeed).';  // fixed or variable

    PropertyName^[17] := 'class';
    PropertyHelp^[17] := 'An arbitrary integer number representing the class of WindGen so that WindGen values may ' +
        'be segregated by class.'; // integer

    PropertyName^[18] := 'Vpu';
    PropertyHelp^[18] := 'Per Unit voltage set point for Model = 3  (Regulated voltage model).  Default is 1.0 pu. '; // per unit set point voltage for power flow model

    PropertyName^[19] := 'maxkvar';
    PropertyHelp^[19] := 'Maximum kvar limit for Model = 3.  Defaults to twice the specified load kvar.  ' +
        'Always reset this if you change PF or kvar properties.';

    PropertyName^[20] := 'minkvar';
    PropertyHelp^[20] := 'Minimum kvar limit for Model = 3. Enter a negative number if WindGen can absorb vars.' +
        ' Defaults to negative of Maxkvar.  Always reset this if you change PF or kvar properties.';

    PropertyName^[21] := 'pvfactor';
    PropertyHelp^[21] := 'Deceleration factor for P-V WindGen model (Model=3).  Default is 0.1. ' +
        'If the circuit converges easily, you may want to use a higher number such as 1.0. ' +
        'Use a lower number if solution diverges. Use Debugtrace=yes to create a file that will ' +
        'trace the convergence of a WindGen model.';

    PropertyName^[22] := 'debugtrace';
    PropertyHelp^[22] := '{Yes | No }  Default is no.  Turn this on to capture the progress of the WindGen model ' +
        'for each iteration.  Creates a separate file for each WindGen named "GEN_name.CSV".';

    PropertyName^[23] := 'Vminpu';
    PropertyHelp^[23] := 'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
        'Below this value, the Windgen model reverts to a constant impedance model. For model 7, the current is ' +
        'limited to the value computed for constant power at Vminpu.';

    PropertyName^[24] := 'Vmaxpu';
    PropertyHelp^[24] := 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
        'Above this value, the Windgen model reverts to a constant impedance model.';

    PropertyName^[25] := 'forceon';
    PropertyHelp^[25] := '{Yes | No}  Forces WindGen ON despite requirements of other dispatch modes. ' +
        'Stays ON until this property is set to NO, or an internal algorithm cancels the forced ON state.';

    PropertyName^[26] := 'kVA';
    PropertyHelp^[26] := 'kVA rating of electrical machine. Defaults to 1.2* kW if not specified. Applied to machine or inverter definition for Dynamics mode solutions. ';

    PropertyName^[27] := 'MVA';
    PropertyHelp^[27] := 'MVA rating of electrical machine.  Alternative to using kVA=.';

    PropertyName^[28] := 'Xd';
    PropertyHelp^[28] := 'Per unit synchronous reactance of machine. Presently used only for Thevinen impedance for power flow calcs of user models (model=6). ' +
        'Typically use a value 0.4 to 1.0. Default is 1.0';

    PropertyName^[29] := 'Xdp';
    PropertyHelp^[29] := 'Per unit transient reactance of the machine.  Used for Dynamics mode and Fault studies.  Default is 0.27.' +
        'For user models, this value is used for the Thevinen/Norton impedance for Dynamics Mode.';

    PropertyName^[30] := 'Xdpp';
    PropertyHelp^[30] := 'Per unit subtransient reactance of the machine.  Used for Harmonics. Default is 0.20.';

    PropertyName^[31] := 'H';
    PropertyHelp^[31] := 'Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0.';

    PropertyName^[32] := 'D';
    PropertyHelp^[32] := 'Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping';

    PropertyName^[33] := 'UserModel';
    PropertyHelp^[33] := 'Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, ' +
        'overriding the default model.  Set to "none" to negate previous setting.';
    PropertyName^[34] := 'UserData';
    PropertyHelp^[34] := 'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.';

    PropertyName^[35] := 'ShaftModel';
    PropertyHelp^[35] := 'Name of user-written DLL containing a Shaft model, which models the prime mover and determines the power on the shaft for Dynamics studies. ' +
        'Models additional mass elements other than the single-mass model in the DSS default model. Set to "none" to negate previous setting.';
    PropertyName^[36] := 'ShaftData';
    PropertyHelp^[36] := 'String (in quotes or parentheses) that gets passed to user-written shaft dynamic model for defining the data for that model.';

    PropertyName^[37] := 'DutyStart';
    PropertyHelp^[37] := 'Starting time offset [hours] into the duty cycle shape for this WindGen, defaults to 0';

    PropertyName^[38] := 'Balanced';
    PropertyHelp^[38] := '{Yes | No*} Default is No.  For Model=7, force balanced current only for 3-phase WindGens. Force zero- and negative-sequence to zero.';

    PropertyName^[39] := 'XRdp';
    PropertyHelp^[39] := 'Default is 20. X/R ratio for Xdp property for FaultStudy and Dynamic modes.';

    PropertyName^[40] := 'DynamicEq';
    PropertyHelp^[40] := 'The name of the dynamic equation (DinamicExp) that will be used for defining the dynamic behavior of the generator. ' +
        'if not defined, the generator dynamics will follow the built-in dynamic equation.';

    PropertyName^[41] := 'DynOut';
    PropertyHelp^[41] := 'The name of the variables within the Dynamic equation that will be used to govern the generator dynamics.' +
        'This generator model requires 2 outputs from the dynamic equation: ' + CRLF + CRLF +
        '1. Shaft speed (velocity) relative to synchronous speed.' + CRLF +
        '2. Shaft, or power, angle (relative to synchronous reference frame).' + CRLF + CRLF +
        'The output variables need to be defined in the same order.';

      {Removed Fuel-related variables 40-44 from Generator model}
      {Added 40-41 to make Windgen comaptible with DynamicExp}

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
    PropertyHelp^[NumPropsThisClass + 1] := 'Name of harmonic voltage or current spectrum for this WindGen. ' +
        'Voltage behind Xd" for machine - default. Current injection for inverter. ' +
        'Default value is "default", which is defined when the DSS starts.';

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TWindGen.NewObject(const ObjName: String): Integer;
begin
    // Make a new WindGen and add it to WindGen class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TWindGenObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TWindGen.SetNcondsForConnection;

begin
    with ActiveWindGenObj do
    begin
        case Connection of
            0:
                NConds := Fnphases + 1;
            1:
                case Fnphases of
                    1, 2:
                        NConds := Fnphases + 1; // L-L and Open-delta
                else
                    NConds := Fnphases;
                end;
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TWindGen.InterpretConnection(const S: String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
var
    TestS: String;

begin
    with ActiveWindGenObj do
    begin
        TestS := lowercase(S);
        case TestS[1] of
            'y', 'w':
                Connection := 0;  {Wye}
            'd':
                Connection := 1;  {Delta or line-Line}
            'l':
                case Tests[2] of
                    'n':
                        Connection := 0;
                    'l':
                        Connection := 1;
                end;

        end;

        SetNCondsForConnection;

            {VBase is always L-N voltage unless 1-phase device or more than 3 phases}

        with WindGenVars do {CASE Connection OF
              1: VBase := kVWindGenBase * 1000.0 ;
              Else}
            case Fnphases of
                2, 3:
                    VBase := kVWindGenBase * InvSQRT3x1000;    // L-N Volts
            else
                VBase := kVWindGenBase * 1000.0;   // Just use what is supplied
            end;
            {End;}
        VBase95 := Vminpu * VBase;
        VBase105 := Vmaxpu * VBase;

        Yorder := Fnconds * Fnterms;
        YprimInvalid[ActiveActor] := true;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function InterpretDispMode(const S: String): Integer;
begin

    case lowercase(S)[1] of
        'l':
            Result := LOADMODE;
    else
        Result := DEFAULT;
    end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TWindGen.Edit(ActorID: Integer): Integer;
var
    VarIdx,
    i,
    ParamPointer: Integer;
    ParamName: String;
    Param: String;


begin
  // continue parsing with contents of Parser
    ActiveWindGenObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveWindGenObj;

    Result := 0;

    with ActiveWindGenObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if (Length(ParamName) = 0) then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[PropertyIdxMap^[ParamPointer]] := Param
            else
            begin
           // first, checks if there is a dynamic eq assigned, then
           // checks if the new property edit the state variables within
                VarIdx := CheckIfDynVar(ParamName, ActorID);
                if VarIdx < 0 then
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for WindGen "' + Name + '"', 560);
            end;

            if ParamPointer > 0 then
                case PropertyIdxMap^[ParamPointer] of
                    0:
                        DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 561);
                    1:
                        NPhases := Parser[ActorID].Intvalue; // num phases
                    2:
                        SetBus(1, param);
                    3:
                        PresentkV := Parser[ActorID].DblValue;
                    4:
                        kWBase := Parser[ActorID].DblValue;
                    5:
                        PFNominal := Parser[ActorID].DblValue;
                    6:
                        GenModel := Parser[ActorID].IntValue;
                    7:
                        YearlyShape := Param;
                    8:
                        DailyDispShape := Param;
                    9:
                        DutyShape := Param;
                    10: ;
                    11: ;
                    12:
                        InterpretConnection(Param);
                    13:
                        Presentkvar := Parser[ActorID].DblValue;
                    14: ;
                    15: ;
                    16:
                        if lowercase(Param[1]) = 'f' then
                            IsFixed := true
                        else
                            IsFixed := false;
                    17:
                        GenClass := Parser[ActorID].IntValue;
                    18:
                        Vpu := Parser[ActorID].DblValue;
                    19:
                        kvarMax := Parser[ActorID].DblValue;
                    20:
                        kvarMin := Parser[ActorID].DblValue;
                    21:
                        PVFactor := Parser[ActorID].DblValue;  //decelaration factor
                    22:
                        DebugTrace := InterpretYesNo(Param);
                    23:
                        VMinPu := Parser[ActorID].DblValue;
                    24:
                        VMaxPu := Parser[ActorID].DblValue;
                    25:
                        FForcedON := InterpretYesNo(Param);
                    26:
                        WindGenVars.kVArating := Parser[ActorID].DblValue;
                    27:
                        WindGenVars.kVArating := Parser[ActorID].DblValue * 1000.0;  // 'MVA';
                    28:
                        WindGenVars.puXd := Parser[ActorID].DblValue;
                    29:
                        WindGenVars.puXdp := Parser[ActorID].DblValue;
                    30:
                        WindGenVars.puXdpp := Parser[ActorID].DblValue;
                    31:
                        WindGenVars.Hmass := Parser[ActorID].DblValue;
                    32:
                        WindGenVars.Dpu := Parser[ActorID].DblValue;
                    33:
                        UserModel.Name := Parser[ActorID].StrValue;  // Connect to user written models
                    34:
                        UserModel.Edit := Parser[ActorID].StrValue;  // Send edit string to user model
                    35:
                        ShaftModel.Name := Parser[ActorID].StrValue;
                    36:
                        ShaftModel.Edit := Parser[ActorID].StrValue;
                    37:
                        DutyStart := Parser[ActorID].DblValue;
                    38:
                        ForceBalanced := InterpretYesNo(Param);
                    39:
                        WindGenVars.XRdp := Parser[ActorID].DblValue;  // X/R for dynamics model
                    40:
                        DynamicEq := Param;
                    41:
                        SetDynOutput(Param);


                else
           // Inherited parameters
                    ClassEdit(ActiveWindGenObj, ParamPointer - NumPropsThisClass)
                end;

            if ParamPointer > 0 then
                case PropertyIdxMap^[ParamPointer] of
                    1:
                        SetNcondsForConnection;  // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
                    4, 5:
                        SyncUpPowerQuantities;

            // if a model 3 WindGen added, force calc of dQdV
                    6:
                        if GenModel = 3 then
                            ActiveCircuit[ActorID].Solution.SolutionInitialized := false;

    {Set shape objects;  returns nil if not valid}
     {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
                    7:
                    begin
                        YearlyShapeObj := LoadShapeClass[ActorID].Find(YearlyShape);
                        if Assigned(YearlyShapeObj) then
                            with YearlyShapeObj do
                                if UseActual then
                                    SetkWkvar(MaxP, MaxQ);
                    end;
                    8:
                    begin
                        DailyDispShapeObj := LoadShapeClass[ActorID].Find(DailyDispShape);
                        if Assigned(DailyDispShapeObj) then
                            with DailyDispShapeObj do
                                if UseActual then
                                    SetkWkvar(MaxP, MaxQ);
                    end;
                    9:
                    begin
                        DutyShapeObj := LoadShapeClass[ActorID].Find(DutyShape);
                        if Assigned(DutyShapeObj) then
                            with DutyShapeObj do
                                if UseActual then
                                    SetkWkvar(MaxP, MaxQ);
                    end;

                    22:
                        if DebugTrace then
                        begin
                            AssignFile(TraceFile, GetOutputDirectory + 'WINDGEN_' + Name + '.CSV');
                            ReWrite(TraceFile);
                            Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                Write(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                            Write(TraceFile, ',Vthev, Theta');
                            Writeln(TraceFile);
                            CloseFile(Tracefile);
                        end;
                    26, 27:
                        kVANotSet := false;
                    40:
                    begin
                        DynamicEqObj := TDynamicExpClass[ActorID].Find(DynamicEq);
                        if Assigned(DynamicEqObj) then
                            with DynamicEqObj do
                                setlength(DynamicEqVals, NumVars);
                    end;
                end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
        YprimInvalid[ActorID] := true;
    end;

end;

//----------------------------------------------------------------------------
function TWindGen.MakeLike(const OtherWindGenName: String): Integer;
var
    OtherWindGen: TWindGenObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line name in the present collection}
    OtherWindGen := Find(OtherWindGenName);
    if (OtherWindGen <> nil) then
        with ActiveWindGenObj do
        begin

            if (Fnphases <> OtherWindGen.Fnphases) then
            begin
                Nphases := OtherWindGen.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YprimInvalid[ActiveActor] := true;
            end;

            WindGenVars.kVWindGenBase := OtherWindGen.WindGenVars.kVWindGenBase;
            Vbase := OtherWindGen.Vbase;
            Vminpu := OtherWindGen.Vminpu;
            Vmaxpu := OtherWindGen.Vmaxpu;
            Vbase95 := OtherWindGen.Vbase95;
            Vbase105 := OtherWindGen.Vbase105;
            kWBase := OtherWindGen.kWBase;
            kvarBase := OtherWindGen.kvarBase;
            WindGenVars.Pnominalperphase := OtherWindGen.WindGenVars.Pnominalperphase;
            PFNominal := OtherWindGen.PFNominal;
            WindGenVars.Qnominalperphase := OtherWindGen.WindGenVars.Qnominalperphase;
            varMin := OtherWindGen.varMin;
            varMax := OtherWindGen.varMax;
            Connection := OtherWindGen.Connection;
     //  Rneut          := OtherWindGen.Rneut;
      // Xneut          := OtherWindGen.Xneut;
            YearlyShape := OtherWindGen.YearlyShape;
            YearlyShapeObj := OtherWindGen.YearlyShapeObj;
            DailyDispShape := OtherWindGen.DailyDispShape;
            DailyDispShapeObj := OtherWindGen.DailyDispShapeObj;
            DutyShape := OtherWindGen.DutyShape;
            DutyShapeObj := OtherWindGen.DutyShapeObj;
            DutyStart := OtherWindGen.DutyStart;
            GenClass := OtherWindGen.GenClass;
            GenModel := OtherWindGen.GenModel;
            IsFixed := OtherWindGen.IsFixed;
            WindGenVars.VTarget := OtherWindGen.WindGenvars.VTarget;
            Vpu := OtherWindGen.Vpu;
            kvarMax := OtherWindGen.kvarMax;
            kvarMin := OtherWindGen.kvarMin;
            FForcedON := OtherWindGen.FForcedON;
            kVANotSet := OtherWindGen.kVANotSet;

            WindGenVars.kVArating := OtherWindGen.WindGenVars.kVArating;
            WindGenVars.puXd := OtherWindGen.WindGenVars.puXd;
            WindGenVars.puXdp := OtherWindGen.WindGenVars.puXdp;
            WindGenVars.puXdpp := OtherWindGen.WindGenVars.puXdpp;
            WindGenVars.Hmass := OtherWindGen.WindGenVars.Hmass;
            WindGenVars.Theta := OtherWindGen.WindGenVars.Theta;
            WindGenVars.Speed := OtherWindGen.WindGenVars.Speed;
            WindGenVars.w0 := OtherWindGen.WindGenVars.w0;
            WindGenVars.dSpeed := OtherWindGen.WindGenVars.dSpeed;
            WindGenVars.D := OtherWindGen.WindGenVars.D;
            WindGenVars.Dpu := OtherWindGen.WindGenVars.Dpu;
            WindGenVars.XRdp := OtherWindGen.WindGenVars.Xrdp;

            UserModel.Name := OtherWindGen.UserModel.Name;  // Connect to user written models
            ShaftModel.Name := OtherWindGen.ShaftModel.Name;

            ClassMakeLike(OtherWindGen);

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue[i] := OtherWindGen.FPropertyValue[i];

            Result := 1;
        end
    else
        DoSimpleMsg('Error in Load MakeLike: "' + OtherWindGenName + '" Not Found.', 562);

end;

//----------------------------------------------------------------------------
function TWindGen.Init(Handle: Integer; ActorID: Integer): Integer;
var
    p: TWindGenObj;

begin

    if (Handle = 0) then
    begin  // init all
        p := elementList.First;
        while (p <> nil) do
        begin
            p.Randomize(0);
            p := elementlist.Next;
        end;
    end
    else
    begin
        Active := Handle;
        p := GetActiveObj;
        p.Randomize(0);
    end;

    DoSimpleMsg('Need to implement TWindGen.Init', -1);
    Result := 0;

end;

{--------------------------------------------------------------------------}
procedure TWindGen.ResetRegistersAll(ActorID: Integer);  // Force all EnergyMeters in the circuit to reset

var
    pGen: TWindGenObj;

begin
    pGen := ActiveCircuit[ActorID].WindGens.First;
    while (pGen <> nil) do
    begin
        pGen.ResetRegisters;
        pGen := ActiveCircuit[ActorID].WindGens.Next;
    end;

end;

{--------------------------------------------------------------------------}
procedure TWindGen.SampleAll(ActorID: Integer);  // Force all EnergyMeters in the circuit to take a sample

var
    pGen: TWindGenObj;

begin
    pGen := ActiveCircuit[ActorID].WindGens.First;
    while pGen <> nil do
    begin
        if pGen.enabled then
            pGen.TakeSample(ActorID);
        pGen := ActiveCircuit[ActorID].WindGens.Next;
    end;
end;

//----------------------------------------------------------------------------
constructor TWindGenObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + WINDGEN_ELEMENT;  // In both PCelement and Genelement list

    Nphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations
    kWBase := 1000.0;
    kvarBase := 60.0;


    kvarMax := kvarBase * 2.0;
    kvarMin := -kvarmax;
    PFNominal := 0.88;
    YearlyShape := '';
    YearlyShapeObj := nil;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
    DailyDispShape := '';
    DailyDispShapeObj := nil;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyShape := '';
    DutyShapeObj := nil;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyStart := 0.0;
    Connection := 0;    // Wye (star)
    GenModel := 1;  {Typical fixed kW negative load}
    GenClass := 1;
    LastYear := 0;
    LastGrowthFactor := 1.0;

    DQDVSaved := 0.0;  // Initialize this here.  Allows WindGens to be turned off and on


    WindGenSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenWindGenSolutionCount := -1;
    YPrimOpenCond := nil;

    WindGenVars.kVWindGenBase := 12.47;
    Vpu := 1.0;
    WindGenVars.VTarget := 1000.0 * Vpu * WindGenVars.kVWindGenBase / SQRT3;  {Line-to-Neutral target}
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;
    IsFixed := false;

     {Machine rating stuff}
    WindGenVars.kVArating := kWBase * 1.2;
    kVANotSet := true;  // Flag for default value for kVA


    with WindGenVars do
    begin
        puXd := 1.0;
        puXdp := 0.28;
        puXdpp := 0.20;
        Xd := puXd * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Xdp := puXdp * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Xdpp := puXdpp * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Hmass := 1.0;       //  W-sec/VA rating
        Theta := 0.0;
        w0 := TwoPi * Basefrequency;
        Speed := 0.0;
        dSpeed := 0.0;
        D := 1.0;
        XRdp := 20.0;
    end;

     {Advertise Genvars struct as public}

    PublicDataStruct := pointer(@WindGenVars);
    PublicDataSize := SizeOf(TWindGenVars);

    UserModel := TWindGenUserModel.Create(@WindGenVars);
    ShaftModel := TWindGenUserModel.Create(@WindGenVars);

  // Register values inherited from Generator model
    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    PVFactor := 0.1;
    DebugTrace := false;
    FForcedON := false;
    GenSwitchOpen := false;
    ShapeIsActual := false;
    ForceBalanced := false;

    Spectrum := 'defaultgen';  // override base class

    GenActive := true;   // variable to use if needed

    InitPropertyValues(0);

    RecalcElementData(ActiveActor);

end;


//----------------------------------------------------------------------------
destructor TWindGenObj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    ShaftModel.Free;
    inherited Destroy;
end;

//----------------------------------------------------------------------------
procedure TWindGenObj.Randomize(Opt: Integer);
begin
    case Opt of
        0:
            RandomMult := 1.0;
        GAUSSIAN:
            RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
        UNIfORM:
            RandomMult := Random;  // number between 0 and 1.0
        LOGNORMAL:
            RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
    end;
end;

//----------------------------------------------------------------------------
{Evaluates if the value provided corresponds to a constant value or to an operand
 for calculating the value using the simulation results}
function TWindGenObj.CheckIfDynVar(myVar: String; ActorID: Integer): Integer;
var
    myOp: Integer;        // Operator found
    myValue: String;         // Value entered by the user
begin

    Result := -1;
    if Assigned(DynamicEqObj) then
    begin

        Result := DynamicEqObj.Get_Var_Idx(myVar);
        if (Result >= 0) and (Result < 50000) then
        begin
            myValue := Parser[ActorID].StrValue;
            if (DynamicEqObj.Check_If_CalcValue(myValue, myOp)) then
            begin
        // Adss the pair (var index + operand index)
                setlength(DynamicEqPair, length(DynamicEqPair) + 2);
                DynamicEqPair[High(DynamicEqPair) - 1] := Result;
                DynamicEqPair[High(DynamicEqPair)] := myOp;
            end
            else // Otherwise, move the value to the values array
                DynamicEqVals[Result][0] := Parser[ActorID].DblValue;
        end
        else
            Result := -1;     // in case is a constant

    end;

end;

//----------------------------------------------------------------------------
{Obtains the indexes of the given variables to use them as reference for setting
the dynamic output for the generator}
procedure TWindGenObj.SetDynOutput(myVar: String);
var
    VarIdx,
    idx: Integer;
    myStrArray: TStringList;
begin
    if DynamicEqObj <> nil then        // Making sure we have a dynamic eq linked
    begin
    // First, set the length for the index array, 2 variables in this case
        setlength(DynOut, 2);
        myStrArray := TStringList.Create;
        InterpretTStringListArray(myVar, myStrArray);
    // ensuring they are lower case
        for idx := 0 to 1 do
        begin

            myStrArray[idx] := LowerCase(myStrArray[idx]);
            VarIdx := DynamicEqObj.Get_Out_Idx(myStrArray[idx]);
            if (VarIdx < 0) then
        // Being here means that the given name doesn't exist or is a constant
                DoSimpleMsg('DynamicExp variable "' + myStrArray[idx] + '" not found or not defined as an output.', 50008)
            else
                DynOut[idx] := VarIdx;

        end;

        myStrArray.Free;
    end
    else
        DoSimpleMsg('A DynamicExp object needs to be assigned to this element before this declaration: DynOut = [' + myVar + ']', 50007);
end;

//----------------------------------------------------------------------------
{Returns the names of the variables to be used as outputs for the dynamic expression}
function TWindGenObj.GetDynOutputStr(): String;
var
    idx: Integer;
begin
    Result := '[';                   // Open array str
    if DynamicEqObj <> nil then        // Making sure we have a dynamic eq linked
    begin
        for idx := 0 to High(DynOut) do
            Result := Result + DynamicEqObj.Get_VarName(DynOut[idx]) + ',';
    end;

    Result := Result + ']';         // Close array str
end;

//----------------------------------------------------------------------------
procedure TWindGenObj.CalcDailyMult(Hr: Double);

begin
    if (DailyDispShapeObj <> nil) then
    begin
        ShapeFactor := DailyDispShapeObj.GetMult(Hr);
        ShapeIsActual := DailyDispShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no daily variation
end;


//----------------------------------------------------------------------------
procedure TWindGenObj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> nil then
    begin
        ShapeFactor := DutyShapeObj.GetMult(Hr + DutyStart);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TWindGenObj.CalcYearlyMult(Hr: Double);

begin
{Yearly curve is assumed to be hourly only}
    if YearlyShapeObj <> nil then
    begin
        ShapeFactor := YearlyShapeObj.GetMult(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Defaults to no variation

end;


//----------------------------------------------------------------------------
procedure TWindGenObj.SetNominalGeneration(ActorID: Integer);
var
    Factor: Double;
    GenOn_Saved: Boolean;

begin
    GenOn_Saved := GenON;
    ShapeFactor := CDOUBLEONE;
    // Check to make sure the generation is ON
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave WindGen in whatever state it was prior to entering Dynamic mode
        begin
            GenON := true;   // Init to on then check if it should be off
 //       IF NOT FForcedON
 //       THEN CASE DispatchMode of
 //****          LOADMODE: IF (DispatchValue > 0.0)   AND (WindGenDispatchReference < DispatchValue)  THEN GenON := FALSE;
 //****          PRICEMODE:IF (DispatchValue > 0.0)   AND (PriceSignal < DispatchValue) THEN GenON := FALSE;
 //       END;
        end;


        if not GenON then
        begin
         // If WindGen is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
            WindGenvars.Pnominalperphase := -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
            WindGenvars.Qnominalperphase := 0.0;
        end
        else
        begin    // WindGen is on, compute it's nominal watts and vars
            with Solution do
                if IsFixed then
                begin
                    Factor := 1.0;   // for fixed WindGens, set constant
                end
                else
                begin
                    case Mode of
                        SNAPSHOT:
                            Factor := ActiveCircuit[ActorID].GenMultiplier * 1.0;
                        DAILYMODE:
                        begin
                            Factor := ActiveCircuit[ActorID].GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour) // Daily dispatch curve
                        end;
                        YEARLYMODE:
                        begin
                            Factor := ActiveCircuit[ActorID].GenMultiplier;
                            CalcYearlyMult(DynaVars.dblHour);
                        end;
                        DUTYCYCLE:
                        begin
                            Factor := ActiveCircuit[ActorID].GenMultiplier;
                            CalcDutyMult(DynaVars.dblHour);
                        end;
                        GENERALTIME,   // General sequential time simulation
                        DYNAMICMODE:
                        begin
                            Factor := ActiveCircuit[ActorID].GenMultiplier;
                                       // This mode allows use of one class of load shape
                            case ActiveCircuit[ActorID].ActiveLoadShapeClass of
                                USEDAILY:
                                    CalcDailyMult(DynaVars.dblHour);
                                USEYEARLY:
                                    CalcYearlyMult(DynaVars.dblHour);
                                USEDUTY:
                                    CalcDutyMult(DynaVars.dblHour);
                            else
                                ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                            end;
                        end;
                        MONTECARLO1,
                        MONTEFAULT,
                        FAULTSTUDY:
                            Factor := ActiveCircuit[ActorID].GenMultiplier * 1.0;
                        MONTECARLO2,
                        MONTECARLO3,
                        LOADDURATION1,
                        LOADDURATION2:
                        begin
                            Factor := ActiveCircuit[ActorID].GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour);
                        end;
                        PEAKDAY:
                        begin
                            Factor := ActiveCircuit[ActorID].GenMultiplier;
                            CalcDailyMult(DynaVars.dblHour);
                        end;
                        AUTOADDFLAG:
                            Factor := 1.0;
                    else
                        Factor := 1.0
                    end;
                end;

            if not (IsDynamicModel or IsHarmonicModel) then         //******
            begin
                if ShapeIsActual then
                    WindGenvars.Pnominalperphase := 1000.0 * ShapeFactor.re / Fnphases
                else
                    WindGenvars.Pnominalperphase := 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;

                with WindGenvars do
                    if GenModel = 3 then
                    begin   { Just make sure present value is reasonable}
                        if Qnominalperphase > varMax then
                            Qnominalperphase := varMax
                        else
                        if Qnominalperphase < varMin then
                            Qnominalperphase := varMin;
                    end
                    else
                    begin
                   { for other WindGen models}
                        if ShapeIsActual then
                            Qnominalperphase := 1000.0 * ShapeFactor.im / Fnphases
                        else
                            Qnominalperphase := 1000.0 * kvarBase * Factor * ShapeFactor.im / Fnphases;
                    end;
            end;
        end; {ELSE GenON}

        if not (IsDynamicModel or IsHarmonicModel) then
        begin       //******

            case GenModel of
                6:
                    Yeq := Cinv(cmplx(0.0, -WindGenvars.Xd));  // Gets negated in CalcYPrim
            else
                with WindGenvars do
                    Yeq := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
                if (Vminpu <> 0.0) then
                    Yeq95 := CDivReal(Yeq, sqr(Vminpu))  // at 95% voltage
                else
                    Yeq95 := Yeq; // Always a constant Z model

                if (Vmaxpu <> 0.0) then
                    Yeq105 := CDivReal(Yeq, Sqr(Vmaxpu))   // at 105% voltage
                else
                    Yeq105 := Yeq;
            end;

          { When we leave here, all the Yeq's are in L-N values}

            if GenModel = 7 then
                with WindGenvars do
                begin
                    PhaseCurrentLimit := Cdivreal(Cmplx(Pnominalperphase, -Qnominalperphase), VBase95);
                    Model7MaxPhaseCurr := Cabs(PhaseCurrentLimit);
                end;

        end;
    end;  {With ActiveCircuit[ActiveActor]}

   // If WindGen state changes, force re-calc of Y matrix
    if GenON <> GenON_Saved then
        YprimInvalid[ActorID] := true;

end;

//----------------------------------------------------------------------------
procedure TWindGenObj.RecalcElementData(ActorID: Integer);

begin

    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase := 1000.0 * kvarBase / Fnphases;
    varMin := 1000.0 * kvarMin / Fnphases;
    varMax := 1000.0 * kvarMax / Fnphases;

    {Populate data structures used for interchange with user-written models.}
    with WindGenvars do
    begin
        Xd := puXd * 1000.0 * SQR(kVWindGenBase) / kVARating;
        Xdp := puXdp * 1000.0 * SQR(kVWindGenBase) / kVArating;
        Xdpp := puXdpp * 1000.0 * SQR(kVWindGenBase) / kVArating;
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;
    end;

    SetNominalGeneration(ActorID);

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    if CompareText(YearlyShape, 'none') = 0 then
        YearlyShape := '';
    if CompareText(DailyDispShape, 'none') = 0 then
        DailyDispShape := '';
    if CompareText(DutyShape, 'none') = 0 then
        DutyShape := '';

    if YearlyShapeObj = nil then
        if Length(YearlyShape) > 0 then
            DoSimpleMsg('WARNING! Yearly load shape: "' + YearlyShape + '" Not Found.', 563);
    if DailyDispShapeObj = nil then
        if Length(DailyDispShape) > 0 then
            DoSimpleMsg('WARNING! Daily load shape: "' + DailyDispShape + '" Not Found.', 564);
    if DutyShapeObj = nil then
        if Length(DutyShape) > 0 then
            DoSimpleMsg('WARNING! Duty load shape: "' + DutyShape + '" Not Found.', 565);

    SpectrumObj := SpectrumClass[ActorID].Find(Spectrum);
    if SpectrumObj = nil then
        DoSimpleMsg('ERROR! Spectrum "' + Spectrum + '" Not Found.', 566);


    YQFixed := -varBase / Sqr(VBase);   //10-17-02  Fixed negative sign
    WindGenvars.Vtarget := Vpu * 1000.0 * WindGenvars.kVWindGenBase;

    if Fnphases > 1 then
        WindGenvars.VTarget := WindGenvars.VTarget / SQRT3;

    // Initialize to Zero - defaults to PQ WindGen
    // Solution object will reset after circuit modifications
    DQDV := DQDVSaved;         // for Model = 3
    DeltaQMax := (varMax - varMin) * 0.10;  // Limit to 10% of range

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    {Update any user-written models}
    if Usermodel.Exists then
        UserModel.FUpdateModel;
    if Shaftmodel.Exists then
        Shaftmodel.FUpdateModel;

end;

//----------------------------------------------------------------------------
procedure TWindGenObj.CalcYPrimMatrix(Ymatrix: TcMatrix; ActorID: Integer);

var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin

    FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with ActiveCircuit[ActorID].solution do
        if IsDynamicModel or IsHarmonicModel then
        begin
            if GenON then
                Y := Yeq   // L-N value computed in initial condition routines
            else
                Y := Cmplx(EPSILON, 0.0);

            if Connection = 1 then
                Y := CDivReal(Y, 3.0); // Convert to delta impedance
            Y.im := Y.im / FreqMultiplier;
            Yij := Cnegate(Y);
            for i := 1 to Fnphases do
            begin
                case Connection of
                    0:
                    begin
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(Fnconds, Fnconds, Y);
                        Ymatrix.SetElemsym(i, Fnconds, Yij);
                    end;
                    1:
                    begin   {Delta connection}
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(i, i, Y);  // put it in again
                        for j := 1 to i - 1 do
                            Ymatrix.SetElemsym(i, j, Yij);
                    end;
                end;
            end;

      (**** Removed Neutral / Neutral may float

       IF Connection = 0 Then   With Ymatrix Do  // Take care of neutral issues
         Begin
           AddElement(Fnconds, Fnconds, YNeut);  // Add in user specified Neutral Z, if any
           // Bump up neutral-ground in case neutral ends up floating
           SetElement(Fnconds, Fnconds, CmulReal(GetElement(Fnconds, Fnconds), 1.000001));
         End;

      *)
        end

        else
        begin  //  Regular power flow WindGen model

       {Yeq is always expected as the equivalent line-neutral admittance}

            Y := cnegate(Yeq);  // negate for generation    Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
            Y.im := Y.im / FreqMultiplier;

            case Connection of

                0:
                    with YMatrix do
                    begin // WYE
                        Yij := Cnegate(Y);
                        for i := 1 to Fnphases do
                        begin
                            SetElement(i, i, Y);
                            AddElement(Fnconds, Fnconds, Y);
                            SetElemsym(i, Fnconds, Yij);
                        end;
                    end;
                1:
                    with YMatrix do
                    begin  // Delta  or L-L
                        Y := CDivReal(Y, 3.0); // Convert to delta impedance
                        Yij := Cnegate(Y);
                        for i := 1 to Fnphases do
                        begin
                            j := i + 1;
                            if j > Fnconds then
                                j := 1;  // wrap around for closed connections
                            AddElement(i, i, Y);
                            AddElement(j, j, Y);
                            AddElemSym(i, j, Yij);
                        end;
                    end;
            end;
        end;  {ELSE IF Solution.mode}

end;


//----------------------------------------------------------------------------
procedure TWindGenObj.CalcYPrim(ActorID: Integer);

var
    i: Integer;

begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV does not fail
    if YprimInvalid[ActorID] then
    begin
        if YPrim_Shunt <> nil then
            YPrim_Shunt.Free;
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        if YPrim_Series <> nil then
            Yprim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> nil then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Shunt.Clear;
        YPrim_Series.Clear;
        YPrim.Clear;
    end;

    if ActiveCircuit[ActorID].Solution.LoadModel = POWERFLOW then
    begin

        // 12-7-99 we'll start with Yeq in system matrix
        SetNominalGeneration(ActorID);
        CalcYPrimMatrix(YPrim_Shunt, ActorID);

    end
    else
    begin

         // ADMITTANCE model wanted

        SetNominalGeneration(ActorID);
        CalcYPrimMatrix(YPrim_Shunt, ActorID);

    end;

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim(ActorID);

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
 {Add the current into the proper location according to connection}

 {Reverse of similar routine in load  (Cnegates are switched)}

var
    j: Integer;

begin
    case Connection of

        0:
        begin  //Wye
            Caccum(TermArray^[i], Curr);
            Caccum(TermArray^[Fnconds], Cnegate(Curr)); // Neutral
        end;

        1:
        begin //DELTA
            Caccum(TermArray^[i], Curr);
            j := i + 1;
            if j > Fnconds then
                j := 1;
            Caccum(TermArray^[j], Cnegate(Curr));
        end;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.WriteTraceRecord(const s: String; ActorID: Integer);

var
    i: Integer;

begin

    try
        if (not InshowResults) then

        begin
            Append(TraceFile);
            Write(TraceFile, Format('%-.g, %d, %-.g, ',
                [ActiveCircuit[ActorID].Solution.DynaVars.t + ActiveCircuit[ActorID].Solution.Dynavars.IntHour * 3600.0,
                ActiveCircuit[ActorID].Solution.Iteration,
                ActiveCircuit[ActorID].LoadMultiplier]),
                GetSolutionModeID, ', ',
                GetLoadModel, ', ',
                GenModel: 0, ', ',
                DQDV: 8: 0, ', ',
                (V_Avg * 0.001732 / WindGenvars.kVWindGenbase): 8: 3, ', ',
                (WindGenvars.Vtarget - V_Avg): 9: 1, ', ',
                (WindGenvars.Qnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                (WindGenvars.Pnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                s, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(InjCurrent^[i])): 8: 1, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(ITerminal^[i])): 8: 1, ', ');
            for i := 1 to nphases do
                Write(TraceFile, (Cabs(Vterminal^[i])): 8: 1, ', ');
            Write(TraceFile, WindGenvars.VThevMag: 8: 1, ', ', WindGenvars.Theta * 180.0 / PI);
            Writeln(TRacefile);
            CloseFile(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoConstantPQGen(ActorID: Integer);

{Compute total terminal current for Constant PQ}

var
    i: Integer;
    Curr, V: Complex;
    Vmag: Double;
//   V012,I012 :Array[0..2] of Complex;
//   Iabc :Array[1..3] of Complex;
begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    ZeroITerminal;

    (*****   Tried this but couldn't get it to work
    CASE Fnphases of

    3:With Genvars Do Begin     // Use Symmetrical Components
          Phase2SymComp(Vterminal, @V012);   // Vterminal is L-N voltages here
                         // Phase2SymComp(InjCurrent, @I012);   // Vterminal is L-G voltages here
          V := V012[1]; // Positive sequence L-N voltage
          Vmag := Cabs(V012[1]);

           { IF   VMag <= VBase95
            THEN Curr := Cnegate(Cmul(Yeq95, V))  // Below 95% (Vminpu) use an impedance model
            ELSE If VMag > VBase105
            THEN Curr := Cnegate(Cmul(Yeq105, V))  // above 105% (Vmaxpu) use an impedance model
            }
            IF   (VMag <= VBase95) or (VMag > VBase105) THEN    Curr := Conjg( Cdiv( CurrentLimit, CDivReal(V, -Vmag)) )
            ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(-Pnominalperphase, -Qnominalperphase), V));    // Current INTO pos seq model

         I012[1] := Curr;  // Pos sequence current into the terminal

          If Connection=1 Then I012[0] := CZERO  Else I012[0] := Cdiv(V012[0], cmplx(0.0, xdpp));
          I012[2] := Cdiv(V012[2], cmplx(0.0, xdpp));

          // Negative and Zero Sequence Contributions
         SymComp2Phase(@Iabc, @I012);    // Iabc now desired terminal current
         IF DebugTrace Then Begin
             Append(TraceFile);
             Write(TraceFile,Format('V1=%-.5g, /_%-.5g, ',[Cabs(V), CDang(V)]));
             Write(TraceFile,Format('I1=%-.5g, /_%-.5g, ',[Cabs(Curr), CDang(Curr)]));
             Write(TraceFile,'Iabc=');
             For i := 1 to 3 Do Write(TraceFile,Format('%-.5g, /_%-.5g, ',[ Cabs(Iabc[i]), CDang(Iabc[i])]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;

          For i := 1 to 3 Do Begin
            ITerminal^[i] := Iabc[i];  // Put into Terminal array directly because we have computed line current above
            Caccum(InjCurrent^[i], Cnegate(Iabc[i]));  // subtract in
            If Connection=0 Then Begin
               Caccum(Iterminal^[Fnconds], Cnegate(Iabc[i]));  // Neutral
               Caccum(InjCurrent^[Fnconds], Iabc[i]);  // Neutral
            End;
          End;
          IterminalUpdated := TRUE;  // so that we con't have to recompute for a report
      End
    ELSE
    ****)


    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        case Connection of
            0:
            begin  {Wye}
                if VMag <= VBase95 then
                    Curr := Cmul(Yeq95, V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
                else
                    with WindGenvars do
                        Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
            end;
            1:
            begin  {Delta}
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                        {leave Vmag as is}
                end;

                if VMag <= VBase95 then
                    Curr := Cmul(CdivReal(Yeq95, 3.0), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(CdivReal(Yeq105, 3.0), V)  // above 105% use an impedance model
                else
                    with WindGenvars do
                        Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
            end;
        end;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(true, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
    {END;}
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoConstantZGen(ActorID: Integer);
var
    i: Integer;
    Curr,
    Yeq2: Complex;

begin

// Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;
    if Connection = 0 then
        Yeq2 := Yeq
    else
        Yeq2 := CdivReal(Yeq, 3.0);

    for i := 1 to Fnphases do
    begin
        Curr := Cmul(Yeq2, Vterminal^[i]);   // Yeq is always line to neutral

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(true, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoPVTypeGen(ActorID: Integer);
{Compute total terminal current for Constant P,|V|}

// Constant P, constant |V|

var

    i: Integer;
    DQ: Double;
    Curr: Complex;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the WindGen
    ZeroITerminal;

    // Guess at a new var output value
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);

    if Connection = 1 then
        V_Avg := V_Avg / (SQRT3 * Fnphases)
    else
        V_Avg := V_Avg / Fnphases;

   // 12-9-99 added empirical 0.7 factor to improve iteration
   // 12-17-99 changed to 0.1 because first guess was consistently too high
    DQ := PVFactor * DQDV * (WindGenvars.Vtarget - V_Avg);   // Vtarget is L-N
    if (Abs(DQ) > DeltaQMax) then
        if (DQ < 0.0) then
            DQ := -DeltaQMax
        else
            DQ := DeltaQMax;
    with WindGenvars do
        Qnominalperphase := Qnominalperphase + DQ;

   { Test Limits}
    with WindGenvars do
    begin
        if (Qnominalperphase > varMax) then
            Qnominalperphase := varMax
        else
        if (Qnominalperphase < varMin) then
            Qnominalperphase := varMin;

       // Compute injection currents using W and var values
       // Do not use comstant Z models outside normal range
       // Presumably the var source will take care of the voltage problems
        for i := 1 to Fnphases do
        begin
            Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), Vterminal^[i]));

            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            set_ITerminalUpdated(true, ActorID);
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        end;
    end; {With}
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoFixedQGen(ActorID: Integer);

{Compute total terminal current for Fixed Q}
// Constant P, Fixed Q  Q is always kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;

begin
    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        case Connection of
            0:
            begin
                if VMag <= VBase95 then
                    Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)  // above 105% use an impedance model
                else
                    Curr := Conjg(Cdiv(Cmplx(WindGenvars.Pnominalperphase, varBase), V));
            end;
            1:
            begin
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                    {leave Vmag as is}
                end;
                if VMag <= VBase95 then
                    Curr := Cmul(Cmplx(Yeq95.re / 3.0, YQfixed / 3.0), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Cmplx(Yeq105.re / 3.0, YQfixed / 3.0), V)  // above 105% use an impedance model
                else
                    Curr := Conjg(Cdiv(Cmplx(WindGenvars.Pnominalperphase, varBase), V));
            end;
        end;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(true, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoFixedQZGen(ActorID: Integer);

{Compute total terminal current for }
// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase
var
    i: Integer;
    Curr,
    V: Complex;
    Vmag: Double;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        Vmag := Cabs(V);

        case Connection of
            0:
            begin
                if Vmag <= VBase95 then
                    Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)
                else
                begin
                    Curr := Conjg(Cdiv(Cmplx(WindGenvars.Pnominalperphase, 0.0), V)); // P component of current
                    Caccum(Curr, Cmul(Cmplx(0.0, YQFixed), V));  // add in Q component of current
                end;
            end;
            1:
            begin
                case Fnphases of
                    2, 3:
                        VMag := VMag / SQRT3;  // L-N magnitude
                else
                      {leave Vmag as is}
                end;
                if Vmag <= VBase95 then
                    Curr := Cmul(Cmplx(Yeq95.re / 3.0, YQfixed / 3.0), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Cmplx(Yeq105.re / 3.0, YQfixed / 3.0), V)
                else
                begin
                    Curr := Conjg(Cdiv(Cmplx(WindGenvars.Pnominalperphase, 0.0), V)); // P component of current
                    Caccum(Curr, Cmul(Cmplx(0.0, YQFixed / 3.0), V));  // add in Q component of current
                end;
            end;
        end;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(true, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end; {FOR}
end;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoUserModel(ActorID: Integer);
{Compute total terminal Current from User-written model}
var
    i: Integer;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array

    if UserModel.Exists then    // Check automatically selects the usermodel if true
    begin
         //AppendToEventLog('Wnominal=', Format('%-.5g',[Pnominalperphase]));
        UserModel.FCalc(Vterminal, Iterminal);
        set_ITerminalUpdated(true, ActorID);
        with ActiveCircuit[ActorID].Solution do
        begin          // Negate currents from user model for power flow WindGen model
            for i := 1 to FnConds do
                Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
        end;
    end
    else
    begin
        DoSimpleMsg('WindGen.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoCurrentLimitedPQ(ActorID: Integer);
{Compute total terminal current for Constant PQ, but limit to max current below
 Vminpu}


var
    i: Integer;
    PhaseCurr, DeltaCurr, VLN, VLL: Complex;
    VMagLN, VMagLL: Double;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load

    if ForceBalanced and (Fnphases = 3) then
    begin    // convert to pos-seq only
        Phase2SymComp(Vterminal, @V012);
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
    end;

    ZeroITerminal;

    for i := 1 to Fnphases do
    begin

        case Connection of
            0:
            begin
                VLN := Vterminal^[i];   // VTerminal is LN for this connection
                VMagLN := Cabs(VLN);
                with WindGenvars do
                    PhaseCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));
                if Cabs(PhaseCurr) > Model7MaxPhaseCurr then
                    PhaseCurr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLN, VMagLN)));

                StickCurrInTerminalArray(ITerminal, Cnegate(PhaseCurr), i);  // Put into Terminal array taking into account connection
                set_ITerminalUpdated(true, ActorID);
                StickCurrInTerminalArray(InjCurrent, PhaseCurr, i);  // Put into Terminal array taking into account connection
            end;
            1:
            begin
                VLL := Vterminal^[i];     // VTerminal is LL for this connection
                VMagLL := Cabs(VLL);
                case Fnphases of
                    2, 3:   // 2 or 3 phase WindGen model 7
                    begin
                        with WindGenvars do
                            DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));
                        if Cabs(DeltaCurr) * SQRT3 > Model7MaxPhaseCurr then
                            DeltaCurr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLL, VMagLL / SQRT3)));
                    end
                else  // 1-phase WindGen model 7
                    with WindGenvars do
                        DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));
                    if Cabs(DeltaCurr) > Model7MaxPhaseCurr then
                        DeltaCurr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLL, VMagLL)));
                end;

                StickCurrInTerminalArray(ITerminal, Cnegate(DeltaCurr), i);  // Put into Terminal array taking into account connection
                set_ITerminalUpdated(true, ActorID);
                StickCurrInTerminalArray(InjCurrent, DeltaCurr, i);  // Put into Terminal array taking into account connection
            end;
        end;

    end;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoDynamicMode(ActorID: Integer);

{Compute Total Current and add into InjTemp}

var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array  and computes VTerminal L-N

   {Inj = -Itotal (in) - Yprim*Vtemp}

    case GenModel of

        6:
            if UserModel.Exists then       // auto selects model
            begin   {We have total currents in Iterminal}
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
            end
            else
            begin
                DoSimpleMsg(Format('Dynamics model missing for WindGen.%s ', [Name]), 5671);
                SolutionAbort := true;
            end;
    else

        case Fnphases of  {No user model, use default Thevinen equivalent for standard WindGen model}
            1:
                with WindGenvars do
                begin
                   // 1-phase WindGens have 2 conductors
                    case Genmodel of
                        7:
                        begin  // simple inverter model
                                  // Assume inverter stays in phase with terminal voltage
                            CalcVthev_Dyn_Mod7(CSub(VTerminal^[1], VTerminal^[2]));
                        end;
                    else
                        CalcVthev_Dyn;  // Update for latest phase angle
                    end;


                    ITerminal^[1] := CDiv(CSub(Csub(VTerminal^[1], Vthev), VTerminal^[2]), Zthev);  // ZThev is based on Xd'
                    if Genmodel = 7 then
                    begin
                        if Cabs(Iterminal^[1]) > Model7MaxPhaseCurr then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(Model7MaxPhaseCurr, cang(Iterminal^[1])));
                    end;

                    ITerminal^[2] := Cnegate(ITerminal^[1]);
                end;

            3:
                with WindGenvars do
                begin
                    Phase2SymComp(Vterminal, @V012);

                    case GenModel of
                        7:
                        begin  // simple inverter model
                                // Positive Sequence Contribution to Iterminal
                                // Assume inverter stays in phase with pos seq voltage
                                // and pos seq current is limited
                            CalcVthev_Dyn_Mod7(V012[1]);

                                // Positive Sequence Contribution to Iterminal
                                // Ref Frame here is all L-N

                            I012[1] := CDiv(Csub(V012[1], Vthev), Zthev); // ZThev is based on Xd'
                            if Cabs(I012[1]) > Model7MaxPhaseCurr  // Limit the current but keep phase angle
                            then
                                I012[1] := ptocomplex(topolar(Model7MaxPhaseCurr, cang(I012[1])));
                            if ForceBalanced  // set the negative sequence current
                            then
                                I012[2] := CZERO
                            else
                                I012[2] := Cdiv(V012[2], Zthev);  // for inverter ZThev is  (Xd' + j0)

                        end
                    else
                            // Positive Sequence Contribution to Iterminal
                        CalcVthev_Dyn;  // Update for latest phase angle

                            // Positive Sequence Contribution to Iterminal
                        I012[1] := CDiv(Csub(V012[1], Vthev), Zthev);  // ZThev is based on Xd'
                        I012[2] := Cdiv(V012[2], Cmplx(0.0, Xdpp));  // machine use Xd"
                    end;

                      {Adjust for WindGen connection}
                    if (Connection = 1) or ForceBalanced then
                        I012[0] := CZERO
                    else
                        I012[0] := Cdiv(V012[0], Cmplx(0.0, Xdpp));

                    SymComp2Phase(ITerminal, @I012);  // Convert back to phase components

                      // Neutral current
                    if Connection = 0 then
                        ITerminal^[FnConds] := Cnegate(CmulReal(I012[0], 3.0));
                end;
        else
            DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase WindGens. WindGen.%s has %d phases.', [name, Fnphases]), 5671);
            SolutionAbort := true;
        end;

    end;

    set_ITerminalUpdated(true, ActorID);

    {Add it into inj current array}
    for i := 1 to FnConds do
        Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

   {Take Care of any shaft model calcs}
    if (GenModel = 6) and ShaftModel.Exists then      // auto selects model
    begin           // Compute Mech Power to shaft
        ShaftModel.FCalc(Vterminal, Iterminal);     // Returns pshaft at least
    end;
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoHarmonicMode(ActorID: Integer);

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

var
    i: Integer;
    E: Complex;
    GenHarmonic: Double;

begin

    ComputeVterminal(ActorID);

    with ActiveCircuit[ActorID].Solution do
    begin
        GenHarmonic := Frequency / GenFundamental;
        E := CmulReal(SpectrumObj.GetMult(GenHarmonic), WindGenvars.VThevHarm); // Get base harmonic magnitude
        RotatePhasorRad(E, GenHarmonic, WindGenvars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            cBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase WindGen
        end;
    end;

   {Handle Wye Connection}
    if Connection = 0 then
        cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
    YPrim.MVMult(InjCurrent, @cBuffer);

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.CalcVTerminalPhase(ActorID: Integer);

var
    i, j: Integer;

begin

{ Establish phase voltages and stick in Vterminal}
    case Connection of

        0:
        begin
            with ActiveCircuit[ActorID].Solution do
                for i := 1 to Fnphases do
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds], ActorID);
        end;

        1:
        begin
            with ActiveCircuit[ActorID].Solution do
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > Fnconds then
                        j := 1;
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[j], ActorID);
                end;
        end;

    end;

    WindGenSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.CalcVTerminal(ActorID: Integer);

{Put terminal voltages in an array}


begin

    ComputeVTerminal(ActorID);

    WindGenSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.CalcGenModelContribution(ActorID: Integer);
// Calculates WindGen current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

begin
    set_ITerminalUpdated(false, ActorID);
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode(ActorID)
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode(ActorID)
        else
        begin
           //  compute currents and put into InjTemp array;
            case GenModel of
                1:
                    DoConstantPQGen(ActorID);
                2:
                    DoConstantZGen(ActorID);
                3:
                    DoPVTypeGen(ActorID);  // Constant P, |V|
                4:
                    DoFixedQGen(ActorID);
                5:
                    DoFixedQZGen(ActorID);
                6:
                    DoUserModel(ActorID);
                7:
                    DoCurrentLimitedPQ(ActorID);
            else
                DoConstantPQGen(ActorID);  // for now, until we implement the other models.
            end;
        end; {ELSE}
    end; {WITH}

   {When this is done, ITerminal is up to date}

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.CalcInjCurrentArray(ActorID: Integer);


// Difference between currents in YPrim and total current


begin


// Now Get Injection Currents
    if GenSwitchOpen then
        ZeroInjCurrent
    else
        CalcGenModelContribution(ActorID);

(*  We're not going to mess with this logic here -- too complicated: Use an open line in series
    to look at open phase conditions.

  ELSE Begin

   // some terminals not closed  use admittance model for injection
      If OpenWindGenSolutionCount <> ActiveCircuit[ActiveActor].Solution.SolutionCount Then Begin

      // Rebuild the Yprimopencond if a new solution because values may have changed.

        // only reallocate when necessary
        If YPrimOpenCond=nil Then YPrimOpenCond := TcMatrix.CreateMatrix(Yorder)
        ELSE YPrimOpenCond.Clear;
        If YPrimOpenCond.Order <> Yorder Then Begin
           YPrimOpenCond.Free;
           YPrimOpenCond := TcMatrix.CreateMatrix(Yorder);
        End;
        CalcYPrimMatrix(YPrimOpenCond);

        {Now Account for the Open Conductors}
        {For any conductor that is open, zero out row and column}
         With YPrimOpenCond Do Begin
           k := 0;
           FOR i := 1 TO Fnterms Do Begin
             FOR j := 1 TO Fnconds Do Begin
                 If Not Terminals^[i].Conductors^[j].Closed Then Begin
                    ZeroRow(j+k);
                    ZeroCol(j+k);
                    SetElement(j+k, j+k, Cmplx(1.0e-12,0.0));  // In case node gets isolated
                 End;
             End;
             k := k+Fnconds;
           End;
         End;
         OpenWindGenSolutionCount := ActiveCircuit[ActiveActor].Solution.SolutionCount;
         
      End;

      With ActiveCircuit[ActiveActor].Solution Do
      FOR i := 1 TO Yorder Do Begin
          Ref := NodeRef^[i];
          If Ref=0 Then Vterminal^[i] := cZero
          ELSE  Vterminal^[i] := V^[ref];
      End;
      YPrimOpenCond.MVmult(InjTemp, Vterminal);
      For i := 1 to Yorder Do InjTemp^[i] := Cnegate(InjTemp^[i]);
   End;
 *)
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer);

// Compute total Currents


begin
    with ActiveCircuit[ActorID].Solution do
    begin
        if IterminalSolutionCount[ActorID] <> ActiveCircuit[ActorID].Solution.SolutionCount then
        begin     // recalc the contribution
            if not GenSwitchOpen then
                CalcGenModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr, ActorID);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent', ActorID);

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TWindGenObj.InjCurrents(ActorID: Integer): Integer;


begin

    with ActiveCircuit[ActorID].Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalGeneration(ActorID); // Set the nominal kW, etc for the type of solution being done

        CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection', ActorID);

       // Add into System Injection Current Array

        Result := inherited InjCurrents(ActorID);

    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);

// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

var
    i: Integer;

begin

    CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current

    try
   // Copy into buffer array
        for i := 1 to Yorder do
            Curr^[i] := InjCurrent^[i];

    except
        ON E: Exception do
            DoErrorMsg('WindGen Object: "' + Name + '" in GetInjCurrents function.',
                E.Message,
                'Current buffer not big enough.', 568);
    end;

end;
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TWindGenObj.ResetRegisters;

var
    i: Integer;

begin
    for i := 1 to NumGenregisters do
        Registers[i] := 0.0;
    for i := 1 to NumGenregisters do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := true;  // initialize for trapezoidal integration
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TWindGenObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double; ActorID: Integer);

begin
    if ActiveCircuit[ActorID].TrapezoidalIntegration then
    begin
        {Trapezoidal Rule Integration}
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else   {Plain Euler integration}
        Registers[Reg] := Registers[Reg] + Interval * Deriv;

    Derivatives[Reg] := Deriv;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TWindGenObj.TakeSample(ActorID: Integer);
// Update Energy from metered zone

var
    S: Complex;
    Smag: Double;
    HourValue: Double;

begin

// Compute energy in WindGen branch
    if Enabled then
    begin

        if GenON then
        begin
            S := cmplx(Get_PresentkW, Get_Presentkvar);
            Smag := Cabs(S);
            HourValue := 1.0;
        end
        else
        begin
            S := CZERO;
            Smag := 0.0;
            HourValue := 0.0;
        end;

        if GenON or ActiveCircuit[ActorID].TrapezoidalIntegration then
      {Make sure we always integrate for Trapezoidal case
       Don't need to for Gen Off and normal integration}
            with ActiveCircuit[ActorID].Solution do
            begin
                if ActiveCircuit[ActorID].PositiveSequence then
                begin
                    S := CmulReal(S, 3.0);
                    Smag := 3.0 * Smag;
                end;
                Integrate(Reg_kWh, S.re, IntervalHrs, ActorID);   // Accumulate the power
                Integrate(Reg_kvarh, S.im, IntervalHrs, ActorID);
                SetDragHandRegister(Reg_MaxkW, abs(S.re));
                SetDragHandRegister(Reg_MaxkVA, Smag);
                Integrate(Reg_Hours, HourValue, IntervalHrs, ActorID);  // Accumulate Hours in operation
                Integrate(Reg_Price, S.re * ActiveCircuit[ActorID].PriceSignal * 0.001, IntervalHrs, ActorID);  // Accumulate Hours in operation
                FirstSampleAfterReset := false;

            end;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TWindGenObj.Get_PresentkW: Double;
begin
    Result := WindGenvars.Pnominalperphase * 0.001 * Fnphases;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TWindGenObj.Get_PresentkV: Double;
begin
    Result := WindGenvars.kVWindGenBase;
end;

function TWindGenObj.Get_Presentkvar: Double;
begin
    Result := WindGenvars.Qnominalperphase * 0.001 * Fnphases;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.InitDQDVCalc;

begin
    DQDV := 0.0;
    WindGenvars.Qnominalperphase := 0.5 * (varmax + varmin);   // avg of the limits
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.BumpUpQ;
{Bump up vars by 10% of range for next calc}
begin
    with WindGenvars do
        Qnominalperphase := Qnominalperphase + 0.1 * (varmax - varmin);
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.RememberQV(ActorID: Integer);

var
    i: Integer;

begin
    var_Remembered := WindGenvars.Qnominalperphase;
    CalcVTerminal(ActorID);
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);
    V_Avg := V_Avg / Fnphases;
    V_Remembered := V_Avg;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.CalcDQDV(ActorID: Integer);
var
    Vdiff: Double;
    i: Integer;
begin

    CalcVTerminal(ActorID);
    V_Avg := 0.0;
    for i := 1 to Fnphases do
        V_Avg := V_Avg + Cabs(Vterminal^[i]);
    V_Avg := V_Avg / Fnphases;

    Vdiff := V_Avg - V_Remembered;
    if (Vdiff <> 0.0) then
        DQDV := (WindGenvars.Qnominalperphase - var_Remembered) / Vdiff
    else
        DQDV := 0.0;  // Something strange has occured
                       // this will force a de facto P,Q model
    DQDVSaved := DQDV;  //Save for next time  Allows WindGen to be enabled/disabled during simulation
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.ResetStartPoint;

begin
    WindGenvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, idx: Integer;

begin
    inherited DumpProperties(F, Complete);

    Writeln(F, '!DQDV=', DQDV: 10: 2);


    with ParentClass do
        for i := 1 to NumProperties do
        begin
            idx := PropertyIdxMap^[i];
            case idx of
                34, 36:
                    Writeln(F, '~ ', PropertyName^[i], '=(', PropertyValue[idx], ')');
                44:
                    Writeln(F, '~ ', PropertyName^[i], '=False')  // This one has no variable associated, not needed

            else
                Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[idx]);
            end;
        end;

    Writeln(F);

end;


procedure TWindGenObj.InitHarmonics(ActorID: Integer);
var
    E, Va: complex;
begin

    YprimInvalid[ActorID] := true;  // Force rebuild of YPrims
    GenFundamental := ActiveCircuit[ActorID].Solution.Frequency;  // Whatever the frequency is when we enter here.

    with WindGenvars do
    begin

        Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         {Compute reference Thevinen voltage from phase 1 current}

        if GenON then
        begin

            ComputeIterminal(ActorID);  // Get present value of current

            with ActiveCircuit[ActorID].solution do
                case Connection of
                    0:
                    begin {wye - neutral is explicit}
                        if not ADiakoptics or (ActorID = 1) then
                            Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]])
                        else
                            Va := Csub(VoltInActor1(NodeRef^[1]), VoltInActor1(NodeRef^[Fnconds]));
                    end;
                    1:
                    begin  {delta -- assume neutral is at zero}
                        if not ADiakoptics or (ActorID = 1) then
                            Va := NodeV^[NodeRef^[1]]
                        else
                            Va := VoltInActor1(NodeRef^[1]);
                    end;
                end;

            E := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
            Vthevharm := Cabs(E);   // establish base mag and angle
            ThetaHarm := Cang(E);
        end
        else
        begin
            Vthevharm := 0.0;
            ThetaHarm := 0.0;
        end;
    end;

end;

procedure TWindGenObj.InitPropertyValues(ArrayOffset: Integer);

begin

    PropertyValue[1] := '3';     //'phases';
    PropertyValue[2] := Getbus(1);         //'bus1';
    PropertyValue[3] := '12.47';
    PropertyValue[4] := '100';
    PropertyValue[5] := '.80';
    PropertyValue[6] := '1';
    PropertyValue[7] := '';
    PropertyValue[8] := '';
    PropertyValue[9] := '';
    PropertyValue[10] := 'Default';
    PropertyValue[11] := '0.0';
    PropertyValue[12] := 'wye';
    PropertyValue[13] := '60';
    PropertyValue[14] := '0'; // 'rneut'; // if entered -, assume open
    PropertyValue[15] := '0';  //'xneut';
    PropertyValue[16] := 'variable'; //'status'  fixed or variable
    PropertyValue[17] := '1'; //'class'
    PropertyValue[18] := '1.0';
    PropertyValue[19] := Str_Real(kvarMax, 3);
    PropertyValue[20] := Str_Real(kvarMin, 3);
    PropertyValue[21] := '0.1';
    PropertyValue[22] := 'no';
    PropertyValue[23] := '0.90';
    PropertyValue[24] := '1.10';
    PropertyValue[25] := 'No';
    PropertyValue[26] := Format('%-g', [WindGenvars.kVARating]);
    PropertyValue[27] := Format('%-g', [WindGenvars.kVARating * 0.001]);
    PropertyValue[28] := Format('%-g', [WindGenvars.puXd]);
    PropertyValue[29] := Format('%-g', [WindGenvars.puXdp]);
    PropertyValue[30] := Format('%-g', [WindGenvars.puXdpp]);
    PropertyValue[31] := Format('%-g', [WindGenvars.Hmass]);
    PropertyValue[32] := Format('%-g', [WindGenvars.Dpu]);
    PropertyValue[33] := '';
    PropertyValue[34] := '';
    PropertyValue[35] := '';
    PropertyValue[36] := '';
    PropertyValue[37] := '0';
    PropertyValue[38] := 'No';
    PropertyValue[39] := '20';
    PropertyValue[40] := '';
    PropertyValue[41] := '[]';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TWindGenObj.InitStateVars(ActorID: Integer);
var
    {VNeut,}
    NumData,
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;

begin
    YprimInvalid[ActorID] := true;  // Force rebuild of YPrims
    with WindGenvars do
    begin

        case Genmodel of
            7:
                Zthev := Cmplx(Xdp, 0.0); // use Xd' as an equivalent R for the inverter
        else
            Zthev := Cmplx(Xdp / XRdp, Xdp);
        end;

        Yeq := Cinv(Zthev);

     {Compute nominal Positive sequence voltage behind transient reactance}

        if GenON then
            with ActiveCircuit[ActorID].Solution do
            begin

                ComputeIterminal(ActorID);

                case Fnphases of

                    1:
                    begin
                        if not ADiakoptics or (ActorID = 1) then
                            Edp := Csub(CSub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[2]]), Cmul(ITerminal^[1], Zthev))
                        else
                            Edp := Csub(CSub(VoltInActor1(NodeRef^[1]), VoltInActor1(NodeRef^[2])), Cmul(ITerminal^[1], Zthev));
                        VThevMag := Cabs(Edp);
                    end;

                    3:
                    begin
                 // Calculate Edp based on Pos Seq only
                        Phase2SymComp(ITerminal, @I012);
                     // Voltage behind Xdp  (transient reactance), volts

                        for i := 1 to FNphases do
                            if not ADiakoptics or (ActorID = 1) then
                                Vabc[i] := NodeV^[NodeRef^[i]]   // Wye Voltage
                            else
                                Vabc[i] := VoltInActor1(NodeRef^[i]);   // Wye Voltage

                        Phase2SymComp(@Vabc, @V012);
                        Edp := Csub(V012[1], Cmul(I012[1], Zthev));    // Pos sequence
                        VThevMag := Cabs(Edp);
                    end;
                else
                    DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase WindGens. WindGen.' + name + ' has %d phases.', [Fnphases]), 5672);
                    SolutionAbort := true;
                end;

                if DynamicEqObj = nil then
                begin
           // Shaft variables
           // Theta is angle on Vthev[1] relative to system reference
           //Theta  := Cang(Vthev^[1]);   // Assume source at 0
                    Theta := Cang(Edp);
                    if GenModel = 7 then
                        Model7LastAngle := Theta;

                    dTheta := 0.0;
                    w0 := Twopi * ActiveCircuit[ActorID].Solution.Frequency;
           // recalc Mmass and D in case the frequency has changed
                    with WindGenvars do
                    begin
                        WindGenvars.Mmass := 2.0 * WindGenvars.Hmass * WindGenvars.kVArating * 1000.0 / (w0);   // M = W-sec
                        D := Dpu * kVArating * 1000.0 / (w0);
                    end;
                    Pshaft := -Power[1, ActorID].re; // Initialize Pshaft to present power Output

                    Speed := 0.0;    // relative to synch speed
                    dSpeed := 0.0;

           // Init User-written models
           //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
                    with ActiveCircuit[ActorID].Solution do
                        if GenModel = 6 then
                        begin
                            if UserModel.Exists then
                                UserModel.FInit(Vterminal, Iterminal);
                            if ShaftModel.Exists then
                                ShaftModel.Finit(Vterminal, Iterminal);
                        end;

                end
                else
                begin
           // Initializes the memory values for the dynamic equation
                    for i := 0 to High(DynamicEqVals) do
                        DynamicEqVals[i][1] := 0.0;
           // Check for initial conditions using calculated values (P0, Q0)
                    NumData := (length(DynamicEqPair) div 2) - 1;
                    for i := 0 to NumData do
                        if DynamicEqObj.IsInitVal(DynamicEqPair[(i * 2) + 1]) then
                        begin
                            case DynamicEqPair[(i * 2) + 1] of
                                9:
                                begin
                                    DynamicEqVals[DynamicEqPair[i * 2]][0] := Cang(Edp);
                                    if GenModel = 7 then
                                        Model7LastAngle := DynamicEqVals[DynamicEqPair[i * 2]][0];
                                end;
                            else
                                DynamicEqVals[DynamicEqPair[i * 2]][0] := PCEValue[1, DynamicEqPair[(i * 2) + 1], ActorID];
                            end;
                        end;

                end;

            end
        else
        begin
            Vthev := cZERO;
            Theta := 0.0;
            dTheta := 0.0;
            w0 := 0;
            Speed := 0.0;
            dSpeed := 0.0;
        end;
    end;  {With}
end;

procedure TWindGenObj.IntegrateStates(ActorID: Integer);
var
    i,
    Numdata: Integer;
    TracePower: Complex;
begin
   // Compute Derivatives and then integrate

    ComputeIterminal(ActorID);

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)

    with ActiveCircuit[ActorID].Solution, WindGenvars do
    begin

        if DynamicEqObj = nil then
        begin
      // Dynamics using the internal equation
            with DynaVars do
                if (IterationFlag = 0) then
                begin {First iteration of new time step}
                    ThetaHistory := Theta + 0.5 * h * dTheta;
                    SpeedHistory := Speed + 0.5 * h * dSpeed;
                end;

      // Compute shaft dynamics
            TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases);
            dSpeed := (Pshaft + TracePower.re - D * Speed) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
            dTheta := Speed;

     // Trapezoidal method
            with DynaVars do
            begin
                Speed := SpeedHistory + 0.5 * h * dSpeed;
                Theta := ThetaHistory + 0.5 * h * dTheta;
            end;

      // Write Dynamics Trace Record
            if DebugTrace then
            begin
                Append(TraceFile);
                Write(TraceFile, Format('t=%-.5g ', [Dynavars.t]));
                Write(TraceFile, Format(' Flag=%d ', [Dynavars.Iterationflag]));
                Write(TraceFile, Format(' Speed=%-.5g ', [Speed]));
                Write(TraceFile, Format(' dSpeed=%-.5g ', [dSpeed]));
                Write(TraceFile, Format(' Pshaft=%-.5g ', [PShaft]));
                Write(TraceFile, Format(' P=%-.5g Q= %-.5g', [TracePower.Re, TracePower.im]));
                Write(TraceFile, Format(' M=%-.5g ', [Mmass]));
                Writeln(TraceFile);
                CloseFile(TraceFile);
            end;

            if GenModel = 6 then
            begin
                if UserModel.Exists then
                    UserModel.Integrate;
                if ShaftModel.Exists then
                    ShaftModel.Integrate;
            end;

        end
        else
        begin
      // Dynamics using an external equation
            with DynaVars do
                if (IterationFlag = 0) then
                begin {First iteration of new time step}
                    SpeedHistory := DynamicEqVals[DynOut[0]][0] + 0.5 * h * DynamicEqVals[DynOut[0]][1]; // first speed
                    ThetaHistory := DynamicEqVals[DynOut[1]][0] + 0.5 * h * DynamicEqVals[DynOut[1]][1]; // then angle
                end;

      // Check for initial conditions using calculated values (P, Q, VMag, VAng, IMag, IAng)
            NumData := (length(DynamicEqPair) div 2) - 1;
            for i := 0 to NumData do
                if not DynamicEqObj.IsInitVal(DynamicEqPair[(i * 2) + 1]) then     // it's not intialization
                begin
                    case DynamicEqPair[(i * 2) + 1] of
                        0:
                            DynamicEqVals[DynamicEqPair[i * 2]][0] := -TerminalPowerIn(Vterminal, Iterminal, FnPhases).re;
                        1:
                            DynamicEqVals[DynamicEqPair[i * 2]][0] := -TerminalPowerIn(Vterminal, Iterminal, FnPhases).im;
                    else
                        DynamicEqVals[DynamicEqPair[i * 2]][0] := PCEValue[1, DynamicEqPair[(i * 2) + 1], ActorID];
                    end;
                end;
      // solves the differential equation using the given values
            DynamicEqObj.SolveEq(DynamicEqVals);
      // Trapezoidal method   - Places the calues in the same vars to keep the code consistent
            with DynaVars do
            begin
                Speed := SpeedHistory + 0.5 * h * DynamicEqVals[DynOut[0]][1];
                Theta := ThetaHistory + 0.5 * h * DynamicEqVals[DynOut[1]][1];
            end;

      // saves the new integration values in memoryspace
            DynamicEqVals[DynOut[0]][0] := Speed;
            DynamicEqVals[DynOut[1]][0] := Theta;
        end;

    end;

end;

function TWindGenObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

var
    N, k: Integer;

begin
    N := 0;
    Result := -9999.99;  // error return value
    if i < 1 then
        Exit;  // Someone goofed

    with WindGenvars do
        case i of
            1:
                Result := (w0 + Speed) / TwoPi;  // Frequency, Hz
            2:
                Result := (Theta) * RadiansToDegrees;  // Report in Deg
            3:
                Result := Cabs(Vthev) / vbase;      // Report in pu
            4:
                Result := Pshaft;
            5:
                Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
            6:
                Result := dTheta;
        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumGenVariables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;

           {If we get here, must be in the Shaft Model if anywhere}
            if ShaftModel.Exists then
            begin
                k := i - (NumGenVariables + N);
                if k > 0 then
                    Result := ShaftModel.FGetVariable(k);
            end;
        end;
        end;

end;

procedure TWindGenObj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;

begin
    N := 0;
    if i < 1 then
        Exit;  // Someone goofed
    with WindGenvars do
        case i of
            1:
                Speed := (Value - w0) * TwoPi;
            2:
                Theta := Value / RadiansToDegrees; // deg to rad
            3: ;// meaningless to set Vd := Value * vbase; // pu to volts
            4:
                Pshaft := Value;
            5:
                dSpeed := Value / RadiansToDegrees;
            6:
                dTheta := Value;
        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumGenVariables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
         // If we get here, must be in the shaft model
            if ShaftModel.Exists then
            begin
                k := (i - (NumGenVariables + N));
                if k > 0 then
                    ShaftModel.FSetVariable(k, Value);
            end;
        end;
        end;
end;

procedure TWindGenObj.GetAllVariables(States: pDoubleArray);

var
    i, N: Integer;
begin
    N := 0;
    if DynamiceqObj = nil then
        for i := 1 to NumGenVariables do
            States^[i] := Variable[i]
    else
        for i := 1 to DynamiceqObj.NumVars * length(DynamicEqVals[0]) do
            States^[i] := DynamiceqObj.Get_DynamicEqVal(i - 1, DynamicEqVals);

    if UserModel.Exists then
    begin
        N := UserModel.FNumVars;
        UserModel.FGetAllVars(@States^[NumGenVariables + 1]);
    end;

    if ShaftModel.Exists then
    begin
        ShaftModel.FGetAllVars(@States^[NumGenVariables + 1 + N]);
    end;
end;

function TWindGenObj.NumVariables: Integer;
begin
    Result := NumGenVariables;
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
    if ShaftModel.Exists then
        Result := Result + ShaftModel.FNumVars;
end;

function TWindGenObj.VariableName(i: Integer): String;
const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of
    {$IFDEF MSWINDOWS}
    Ansichar
    {$ELSE}
char
    {$ENDIF}
    ;
    pName: Pansichar;

begin
    n := 0;
    if i < 1 then
        Exit;  // Someone goofed
    case i of
        1:
            Result := 'Frequency';
        2:
            Result := 'Theta (Deg)';
        3:
            Result := 'Vd';
        4:
            Result := 'PShaft';
        5:
            Result := 'dSpeed (Deg/sec)';
        6:
            Result := 'dTheta (Deg)';
    else
    begin
        if UserModel.Exists then  // Checks for existence and Selects
        begin
            pName := @Buff;
            n := UserModel.FNumVars;
            i2 := i - NumGenVariables;
            if i2 <= n then
            begin
                 // DLL functions require AnsiString (AnsiString) type
                UserModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;

        if ShaftModel.Exists then
        begin
            pName := @Buff;
            i2 := i - NumGenVariables - n;
            if i2 > 0 then
                UserModel.FGetVarName(i2, pName, BuffSize);
            Result := String(pName);
        end;
    end;
    end;

end;

function TWindGenObj.GetPropertyValue(Index: Integer): String;

begin
    Result := '';
    case Index of
        3:
            Result := Format('%.6g', [WindGenvars.kVWindGenBase]);
        4:
            Result := Format('%.6g', [kWBase]);
        5:
            Result := Format('%.6g', [PFNominal]);
        7:
            Result := Yearlyshape;
        8:
            Result := Dailydispshape;
        9:
            Result := DutyShape;
        13:
            Result := Format('%.6g', [kvarBase]);
        19:
            Result := Format('%.6g', [kvarMax]);
        20:
            Result := Format('%.6g', [kvarMin]);
        26:
            Result := Format('%.6g', [WindGenvars.kVArating]);
        27:
            Result := Format('%.6g', [WindGenvars.kVArating * 0.001]);
        34, 36:
        begin
            Result := '(' + inherited GetPropertyValue(index) + ')';
        end;
        37:
            Result := Format('%.6g', [DutyStart]);
        38:
            if ForceBalanced then
                Result := 'Yes'
            else
                Result := 'No';
        40:
            Result := DynamicEq;
        41:
            Result := GetDynOutputStr();
    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TWindGenObj.MakePosSequence(ActorID: Integer);

var
    S: String;
    V: Double;

begin

    S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := WindGenvars.kVWindGenBase / SQRT3
    else
        V := WindGenvars.kVWindGenBase;

    S := S + Format(' kV=%-.5g', [V]);

  // Divide the load by no. phases
    if Fnphases > 1 then
    begin
        S := S + Format(' kW=%-.5g  PF=%-.5g', [kWbase / Fnphases, PFNominal]);
        if (PrpSequence^[19] <> 0) or (PrpSequence^[20] <> 0) then
            S := S + Format(' maxkvar=%-.5g  minkvar=%-.5g', [kvarmax / Fnphases, kvarmin / Fnphases]);
        if PrpSequence^[26] > 0 then
            S := S + Format(' kva=%-.5g  ', [WindGenvars.kvarating / Fnphases]);
        if PrpSequence^[27] > 0 then
            S := S + Format(' MVA=%-.5g  ', [WindGenvars.kvarating / 1000.0 / Fnphases]);
    end;

    Parser[ActorID].CmdString := S;
    Edit(ActorID);

    inherited;
end;

procedure TWindGenObj.Set_ConductorClosed(Index: Integer; ActorID: Integer;
    Value: Boolean);
begin
    inherited;

 // Just turn WindGen on or off;

    if Value then
        GenSwitchOpen := false
    else
        GenSwitchOpen := true;

end;


procedure TWindGenObj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    SyncUpPowerQuantities;
end;

procedure TWindGenObj.Set_PresentkV(const Value: Double);
begin
    with WindGenvars do
    begin
        kVWindGenBase := Value;
        case FNphases of
            2, 3:
                VBase := kVWindGenBase * InvSQRT3x1000;
        else
            VBase := kVWindGenBase * 1000.0;
        end;
    end;
end;

procedure TWindGenObj.Set_Presentkvar(const Value: Double);
var
    kVA_Gen: Double;

begin
    kvarBase := Value;
    WindGenvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases; // init to something reasonable
    kVA_Gen := Sqrt(Sqr(kWBase) + Sqr(kvarBase));
    if kVA_Gen <> 0.0 then
        PFNominal := kWBase / kVA_Gen
    else
        PFNominal := 1.0;
    if (kWBase * kvarBase) < 0.0 then
        PFNominal := -PFNominal;

    kvarMax := 2.0 * kvarBase;
    kvarMin := -kvarMax;
end;

procedure TWindGenObj.Set_PresentkW(const Value: Double);
begin

    kWBase := Value;
    SyncUpPowerQuantities;

end;

procedure TWindGenObj.SyncUpPowerQuantities;
begin

   // keep kvar nominal up to date with kW and PF
    if (PFNominal <> 0.0) then
    begin
        kvarBase := kWBase * sqrt(1.0 / Sqr(PFNominal) - 1.0);
        WindGenvars.Qnominalperphase := 1000.0 * kvarBase / Fnphases;
        kvarMax := 2.0 * kvarBase;
        kvarMin := -kvarMax;
        if PFNominal < 0.0 then
            kvarBase := -kvarBase;

        if kVANotSet then
            WindGenvars.kVARating := kWBase * 1.2;

    end;

end;

procedure TWindGenObj.SetDragHandRegister(Reg: Integer;
    const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

procedure TWindGenObj.SetkWkvar(const PkW, Qkvar: Double);
begin

    kWBase := PkW;
    Presentkvar := Qkvar;

end;

procedure TWindGenObj.CalcVthev_Dyn;
begin
    if GenSwitchOpen then
        WindGenvars.VThevMag := 0.0;
    Vthev := pclx(WindGenvars.VthevMag, WindGenvars.Theta);
end;

procedure TWindGenObj.CalcVthev_Dyn_Mod7(const V: Complex);
{Adjust VThev to be in phase with V, if possible}
{
 If the voltage magnitude drops below 15% or so, the accuracy of determining the
 phase angle gets flaky. This algorithm approximates the action of a PLL that will
 hold the last phase angle until the voltage recovers.
}
var
    Model7angle: Double;
begin
    if GenSwitchOpen then
        WindGenvars.VThevMag := 0.0;
   {
      For Phases=1, Vbase is voltage across the terminals.
      Else it is LN voltage.
   }
    if Cabs(V) > 0.2 * Vbase then
        Model7angle := Cang(V)
    else
        Model7Angle := Model7LastAngle;

    Vthev := pclx(WindGenvars.VthevMag, Model7angle);
    Model7Lastangle := Model7angle;

end;

end.
