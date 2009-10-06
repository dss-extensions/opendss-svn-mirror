unit Storage;

{
  ----------------------------------------------------------
  Copyright (c) 2009, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    10/04/2009 Created from Generator Model
    
    
}
{
  The storage element is essentially a generator that can be dispatched
  to either produce power or consume power commensurate with rating and
  amount of stored energy.
}

//  The Storage element is assumed balanced over the no. of phases defined

// If you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation (i.e. multiplier = 1.0 always)
//    Daily:   Defaults to No variation
//    Dutycycle: Defaults to Daily shape

interface

USES  StoreUserModel, DSSClass,  PCClass, PCElement, ucmatrix, ucomplex, LoadShape, GrowthShape, Spectrum, ArrayDef, Dynamics;

//**** DEFINE REGISTERS AND VARIABLES

Const  NumStorageRegisters = 6;    // Number of energy meter registers
       NumStorageVariables = 6;

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

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageObj = class(TPCElement)
      Private
        Yeq             :Complex;   // at nominal
        Yeq95           :Complex;   // at 95%
        Yeq105          :Complex;   // at 105%

        CurrentLimit    :Complex;
        DebugTrace      :Boolean;
        DeltaQMax       :Double;  // Max allowable var change on Model=3 per iteration
        DispatchMode    :Integer;
        DispatchValue   :Double;
        FForcedON       :Boolean;
        FirstSampleAfterReset  :Boolean;
        Fixed           :Boolean;   // if Fixed, always at base value
        StorageSolutionCount    :Integer;
        StorageFundamental  :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        StoreON           :Boolean;           {Indicates whether generator is currently on}
        StorageObjSwitchOpen   :Boolean;

        kVANotSet       :Boolean;
        kVArating       :Double;
        kVStorageBase   :Double;

        kWhRating       :Double;
        kWhStored       :Double;
        kWhReserve      :Double;
        pctR            :Double;
        pctX            :Double;

        LastGrowthFactor :Double;
        LastYear         :Integer;   // added for speedup so we don't have to search for growth factor a lot
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
        Tracefile       : TextFile;
        UserModel       : TStoreUserModel;   {User-Written Models}

        varBase         :Double; // Base vars per phase
        VBase           :Double;  // Base volts suitable for computing currents
        VBase105        :Double;
        VBase95         :Double;
        Vmaxpu          :Double;
        Vminpu          :Double;
        Vthev           :Complex;  {Thevinen equivalent voltage (complex) for dynamic model}
        Vthevharm       :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        VthevMag        :Double;    {Thevinen equivalent voltage for dynamic model}
        YPrimOpenCond   :TCmatrix;  // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
        YQFixed         :Double;  // Fixed value of y for type 7 load

        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);
        Procedure CalcStorageModelContribution;
        Procedure CalcInjCurrentArray;
        Procedure CalcVterminal;
        Procedure CalcVTerminalPhase;
        Procedure CalcVthev_Dyn;      // 3-phase Voltage behind transient reactance
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
        StorageModel    :Integer;   // Variation with voltage
//****        StoreVars       :TStorageVars; {State Variables}
        kvarBase        :Double;
        kWBase          :Double;
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
       Property ForcedON     :Boolean Read FForcedON       Write FForcedON;
       Property PresentkV    :Double  Read Get_PresentkV   Write Set_PresentkV;
       Property PowerFactor  :Double  Read PFNominal       Write Set_PowerFactor;

   End;

VAR
    ActiveStorageObj:TStorageObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit,  Sysutils, Command, Math, MathUtil, DSSClassDefs, DSSGlobals, Utilities;

//**** REDEFINE THIS NUMBER
Const

{
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit function
    3) add a statement(s) to InitPropertyValues function to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, if needed
}
  propKV         =  3;
  propKW         =  4;
  propPF         =  5;
  propMODEL      =  6;
  propYEARLY     =  7;
  propDAILY      =  8;
  propDUTY       =  9;
  propDISPMODE   = 10;
  prop11         = 11;
  propCONNECTION = 12;
  propKVAR       = 13;
  propPCTR       = 14;
  propPCTX       = 15;
  prop16         = 16;
  propCLASS      = 17;
  propDISPOUTVAL = 18;
  propDISPINVAL  = 19;
  prop20         = 20;
  prop21         = 21;
  prop22         = 22;
  propVMINPU     = 23;
  propVMAXPU     = 24;
  propFORCEDON   = 25;
  propKVA        = 26;
  propMVA        = 27;
  propKWHRATED   = 28;
  propKWHSTORED  = 29;
  propPCTRESERVE = 30;      
  propUSERMODEL  = 31;
  propUSERDATA   = 32;
  propDEBUGTRACE = 33;

  NumPropsThisClass = 33; // Make this agree with the last property constant


//**** DEFINE DISPATCH MODES

  // Dispatch modes
      DEFAULT = 0;
      LOADMODE = 1;
      PRICEMODE = 2;

Var cBuffer:Array[1..24] of Complex;  // Temp buffer for calcs  24-phase generator?
    CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TStorage.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name := 'Generator';
     DSSClassType := DSSClassType + GEN_ELEMENT;  // In both PCelement and Genelement list

     ActiveElement := 0;

//****DEFINE REGISTER NAMES
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
     AddProperty('phases', 1, 'Number of Phases, this Storage element.  Power is evenly divided among phases.');
     AddProperty('bus1',   2, 'Bus to which the Storage element is connected.  May include specific node specification.');
     AddProperty('kv',        propKV,
                              'Nominal rated (1.0 per unit) voltage, kV, for Storage element. For 2- and 3-phase Generators, specify phase-phase kV. '+
                              'Otherwise, specify actual kV across each branch of the Storage element. '+
                              'If wye (star), specify phase-neutral kV. '+
                              'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
     AddProperty('kW',        propKW,
                              'Total base kW for the Generator.  A positive value denotes power coming OUT of the element, '+CRLF+
                              'which is the opposite of a load. This value is modified depending on the dispatch mode. ' +
                              'Unaffected by the global load multiplier and growth curves. ' +
                              'If you want there to be more generation, you must add more generators or change this value.');
     AddProperty('pf',        propPF,
                              'Generator power factor. Default is 0.80. Enter negative for leading powerfactor '+
                              '(when kW and kvar have opposite signs.)'+CRLF+
                              'A positive power factor for a generator signifies that the Storage element produces vars ' + CRLF +
                              'as is typical for a generator.  ');
     AddProperty('kvar',      propKVAR,
                              'Specify the base kvar.  Alternative to specifying the power factor.  Side effect: '+
                              ' the power factor value is altered to agree based on present value of kW.');
     AddProperty('kVA',       propKVA,
                              'kVA rating of power output. Defaults to 1.0* kW if not specified.');
     AddProperty('MVA',       propMVA,
                              'MVA rating of power output.  Alternative to using kVA=.');

     AddProperty('kWhrated',  propKWHRATED,
                              'Rated storage capacity in kWh. Default is 50.');
     AddProperty('kWhstored', propKWHSTORED,
                              'Present amount of energy stored, kWh. Default is 50.');
     AddProperty('%reserve',  propPCTRESERVE,
                              'Percent of rated kWh storage capacity to be held in reserve for normal operation. Default = 20. ' + CRLF +
                              'This is treated as the minimum energy discharge level unless there is an emergency.');
     AddProperty('%R',        propPCTR,
                              'Equivalent percent internal resistance, ohms. Default is 0.');
     AddProperty('%X',        propPCTX,
                              'Equivalent percent internal reactance, ohms. Default is 50%. (Limits fault current to 2 pu.');
     AddProperty('model',     propMODEL,
                              'Integer code for the model to use for powet output variation with voltage. '+
                              'Valid values are:' +CRLF+CRLF+
                              '1:Storage element injects a constant kW at specified power factor.'+CRLF+
                              '2:Storage element is modeled as a constant admittance.'  +CRLF+
                              '3:Compute load injection from User-written Model.');

     AddProperty('Vminpu',       propVMINPU,
                                 'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
                                 'Below this value, the load model reverts to a constant impedance model.');
     AddProperty('Vmaxpu',       propVMAXPU,
                                 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
                                 'Above this value, the load model reverts to a constant impedance model.');
     AddProperty('yearly',       propYEARLY,
                                 'Dispatch shape to use for yearly simulations.  Must be previously defined '+
                                 'as a Loadshape object. If this is not specified, the daily dispatch shape is repeated. '+
                                 'If the Storage element is assumed to be ON continuously, specify this value as FIXED, or '+
                                 'designate a curve that is 1.0 per unit at all times. '+
                                 'Nominally for 8760 simulations.  If there are fewer points in the designated shape than '+
                                 'the number of points in the solution, the curve is repeated.');
     AddProperty('daily',        propDAILY,
                                 'Dispatch shape to use for daily simulations.  Must be previously defined '+
                                 'as a Loadshape object of 24 hrs, typically.  If Storage element is assumed to be '+
                                 'ON continuously, specify this value as FIXED, or designate a Loadshape object'+
                                 'that is 1.0 perunit for all hours.'); // daily dispatch (hourly)
     AddProperty('duty',          propDUTY,
                                  'Load shape to use for duty cycle dispatch simulations such as for wind generation. ' +
                                 'Must be previously defined as a Loadshape object. '+
                                 'Typically would have time intervals less than 1 hr -- perhaps, in seconds. '+
                                 'Designate the number of points to solve using the Set Number=xxxx command. '+
                                 'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation
     AddProperty('dispmode',     propDISPMODE,
                                 '{Default | Loadlevel | Price } Default = Default. Dispatch mode. '+
                                 'In default mode, Storage element is either always on or follows dispatch curve as specified. '+
                                 'Otherwise, the Storage element comes on when either the global default load level or the price level '+
                                 'exceeds the dispatch value.'); // = 0 | >0
     AddProperty('dispoutvalue', propDISPOUTVAL,
                                 'Dispatch trigger value for discharging the storage. '+CRLF+
                                 'If = 0.0 Then Storage element follow dispatch curves, if any. ' +CRLF+
                                 'If > 0  Then Storage element is ON only when either the price signal exceeds this value or the load multiplier '+
                                 '(set loadmult=) times the default yearly growth factor ' +
                                 'exceeds this value.  Then the Storage element follows dispatch curves, if any (see also Status).');  // = 0 | >0
     AddProperty('dispinvalue', propDISPINVAL,
                                ' Dispatch trigger value for charging the storage. '+CRLF+
                                'If = 0.0 Then Storage element follow dispatch curves, if any. ' +CRLF+
                                'If > 0  Then Storage element is ON only when either the price signal exceeds this value or the load multiplier '+
                                '(set loadmult=) times the default yearly growth factor ' +
                                'exceeds this value.  Then the Storage element follows dispatch curves, if any (see also Status).');  // = 0 | >0
     AddProperty('conn',        propCONNECTION,
                                '={wye|LN|delta|LL}.  Default is wye.');

     AddProperty('class',       propCLASS,
                                'An arbitrary integer number representing the class of Generator so that Generator values may '+
                                'be segregated by class.'); // integer

     AddProperty('forceon',     propFORCEDON,
                                '{Yes | No}  Forces Storage element ON despite requirements of other dispatch modes. ' +
                                'Stays ON until this property is set to NO, or an internal algorithm cancels the forced ON state.');
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
        End;
   End;
  End;
End;

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
   i,
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

         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
            0               : DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
            1               : NPhases    := Parser.Intvalue; // num phases
            2               : SetBus(1, param);
           propKV           : PresentkV    := Parser.DblValue;
           propKW           : kWBase       := Parser.DblValue;
           propPF           : PFNominal    := Parser.DblValue;
           propMODEL        : StorageModel := Parser.IntValue;
           propYEARLY       : YearlyShape  := Param;
           propDAILY        : DailyShape  := Param;
           propDUTY         : DutyShape     := Param;
           propDISPMODE     : DispatchMode  := InterpretDispMode(Param);
           prop11           : ;
           propCONNECTION   : InterpretConnection(Param);
           propKVAR         : Presentkvar   := Parser.DblValue;
           propPCTR         : pctR          := Parser.DblValue;
           propPCTX         : pctX          := Parser.DblValue;
           prop16           : ;
           propCLASS        : StorageClass  := Parser.IntValue;
           propDISPOUTVAL   : ; //**** DispatchValue := Parser.DblValue;
           propDISPINVAL    : ;
           prop20           : ;
           prop21           : ;
           prop22           : ;
           propVMINPU       : VMinPu       := Parser.DblValue;
           propVMAXPU       : VMaxPu       := Parser.DblValue;
           propFORCEDON     : FForcedON     := InterpretYesNo(Param);
           propKVA          : kVArating    := Parser.DblValue;
           propMVA          : kVArating    := Parser.DblValue * 1000.0;  // 'MVA';
           propKWHRATED     : kWhrating    := Parser.DblValue;
           propKWHSTORED    : kWhstored    := Parser.DblValue;
           propPCTRESERVE   : kWhReserve    := Parser.DblValue;
           propUSERMODEL    : UserModel.Name := Parser.StrValue;  // Connect to user written models
           propUSERDATA     : UserModel.Edit := Parser.StrValue;  // Send edit string to user model
           propDEBUGTRACE   : DebugTrace   := InterpretYesNo(Param);


         ELSE
           // Inherited parameters
             ClassEdit(ActiveStorageObj, ParamPointer - NumPropsThisClass)
         End;

         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
            1: SetNcondsForConnection;  // Force Reallocation of terminal info
            propKW,propPF: SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF

    {Set loadshape objects;  returns nil if not valid}
            propYEARLY: YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
            propDAILY:  DailyShapeObj := LoadShapeClass.Find(DailyShape);
            propDUTY:   DutyShapeObj := LoadShapeClass.Find(DutyShape);

//****    WHAT GOES IN TRACE FILE?
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
//**** Is this needed?
            propKVA, propMVA: kVANotSet := FALSE;
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

       If (Fnphases <> OtherStorageObj.Fnphases)
       Then Begin
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
       kWBase         := OtherStorageObj.kWBase;
       kvarBase       := OtherStorageObj.kvarBase;
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
//****       DispatchValue  := OtherStorageObj.DispatchValue;
       StorageClass   := OtherStorageObj.StorageClass;
       StorageModel   := OtherStorageObj.StorageModel;
       Fixed          := OtherStorageObj.Fixed;

       FForcedON      := OtherStorageObj.FForcedON;
       kVANotSet      := OtherStorageObj.kVANotSet;

       kVArating      := OtherStorageObj.kVArating;

       kWhRating       := OtherStorageObj.kWhRating;
       kWhStored       := OtherStorageObj.kWhStored;
       kWhReserve      := OtherStorageObj.kWhReserve;  // per unit of kWhRating
       pctR            := OtherStorageObj.pctR;
       pctX            := OtherStorageObj.pctX;

       UserModel.Name    := OtherStorageObj.UserModel.Name;  // Connect to user written models

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

   If (Handle = 0)
   Then Begin  // init all
     p := elementList.First;
     WHILE (p <> nil) Do
     Begin
        p.Randomize(0);
        p := elementlist.Next;
     End;
   End
   ELSE Begin
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
   pGen:TStorageObj;

Begin
      pGen := ActiveCircuit.Generators.First;
      WHILE (pGen <> Nil) Do
      Begin
          pGen.ResetRegisters;
          pGen := ActiveCircuit.Generators.Next;
      End;

End;

{--------------------------------------------------------------------------}
Procedure TStorage.SampleAll;  // Force all EnergyMeters in the circuit to take a sample

VAR
   pGen:TStorageObj;

Begin

{*** Do we need storage list in Active Circuit or is class enough ?}
      pGen := ActiveCircuit.Generators.First;
      WHILE pGen<>Nil Do
      Begin
          If pGen.enabled Then pGen.TakeSample;
          pGen := ActiveCircuit.Generators.Next;
      End;
End;

//----------------------------------------------------------------------------
Constructor TStorageObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType ; // + STORAGE_ELEMENT;  // In both PCelement and Genelement list

     Nphases      := 3;
     Fnconds      := 4;  // defaults to wye
     Yorder       := 0;  // To trigger an initial allocation
     Nterms       := 1;  // forces allocations
     kWBase       := 1000.0;
     kvarBase     := 60.0;


     PFNominal    := 0.88;

     YearlyShape       := '';
     YearlyShapeObj    := nil;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
     DailyShape    := '';
     DailyShapeObj := nil;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
     DutyShape         := '';
     DutyShapeObj      := nil;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
     Connection        := 0;    // Wye (star)
//**** CHANGE THE NAME OF STORAGEMODEL
     StorageModel      := 1;  {Typical fixed kW negative load}
     StorageClass      := 1;
     LastYear          := 0;
     LastGrowthFactor  := 1.0;

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
     Fixed            := FALSE;

     
     kWhRating       := 50;
     kWhStored       := 50;
     kWhReserve      := 0.2;  // per unit of kWhRating
     pctR            := 0.0;;
     pctX            := 50.0;

     {Output rating stuff}
     kVArating  := kWBase *1.0;
     kVANotSet   := TRUE;  // Flag for default value for kVA
     
//**** ?????     UserModel  := TStoreUserModel.Create(@StoreVars) ;

     DispatchValue    := 0.0;   // Follow curves

     Reg_kWh    := 1;
     Reg_kvarh  := 2;
     Reg_MaxkW  := 3;
     Reg_MaxkVA := 4;
     Reg_Hours  := 5;
     Reg_Price  := 6;

     DebugTrace := FALSE;
     FForcedON := FALSE;
     StorageObjSwitchOpen := FALSE;

     Spectrum := '';  // override base class


     InitPropertyValues(0);

     RecalcElementData;

End;


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
       0: RandomMult := 1.0;
       GAUSSIAN: RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:  RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   End;
End;

//----------------------------------------------------------------------------
Procedure TStorageObj.CalcDailyMult(Hr:Double);

Begin
     If (DailyShapeObj <> Nil) Then
       Begin
         ShapeFactor := DailyShapeObj.GetMult(Hr);
       End
     ELSE ShapeFactor := CDOUBLEONE;  // Default to no daily variation
End;


//----------------------------------------------------------------------------
Procedure TStorageObj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil Then
       Begin
         ShapeFactor := DutyShapeObj.GetMult(Hr);
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
End;

//----------------------------------------------------------------------------
Procedure TStorageObj.CalcYearlyMult(Hr:Double);

Begin
{Yearly curve is assumed to be hourly only}
 If YearlyShapeObj<>Nil Then
      ShapeFactor := YearlyShapeObj.GetMult(Hr)
 ELSE
      ShapeFactor := CDOUBLEONE;  // Defaults to no variation

End;



//----------------------------------------------------------------------------
Procedure TStorageObj.SetNominalStorageOuput;
VAR
   Factor:Double;
   StoreON_Saved:Boolean;

Begin
   StoreON_Saved := StoreON;
   ShapeFactor := CDOUBLEONE;
    // Check to make sure the generation is ON
   With ActiveCircuit, ActiveCircuit.Solution Do
   Begin
    IF NOT (IsDynamicModel or IsHarmonicModel) THEN     // Leave generator in whatever state it was prior to entering Dynamic mode
      Begin
        StoreON := TRUE;   // Init to on then check if it should be off
        IF NOT FForcedON
        THEN CASE DispatchMode of
           LOADMODE: IF (DispatchValue > 0.0)   AND (GeneratorDispatchReference < DispatchValue)  THEN StoreON := FALSE;
           PRICEMODE:IF (DispatchValue > 0.0)   AND (PriceSignal < DispatchValue) THEN StoreON := FALSE;
        END;
      End;


    IF NOT StoreON  THEN
      Begin
         // If Generator is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
          Pnominalperphase   := -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
          Qnominalperphase := 0.0;
      End
    ELSE
      Begin    // Generator is on, compute it's nominal watts and vars
        With Solution Do
          If Fixed Then
            Begin
               Factor := 1.0;   // for fixed generators, set constant
            End
          ELSE
            Begin
              CASE Mode OF
                SNAPSHOT:     Factor := ActiveCircuit.GenMultiplier * 1.0;
                DAILYMODE:    Begin
                                Factor := ActiveCircuit.GenMultiplier  ;
                                CalcDailyMult(dblHour) // Daily dispatch curve
                              End;
                YEARLYMODE:   Begin Factor := ActiveCircuit.GenMultiplier; CalcYearlyMult(dblHour);  End;
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,
                DYNAMICMODE:  Factor := ActiveCircuit.GenMultiplier * 1.0;
                MONTECARLO2,
                MONTECARLO3,
                LOADDURATION1,
                LOADDURATION2:Begin Factor := ActiveCircuit.GenMultiplier; CalcDailyMult(dblHour); End;
                PEAKDAY:      Begin Factor := ActiveCircuit.GenMultiplier; CalcDailyMult(dblHour); End;
                DUTYCYCLE:    Begin Factor := ActiveCircuit.GenMultiplier; CalcDutyMult(dblHour) ; End;
                AUTOADDFLAG:  Factor := 1.0;
              ELSE
                Factor := 1.0
              End;
            End;

        IF NOT (IsDynamicModel or IsHarmonicModel) THEN         //******
          Begin
              Pnominalperphase   := 1000.0 * kWBase   * Factor * ShapeFactor.re / Fnphases;
              Qnominalperphase   := 1000.0 * kvarBase * Factor * ShapeFactor.im / Fnphases;
          End;
      End; {ELSE StoreON}


      IF NOT (IsDynamicModel or IsHarmonicModel) THEN  Begin       //******

          CASE StorageModel  of
//****?????
               3: // Yeq := Cinv(cmplx(0.0, -StoreVars.Xd))  ;  // Gets negated in CalcYPrim
          ELSE
//**** Probably want to do this as a fixed value of some sort
              Yeq  := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
              If   (Vminpu <> 0.0) Then Yeq95 := CDivReal(Yeq, sqr(Vminpu))  // at 95% voltage
                                   Else Yeq95 := Yeq; // Always a constant Z model

              If   (Vmaxpu <> 0.0) Then  Yeq105 := CDivReal(Yeq, Sqr(Vmaxpu))   // at 105% voltage
                                   Else  Yeq105 := Yeq;
          END;

          { When we leave here, all the Yeq's are in L-N values}


      END;
   End;  {With ActiveCircuit}

   // If generator state changes, force re-calc of Y matrix
   If StoreON <> StoreON_Saved Then Begin
     YPrimInvalid := True;
   End;
End;

//----------------------------------------------------------------------------
Procedure TStorageObj.RecalcElementData;


Begin

    VBase95  := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase := 1000.0 * kvarBase / Fnphases;

    SetNominalStorageOuput;

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    If YearlyShapeObj=Nil Then
      If Length(YearlyShape)>0 Then DoSimpleMsg('WARNING! Yearly load shape: "'+ YearlyShape +'" Not Found.', 563);
    If DailyShapeObj=Nil Then
      If Length(DailyShape)>0 Then DoSimpleMsg('WARNING! Daily load shape: "'+ DailyShape +'" Not Found.', 564);
    If DutyShapeObj=Nil Then
      If Length(DutyShape)>0 Then DoSimpleMsg('WARNING! Duty load shape: "'+ DutyShape +'" Not Found.', 565);

    SpectrumObj := SpectrumClass.Find(Spectrum);
    If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 566);

    YQFixed := -varBase / Sqr(VBase);   //10-17-02  Fixed negative sign

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
   i,j :Integer;
   FreqMultiplier :Double;

Begin

   FYprimFreq := ActiveCircuit.Solution.Frequency  ;
   FreqMultiplier := FYprimFreq / BaseFrequency;

   With  ActiveCircuit.solution  Do
   IF IsDynamicModel or IsHarmonicModel Then
     Begin
       IF StoreON Then   Y  := Yeq   // L-N value computed in initialization routines
       ELSE Y := Cmplx(EPSILON, 0.0);

       IF Connection=1 Then Y := CDivReal(Y, 3.0); // Convert to delta impedance
       Y.im := Y.im / FreqMultiplier;
       Yij := Cnegate(Y);
       FOR i := 1 to Fnphases Do
         Begin
           Case Connection of
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
           End;
         End;


     End

   ELSE Begin  //  Regular power flow generator model

       {Yeq is always expected as the equivalent line-neutral admittance}

       Y := cnegate(Yeq);  // negate for generation    Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
       Y.im           := Y.im / FreqMultiplier;

         CASE Connection OF

           0: With YMatrix Do Begin // WYE
                     Yij := Cnegate(Y);
                     FOR i := 1 to Fnphases Do Begin
                     SetElement(i, i, Y);
                     AddElement(Fnconds, Fnconds, Y);
                     SetElemsym(i, Fnconds, Yij);
                 End;

              End;
           1: With YMatrix Do Begin  // Delta  or L-L
                  Y    := CDivReal(Y, 3.0); // Convert to delta impedance
                  Yij  := Cnegate(Y);
                  FOR i := 1 to Fnphases Do Begin
                     j := i+1;
                     If j>Fnconds Then j := 1;  // wrap around for closed connections
                     AddElement(i,i, Y);
                     AddElement(j,j, Y);
                     AddElemSym(i,j, Yij);
                  End;
              End;
         End;
     End;  {ELSE IF Solution.mode}

End;


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


     // ADMITTANCE model wanted

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
                    StorageModel:0,', ',
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
//****  need to instantiate UserModel first
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

Var
   i     : Integer;
   V012,
   I012  : Array[0..2] of Complex;

Begin

   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   {Inj = -Itotal (in) - Yprim*Vtemp}
   If (StorageModel=6) and UserModel.Exists Then       // auto selects model
     Begin   {We have total currents in Itemp}
        UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
     End

   Else  {No user model, use default Thevinen equivalent}
      Begin

        Phase2SymComp(Vterminal, @V012);

        // Positive Sequence Contribution to Iterminal
        CalcVthev_Dyn;  // Update for latest phase angle

//****
        // Positive Sequence Contribution to Iterminal
   (*
        I012[1] := CDiv(Csub(V012[1], Vthev), Cmplx(0.0, Xdp));
        I012[2] := Cdiv(V012[2], Cmplx(0.0, Xdpp));
        If Connection=1 Then I012[0] := CZERO
                        Else I012[0] := Cdiv(V012[0], Cmplx(0.0, Xdpp));
   *)     SymComp2Phase(ITerminal, @I012);
   
        If Connection=0 Then ITerminal^[FnConds] := Cnegate(CmulReal(I012[0], 3.0));

      End;
   IterminalUpdated := TRUE;

    {Add it into inj current array}
   FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

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
        E := CmulReal(SpectrumObj.GetMult(StorageHarmonic), VThevHarm); // Get base harmonic magnitude
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
Procedure TStorageObj.CalcVTerminal;

{Put terminal voltages in an array}


Begin

   ComputeVTerminal;

   StorageSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TStorageObj.CalcStorageModelContribution;
// Calculates generator current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

Begin
  IterminalUpdated := FALSE;
  WITH  ActiveCircuit, ActiveCircuit.Solution DO Begin
      IF      IsDynamicModel THEN  DoDynamicMode
      ELSE IF IsHarmonicModel and (Frequency <> Fundamental) THEN  DoHarmonicMode
      ELSE  Begin
           //  compute currents and put into InjTemp array;
           CASE StorageModel OF
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
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =


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
     IF ActiveCircuit.TrapezoidalIntegration
     THEN Begin
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

      IF StoreON Then Begin
        S := cmplx(Get_PresentkW, Get_Presentkvar);
        Smag := Cabs(S);
        HourValue := 1.0;
      End
      Else Begin
         S := CZERO;
         Smag := 0.0;
         HourValue :=0.0;
      End;

      IF StoreON or ActiveCircuit.TrapezoidalIntegration THEN
      {Make sure we always integrate for Trapezoidal case
       Don't need to for Gen Off and normal integration}
      WITH ActiveCircuit.Solution Do Begin
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

      
Procedure TStorageObj.InitHarmonics;
Var
  E, Va:complex;

//****  NEED A HARMONICS ALGORITHM: CURRENT OR VOLTAGE?

begin

     YPrimInvalid   := TRUE;  // Force rebuild of YPrims
     StorageFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.


//****     Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

     {Compute reference Thevinen voltage from phase 1 current}

     IF StoreON Then
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

//****                 E := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
         Vthevharm := Cabs(E);   // establish base mag and angle
         ThetaHarm := Cang(E);
       End
     ELSE  Begin
         Vthevharm := 0.0;
         ThetaHarm := 0.0;
     End;

end;

procedure TStorageObj.InitPropertyValues(ArrayOffset: Integer);

begin

     PropertyValue[1]      := '3';     //'phases';
     PropertyValue[2]      := Getbus(1);         //'bus1';
     PropertyValue[propKV]      := Format('%-g', [kVStorageBase]);
     PropertyValue[propKW]      := Format('%-g', [kWBase]);
     PropertyValue[propPF]      := Format('%-g', [PFNominal]);
     PropertyValue[propMODEL]      := '1';
     PropertyValue[propYEARLY]      := '';
     PropertyValue[propDAILY]      := '';
     PropertyValue[propDUTY]      := '';
     PropertyValue[propDISPMODE]     := 'Default';
     PropertyValue[prop11]     := '';
     PropertyValue[propCONNECTION]     := 'wye';
     PropertyValue[propKVAR]     := Format('%-g', [Presentkvar]);

     PropertyValue[propPCTR]     := Format('%-g', [pctR]);
     PropertyValue[propPCTX]     := Format('%-g', [pctX]);

     PropertyValue[prop16]     := '';
     PropertyValue[propCLASS]     := '1'; //'class'
     PropertyValue[propDISPOUTVAL]     := '';
     PropertyValue[propDISPINVAL]     := '';
     PropertyValue[prop20]     := '';
     PropertyValue[prop21]     := '';
     PropertyValue[prop22]     := '';

     PropertyValue[propVMINPU]     := '0.90';
     PropertyValue[propVMAXPU]     := '1.10';
     PropertyValue[propFORCEDON]     := 'No';
     PropertyValue[propKVA]     := Format('%-g', [kVARating]);
     PropertyValue[propMVA]     := Format('%-g', [kVARating*0.001]);
     PropertyValue[propKWHRATED]     := Format('%-g', [kWhRating]);
     PropertyValue[propKWHSTORED]     := Format('%-g', [kWhStored]);
     PropertyValue[propPCTRESERVE]     := Format('%-g', [kWhReserve]);

     PropertyValue[propUSERMODEL]     := '';  // Usermodel
     PropertyValue[propUSERDATA]     := '';  // Userdata
     PropertyValue[propDEBUGTRACE]     := 'NO';

  inherited  InitPropertyValues(NumPropsThisClass);

end;

PROCEDURE TStorageObj.InitStateVars;
Var
    VNeut,
    Edp   :Complex;
    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;

begin
  YPrimInvalid := TRUE;  // Force rebuild of YPrims


//****     Yeq := Cinv(Cmplx(0.0, Xdp));

     {Compute nominal Positive sequence voltage behind transient reactance}

     IF StoreON Then With ActiveCircuit.Solution Do
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
         With ActiveCircuit.Solution Do If StorageModel=6 then Begin
           If UserModel.Exists Then UserModel.FInit( Vterminal, Iterminal);
         End;

       End
     ELSE  Begin
         Vthev  := cZERO;
//****  ANY OTHERS

     End;


end;

procedure TStorageObj.IntegrateStates;

Var
    TracePower:Complex;


begin
   // Compute Derivatives and then integrate

   ComputeIterminal;

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)
    With ActiveCircuit.Solution Do  Begin

      With DynaVars Do
      If (IterationFlag = 0) Then Begin {First iteration of new time step}
  //****        ThetaHistory := Theta + 0.5*h*dTheta;
  //****        SpeedHistory := Speed + 0.5*h*dSpeed;
      End;


      // Compute shaft dynamics
      TracePower := TerminalPowerIn(Vterminal,Iterminal,FnPhases) ;
  //****     dSpeed := (Pshaft + TracePower.re - D*Speed) / Mmass;
  //****     dTheta  := Speed ;

     // Trapezoidal method
      With DynaVars Do Begin
  //****      Speed := SpeedHistory + 0.5*h*dSpeed;
   //****     Theta := ThetaHistory + 0.5*h*dTheta;
      End;

      // Write Dynamics Trace Record
        IF DebugTrace Then
          Begin
             Append(TraceFile);
             Write(TraceFile,Format('t=%-.5g ',[Dynavars.t]));
             Write(TraceFile,Format(' Flag=%d ',[Dynavars.Iterationflag]));
   //****           Write(TraceFile,Format(' Speed=%-.5g ',[Speed]));
   //****           Write(TraceFile,Format(' dSpeed=%-.5g ',[dSpeed]));
   //****           Write(TraceFile,Format(' Pshaft=%-.5g ',[PShaft]));
             Write(TraceFile,Format(' P=%-.5g Q= %-.5g',[TracePower.Re, TracePower.im]));
    //****          Write(TraceFile,Format(' M=%-.5g ',[Mmass]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;

       If StorageModel=6 then Begin
         If UserModel.Exists    Then UserModel.Integrate;
       End;


   End;
end;

function TStorageObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

Var
      N, k:Integer;

begin
     N := 0;
    Result := -9999.99;  // error return value
    If i < 1 Then Exit;  // Someone goofed
(****
    With StoreVars Do
    Case i of
       1: Result := (w0+Speed)/TwoPi;  // Frequency, Hz
       2: Result := (Theta ) * RadiansToDegrees;  // Report in Deg
       3: Result := Cabs(Vthev)/vbase;      // Report in pu
       4: Result := Pshaft;
       5: Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
       6: Result := dTheta ;
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
 ****)
end;

procedure TStorageObj.Set_Variable(i: Integer;  Value: Double);
var N, k:Integer;

begin
  N := 0;
  If i<1 Then Exit;  // Someone goofed

(****
  With StoreVars Do
    Case i of
       1: Speed := (Value-w0)*TwoPi;
       2: Theta := Value/RadiansToDegrees; // deg to rad
       3: ;// meaningless to set Vd := Value * vbase; // pu to volts
       4: Pshaft := Value;
       5: dSpeed := Value / RadiansToDegrees;
       6: dTheta := Value ;
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
****)

end;

procedure TStorageObj.GetAllVariables(States: pDoubleArray);

Var  i, N:Integer;
begin
     N := 0;
     For i := 1 to NumStorageVariables Do States^[i] := Variable[i];

     If UserModel.Exists Then Begin
        N := UserModel.FNumVars;
        UserModel.FGetAllVars(@States^[NumStorageVariables+1]);
     End;

end;

function TStorageObj.NumVariables: Integer;
begin
     Result  := NumStorageVariables;
     If UserModel.Exists    then Result := Result + UserModel.FNumVars;
end;

Function TStorageObj.VariableName(i: Integer):String;
Const
    BuffSize = 255;
Var
    n,
    i2    :integer;
    Buff  :Array[0..BuffSize] of Char;
    pName :pchar;
    
begin
    n:=0;
    If i<1 Then Exit;  // Someone goofed

//**** REDEFINE VARIABLE NAMES

    Case i of
        1:Result := 'Frequency';
        2:Result := 'Theta (Deg)';
        3:Result := 'Vd';
        4:Result := 'PShaft';
        5:Result := 'dSpeed (Deg/sec)';
        6:Result := 'dTheta (Deg)';
    Else Begin
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





function TStorageObj.GetPropertyValue(Index: Integer): String;

begin

      Result := '';
      CASE Index of
      // Special handlers
         propKV:  Result := Format('%.6g', [kVStorageBase]);
         propKW:  Result := Format('%.6g', [kWBase]);
         propPF:  Result := Format('%.6g', [PFNominal]);
         propKVAR: Result := Format('%.6g', [kvarBase]);
         propKVA: Result := Format('%.6g', [kVArating]);
         propMVA: Result := Format('%.6g', [kVArating*0.001]);
         propUSERDATA: Begin
                    Result := '(' + inherited GetPropertyValue(index) + ')';
                End
      ELSE  // take the generic handler
         Result := Inherited GetPropertyValue(index);
      END;
end;

procedure TStorageObj.MakePosSequence;

Var
    S :String;
    V :Double;

begin

  S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0) Then   V :=  kVStorageBase/SQRT3
  Else V :=  kVStorageBase;

  S := S + Format(' kV=%-.5g',[V]);

  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  PF=%-.5g',[kWbase/Fnphases, PFNominal]);
      If PrpSequence^[propKVA]>0 Then S := S + Format(' kva=%-.5g  ',[kvarating/Fnphases]);
      If PrpSequence^[propMVA]>0 Then S := S + Format(' MVA=%-.5g  ',[kvarating/1000.0/Fnphases]);
  End;


  inherited;

  Parser.CmdString := S;
  Edit;

end;

procedure TStorageObj.Set_ConductorClosed(Index: Integer;
  Value: Boolean);
begin
   inherited;

 // Just turn generator on or off;

   If Value Then StorageObjSwitchOpen := FALSE Else StorageObjSwitchOpen := TRUE;

end;



procedure TStorageObj.Set_PowerFactor(const Value: Double);
begin
     PFNominal := Value;
     SyncUpPowerQuantities;
end;

procedure TStorageObj.Set_PresentkV(const Value: Double);
begin
      kVStorageBase := Value ;
      Case FNphases Of
           2,3: VBase := kVStorageBase * InvSQRT3x1000;
      Else
             VBase := kVStorageBase * 1000.0 ;
      End;
end;

procedure TStorageObj.Set_Presentkvar(const Value: Double);
Var
   kVA_Gen :Double;

begin
   kvarBase := Value;
   Qnominalperphase := 1000.0 * kvarBase  / Fnphases; // init to something reasonable
   kVA_Gen := Sqrt(Sqr(kWBase) + Sqr(kvarBase)) ;
   IF kVA_Gen <> 0.0 THEN PFNominal := kWBase / kVA_Gen ELSE PFNominal := 1.0;
   If (kWBase*kvarBase) < 0.0 Then PFNominal := -PFNominal;

end;

procedure TStorageObj.Set_PresentkW(const Value: Double);
begin

   kWBase := Value;
   SyncUpPowerQuantities;

End;

procedure TStorageObj.SyncUpPowerQuantities;
Begin

   // keep kvar nominal up to date with kW and PF
   If (PFNominal <> 0.0)  Then Begin
      kvarBase := kWBase* sqrt(1.0/Sqr(PFNominal) - 1.0);
      Qnominalperphase := 1000.0* kvarBase / Fnphases;
      If PFNominal<0.0 Then kvarBase := -kvarBase;

      If kVANotSet Then kVARating := kWBase * 1.2;

   End;

end;

procedure TStorageObj.SetDragHandRegister(Reg: Integer;
  const Value: Double);
begin
    If Value>Registers[reg] Then Registers[Reg] := Value;
end;

procedure TStorageObj.CalcVthev_Dyn;
begin
   If StorageObjSwitchOpen Then VThevMag := 0.0;
//**** WHAT HERE???   Vthev := pclx(VthevMag, Theta);
end;

initialization

   CDOUBLEONE := CMPLX(1.0, 1.0);

end.
