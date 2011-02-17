unit PVsystem;

{
  ----------------------------------------------------------
  Copyright (c) 2011, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    1/28/2011 Created from Storage Model


  To Do:
    Make connection to User model
    Yprim for various modes
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
}
{
  The PVsystem element is essentially a generator that consists of a PV panel and an inverter.

  The PVsystem element can also produce or absorb vars within the kVA rating of the inverter.

}

//  The PVsystem element is assumed balanced over the no. of phases defined


interface

USES  PVsystemUserModel, DSSClass,  PCClass, PCElement, ucmatrix, ucomplex,
      LoadShape, TempShape, XYCurve, Spectrum, ArrayDef, Dynamics;

Const  NumPVSystemRegisters = 5;    // Number of energy meter registers
       NumPVSystemVariables = 4;    // No state variables that need integrating.

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TPVSystem = CLASS(TPCClass)
     private

       PROCEDURE InterpretConnection(const S:String);
       PROCEDURE SetNcondsForConnection;
     Protected
       PROCEDURE DefineProperties;
       FUNCTION MakeLike(Const OtherPVsystemObjName:STring):Integer;Override;
     public
       RegisterNames:Array[1..NumPVSystemRegisters] of String;

       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;
       FUNCTION Init(Handle:Integer):Integer; override;
       FUNCTION NewObject(const ObjName:String):Integer; override;

       PROCEDURE ResetRegistersAll;
       PROCEDURE SampleAll;
       PROCEDURE UpdateAll;

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TPVsystemObj = class(TPCElement)
      Private
        Yeq             :Complex;   // at nominal
        Yeq95           :Complex;   // at 95%
        Yeq105          :Complex;   // at 105%
        YeqIdling       :Complex;   // in shunt representing idle impedance

        DebugTrace      :Boolean;
        PVSystemSolutionCount   :Integer;
        PVSystemFundamental     :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        PVsystemObjSwitchOpen   :Boolean;
        FirstSampleAfterReset   :Boolean;

        kVArating       :Double;
        kVPVSystemBase   :Double;
        kvar_out        :Double;
        kW_out          :Double;
        Irradiance      :Double;
        kvarRequested   :Double;

        pctR            :Double;
        pctX            :Double;

        OpenPVSystemSolutionCount :Integer;
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
        UserModel       :TPVsystemUserModel;   {User-Written Models}

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
        PROCEDURE CalcPVSystemModelContribution;
        PROCEDURE CalcInjCurrentArray;
        (*PROCEDURE CalcVterminal;*)
        PROCEDURE CalcVTerminalPhase;
        PROCEDURE CalcYearlyMult(Hr:double);
        PROCEDURE CalcYPrimMatrix(Ymatrix:TcMatrix);

        PROCEDURE DoConstantPQPVsystemObj;
        PROCEDURE DoConstantZPVsystemObj;
        PROCEDURE DoDynamicMode;
        PROCEDURE DoHarmonicMode;
        PROCEDURE DoUserModel;

        PROCEDURE Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);
        PROCEDURE SetDragHandRegister(Reg:Integer; const Value:Double);
        PROCEDURE StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);

        PROCEDURE WriteTraceRecord(const s:string);

        PROCEDURE SyncUpPowerQuantities;
        // PROCEDURE SetKWandKvarOut;
        PROCEDURE UpdatePVSystem;    // Update PVSystem elements based on present kW and IntervalHrs variable

        FUNCTION Get_PresentkW:Double;
        FUNCTION Get_Presentkvar:Double;
        FUNCTION Get_PresentkV: Double;
        PROCEDURE Set_PresentkV(const Value: Double);
        PROCEDURE Set_Presentkvar(const Value: Double);
        PROCEDURE Set_PresentkW(const Value: Double);
        PROCEDURE Set_PowerFactor(const Value: Double);
        procedure Set_pctkvarOut(const Value: Double);
         procedure Set_pctkWOut(const Value: Double);

      Protected
        PROCEDURE Set_ConductorClosed(Index:Integer; Value:Boolean); Override;
        PROCEDURE GetTerminalCurrents(Curr:pComplexArray); Override ;

      public

        Connection      :Integer;  {0 = line-neutral; 1=Delta}
        DailyShape      :String;  // Daily (24 HR) PVSystem element irradiance shape
        DailyShapeObj   :TLoadShapeObj;  // Daily PVSystem element irradianceShape for this load
        DutyShape       :String;  // Duty cycle irradiance shape for changes typically less than one hour
        DutyShapeObj    :TLoadShapeObj;  // irradiance Shape for this PVSystem element
        YearlyShape     :String;  //
        YearlyShapeObj  :TLoadShapeObj;  // Yearly irradiance Shape for this PVSystem element

        DailyTShape      :String;
        DailyTShapeObj   :TLoadShapeObj;
        DutyTShape       :String;
        DutyTShapeObj    :TLoadShapeObj;
        YearlyTShape     :String;
        YearlyTShapeObj  :TLoadShapeObj;

        InverterCurve         :String;
        InverterCurveObj      :TXYCurveObj;
        Temp_PowerCurve       :String;
        Temp_PowerCurveObj    :TXYCurveObj;

        FClass   :Integer;
        VoltageModel    :Integer;   // Variation with voltage
        PFNominal       :Double;

        FpctkWout       :Double;   // percent of kW rated output currently dispatched
        Fpctkvarout     :Double;

        Registers,  Derivatives         :Array[1..NumPVSystemRegisters] of Double;

        constructor Create(ParClass :TDSSClass; const SourceName :String);
        destructor  Destroy; override;

        PROCEDURE RecalcElementData; Override;
        PROCEDURE CalcYPrim; Override;

        FUNCTION  InjCurrents:Integer; Override;
        PROCEDURE GetInjCurrents(Curr:pComplexArray); Override;
        FUNCTION  NumVariables:Integer;Override;
        PROCEDURE GetAllVariables(States:pDoubleArray);Override;
        FUNCTION  Get_Variable(i: Integer): Double; Override;
        PROCEDURE Set_Variable(i: Integer; Value: Double);  Override;
        FUNCTION  VariableName(i:Integer):String ;Override;

        PROCEDURE SetNominalPVSystemOuput;
        PROCEDURE Randomize(Opt:Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform


        PROCEDURE ResetRegisters;
        PROCEDURE TakeSample;

        // Support for Dynamics Mode
        PROCEDURE InitStateVars; Override;
        PROCEDURE IntegrateStates;Override;

        // Support for Harmonics Mode
        PROCEDURE InitHarmonics; Override;

       PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(VAR F:TextFile; Complete:Boolean);Override;
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;

       Property PresentkW    :Double  Read Get_PresentkW   Write Set_PresentkW;
       Property Presentkvar  :Double  Read Get_Presentkvar Write Set_Presentkvar;
       Property PresentkV    :Double  Read Get_PresentkV   Write Set_PresentkV;
       Property PowerFactor  :Double  Read PFNominal       Write Set_PowerFactor;

       Property PctkWOut     :Double  Read FpctkWOut       Write Set_pctkWOut;
       Property PctkVarOut   :Double  Read FpctkvarOut     Write Set_pctkvarOut;

   End;

VAR
    ActivePVsystemObj:TPVsystemObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit,  Sysutils, Command, Math, MathUtil, DSSClassDefs, DSSGlobals, Utilities;

Const

{  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit FUNCTION
    3) add a statement(s) to InitPropertyValues FUNCTION to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, If needed
 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =}

  propKV         =  3;
  propIrradiance =  4;
  propPF         =  5;
  propMODEL      =  6;
  propYEARLY     =  7;
  propDAILY      =  8;
  propDUTY       =  9;
  propTYEARLY    = 10;
  propTDAILY     = 11;
  propTDUTY      = 12;
  propCONNECTION = 13;
  propKVAR       = 14;
  propPCTR       = 15;
  propPCTX       = 16;
  propCLASS      = 17;
  propInvEffCurve= 18;
  propTemp       = 19;
  propPmpp       = 20;
  propP_T_Curve  = 21;
  propCutin      = 22;
  propCutout     = 23;
  propVMINPU     = 24;
  propVMAXPU     = 25;
  propKVA        = 26;
  propUSERMODEL  = 27;
  propUSERDATA   = 28;
  propDEBUGTRACE = 29;

  NumPropsThisClass = 29; // Make this agree with the last property constant

VAR cBuffer:Array[1..24] of Complex;  // Temp buffer for calcs  24-phase PVSystem element?
    CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TPVsystem.Create;  // Creates superstructure for all PVSystem elements
Begin
     Inherited Create;
     Class_Name := 'PVSystem';
     DSSClassType := DSSClassType + PVSYSTEM_ELEMENT;  // In both PCelement and PVSystem element list

     ActiveElement := 0;

     // Set Register names
     RegisterNames[1]  := 'kWh';
     RegisterNames[2]  := 'kvarh';
     RegisterNames[3]  := 'Max kW';
     RegisterNames[4]  := 'Max kVA';
     RegisterNames[5]  := 'Hours';

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TPVsystem.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystem.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;   {see DSSClass}

     // Define Property names
     {
      Using the AddProperty FUNCTION, you can list the properties here in the order you want
      them to appear when properties are accessed sequentially without tags.   Syntax:

      AddProperty( <name of property>, <index in the EDIT Case statement>, <help text>);

     }
     AddProperty('phases',    1,
                              'Number of Phases, this PVSystem element.  Power is evenly divided among phases.');
     AddProperty('bus1',      2,
                              'Bus to which the PVSystem element is connected.  May include specific node specification.');
     AddProperty('kv',        propKV,
                              'Nominal rated (1.0 per unit) voltage, kV, for PVSystem element. For 2- and 3-phase PVSystem elements, specify phase-phase kV. '+
                              'Otherwise, specify actual kV across each branch of the PVSystem element. '+
                              'If 1-phase wye (star or LN), specify phase-neutral kV. '+
                              'If 1-phase delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
     AddProperty('irradiance', propIrradiance,
                              'Get/set the present irradiance value in kW/sq-m. Used as base value for shape multipliers. '+
                              'Generally entered as peak value for the time period of interest and the load shape objects defined as per unit.' );
     AddProperty('pf',        propPF,
                              'Nominally, the power factor for the output power. Default is 1.0. ' +
                              'Setting this property will also set the kvar property.' +
                              'Enter negative when kW and kvar have opposite signs.'+CRLF+
                              'A positive power factor signifies that the PVSystem element produces vars ' + CRLF +
                              'as is typical for a generator.  ');
     AddProperty('conn',      propCONNECTION,
                              '={wye|LN|delta|LL}.  Default is wye.');
     AddProperty('kvar',      propKVAR,
                              'Get/set the present kvar value.  Alternative to specifying the power factor.  Side effect: '+
                              ' the power factor value is altered to agree based on present value of output kW.');
     AddProperty('kVA',       propKVA,
                              'kVA rating of inveerter. Used as the base for Dynamics mode and Harmonics mode values.');

     AddProperty('%R',        propPCTR,
                              'Equivalent percent internal resistance, ohms. Default is 0. Placed in series with internal voltage source' +
                              ' for harmonics and dynamics modes. Use a combination of %IdlekW and %EffCharge and %EffDischarge to account for ' +
                              'losses in power flow modes.');
     AddProperty('%X',        propPCTX,
                              'Equivalent percent internal reactance, ohms. Default is 50%. Placed in series with internal voltage source' +
                              ' for harmonics and dynamics modes. (Limits fault current to 2 pu.) ' +
                              'Use %Idlekvar and kvar properties to account for any reactive power during power flow solutions.');
     AddProperty('model',     propMODEL,
                              'Integer code (default=1) for the model to use for power output variation with voltage. '+
                              'Valid values are:' +CRLF+CRLF+
                              '1:PVSystem element injects a CONSTANT kW at specified power factor.'+CRLF+
                              '2:PVSystem element is modeled as a CONSTANT ADMITTANCE.'  +CRLF+
                              '3:Compute load injection from User-written Model.');

     AddProperty('Vminpu',       propVMINPU,
                                 'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
                                 'Below this value, the load model reverts to a constant impedance model.');
     AddProperty('Vmaxpu',       propVMAXPU,
                                 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
                                 'Above this value, the load model reverts to a constant impedance model.');
     AddProperty('yearly',       propYEARLY,
                                 'Dispatch shape to use for yearly simulations.  Must be previously defined '+
                                 'as a Loadshape object. If this is not specified, the Daily dispatch shape, If any, is repeated '+
                                 'during Yearly solution modes. In the default dispatch mode, ' +
                                 'the PVSystem element uses this loadshape to trigger State changes.');
     AddProperty('daily',        propDAILY,
                                 'Dispatch shape to use for daily simulations.  Must be previously defined '+
                                 'as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, '+
                                 'the PVSystem element uses this loadshape to trigger State changes.'); // daily dispatch (hourly)
     AddProperty('duty',          propDUTY,
                                 'Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
                                 'Must be previously defined as a Loadshape object. '+
                                 'Typically would have time intervals of 1-5 seconds. '+
                                 'Designate the number of points to solve using the Set Number=xxxx command. '+
                                 'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation

     AddProperty('class',       propCLASS,
                                'An arbitrary integer number representing the class of PVSystem element so that PVSystem values may '+
                                'be segregated by class.'); // integer

     AddProperty('UserModel',   propUSERMODEL,
                                'Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, ' +
                                'overriding the default model.  Set to "none" to negate previous setting.');
     AddProperty('UserData',    propUSERDATA,
                                'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
     AddProperty('debugtrace',  propDEBUGTRACE,
                                '{Yes | No }  Default is no.  Turn this on to capture the progress of the PVSystem model ' +
                                'for each iteration.  Creates a separate file for each PVSystem element named "PVSystem_name.CSV".' );



     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
     PropertyHelp[NumPropsThisClass +1] := 'Name of harmonic voltage or current spectrum for this PVSystem element. ' +
                         'Current injection is assumed for inverter. ' +
                         'Default value is "default", which is defined when the DSS starts.';

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION TPVsystem.NewObject(const ObjName:String):Integer;
Begin
    // Make a new PVSystem element and add it to PVSystem class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TPVsystemObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystem.SetNcondsForConnection;

Begin
      With ActivePVsystemObj Do
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
PROCEDURE TPVsystem.UpdateAll;
VAR
     i :Integer;
Begin
     For i := 1 to ElementList.ListSize  Do
        With TPVsystemObj(ElementList.Get(i)) Do
          If Enabled Then UpdatePVSystem;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystem.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
VAR
     TestS:String;

Begin                       
      With ActivePVsystemObj Do Begin
          TestS := lowercase(S);
          CASE TestS[1] OF
            'y','w': Connection := 0;  {Wye}
            'd': Connection := 1;  {Delta or line-Line}
            'l': CASE Tests[2] OF
                 'n': Connection := 0;
                 'l': Connection := 1;
                 END;
          END;

          SetNCondsForConnection;

          {VBase is always L-N voltage unless 1-phase device or more than 3 phases}

          CASE Fnphases Of
               2,3: VBase := kVPVSystemBase * InvSQRT3x1000;    // L-N Volts
          ELSE
               VBase := kVPVSystemBase * 1000.0 ;   // Just use what is supplied
          END;

          VBase95  := Vminpu * VBase;
          VBase105 := Vmaxpu * VBase;

          Yorder := Fnconds * Fnterms;
          YPrimInvalid := True;
      End;
End;


//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -

FUNCTION TPVsystem.Edit:Integer;

VAR
       i, iCase,
       ParamPointer:Integer;
       ParamName:String;
       Param:String;

Begin

  // continue parsing with contents of Parser
  ActivePVsystemObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActivePVsystemObj;

  Result := 0;

  With ActivePVsystemObj Do
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
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for PVSystem "'+Name+'"', 560);

         If ParamPointer > 0 Then
         Begin
             iCase := PropertyIdxMap[ParamPointer];
             CASE iCASE OF
                0               : DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
                1               : NPhases    := Parser.Intvalue; // num phases
                2               : SetBus(1, param);
               propKV           : PresentkV    := Parser.DblValue;
               propIrradiance   : Irradiance   := Parser.DblValue;
               propPF           : PFNominal    := Parser.DblValue;
               propMODEL        : VoltageModel := Parser.IntValue;
               propYEARLY       : YearlyShape  := Param;
               propDAILY        : DailyShape  := Param;
               propDUTY         : DutyShape     := Param;
               propCONNECTION   : InterpretConnection(Param);
               propKVAR         : Presentkvar   := Parser.DblValue;
               propPCTR         : pctR          := Parser.DblValue;
               propPCTX         : pctX          := Parser.DblValue;
               propCLASS        : FClass       := Parser.IntValue;
               propVMINPU       : VMinPu       := Parser.DblValue;
               propVMAXPU       : VMaxPu       := Parser.DblValue;
               propKVA          : kVArating    := Parser.DblValue;
               propUSERMODEL    : UserModel.Name := Parser.StrValue;  // Connect to user written models
               propUSERDATA     : UserModel.Edit := Parser.StrValue;  // Send edit string to user model
               propDEBUGTRACE   : DebugTrace   := InterpretYesNo(Param);


             ELSE
               // Inherited parameters
                 ClassEdit(ActivePVsystemObj, ParamPointer - NumPropsThisClass)
             END;

             CASE iCase OF
                1: SetNcondsForConnection;  // Force Reallocation of terminal info
                propPF: SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF

        {Set loadshape objects;  returns nil If not valid}
                propYEARLY: YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                propDAILY:  DailyShapeObj  := LoadShapeClass.Find(DailyShape);
                propDUTY:   DutyShapeObj   := LoadShapeClass.Find(DutyShape);

                propTYEARLY: YearlyTShapeObj := TShapeClass.Find(YearlyTShape);
                propTDAILY:  DailyTShapeObj  := TShapeClass.Find(DailyTShape);
                propTDUTY:   DutyTShapeObj   := TShapeClass.Find(DutyTShape);

                propDEBUGTRACE: IF DebugTrace THEN Begin   // Init trace file
                       AssignFile(TraceFile, DSSDataDirectory + 'STOR_'+Name+'.CSV');
                       ReWrite(TraceFile);
                       Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, PVSystemModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                       For i := 1 to nphases Do Write(Tracefile,  ', |Iinj'+IntToStr(i)+'|');
                       For i := 1 to nphases Do Write(Tracefile,  ', |Iterm'+IntToStr(i)+'|');
                       For i := 1 to nphases Do Write(Tracefile,  ', |Vterm'+IntToStr(i)+'|');
                       Write(TraceFile, ',Vthev, Theta');
                       Writeln(TraceFile);
                       CloseFile(Tracefile);
                 End;

             END;
         End;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
FUNCTION TPVsystem.MakeLike(Const OtherPVsystemObjName:String):Integer;

// Copy over essential properties from other object

VAR
     OtherPVsystemObj:TPVsystemObj;
     i:Integer;
Begin
     Result := 0;
     {See If we can find this line name in the present collection}
     OtherPVsystemObj := Find(OtherPVsystemObjName);
     If   (OtherPVsystemObj <> Nil)
     Then With ActivePVsystemObj Do
     Begin
         If (Fnphases <> OtherPVsystemObj.Fnphases) Then Begin
           Nphases := OtherPVsystemObj.Fnphases;
           NConds := Fnphases;  // Forces reallocation of terminal stuff
           Yorder := Fnconds*Fnterms;
           YPrimInvalid := True;
         End;

         kVPVSystemBase := OtherPVsystemObj.kVPVSystemBase;
         Vbase          := OtherPVsystemObj.Vbase;
         Vminpu         := OtherPVsystemObj.Vminpu;
         Vmaxpu         := OtherPVsystemObj.Vmaxpu;
         Vbase95        := OtherPVsystemObj.Vbase95;
         Vbase105       := OtherPVsystemObj.Vbase105;
         kW_out         := OtherPVsystemObj.kW_out;
         kvar_out       := OtherPVsystemObj.kvar_out;
         Pnominalperphase   := OtherPVsystemObj.Pnominalperphase;
         PFNominal      := OtherPVsystemObj.PFNominal;
         Qnominalperphase   := OtherPVsystemObj.Qnominalperphase;
         Connection     := OtherPVsystemObj.Connection;
         YearlyShape    := OtherPVsystemObj.YearlyShape;
         YearlyShapeObj := OtherPVsystemObj.YearlyShapeObj;
         DailyShape     := OtherPVsystemObj.DailyShape;
         DailyShapeObj  := OtherPVsystemObj.DailyShapeObj;
         DutyShape      := OtherPVsystemObj.DutyShape;
         DutyShapeObj   := OtherPVsystemObj.DutyShapeObj;
         YearlyTShape    := OtherPVsystemObj.YearlyTShape;
         YearlyTShapeObj := OtherPVsystemObj.YearlyTShapeObj;
         DailyTShape     := OtherPVsystemObj.DailyTShape;
         DailyTShapeObj  := OtherPVsystemObj.DailyTShapeObj;
         DutyTShape      := OtherPVsystemObj.DutyTShape;
         DutyTShapeObj   := OtherPVsystemObj.DutyTShapeObj;
         FClass   := OtherPVsystemObj.FClass;
         VoltageModel   := OtherPVsystemObj.VoltageModel;

         kVArating      := OtherPVsystemObj.kVArating;

         pctkWout        := OtherPVsystemObj.pctkWout;

         pctR            := OtherPVsystemObj.pctR;
         pctX            := OtherPVsystemObj.pctX;

         RandomMult      :=  OtherPVsystemObj.RandomMult;

         UserModel.Name   := OtherPVsystemObj.UserModel.Name;  // Connect to user written models

         ClassMakeLike(OtherPVsystemObj);

         For i := 1 to ParentClass.NumProperties Do
             FPropertyValue^[i] := OtherPVsystemObj.FPropertyValue^[i];

         Result := 1;
     End
     ELSE  DoSimpleMsg('Error in PVSystem MakeLike: "' + OtherPVsystemObjName + '" Not Found.', 562);

End;

//----------------------------------------------------------------------------
FUNCTION TPVsystem.Init(Handle:Integer):Integer;
VAR
   p:TPVsystemObj;

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

     DoSimpleMsg('Need to implement TPVsystem.Init', -1);
     Result := 0;
End;

{--------------------------------------------------------------------------}
PROCEDURE TPVsystem.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

VAR
      idx  :Integer;

Begin
      idx := First;
      WHILE idx > 0 Do
      Begin
           TPVsystemObj(GetActiveObj).ResetRegisters;
           idx := Next;
      End;
End;

{--------------------------------------------------------------------------}
PROCEDURE TPVsystem.SampleAll;  // Force all EnergyMeters in the circuit to take a sample

VAR
      i :Integer;
Begin
      For i := 1 to ElementList.ListSize  Do
        With TPVsystemObj(ElementList.Get(i)) Do
          If Enabled Then TakeSample;
End;

//----------------------------------------------------------------------------
Constructor TPVsystemObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin

     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType ; // + PVSystem_ELEMENT;  // In both PCelement and PVSystemelement list

     Nphases    := 3;
     Fnconds    := 4;  // defaults to wye
     Yorder     := 0;  // To trigger an initial allocation
     Nterms     := 1;  // forces allocations

     YearlyShape       := '';
     YearlyShapeObj    := nil;  // If YearlyShapeobj = nil Then the load alway stays nominal * global multipliers
     DailyShape        := '';
     DailyShapeObj     := nil;  // If DaillyShapeobj = nil Then the load alway stays nominal * global multipliers
     DutyShape         := '';
     DutyShapeObj      := nil;  // If DutyShapeobj = nil Then the load alway stays nominal * global multipliers
     YearlyTShape       := '';
     YearlyTShapeObj    := nil;  // If YearlyShapeobj = nil Then the load alway stays nominal * global multipliers
     DailyTShape        := '';
     DailyTShapeObj     := nil;  // If DaillyShapeobj = nil Then the load alway stays nominal * global multipliers
     DutyTShape         := '';
     DutyTShapeObj      := nil;  // If DutyShapeobj = nil Then the load alway stays nominal * global multipliers
     Connection        := 0;    // Wye (star)
     VoltageModel      := 1;  {Typical fixed kW negative load}
     FClass      := 1;

     PVSystemSolutionCount     := -1;  // For keep track of the present solution in Injcurrent calcs
     OpenPVSystemSolutionCount := -1;
     YPrimOpenCond            := nil;

     kVPVSystemBase    := 12.47;
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
     kVArating    := 25.0;

     pctR            := 0.0;;
     pctX            := 50.0;

     FpctkWout        := 100.0;
     Fpctkvarout      := 100.0;


     // kVANotSet    := TRUE;  // Flag to set the default value for kVA

     UserModel  := TPVsystemUserModel.Create;

     Reg_kWh    := 1;
     Reg_kvarh  := 2;
     Reg_MaxkW  := 3;
     Reg_MaxkVA := 4;
     Reg_Hours  := 5;
     Reg_Price  := 6;

     DebugTrace := FALSE;
     PVsystemObjSwitchOpen := FALSE;
     Spectrum := '';  // override base class
     SpectrumObj := nil;

     InitPropertyValues(0);
     RecalcElementData;

End;



//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.InitPropertyValues(ArrayOffset: Integer);

// Define default values for the properties

Begin

     PropertyValue[1]      := '3';         //'phases';
     PropertyValue[2]      := Getbus(1);   //'bus1';

     PropertyValue[propKV]      := Format('%-g', [kVPVSystemBase]);
     PropertyValue[propIrradiance]      := Format('%-g', [Irradiance]);
     PropertyValue[propPF]      := Format('%-g', [PFNominal]);
     PropertyValue[propMODEL]     := '1';
     PropertyValue[propYEARLY]    := '';
     PropertyValue[propDAILY]     := '';
     PropertyValue[propDUTY]      := '';
     PropertyValue[propCONNECTION]:= 'wye';
     PropertyValue[propKVAR]      := Format('%-g', [Presentkvar]);

     PropertyValue[propPCTR]      := Format('%-g', [pctR]);
     PropertyValue[propPCTX]      := Format('%-g', [pctX]);

     PropertyValue[propCLASS]     := '1'; //'class'

     PropertyValue[propVMINPU]    := '0.90';
     PropertyValue[propVMAXPU]    := '1.10';
     PropertyValue[propKVA]       := Format('%-g', [kVARating]);

     PropertyValue[propUSERMODEL] := '';  // Usermodel
     PropertyValue[propUSERDATA]  := '';  // Userdata
     PropertyValue[propDEBUGTRACE]:= 'NO';

  inherited  InitPropertyValues(NumPropsThisClass);

End;


//----------------------------------------------------------------------------
FUNCTION TPVsystemObj.GetPropertyValue(Index: Integer): String;

Begin

      Result := '';
      CASE Index of
          propKV         : Result := Format('%.6g', [kVPVSystemBase]);
          propIrradiance : Result := Format('%.6g', [Irradiance]);
          propPF         : Result := Format('%.6g', [PFNominal]);
          propMODEL      : Result := Format('%d',   [VoltageModel]);
          propYEARLY     : Result := YearlyShape;
          propDAILY      : Result := DailyShape;
          propDUTY       : Result := DutyShape;

          propTYEARLY     : Result := YearlyTShape;
          propTDAILY      : Result := DailyTShape;
          propTDUTY       : Result := DutyTShape;

          {propCONNECTION :;}
          propKVAR       : Result := Format('%.6g', [kvar_out]);
          propPCTR       : Result := Format('%.6g', [pctR]);
          propPCTX       : Result := Format('%.6g', [pctX]);
          {propCLASS      = 17;}
          propVMINPU     : Result := Format('%.6g', [VMinPu]);
          propVMAXPU     : Result := Format('%.6g', [VMaxPu]);
          propKVA        : Result := Format('%.6g', [kVArating]);

          propUSERMODEL  : Result := UserModel.Name;
          propUSERDATA   : Result := '(' + inherited GetPropertyValue(index) + ')';
          {propDEBUGTRACE = 33;}
      ELSE  // take the generic handler
           Result := Inherited GetPropertyValue(index);
      END;
End;

//----------------------------------------------------------------------------
Destructor TPVsystemObj.Destroy;
Begin
      YPrimOpenCond.Free;
      UserModel.Free;
      Inherited Destroy;
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.Randomize(Opt:Integer);
Begin

   CASE Opt OF
       0:         RandomMult := 1.0;
       GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:   RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   END;

End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.CalcDailyMult(Hr:Double);

Begin
     If (DailyShapeObj <> Nil) Then
       Begin
            ShapeFactor := DailyShapeObj.GetMult(Hr);
       End
     ELSE ShapeFactor := CDOUBLEONE;  // Default to no  variation

End;


//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil Then
       Begin
             ShapeFactor := DutyShapeObj.GetMult(Hr);
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.CalcYearlyMult(Hr:Double);

Begin
     If YearlyShapeObj<>Nil Then
       Begin
            ShapeFactor := YearlyShapeObj.GetMult(Hr) ;
       End
     ELSE CalcDailyMult(Hr);  // Defaults to Daily curve
End;



//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.SetNominalPVSystemOuput;

Begin

   ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    // Check to make sure the PVSystem element is ON
   With ActiveCircuit, ActiveCircuit.Solution Do
   Begin
    IF NOT (IsDynamicModel or IsHarmonicModel) THEN     // Leave PVSystem element in whatever state it was prior to entering Dynamic mode
    Begin
          // Check dispatch to see what state the PVSystem element should be in

           With Solution Do
            CASE Mode OF
                SNAPSHOT:    ; {Just solve for the present kW, kvar}  // Don't check for state change
                DAILYMODE:    CalcDailyMult(dblHour); // Daily dispatch curve
                YEARLYMODE:   CalcYearlyMult(dblHour);
             (*
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,
                DYNAMICMODE:   ; // {do nothing yet}   *)

                // Assume Daily curve, If any, for the following
                MONTECARLO2,
                MONTECARLO3,
                LOADDURATION1,
                LOADDURATION2: CalcDailyMult(dblHour);
                PEAKDAY:       CalcDailyMult(dblHour);

                DUTYCYCLE:     CalcDutyMult(dblHour) ;
                {AUTOADDFLAG:  ; }
            END;

          Pnominalperphase   := 1000.0 * kW_out    / Fnphases;
          Qnominalperphase   := 1000.0 * kvar_out  / Fnphases;

          CASE VoltageModel  of
//****  Fix this when user model gets connected in
               3: // Yeq := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim
          ELSE
              Yeq  := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
              If   (Vminpu <> 0.0) Then Yeq95 := CDivReal(Yeq, SQR(Vminpu))  // at 95% voltage
                                   Else Yeq95 := Yeq; // Always a constant Z model

              If   (Vmaxpu <> 0.0) Then  Yeq105 := CDivReal(Yeq, SQR(Vmaxpu))   // at 105% voltage
                                   Else  Yeq105 := Yeq;
          END;
              { When we leave here, all the Yeq's are in L-N values}

     End;  {If  NOT (IsDynamicModel or IsHarmonicModel)}
   End;  {With ActiveCircuit}


End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.RecalcElementData;

Begin

    VBase95  := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase := 1000.0 * kvar_out / Fnphases;

    // values in ohms for thevenin equivalents
    RThev := pctR * 0.01 * SQR(PresentkV)/kVARating * 1000.0;
    XThev := pctX * 0.01 * SQR(PresentkV)/kVARating * 1000.0;


    SetNominalPVSystemOuput;

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    If YearlyShapeObj=Nil Then
      If Length(YearlyShape)>0 Then DoSimpleMsg('WARNING! Yearly load shape: "'+ YearlyShape +'" Not Found.', 563);
    If DailyShapeObj=Nil Then
      If Length(DailyShape)>0 Then DoSimpleMsg('WARNING! Daily load shape: "'+ DailyShape +'" Not Found.', 564);
    If DutyShapeObj=Nil Then
      If Length(DutyShape)>0 Then DoSimpleMsg('WARNING! Duty load shape: "'+ DutyShape +'" Not Found.', 565);

    If Length(Spectrum)> 0 Then Begin
          SpectrumObj := SpectrumClass.Find(Spectrum);
          If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 566);
    End
    Else SpectrumObj := Nil;

    // Initialize to Zero - defaults to PQ PVSystem element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    {Update any user-written models}
    If Usermodel.Exists  Then UserModel.FUpdateModel;

End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.CalcYPrimMatrix(Ymatrix:TcMatrix);

VAR
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
           Y  := Yeq;   // L-N value computed in initialization routines

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
     Begin  //  Regular power flow PVSystem element model

       {Yeq is always expected as the equivalent line-neutral admittance}


       Y := Cadd(cnegate(Yeq), YeqIdling);   // negate for generation    Yeq is L-N quantity

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
PROCEDURE TPVsystemObj.CalcYPrim;

VAR
        i:integer;

Begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV Does not fail
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

     SetNominalPVSystemOuput;
     CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
     Inherited CalcYPrim;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);
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
PROCEDURE TPVsystemObj.WriteTraceRecord(const s:string);

VAR i:Integer;

Begin

      Try
      If (Not InshowResults) Then
      Begin
           Append(TraceFile);
           Write(TraceFile,Format('%-.g, %d, %-.g, ',
                    [ActiveCircuit.Solution.DynaVARs.t,
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

           Writeln(TRacefile);
           CloseFile(TraceFile);
      End;
      Except
            On E:Exception Do Begin End;

      End;
End;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.DoConstantPQPVsystemObj;

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
PROCEDURE TPVsystemObj.DoConstantZPVsystemObj;

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
PROCEDURE TPVsystemObj.DoUserModel;
{Compute total terminal Current from User-written model}
VAR
   i:Integer;

Begin

   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   If UserModel.Exists Then    // Check automatically selects the usermodel If true
     Begin
         UserModel.FCalc (Vterminal, Iterminal);
         IterminalUpdated := TRUE;
         With ActiveCircuit.Solution Do  Begin          // Negate currents from user model for power flow PVSystem element model
               FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
         End;
     End
   Else
     Begin
        DoSimpleMsg('PVSystem.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
     End;

End;



// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.DoDynamicMode;

{Compute Total Current and add into InjTemp}
{
   For now, just assume the PVSystem element is constant power
   for the duration of the dynamic simulation.
}


Begin

  DoConstantPQPVsystemObj;

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.DoHarmonicMode;

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

VAR
   i     :Integer;
   E     :Complex;
   PVSystemHarmonic :double;

Begin

   ComputeVterminal;

   WITH ActiveCircuit.Solution Do
     Begin
        PVSystemHarmonic := Frequency/PVSystemFundamental;
        If SpectrumObj <> Nil Then
             E := CmulReal(SpectrumObj.GetMult(PVSystemHarmonic), VThevHarm) // Get base harmonic magnitude
        Else E := CZERO;

        RotatePhasorRad(E, PVSystemHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift
        FOR i := 1 to Fnphases DO Begin
           cBuffer[i] := E;
           If i < Fnphases Then RotatePhasorDeg(E, PVSystemHarmonic, -120.0);  // Assume 3-phase PVSystem element
        End;
     END;

   {Handle Wye Connection}
   IF Connection=0 THEN cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
   YPrim.MVMult(InjCurrent,@cBuffer);

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.CalcVTerminalPhase;

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

   PVSystemSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
(*
PROCEDURE TPVsystemObj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   PVSystemSolutionCount := ActiveCircuit.Solution.SolutionCount;
End;
*)


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.CalcPVSystemModelContribution;

// Calculates PVSystem element current and adds it properly into the injcurrent array
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
                      1: DoConstantPQPVsystemObj;
                      2: DoConstantZPVsystemObj;
                      3: DoUserModel;
                 ELSE
                      DoConstantPQPVsystemObj;  // for now, until we implement the other models.
                 End;
            End; {ELSE}
     END; {WITH}

   {When this is Done, ITerminal is up to date}

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.CalcInjCurrentArray;
// Difference between currents in YPrim and total current
Begin
      // Now Get Injection Currents
       If PVsystemObjSwitchOpen Then ZeroInjCurrent
       Else CalcPVSystemModelContribution;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.GetTerminalCurrents(Curr:pComplexArray);

// Compute total Currents

Begin
   WITH ActiveCircuit.Solution  DO
     Begin
        If IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount Then Begin     // recalc the contribution
          IF Not PVsystemObjSwitchOpen Then CalcPVSystemModelContribution;  // Adds totals in Iterminal as a side effect
        End;
        Inherited GetTerminalCurrents(Curr);
     End;

   If (DebugTrace) Then WriteTraceRecord('TotalCurrent');

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TPVsystemObj.InjCurrents:Integer;

Begin
     With ActiveCircuit.Solution Do
      Begin
         If LoadsNeedUpdating Then SetNominalPVSystemOuput; // Set the nominal kW, etc for the type of solution being Done

         CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

         If (DebugTrace) Then WriteTraceRecord('Injection');

         // Add into System Injection Current Array

         Result := Inherited InjCurrents;
      End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.GetInjCurrents(Curr:pComplexArray);

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
        DoErrorMsg('PVSystem Object: "' + Name + '" in GetInjCurrents FUNCTION.',
                    E.Message,
                   'Current buffer not big enough.', 568);
   End;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.ResetRegisters;

VAR
   i : Integer;

Begin
     For i := 1 to NumPVSystemRegisters Do Registers[i]   := 0.0;
     For i := 1 to NumPVSystemRegisters Do Derivatives[i] := 0.0;
     FirstSampleAfterReset := True;  // initialize for trapezoidal integration
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double);

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
PROCEDURE TPVsystemObj.TakeSample;
// Update Energy from metered zone

VAR
     S         :Complex;
     Smag      :double;
     HourValue :Double;

Begin

// Compute energy in PVSystem element branch
     IF  Enabled  THEN Begin

          S := cmplx(Get_PresentkW, Get_Presentkvar);
          Smag := Cabs(S);
          HourValue := 1.0;

        IF  ActiveCircuit.TrapezoidalIntegration THEN
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
PROCEDURE TPVsystemObj.UpdatePVSystem;
{Update PVSystem levels}
Begin

    { Do Nothing}

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TPVsystemObj.Get_PresentkW:Double;
Begin
     Result := Pnominalperphase * 0.001 * Fnphases;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
FUNCTION TPVsystemObj.Get_PresentkV: Double;
Begin
     Result := kVPVSystemBase;
End;

FUNCTION TPVsystemObj.Get_Presentkvar:Double;
Begin
     Result := Qnominalperphase * 0.001 * Fnphases;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPVsystemObj.DumpProperties(VAR F:TextFile; Complete:Boolean);

VAR
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
PROCEDURE TPVsystemObj.InitHarmonics;

// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X

VAR
  E, Va:complex;

Begin
     YPrimInvalid       := TRUE;  // Force rebuild of YPrims
     PVSystemFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.

     Yeq := Cinv(Cmplx(RThev, XThev));      // used for current calcs  Always L-N

     {Compute reference Thevinen voltage from phase 1 current}

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

End;


//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.InitStateVars;

// for going into dynamics mode
(*
VAR
    VNeut,
    Edp   :Complex;
    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;
*)
Begin
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
             With ActiveCircuit.Solution Do If VoltageModel=6 Then Begin
               If UserModel.Exists Then UserModel.FInit( Vterminal, Iterminal);
             End;
       End
     ELSE  Begin
         Vthev  := cZERO;
//****  ANY OTHERS
     End;
*)
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.IntegrateStates;

// dynamics mode integration routine

(****
VAR
    TracePower:Complex;

****)
Begin
   // Compute Derivatives and Then integrate

(*    For now, Do nothing; no state vars to integrate

   ComputeIterminal;

// Check for user-written exciter model.
    //FUNCTION(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)
    With ActiveCircuit.Solution Do  Begin

      With DynaVARs Do
      If (IterationFlag = 0) Then Begin {First iteration of new time step}
          ThetaHistory := Theta + 0.5*h*dTheta;
          SpeedHistory := Speed + 0.5*h*dSpeed;
      End;


      // Compute shaft dynamics
      TracePower := TerminalPowerIn(Vterminal,Iterminal,FnPhases) ;
      dSpeed := (Pshaft + TracePower.re - D*Speed) / Mmass;
       dTheta  := Speed ;

     // Trapezoidal method
      With DynaVARs Do Begin
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

       If VoltageModel=6 Then Begin
         If UserModel.Exists    Then UserModel.Integrate;
       End;


   End;
*)

End;



//----------------------------------------------------------------------------
FUNCTION TPVsystemObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

VAR
      N, k:Integer;

Begin
    Result := -9999.99;  // error return value; no state fars
    If i < 1 Then Exit;
// for now, report kWhstored and mode
    CASE i of
       1: Result := -1.0;      // NEED TO DEFINE THESE
       2: Result := -1.0;
       3: Result := pctkWout;
       4: Result := -1.0;
     ELSE
        Begin
             If UserModel.Exists Then
             Begin
                  N := UserModel.FNumVars;
                  k := (i-NumPVSystemVariables);
                  If k <= N Then Begin
                      Result := UserModel.FGetVariable(k);
                      Exit;
                  End;
             End;
        End;
     END;
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.Set_Variable(i: Integer;  Value: Double);
var N, k:Integer;

Begin
  If i<1 Then Exit;  // No variables to set

    CASE i of
       1: {kWhStored := Value};
       2: ; // DEFINE SOMETHING FOR THIS
       3: pctkWout  := Value;
       4: {pctkWin   := Value};
     ELSE
       Begin
         If UserModel.Exists Then
         Begin
              N := UserModel.FNumVars;
              k := (i-NumPVSystemVariables) ;
              If  k<= N Then Begin
                  UserModel.FSetVariable( k, Value );
                  Exit;
              End;
          End;
       End;
     END;

End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.GetAllVariables(States: pDoubleArray);

VAR  i{, N}:Integer;
Begin
     For i := 1 to NumPVSystemVariables Do States^[i] := Variable[i];

     If UserModel.Exists Then Begin
        {N := UserModel.FNumVars;}
        UserModel.FGetAllVars(@States^[NumPVSystemVariables+1]);
     End;

End;

//----------------------------------------------------------------------------
FUNCTION TPVsystemObj.NumVariables: Integer;
Begin
     Result  := NumPVSystemVariables;
     If UserModel.Exists    Then Result := Result + UserModel.FNumVars;
End;

//----------------------------------------------------------------------------
FUNCTION TPVsystemObj.VariableName(i: Integer):String;

Const
    BuffSize = 255;
VAR
    n,
    i2    :integer;
    Buff  :Array[0..BuffSize] of AnsiChar;
    pName :pAnsichar;

Begin
      If i<1 Then Exit;  // Someone goofed

      CASE i of
          1:Result := 'undefined';
          2:Result := 'UNDEFINED';
          3:Result := '% POWER OUTPUT';
          4:Result := 'UNDEFINED';
      ELSE
          Begin
            If UserModel.Exists Then
            Begin
                  pName := @Buff;
                  n := UserModel.FNumVars;
                  i2 := i-NumPVSystemVariables;
                  If i2 <= n Then
                  Begin
                       UserModel.FGetVarName(i2, pName, BuffSize);
                       Result := String(pName);
                       Exit;
                  End;
            End;
          End;
      END;

End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.MakePosSequence;

VAR
    S :String;
    V :Double;

Begin

  S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0)
    Then V :=  kVPVSystemBase/SQRT3
    Else V :=  kVPVSystemBase;

  S := S + Format(' kV=%-.5g',[V]);

  If Fnphases>1 Then
  Begin
       S := S + Format(' kva=%-.5g  PF=%-.5g',[kVArating/Fnphases, PFNominal]);
  End;

  Parser.CmdString := S;
  Edit;

  inherited;   // write out other properties
End;

PROCEDURE TPVsystemObj.Set_ConductorClosed(Index: Integer;
  Value: Boolean);
Begin
   inherited;

 // Just turn PVSystem element on or off;

   If Value Then PVsystemObjSwitchOpen := FALSE Else PVsystemObjSwitchOpen := TRUE;

End;

procedure TPVsystemObj.Set_pctkvarOut(const Value: Double);
begin
     FpctkvarOut := Value;
   // Force recompute of target PF and requested kVAr
     Presentkvar := kVARating * sqrt(1.0/SQR(PFNominal) - 1.0) * FpctkvarOut  / 100.0;
end;

procedure TPVsystemObj.Set_pctkWOut(const Value: Double);
begin
     FpctkWOut := Value;
     kW_Out    := FpctkWOut * kVARating / 100.0;
end;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.Set_PowerFactor(const Value: Double);
Begin
     PFNominal := Value;
     SyncUpPowerQuantities;
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.Set_PresentkV(const Value: Double);
Begin
      kVPVSystemBase := Value ;
      CASE FNphases Of
           2,3: VBase := kVPVSystemBase * InvSQRT3x1000;
      ELSE
           VBase := kVPVSystemBase * 1000.0 ;
      END;
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.Set_Presentkvar(const Value: Double);
VAR
     kVA_Gen :Double;
Begin
     kvar_out := Value;
     kvarRequested := Value;
     {Requested kVA output}
     kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out)) ;
     If kVA_Gen > kVArating Then kVA_Gen := kVARating;  // Limit kVA to rated value
     IF kVA_Gen <> 0.0 THEN PFNominal := kW_out / kVA_Gen ELSE PFNominal := 1.0;
     If (kW_out*kvar_out) < 0.0 Then PFNominal := -PFNominal;
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.Set_PresentkW(const Value: Double);
Begin
     FpctkWOut := Value/kVARating * 100.0;
     kW_Out   := Value;
     //SyncUpPowerQuantities;
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.SyncUpPowerQuantities;
Begin
     // keep kvar nominal up to date with kW and PF
     If (PFNominal <> 0.0)  Then
     Begin
          kvar_out := kW_out* sqrt(1.0/Sqr(PFNominal) - 1.0);
          Qnominalperphase := 1000.0* kvar_out / Fnphases;
          If PFNominal<0.0 Then kvar_out := -kvar_out;
          // If kVANotSet Then kVARating := kWrating;
     End;
End;

//----------------------------------------------------------------------------
PROCEDURE TPVsystemObj.SetDragHandRegister(Reg: Integer; const Value: Double);
Begin
    If Value>Registers[reg] Then Registers[Reg] := Value;
End;


//----------------------------------------------------------------------------

initialization

   CDOUBLEONE := CMPLX(1.0, 1.0);

end.
