unit DynamicExp;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{The dynamics experssion object implements differential equations
and state variables that can be called by other objects to represent
their dynamic behavior when operating in dynamics simulation mode.

Last update by Davis Montenegro 04/13/2022
}

uses
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    Arraydef;

type

    TDynamicExp = class(TDSSClass)
    PRIVATE
        SymComponentsChanged: Boolean;
        MatrixChanged: Boolean;

        function Get_Code: String;  // Returns active line code string
        procedure Set_Code(const Value: String);  // sets the  active linecode

        procedure SetZ1Z0(i: Integer; Value: Double);
        procedure SetUnits(const s: String);  // decode units specification

        procedure DoMatrix(i: Integer; ActorID: Integer);  // set impedances as matrices


    PROTECTED
        procedure DefineProperties;
        function MakeLike(const LineName: String): Integer; OVERRIDE;
    PUBLIC

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

       // Set this property to point ActiveLineCodeObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TDynamicExpObj = class(TDSSObject)
    PRIVATE

        FNeutralConductor: Integer;

        procedure Set_NPhases(Value: Integer);
        procedure DoKronReduction;
        function get_Rmatrix: String;
        function get_Xmatrix: String;
        function get_CMatrix: String;

    PUBLIC
        NumAmpRatings,
        FNPhases: Integer;

        SymComponentsModel,
        ReduceByKron: Boolean;

        Z,         // Base Frequency Series Z matrix
        Zinv,
        YC: TCMatrix;  // Shunt capacitance matrix at Base frequency.

        BaseFrequency: Double;

        R1,
        X1,
        R0,
        X0,
        C1,
        C0,
        NormAmps,
        EmergAmps,
        FaultRate,
        PctPerm,
        HrsToRepair,
        Rg,
        Xg,
        rho: Double;
        AmpRatings: TRatingsArray;
        FLineType: Integer; // Pointer to code for type of line

        Units: Integer;  {See LineUnits}

        constructor Create(ParClass: TDSSClass; const LineCodeName: String);
        destructor Destroy; OVERRIDE;
        property NumPhases: Integer READ FNPhases WRITE Set_Nphases;
        procedure CalcMatricesFromZ1Z0;

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;

var
    DynamicExpClass: TDynamicExp;
    ActiveDynamicExpObj: TDynamicExpObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Ucomplex,
    Utilities,
    LineUnits;

const
    NumPropsThisClass = 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TDynamicExp.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'LineCode';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := true;

    DynamicExpClass := Self;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TDynamicExp.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDynamicExp.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


    PropertyName[1] := 'nvariables';
    PropertyName[2] := 'varnames';
    PropertyName[3] := 'expression';

    PropertyHelp[1] := 'Number of state variables to be considered in the differential equation.';
    PropertyHelp[2] := 'Array of strings with the names of the state variables.';
    PropertyHelp[3] := 'It is the differential expression using OpenDSS syntax for example:' + CRLF +
        'dt(w) = 1/M*(P_m - D*w - P_e)';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveDSSObject[ActiveActor] := TDynamicExpObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

function TDynamicExpObj.get_Rmatrix: String;
var
    j: Integer;
    i: Integer;
begin
    Result := '[';
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            Result := Result + Format('%12.8f ', [Z.GetElement(i, j).re]);
        end;
        if i < FNphases then
            Result := Result + '|';
    end;
    Result := Result + ']';
end;

function TDynamicExpObj.get_Xmatrix: String;
var
    j: Integer;
    i: Integer;
begin
    Result := '[';
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            Result := Result + Format('%12.8f ', [Z.GetElement(i, j).im]);
        end;
        if i < FNphases then
            Result := Result + '|';
    end;
    Result := Result + ']';
end;

function TDynamicExpObj.get_CMatrix: String;
var
    i, j: Integer;
begin
    Result := '[';
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            Result := Result + Format('%12.8f ', [Yc.GetElement(i, j).im / TwoPi / BaseFrequency * 1.0E9]);
        end;
        if i < FNphases then
            Result := Result + '|';
    end;
    Result := Result + ']';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDynamicExp.SetUnits(const s: String);
// decodes the units string and sets the Units variable

begin

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDynamicExp.SetZ1Z0(i: Integer; Value: Double);
// set symmetrical component impedances and a flag to indicate they were changed
begin


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDynamicExp.DoMatrix(i: Integer; ActorID: Integer);

var
    OrderFound, Norder, j: Integer;
    MatBuffer: pDoubleArray;
    Zvalues: pComplexArray;
    Factor: Double;

begin

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveDynamicExpObj := ElementList.Active;
    ActiveDSSObject[ActorID] := ActiveDynamicExpObj;
    SymComponentsChanged := false;
    MatrixChanged := false;

    with ActiveDynamicExpObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 101);
                1:
                    Numphases := Parser[ActorID].IntValue;  // Use property value to force reallocations
                2:
                    SetZ1Z0(1, Parser[ActorID].Dblvalue);  {R1}
                3:
                    SetZ1Z0(2, Parser[ActorID].Dblvalue);  {X0}

            else
                ClassEdit(ActiveDynamicExpObj, Parampointer - NumPropsThisClass)
            end;

            case ParamPointer of
                9..11:
                    SymComponentsModel := false;
                18:
                    if ReduceByKron and not SymComponentsModel then
                        DoKronReduction;
            end;


            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        if SymComponentsModel then
            CalcMatricesFromZ1Z0;
        if MatrixChanged then
        begin
            Zinv.Copyfrom(Z);
            Zinv.Invert;
        end;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.MakeLike(const LineName: String): Integer;
var
    OtherDynExpCode: TDynamicExpObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.Init(Handle: Integer; ActorID: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TDynamicExp.Init', -1);
    REsult := 0;
end;

function TDynamicExp.Get_Code: String;  // Returns active line code string

begin

    Result := '';

end;

procedure TDynamicExp.Set_Code(const Value: String);  // sets the  active linecode
var
    DynExpCodeObj: TDynamicExpObj;
begin

    ActiveDynamicExpObj := nil;
    DynExpCodeObj := ElementList.First;
    while DynExpCodeObj <> nil do
    begin

        if CompareText(DynExpCodeObj.Name, Value) = 0 then
        begin
            ActiveDynamicExpObj := DynExpCodeObj;
            Exit;
        end;

        DynExpCodeObj := ElementList.Next;
    end;

    DoSimpleMsg('Linecode: "' + Value + '" not Found.', 103);

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineCode Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TDynamicExpObj.Create(ParClass: TDSSClass; const LineCodeName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(LineCodeName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases

    InitPropertyValues(0);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TDynamicExpObj.Destroy;
begin
    Z.Free;
    Zinv.Free;
    Yc.Free;

    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDynamicExpObj.Set_NPhases(Value: Integer);
// Set the number of phases and reallocate phase-sensitive arrays
// Need to preserve values in Z matrices

begin
    if Value > 0 then
    begin
        if FNphases <> Value then
        begin    // If size is no different, we don't need to do anything
            FNPhases := Value;
            FNeutralConductor := FNphases;  // Init to last conductor
        // Put some reasonable values in these matrices
            CalcMatricesFromZ1Z0;  // reallocs matrices
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDynamicExpObj.CalcMatricesFromZ1Z0;
var
    Zs, Zm, Ys, Ym, Ztemp: Complex;
    i, j: Integer;
    Yc1, Yc0, OneThird: Double;

begin
    if Z <> nil then
        Z.Free;
    if Zinv <> nil then
        Zinv.Free;
    if Yc <> nil then
        Yc.Free;

    // For a line, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(FNphases);
    Zinv := TCMatrix.CreateMatrix(FNphases);
    Yc := TCMatrix.CreateMatrix(FNphases);

    OneThird := 1.0 / 3.0;  // Do this to get more precision in next few statements

    Ztemp := CmulReal(cmplx(R1, X1), 2.0);
    Zs := CmulReal(CAdd(Ztemp, Cmplx(R0, X0)), OneThird);
    Zm := CmulReal(Csub(cmplx(R0, X0), Cmplx(R1, X1)), OneThird);

    Yc1 := TwoPi * BaseFrequency * C1;
    Yc0 := TwoPi * BaseFrequency * C0;

    Ys := CMulReal(Cadd(CMulReal(Cmplx(0.0, Yc1), 2.0), Cmplx(0.0, Yc0)), OneThird);
    Ym := CmulReal(Csub(cmplx(0.0, Yc0), Cmplx(0.0, Yc1)), OneThird);

    for i := 1 to FNphases do
    begin
        Z.SetElement(i, i, Zs);
        Yc.SetElement(i, i, Ys);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
            Yc.SetElemsym(i, j, Ym);
        end;
    end;
    Zinv.Copyfrom(Z);
    Zinv.Invert;
end;

procedure TDynamicExpObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    k,
    i, j: Integer;
    TempStr: String;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin

        Writeln(F, '~ ', PropertyName^[1], '=', FNphases: 0);
        Writeln(F, '~ ', PropertyName^[2], '=', R1: 0: 5);
        Writeln(F, '~ ', PropertyName^[3], '=', X1: 0: 5);
        Writeln(F, '~ ', PropertyName^[4], '=', R0: 0: 5);
        Writeln(F, '~ ', PropertyName^[5], '=', X0: 0: 5);
        Writeln(F, '~ ', PropertyName^[6], '=', C1 * 1.0e9: 0: 5);
        Writeln(F, '~ ', PropertyName^[7], '=', C0 * 1.0e9: 0: 5);
        Writeln(F, '~ ', PropertyName^[8], '=', PropertyValue[8]);
        Write(F, '~ ', PropertyName^[9], '=', '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                Write(F, Z.GetElement(i, j).re: 0: 8, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');
        Write(F, '~ ', PropertyName^[10], '=', '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                Write(F, Z.GetElement(i, j).im: 0: 8, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');
        Write(F, '~ ', PropertyName^[11], '=', '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                Write(F, (Yc.GetElement(i, j).im / TwoPi / BaseFrequency * 1.0E9): 0: 8, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');


        for i := 12 to 21 do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

        Writeln(F, Format('~ %s=%d', [PropertyName^[22], FNeutralConductor]));
        Writeln(F, Format('~ %s=%d', [PropertyName^[25], NumAmpRatings]));
        TempStr := '[';
        for  k := 1 to NumAmpRatings do
            TempStr := TempStr + floattoStrf(AmpRatings[k - 1], ffGeneral, 8, 4) + ',';
        TempStr := TempStr + ']';
        Writeln(F, Format('~ %s=%s', [PropertyName^[26]]) + TempStr);


    end;

end;

function TDynamicExpObj.GetPropertyValue(Index: Integer): String;
var
    j: Integer;
begin
    case Index of
        1:
            Result := Format('%d', [FnPhases]);
        2:
            if SymComponentsModel then
                Result := Format('%.5g', [R1])
            else
                Result := '----';
        3:
            if SymComponentsModel then
                Result := Format('%.5g', [X1])
            else
                Result := '----';

    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TDynamicExpObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '0'; // 'nvariables';
    PropertyValue[2] := '[]'; // 'varnames';
    PropertyValue[3] := ''; // 'expression';


    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TDynamicExpObj.DoKronReduction;
var
    NewZ, NewYC: TcMatrix;

begin
    if FneutralConductor = 0 then
        Exit;   // Do Nothing

    NewZ := nil;
    NewYC := nil;

    if Fnphases > 1 then
    begin
        try
            NewZ := Z.Kron(FNeutralConductor);       // Perform Kron Reductions into temp space
        { Have to invert the Y matrix to eliminate properly}
            YC.Invert;  // Vn = 0 not In
            NewYC := YC.Kron(FNeutralConductor);
        except
            On E: Exception do
                DoSimpleMsg(Format('Kron Reduction failed: LineCode.%s. Attempting to eliminate Neutral Conductor %d.', [Name, FNeutralConductor]), 103);
        end;

        // Reallocate into smaller space   if Kron was successful

        if (NewZ <> nil) and (NewYC <> nil) then
        begin

            NewYC.Invert;  // Back to Y

            Numphases := NewZ.order;

            // Get rid of Z and YC and replace
            Z.Free;
            YC.Free;

            Z := NewZ;
            YC := NewYC;

            FNeutralConductor := 0;
            ReduceByKron := false;

            {Change Property values to reflect Kron reduction for save circuit function}
            PropertyValue[1] := Format('%d', [FnPhases]);
            PropertyValue[9] := get_Rmatrix;
            PropertyValue[10] := get_Xmatrix;
            PropertyValue[11] := get_Cmatrix;

        end
        else
        begin
            DoSimpleMsg(Format('Kron Reduction failed: LineCode.%s. Attempting to eliminate Neutral Conductor %d.', [Name, FNeutralConductor]), 103);
        end;

    end
    else
    begin
        DoSimpleMsg('Cannot perform Kron Reduction on a 1-phase LineCode: LineCode.' + Name, 103);
    end;
    ;

end;


end.
