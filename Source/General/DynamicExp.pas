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
    Arraydef,
    Classes;

type

    TDynamicExp = class(TDSSClass)
    PRIVATE
        SymComponentsChanged: Boolean;
        MatrixChanged: Boolean;

        function Get_Code: String;  // Returns active line code string
        procedure Set_Code(const Value: String);  // sets the  active linecode

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

        FNumvars: Integer;
        FvarValues: array of Double;
        FvarInitValues: array of Double;
        FvarNames: TStringList;
        FvarIOType: array of Integer;

    PUBLIC
        BaseFrequency: Double;
        FActiveVar: String;

        constructor Create(ParClass: TDSSClass; const LineCodeName: String);
        destructor Destroy; OVERRIDE;

        function InterpretDiffEq(Exp: String): Boolean;
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
    NumPropsThisClass = 6;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TDynamicExp.Create;  // Creates superstructure for all DynamicExp objects
begin
    inherited Create;
    Class_Name := 'DynamicExp';
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
    PropertyName[3] := 'varinit';
    PropertyName[4] := 'var';
    PropertyName[5] := 'varvalue';
    PropertyName[6] := 'expression';

    PropertyHelp[1] := '(Int) Number of state variables to be considered in the differential equation.';
    PropertyHelp[2] := '([String]) Array of strings with the names of the state variables.';
    PropertyHelp[3] := '([dbl]) Array of doubles indicating the intial values of state variables.';
    PropertyHelp[4] := '(String) Activates the state variable using the given name.';
    PropertyHelp[5] := '(dbl) Floating point number indicating the value of the active state variable.';
    PropertyHelp[6] := 'It is the differential expression using OpenDSS syntax for example:' + CRLF +
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


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDynamicExp.Edit(ActorID: Integer): Integer;
var
    idx,
    ParamPointer: Integer;
    ParamName,
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 50001);
                1:
                begin
                    FNumVars := Parser[ActorID].IntValue;  // Use property value to force reallocations
                    setlength(FVarValues, FNumVars);
                end;
                2:
                begin
                    InterpretTStringListArray(Param, FVarNames);
                  // ensuring they are lower case
                    for idx := 0 to (FVarNames.Count - 1) do
                        FVarNames[idx] := LowerCase(FVarNames[idx]);
                    if (FNumVars <> FVarnames.Count) then
                    begin
                        FNumVars := FVarNames.Count;
                        setlength(FVarValues, FNumVars);
                    end;
                end;
                3:
                begin
                    setlength(FVarInitValues, FNumVars);
                    Parser[ActorID].ParseAsVector(FnumVars, @(FVarInitValues[0]))
                end;
                4:
                begin
                    FActiveVar := LowerCase(Parser[ActorID].StrValue);
                    if not FVarNames.Find(FActiveVar, ActiveElement) then
                    begin
                    // Being here means that the given name doesn't exist
                        DoSimpleMsg('DynamicExp "' + FActiveVar + '" not found "', 50001);
                        FActiveVar := '';
                    end;
                end;
                5:
                begin
                    if ((ActiveElement < length(FVarValues)) and (ActiveElement >= 0)) then
                        FVarValues[ActiveElement] := Parser[ActorID].DblValue
                    else
                        DoSimpleMsg('There is not valid DynamicExp active"', 50002);
                end;
                6:
                begin
                    if (InterpretDiffEq(Parser[ActorID].StrValue)) then
                        DoSimpleMsg('There are errors in the dynamic expression"', 50003);
                end

            else
                ClassEdit(ActiveDynamicExpObj, Parampointer - NumPropsThisClass)
            end;
{
           CASE ParamPointer OF
               1 : ;
           END;

}
            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
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
     {See if we can find this DynamicExp code in the present collection}


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

    DoSimpleMsg('DynamicExp: "' + Value + '" not Found.', 103);

end;


  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //      TLineCode Obj
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TDynamicExpObj.Create(ParClass: TDSSClass; const LineCodeName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(LineCodeName);
    DSSObjType := ParClass.DSSClassType;
    FVarNames := nil;
    FNumVars := 20;
    FActiveVar := '';
    FVarNames.Clear;
    setlength(FVarValues, 0);
    setlength(FVarInitValues, 0);
    setlength(FVarIOType, 0);

    InitPropertyValues(0);
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TDynamicExpObj.Destroy;
begin
    FVarNames.Clear;
    setlength(FVarValues, 0);
    setlength(FVarInitValues, 0);
    setlength(FVarIOType, 0);
    inherited destroy;
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function TDynamicExpObj.InterpretDiffEq(Exp: String): Boolean;
begin

    Result := false;
end;

  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TDynamicExpObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    k,
    i, j: Integer;
    TempStr: String;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin

        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    end;
end;

function TDynamicExpObj.GetPropertyValue(Index: Integer): String;
var
    j: Integer;
begin
    case Index of
        1:
            Result := Format('%d', [FNumVars]);

    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TDynamicExpObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '0';      // 'nvariables';
    PropertyValue[2] := '[]';     // 'varnames';
    PropertyValue[3] := '[0.0]';  // 'varinit';
    PropertyValue[4] := '';       // 'var';
    PropertyValue[5] := '0.0';    // 'varvalue';
    PropertyValue[6] := '';       // 'expression';


    inherited  InitPropertyValues(NumPropsThisClass);

end;


end.
