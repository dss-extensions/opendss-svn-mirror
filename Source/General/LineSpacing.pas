unit LineSpacing;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

USES
   Sysutils, Arraydef, Command, DSSClass, DSSObject;


TYPE
   SpcParmChoice = (X, H);

   TLineSpacing = class(TDSSClass)
     private

       Function Get_Code:String;  // Returns active line code string
       Procedure Set_Code(const Value:String);  // sets the  active LineSpacing
       Procedure InterpretArray(const S:String; which:SpcParmChoice);

     Protected
       Procedure DefineProperties;
       Function MakeLike(Const LineName:String):Integer;  Override;
     public

       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;


       // Set this property to point ActiveLineSpacingObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;


   TLineSpacingObj = class(TDSSObject)
     private
        FNConds      :Integer;
        FNPhases     :Integer;
        FX           :pDoubleArray;
        FY           :pDoubleArray;
        FUnits       :Integer;
        FEqDistPhPh  :Double;
        FEqDistPhN   :Double;
        FAvgHeightPh :Double;
        FAvgHeightN  :Double;
        FEquivalentSpacing  :Boolean;
        DataChanged  :Boolean;

        procedure set_Nwires(const Value: Integer);

        // CIM Accessors
        function Get_FX (i: integer) : Double;
        function Get_FY (i: integer) : Double;

      public

        constructor Create(ParClass:TDSSClass; const LineSpacingName:String);
        destructor Destroy; override;

        FUNCTION  GetPropertyValue(Index:Integer):String; Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer); Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean); Override;

        // CIM XML accessors
        Property Xcoord[i:Integer]: Double Read Get_FX;
        Property Ycoord[i:Integer]: Double Read Get_FY;
        Property EqDistPhPh: Double Read FEqDistPhPh;
        Property EqDistPhN: Double Read FEqDistPhN;
        Property AvgHeightPh: Double Read FAvgHeightPh;
        Property AvgHeightN: Double Read FAvgHeightN;
        Property NWires: Integer Read FNConds Write set_Nwires;
        Property NPhases: Integer Read FNPhases;
        Property Units: Integer Read FUnits;
        Property EquivalentSpacing: Boolean Read FEquivalentSpacing;
   end;

VAR
   ActiveLineSpacingObj:TLineSpacingObj;

implementation

USES  ParserDel,  DSSClassDefs,  DSSGlobals, Ucomplex, Utilities,  LineUNits;

Const      NumPropsThisClass = 10;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLineSpacing.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name    := 'LineSpacing';
     DSSClassType  := DSS_OBJECT;
     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(PropertyName, NumProperties);
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TLineSpacing.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineSpacing.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     PropertyName^[1]  := 'nconds';
     PropertyName^[2]  := 'nphases';
     PropertyName^[3]  := 'x';
     PropertyName^[4]  := 'h';
     PropertyName^[5]  := 'units';
     PropertyName^[6]  := 'detailed';
     PropertyName^[7]  := 'EqDistPhPh';
     PropertyName^[8]  := 'EqDistPhN';
     PropertyName^[9]  := 'AvgPhaseHeight';
     PropertyName^[10] := 'AvgNeutralHeight';

     PropertyHelp^[1] := 'Number of wires in this geometry. Default is 3. Triggers memory allocations. Define first!';
     PropertyHelp^[2] := 'Number of retained phase conductors. If less than the number of wires, list the retained phase coordinates first.';
     PropertyHelp^[3] := 'Array of wire X coordinates.';
     PropertyHelp^[4] := 'Array of wire Heights.';
     PropertyHelp^[5] := 'Units for x and h: {mi|kft|km|m|Ft|in|cm } Initial default is "ft", but defaults to last unit defined';
     PropertyHelp^[6] := '{Yes/True | No/False} Default = Yes. Determines whether the spacing uses a detailed cross-section coordinates with x and h arrays (Yes/True), or uses equivalent spacing fields (No/False). The equivalent spacing fields are EqDistPhPh, EqDistPhN, AvgPhaseHeight and AvgNeutralHeight.';
     PropertyHelp^[7] := 'Equivalent distance between phase conductors. Used for equivalent distance modeling (detailed=yes) as opposed to detailed cross-section coordinates. ';
     PropertyHelp^[8] := 'Equivalent distance between phase and neutral conductors. Used for equivalent distance modeling (detailed=yes) as opposed to detailed cross-section coordinates.';
     PropertyHelp^[9] := 'Average height of phase conductors. Used for equivalent distance modeling (detailed=yes) as opposed to detailed cross-section coordinates.';
     PropertyHelp^[10]:= 'Average height of neutral conductors. Used for equivalent distance modeling (detailed=yes) as opposed to detailed cross-section coordinates.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineSpacing.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit[ActiveActor] Do
   Begin
    ActiveDSSObject[ActiveActor] := TLineSpacingObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
   End;
END;

Procedure TLineSpacing.InterpretArray(const S:String; which:SpcParmChoice);
var
  Str:String;
  i:Integer;
begin
  AuxParser[ActiveActor].CmdString := S;
  with ActiveLineSpacingObj do begin
    for i := 1 to NWires do begin
      AuxParser[ActiveActor].NextParam; // ignore any parameter name  not expecting any
      Str := AuxParser[ActiveActor].StrValue;
      if Length(Str) > 0 then
        case which of
          X:  FX^[i] := AuxParser[ActiveActor].Dblvalue;
          H:  FY^[i] := AuxParser[ActiveActor].Dblvalue;
        end;
    end;
  end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineSpacing.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveLineSpacingObj := ElementList.Active;
  ActiveDSSObject[ActorID] := ActiveLineSpacingObj;

  WITH ActiveLineSpacingObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0 ) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 10101);
            1: NWires        := Parser[ActorID].IntValue;  // Use property value to force reallocations
            2: FNPhases := Parser[ActorID].IntValue;
            3: InterpretArray (Param, X);
            4: InterpretArray (Param, H);
            5: FUnits := GetUnitsCode(Param);
            6: FEquivalentSpacing := not InterpretYesNo(Param);
            7: FEqDistPhPh := Parser[ActorID].DblValue;
            8: FEqDistPhN := Parser[ActorID].DblValue;
            9: FAvgHeightPh := Parser[ActorID].DblValue;
            10: FAvgHeightN := Parser[ActorID].DblValue;
         ELSE
           // Inherited parameters
           ClassEdit(ActiveLineSpacingObj, Parampointer - NumPropsThisClass)
         END;

         Case ParamPointer of
            1..5: DataChanged := TRUE;
         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     END;

  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineSpacing.MakeLike(Const LineName:String):Integer;
VAR
   OtherLineSpacing:TLineSpacingObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherLineSpacing := Find(LineName);
   IF OtherLineSpacing<>Nil THEN
   WITH ActiveLineSpacingObj DO BEGIN

       NWires := OtherLineSpacing.NWires;   // allocates
       FNPhases := OtherLineSpacing.NPhases;
       For i := 1 to FNConds Do FX^[i] := OtherLineSpacing.FX^[i];
       For i := 1 to FNConds Do FY^[i] := OtherLineSpacing.FY^[i];
       FEquivalentSpacing := OtherLineSpacing.EquivalentSpacing;
       FEqDistPhPh := OtherLineSpacing.EqDistPhPh;
       FEqDistPhN := OtherLineSpacing.EqDistPhN;
       FAvgHeightPh := OtherLineSpacing.AvgHeightPh;
       FAvgHeightN := OtherLineSpacing.AvgHeightN;
       FUnits := OtherLineSpacing.FUnits;
       DataChanged := TRUE;
       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherLineSpacing.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in LineSpacing MakeLike: "' + LineName + '" Not Found.', 102);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineSpacing.Init(Handle:Integer; ActorID : Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TLineSpacing.Init', -1);
   Result := 0;
END;

Function TLineSpacing.Get_Code:String;  // Returns active line code string

BEGIN

  Result := TLineSpacingObj(ElementList.Active).Name;

END;

Procedure TLineSpacing.Set_Code(const Value:String);  // sets the  active LineSpacing
VAR
  LineSpacingObj:TLineSpacingObj;
BEGIN

    ActiveLineSpacingObj := Nil;
    LineSpacingObj       := ElementList.First;
    WHILE LineSpacingObj<>Nil DO BEGIN

       IF CompareText(LineSpacingObj.Name, Value)=0 THEN BEGIN
          ActiveLineSpacingObj := LineSpacingObj;
          Exit;
       END;

       LineSpacingObj := ElementList.Next;
    END;

    DoSimpleMsg('LineSpacing: "' + Value + '" not Found.', 103);

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineSpacing Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



constructor TLineSpacingObj.Create(ParClass:TDSSClass; const LineSpacingName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(LineSpacingName);
     DSSObjType := ParClass.DSSClassType;

     DataChanged := TRUE;
     FX          := nil;
     FY          := nil;
     Funits      := UNITS_FT;
     NWires      := 3;  // Allocates terminals
     FNPhases     := 3;
     FEqDistPhPh  := 0.0;
     FEqDistPhN   := 0.0;
     FAvgHeightPh := 0.0;
     FAvgHeightN  := 0.0;
     FEquivalentSpacing := FALSE;

     InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineSpacingObj.Destroy;
BEGIN

    Reallocmem(FY, 0);
    Reallocmem(FX, 0);

    Inherited destroy;

END;


PROCEDURE TLineSpacingObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
    Begin
       For i := 1 to 10 Do  Begin
//         if (not FEquivalentSpacing and (i > 5)) then continue
//         else if (FEquivalentSpacing and ((i = 3) or (i = 4))) then continue;
         Writeln(F,'~ ',PropertyName^[i],'=',GetPropertyValue(i));
       End;
    End;

end;

function ArrayString(pF:pDoubleArray; N:Integer): String;
var
  i: Integer;
  r: String;
begin
  r := '[';
  if N > 0 then r := r + Format('%-g',[pF^[1]]);
  for i:= 2 to N do r := r + Format(',%-g',[pF^[i]]);
  Result := r + ']';
end;

function TLineSpacingObj.GetPropertyValue(Index: Integer): String;

{Return Property Value for Active index}

begin

  CASE Index OF
      3: Result := ArrayString (FX, FNConds);
      4: Result := ArrayString (FY, FNConds);
      5: Result :=  LineUnitsStr(FUnits);
   ELSE
     // Inherited parameters
     Result     := Inherited GetPropertyValue(Index);
   END;

end;

function TLineSpacingObj.Get_FX(i:Integer) : Double;
begin
  If i <= FNConds Then Result := FX^[i] Else Result := 0.0;
end;

function TLineSpacingObj.Get_FY(i:Integer) : Double;
begin
  If i <= FNConds Then Result := FY^[i] Else Result := 0.0;
end;

procedure TLineSpacingObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] :=  '3';
     PropertyValue[2] :=  '3';
     PropertyValue[3] :=  '0';
     PropertyValue[4] :=  '32';
     PropertyValue[5] :=  'ft';
     PropertyValue[6] :=  'Yes';
     PropertyValue[7] :=  '0';
     PropertyValue[8] :=  '0';
     PropertyValue[9] :=  '0';
     PropertyValue[10] :=  '0';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TLineSpacingObj.set_NWires(const Value: Integer);
begin
  FNconds := Value;
  ReAllocmem (FX, Sizeof(FX^[1])*FNconds);
  ReAllocmem (FY, Sizeof(FY^[1])*FNconds);
  FUnits    := UNITS_FT;
end;

end.
