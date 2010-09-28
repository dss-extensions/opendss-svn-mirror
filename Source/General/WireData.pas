unit WireData;
 {
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{The WireData object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the WireData Class.
 This sets the active WireData object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.

 }

USES
   Command, DSSClass, DSSObject;


TYPE



   TWireData = class(TDSSClass)
     private

       Function Get_Code:String;  // Returns active line code string
       Procedure Set_Code(const Value:String);  // sets the  active WireData

     Protected
       Procedure DefineProperties;
       Function MakeLike(Const LineName:String):Integer;  Override;
     public
       
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       // Set this property to point ActiveWireDataObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TWireDataObj = class(TDSSObject)
     private
        FRDC              :Double;
        FR60              :Double;
        FGMR60            :Double;
        Fradius           :Double;
        FGMRUnits         :Integer;
        FResistanceUnits  :Integer;
        FRadiusUnits      :Integer;

      public

        NormAmps          :Double;
        EmergAmps         :Double;

        constructor Create(ParClass:TDSSClass; const WireDataName:String);
        destructor Destroy; override;

        Property Rdc:Double Read FRDC;
        Property Rac:Double Read FR60;
        Property GMR:Double Read FGMR60;
        Property Radius:Double Read FRadius;
        Property ResUnits:Integer Read FresistanceUnits;
        Property RadiusUnits:Integer Read FradiusUnits;
        Property GMRUnits:Integer Read FGMRUnits;

        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

   TWireDataArray = Array[1..100] of TWireDataObj;
   pWireDataArray = ^TWireDataArray;

VAR
   ActiveWireDataObj:TWireDataObj;

implementation

USES  ParserDel,  DSSGlobals, DSSClassDefs, Sysutils, Ucomplex, Arraydef,  LineUNits;

Const      NumPropsThisClass = 10;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TWireData.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name := 'WireData';
     DSSClassType := DSS_OBJECT;
     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TWireData.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TWireData.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     PropertyName[1] := 'Rdc';
     PropertyName[2] := 'Rac';
     PropertyName[3] := 'Runits';
     PropertyName[4] := 'GMRac';
     PropertyName[5] := 'GMRunits';
     PropertyName[6] := 'radius';
     PropertyName[7] := 'radunits';
     PropertyName[8] := 'normamps';
     PropertyName[9] := 'emergamps';
     PropertyName[10] := 'diam';



     PropertyHelp[1] := 'dc Resistance, ohms per unit length (see Runits). Defaults to Rac/1.02 if not specified.';
     PropertyHelp[2] := 'Resistance at 60 Hz per unit length. Defaults to 1.02*Rdc if not specified.';
     PropertyHelp[3] := 'Length units for resistance: ohms per {mi|kft|km|m|Ft|in|cm } Default=none.';
     PropertyHelp[4] := 'GMR at 60 Hz. Defaults to .7788*radius if not specified.';
     PropertyHelp[5] := 'Units for GMR: {mi|kft|km|m|Ft|in|cm } Default=none.';
     PropertyHelp[6] := 'Outside radius of conductor. Defaults to GMR/0.7788 if not specified.';
     PropertyHelp[7] := 'Units for outside radius: {mi|kft|km|m|Ft|in|cm } Default=none.';
     PropertyHelp[8] := 'Normal ampacity, amperes. Defaults to Emergency amps/1.5 if not specified.';
     PropertyHelp[9] := 'Emergency ampacity, amperes. Defaults to 1.5 * Normal Amps if not specified.';
     PropertyHelp[10] := 'Diameter; Alternative method for entering radius.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TWireData.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit Do
   Begin
    ActiveDSSObject := TWireDataObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
   End;
END;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TWireData.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveWireDataObj := ElementList.Active;
  ActiveDSSObject := ActiveWireDataObj;

  WITH ActiveWireDataObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 101);
            1: FRDC := Parser.Dblvalue;  // Use property value to force reallocations
            2: FR60 := Parser.DblValue;
            3: FresistanceUnits := GetUnitsCode(Param);
            4: FGMR60           := Parser.DblValue;
            5: FGMRUnits        := GetUnitsCode(Param);
            6: Fradius          := Parser.DblValue;
            7: FRadiusUnits     := GetUnitsCode(Param);
            8: NormAmps         := Parser.DblValue ;
            9: EmergAmps        := Parser.DblValue ;
           10: Fradius          := Parser.DblValue / 2.0;
         ELSE
           // Inherited parameters
           ClassEdit(ActiveWireDataObj, Parampointer - NumPropsThisClass)
         END;

         {Set defaults}
         CASE ParamPointer OF

            1: If FR60<0.0      Then FR60 := 1.02* FRDC;
            2: If FRDC<0.0      Then FRDC := FR60 / 1.02;
            4: If Fradius<0.0   Then Fradius := FGMR60 / 0.7788;
            5: If FradiusUnits =0 Then FradiusUnits := FGMRunits;
            6: If FGMR60<0.0    Then FGMR60 := 0.7788 * FRadius;
            7: If FGMRUnits=0   Then FGMRunits := FradiusUnits;
            8: IF EmergAmps<0.0 Then EmergAmps := 1.5*NormAmps;
            9: If NormAmps<0.0  Then NormAmps := EmergAmps/1.5;
            10: If FGMR60<0.0   Then FGMR60 := 0.7788 * FRadius;
         END;

          {Check for critical errors}
         CASE ParamPointer OF
            4: If (Fradius = 0.0)  Then DoSimpleMsg('Error: Radius is specified as zero for Wiredata.' + Name,999);
            6: If (FGMR60 = 0.0)   Then DoSimpleMsg('Error: GMR is specified as zero for Wiredata.' + Name,999);
         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END;

  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TWireData.MakeLike(Const LineName:String):Integer;
VAR
   OtherWireData:TWireDataObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherWireData := Find(LineName);
   IF OtherWireData<>Nil THEN
   WITH ActiveWireDataObj DO BEGIN

       FRDC:= OtherWireData.FRDC;
       FR60:= OtherWireData.FR60;
       FResistanceUnits:= OtherWireData.FResistanceUnits;
       FGMR60:= OtherWireData.FGMR60;
       FGMRUnits:= OtherWireData.FGMRUnits;
       FRadius:= OtherWireData.FRadius;
       FRadiusUnits:= OtherWireData.FRadiusUnits;
       NormAmps := OtherWireData.NormAmps;
       EmergAmps := OtherWireData.EmergAmps;

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherWireData.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in Line MakeLike: "' + LineName + '" Not Found.', 102);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TWireData.Init(Handle:Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TWireData.Init', -1);
   REsult := 0;
END;

Function TWireData.Get_Code:String;  // Returns active line code string

BEGIN

  Result := TWireDataObj(ElementList.Active).Name;

END;

Procedure TWireData.Set_Code(const Value:String);  // sets the  active WireData
VAR
  WireDataObj:TWireDataObj;
BEGIN

    ActiveWireDataObj := Nil;
    WireDataObj := ElementList.First;
    WHILE WireDataObj<>Nil DO BEGIN

       IF CompareText(WireDataObj.Name, Value)=0 THEN BEGIN
          ActiveWireDataObj := WireDataObj;
          Exit;
       END;

       WireDataObj := ElementList.Next;
    END;

    DoSimpleMsg('WireData: "' + Value + '" not Found.', 103);

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TWireData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TWireDataObj.Create(ParClass:TDSSClass; const WireDataName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(WireDataName);
     DSSObjType := ParClass.DSSClassType;

    FRDC              := -1.0;
    FR60              := -1.0;
    FGMR60            := -1.0;
    Fradius           := -1.0;
    FGMRUnits         := 0;
    FResistanceUnits  := 0;
    FRadiusUnits      := 0;

    Normamps    := -1.0;
    EmergAmps   :=-1.0;

     InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TWireDataObj.Destroy;
BEGIN
    Inherited destroy;
END;



PROCEDURE TWireDataObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
    Begin

         For i := 1 to NumProperties Do
         Begin
            Write(F,'~ ',PropertyName^[i],'=');
            Case i of
              1: Writeln(F, Format('%.6g',[FRDC]));
              2: Writeln(F, Format('%.6g',[FR60]));
              3: Writeln(F, Format('%s',[LineUnitsStr(FresistanceUnits)]));
              4: Writeln(F, Format('%.6g',[FGMR60]));
              5: Writeln(F, Format('%s',[LineUnitsStr(FGMRUnits)]));
              6: Writeln(F, Format('%.6g',[Fradius]));
              7: Writeln(F, Format('%s',[LineUnitsStr(FRadiusUnits)]));
              8: Writeln(F, Format('%.6g',[NormAmps]));
              9: Writeln(F, Format('%.6g',[EmergAmps]));
             10: Writeln(F, Format('%.6g',[radius*2.0]));
           END;
         End;

     End;

end;

procedure TWireDataObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '-1';
     PropertyValue[2] :=  '-1';
     PropertyValue[3] :=  'none';
     PropertyValue[4] :=  '-1';
     PropertyValue[5] :=  'none';
     PropertyValue[6] :=  '-1';
     PropertyValue[7] :=  'none';
     PropertyValue[8] :=  '-1';
     PropertyValue[9] :=  '-1';
     PropertyValue[10] :=  '-1';

    inherited  InitPropertyValues(NumPropsThisClass);

end;


end.
