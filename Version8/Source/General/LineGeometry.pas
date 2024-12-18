unit LineGeometry;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2020, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{The LineGeometry object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LineGeometry Class.
 This sets the active LineGeometry object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.

 }

USES
   Sysutils, Arraydef, Command, DSSClass, DSSObject, uCMatrix,
   LineConstants, ConductorData, CNData, TSData, LineSpacing;


TYPE
   ELineGeometryProblem = class(Exception);

   TLineGeometry = class(TDSSClass)
     private

       Function Get_Code:String;  // Returns active line code string
       Procedure Set_Code(const Value:String);  // sets the  active LineGeometry

     Protected
       Procedure DefineProperties;
       Function MakeLike(Const LineName:String):Integer;  Override;
     public

       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;


       // Set this property to point ActiveLineGeometryObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;


   TLineGeometryObj = class(TDSSObject)
     private
        FPhaseChoice :pConductorChoiceArray;
        FNConds      :Integer;
        FNPhases     :Integer;
        FCondName    :pStringArray;
        FWireData    :pConductorDataArray;
        FX           :pDoubleArray;
        FY           :pDoubleArray;
        FEqDist      :array of double; // This array always has four elements EqDistPhPh, EqDistPhN, AvgHeightPh, AvgHeightN
        FEquivalentSpacing :Boolean;  // to tell the calcs when to use equivalent spacing info
        FCondsUser    :String;      // use this to preserve conductors array for dumping to avoid losing user-defined None positions.
        FUnits       :pIntegerArray;
        FLastUnit    :Integer;
        DataChanged  :Boolean;
        FReduce      :Boolean;
        FActiveCond  :Integer;
        FSpacingType :String;

        FLineData    :TLineConstants;
        procedure ChangeLineConstantsType(newPhaseChoice:ConductorChoice);

        procedure set_Nconds(const Value: Integer);
        procedure set_Nphases(const Value: Integer);
        procedure set_ActiveCond(const Value: Integer);
        function  Get_YCmatrix(f, Lngth: double; Units: Integer): Tcmatrix;
        function  Get_Zmatrix(f, Lngth: double; Units: Integer): Tcmatrix;
        function  Get_RhoEarth:Double;
        procedure Set_RhoEarth(const Value: Double);
        function  Get_EpsRMedium:Double;
        procedure Set_EpsRMedium(const Value: Double);
        function  Get_HeightOffset:Double;
        procedure Set_HeightOffset(const Value: Double);
        function  Get_HeightUnit:Integer;
        procedure Set_HeightUnit(const Value: Integer);
        function  get_Nconds: Integer;
        Procedure UpdateLineGeometryData(f:Double);   // call this before using the line data

        // CIM Accessors
        function Get_FX (i: integer) : Double;
        function Get_FY (i: integer) : Double;
        function Get_FUnits (i: integer) : Integer;
        function Get_ConductorName (i: integer) : String;
        function Get_ConductorData (i: integer) : TConductorDataObj;
        function Get_PhaseChoice(i: Integer): ConductorChoice;

      public

        NormAmps          : Double;
        EmergAmps         : Double;
        NumAmpRatings     : Integer;
        AmpRatings        : TRatingsArray;
        FLineType         : Integer; // Pointer to code for type of line

        constructor Create(ParClass:TDSSClass; const LineGeometryName:String);
        destructor Destroy; override;

        FUNCTION  GetPropertyValue(Index:Integer):String; Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer); Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean); Override;
        PROCEDURE SaveWrite(Var F:TextFile); Override;

        // called from a Line object that has its own Spacing and Wires input
        // automatically sets reduce=y if the spacing has more wires than phases
        Procedure LoadSpacingAndWires (Spc: TLineSpacingObj; Wires: pConductorDataArray);

        Property Nconds:Integer     read get_Nconds  write set_Nconds;
        Property Nphases:Integer    read FNphases    write set_Nphases;
        Property ActiveCond:Integer read FActiveCond write set_ActiveCond;
        Property Zmatrix [f, Lngth:double; Units:Integer]:Tcmatrix read Get_Zmatrix;
        Property YCmatrix[f, Lngth:double; Units:Integer]:Tcmatrix read Get_YCmatrix;
        Property RhoEarth:Double    Read Get_RhoEarth Write Set_RhoEarth;
        Property EpsRmedium:Double    Read Get_EpsRmedium Write Set_EpsRmedium;
        Property heightOffset:Double    Read Get_HeightOffset Write Set_HeightOffset;
        Property heightUnit:Integer    Read Get_HeightUnit Write Set_HeightUnit;

        // CIM XML accessors
        Property Xcoord[i:Integer]: Double Read Get_FX;
        Property Ycoord[i:Integer]: Double Read Get_FY;
        Property Units[i:Integer]: Integer Read Get_FUnits;
        Property ConductorName[i:Integer]: String Read Get_ConductorName;
        Property ConductorData[i: Integer]: TConductorDataObj Read Get_ConductorData;
        Property NWires: Integer Read FNConds;
        Property PhaseChoice [i:Integer]: ConductorChoice Read Get_PhaseChoice;
   end;

VAR
   ActiveLineGeometryObj:TLineGeometryObj;

implementation

USES  ParserDel,  DSSClassDefs,  DSSGlobals, Ucomplex, Utilities,  LineUnits,
      OHLineConstants, CNTSLineConstants;

Const      NumPropsThisClass = 20;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLineGeometry.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name    := 'LineGeometry';
     DSSClassType  := DSS_OBJECT;
     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(PropertyName, NumProperties);
     CommandList.Abbrev := TRUE;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TLineGeometry.Destroy;

BEGIN
  // ElementList and  CommandList freed in inherited destroy
  Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineGeometry.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     PropertyName^[1]  := 'nconds';
     PropertyName^[2]  := 'nphases';
     PropertyName^[3]  := 'cond';
     PropertyName^[4]  := 'wire';
     PropertyName^[5]  := 'x';
     PropertyName^[6]  := 'h';
     PropertyName^[7]  := 'units';
     PropertyName^[8]  := 'normamps';
     PropertyName^[9]  := 'emergamps';
     PropertyName^[10] := 'reduce';
     PropertyName^[11] := 'spacing';
     PropertyName^[12] := 'wires';
     PropertyName^[13] := 'cncable';
     PropertyName^[14] := 'tscable';
     PropertyName^[15] := 'cncables';
     PropertyName^[16] := 'tscables';
     PropertyName^[17] := 'Seasons';
     PropertyName^[18] := 'Ratings';
     PropertyName^[19] := 'LineType';
     PropertyName^[20] := 'conductors';

     PropertyHelp^[1] := 'Number of conductors in this geometry. Default is 3. Triggers memory allocations. Define first!';
     PropertyHelp^[2] := 'Number of phases. Default =3; All other conductors are considered neutrals and might be reduced out.';
     PropertyHelp^[3] := 'Set this = number of the conductor you wish to define. Default is 1.';
     PropertyHelp^[4] := 'Code from WireData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
                        'Specifies use of Overhead Line parameter calculation,' + CRLF +
                        'Unless Tape Shield cable previously assigned to phases, and this wire is a neutral.';
     PropertyHelp^[5] := 'x coordinate.';
     PropertyHelp^[6] := 'Height of conductor.';
     PropertyHelp^[7] := 'Units for x and h: {mi|kft|km|m|Ft|in|cm } Initial default is "ft", but defaults to last unit defined';
     PropertyHelp^[8] := 'Normal ampacity, amperes for the line. Defaults to first conductor if not specified.';
     PropertyHelp^[9] := 'Emergency ampacity, amperes. Defaults to first conductor if not specified.';
     PropertyHelp^[10] := '{Yes | No} Default = no. Reduce to Nphases (Kron Reduction). Reduce out neutrals.';
     PropertyHelp^[11] := 'Reference to a LineSpacing for use in a line constants calculation.' + CRLF +
                          'Alternative to x, h, and units. MUST BE PREVIOUSLY DEFINED.' + CRLF +
                          'Must match "nconds" as previously defined for this geometry.' + CRLF +
                          'Must be used in conjunction with the Wires property.';
     PropertyHelp^[12] := 'Array of WireData names for use in a line constants calculation.' + CRLF +
                          'Alternative to individual wire inputs. ALL MUST BE PREVIOUSLY DEFINED.' + CRLF +
                          'Must match "nconds" as previously defined for this geometry,' + CRLF +
                          'unless TSData or CNData were previously assigned to phases, and these wires are neutrals.' + CRLF +
                          'Must be used in conjunction with the Spacing property.';
     PropertyHelp^[13] := 'Code from CNData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
                         'Specifies use of Concentric Neutral cable parameter calculation.';
     PropertyHelp^[14] := 'Code from TSData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
                         'Specifies use of Tape Shield cable parameter calculation.';
     PropertyHelp^[15] := 'Array of CNData names for cable parameter calculation.' + CRLF +
                         'All must be previously defined, and match "nphases" for this geometry.' + CRLF +
                         'You can later define "nconds-nphases" wires for bare neutral conductors.';
     PropertyHelp^[16] := 'Array of TSData names for cable parameter calculation.' + CRLF +
                         'All must be previously defined, and match "nphases" for this geometry.' + CRLF +
                         'You can later define "nconds-nphases" wires for bare neutral conductors.';
     PropertyHelp^[17] := 'Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the ' +
                         '"Ratings" property. Defaults to first conductor if not specified.' ;
     PropertyHelp^[18] := 'An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert' +
                         CRLF + 'multiple ratings to change during a QSTS simulation to evaluate different ratings in lines.' +
                         'Defaults to first conductor if not specified.';
     PropertyHelp^[19] := 'Code designating the type of line. ' +  CRLF +
                         'One of: OH, UG, UG_TS, UG_CN, SWT_LDBRK, SWT_FUSE, SWT_SECT, SWT_REC, SWT_DISC, SWT_BRK, SWT_ELBOW, BUSBAR' + CRLF +  CRLF +
                         'OpenDSS currently does not use this internally. For whatever purpose the user defines. Default is OH.' ;
     PropertyHelp^[20] := 'Array of conductor names for use in line constants calculation.' + CRLF +
                          'Must be used in conjunction with the Spacing property.' + CRLF +
                          'Specify the Spacing first, and "ncond" wires.' + CRLF +
                          'Specify the conductor type followed by the conductor name. e.g., "conductors=[cndata.cncablename, tsdata.tscablename, wiredata.wirename]"'+ CRLF +
                          'If a given position in the spacing is not to be used in the line, use "none" in the entry of the conductors array.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineGeometry.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit[ActiveActor] Do
   Begin
    ActiveDSSObject[ActiveActor] := TLineGeometryObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
   End;
END;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineGeometry.Edit(ActorID : Integer):Integer;
VAR
   i, j,
   istart,
   istop,
   ParamPointer     :Integer;
   ParamName,
   Param            :String;
   dotpos, actualNConds, actualNPhases :Integer;
   CondName, CondClass :String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveLineGeometryObj := ElementList.Active;
  ActiveDSSObject[ActorID] := ActiveLineGeometryObj;

  WITH ActiveLineGeometryObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0 ) and (ParamPointer <= NumProperties  ) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 10101);
            1: NConds        := Parser[ActorID].IntValue;  // Use property value to force reallocations
            2: FNphases      := Parser[ActorID].IntValue;
            3: ActiveCond    := Parser[ActorID].IntValue;
            4: Begin
                FCondName^[ActiveCond] := Param;
                if (FPhaseChoice^[ActiveCond] = Unknown) or (FPhaseChoice^[ActiveCond] = Overhead) then
                  ChangeLineConstantsType (Overhead);
            end;
            5: FX^[ActiveCond] := Parser[ActorID].DblValue;
            6: FY^[ActiveCond] := Parser[ActorID].DblValue;
            7: Begin
                FUnits^[ActiveCond] := GetUnitsCode(Param);
                FLastUnit           := FUnits^[ActiveCond];
               End;
            8: NormAmps    := Parser[ActorID].DblValue ;
            9: EmergAmps   := Parser[ActorID].DblValue ;
           10: Freduce     := InterpretYesNo(Param);
           11: Begin
                  FSpacingType := Parser[ActorID].StrValue;
                  if LineSpacingClass[ActorID].SetActive(FSpacingType) then begin
                    ActiveLineSpacingObj := LineSpacingClass[ActorID].GetActiveObj;
                    if (FNConds = ActiveLineSpacingObj.NWires) then begin
                      FLastUnit := ActiveLineSpacingObj.Units;
                      FEquivalentSpacing :=ActiveLineSpacingObj.EquivalentSpacing;
                      if ActiveLineSpacingObj.EquivalentSpacing then
                      begin
                        FEqDist[1] := ActiveLineSpacingObj.EqDistPhPh;
                        FEqDist[2] := ActiveLineSpacingObj.EqDistPhN;
                        FEqDist[3] := ActiveLineSpacingObj.AvgHeightPh;
                        FEqDist[4] := ActiveLineSpacingObj.AvgHeightN;
                      end
                      else
                      begin
                        for i:=1 to FNConds do begin
                          FX^[i] := ActiveLineSpacingObj.Xcoord[i];
                          FY^[i] := ActiveLineSpacingObj.Ycoord[i];
                          FUnits^[i] := FLastUnit;
                        end;
                      end;
                    end else
                      DoSimpleMsg('LineSpacing object ' + FSpacingType + ' has the wrong number of wires.', 10103);
                  end else
                    DoSimpleMsg('LineSpacing object ' + FSpacingType + ' has not been defined.', 10103);
           End;
           13: Begin FCondName^[ActiveCond] := Param; ChangeLineConstantsType (ConcentricNeutral) End;
           14: Begin FCondName^[ActiveCond] := Param; ChangeLineConstantsType (TapeShield) End;
           12,
           15,
           16: Begin
                  istart := 1;
                  istop := FNConds;

                  if ParamPointer = 15 then begin
                    ChangeLineConstantsType (ConcentricNeutral);
                    istop := FNPhases;
                  end else if ParamPointer = 16 then begin
                    ChangeLineConstantsType (TapeShield);
                    istop := FNPhases;
                  end
                  else
                    if ParamPointer = 12 then
                    begin
                      if FPhaseChoice^[ActiveCond] = Unknown then
                        ChangeLineConstantsType (Overhead)
                      else // these are buried neutral wires
                        if FPhaseChoice^[ActiveCond] <> Overhead then istart := FNPhases + 1; // to fix the bug introduced with ActiveCond
                    end;

                  AuxParser[ActorID].CmdString := Parser[ActorID].StrValue;
                  for i := istart to istop do begin
                    AuxParser[ActorID].NextParam; // ignore any parameter name  not expecting any
                    FCondName[i] := AuxParser[ActorID].StrValue;
                    if ParamPointer=15 then
                      CNDataClass[ActorID].code := FCondName[i]
                    else if ParamPointer=16 then
                      TSDataClass[ActorID].code := FCondName[i]
                    else
                      WireDataClass[ActorID].Code := FCondName[i];
                    if Assigned(ActiveConductorDataObj) then begin
                      FWireData^[i] := ActiveConductorDataObj;
                      if (i=1) then begin
                        If (ActiveConductorDataObj.NormAmps > 0.0) and (Normamps = 0.0) Then Normamps  := ActiveConductorDataObj.NormAmps;
                        If (ActiveConductorDataObj.Emergamps > 0.0) and (Emergamps = 0.0) Then Emergamps := ActiveConductorDataObj.EmergAmps;
                        If (ActiveConductorDataObj.NumAmpRatings > 1) and (NumAmpRatings = 1) Then NumAmpRatings  := ActiveConductorDataObj.NumAmpRatings;
                        If (length(ActiveConductorDataObj.AmpRatings) > 1) and (length(AmpRatings) = 1)
                          Then Begin
                            setlength(AmpRatings,NumAmpRatings);
                            AmpRatings  := ActiveConductorDataObj.AmpRatings;
                          End;
                      end;
                    end else
                      if ParamPointer=15 then
                        DoSimpleMsg('CNData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103)
                      else if ParamPointer=16 then
                        DoSimpleMsg('TSData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103)
                      else
                        DoSimpleMsg('WireData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103);
                  end
               End;
           17: Begin
                 NumAmpRatings         :=  Parser[ActorID].IntValue;
                 setlength(AmpRatings,NumAmpRatings);
               End;
           18: Begin
                 setlength(AmpRatings,NumAmpRatings);
                 Param := Parser[ActorID].StrValue;
                 NumAmpRatings := InterpretDblArray(Param, NumAmpRatings, Pointer(AmpRatings));
               End;
           19: FLineType := LineTypeList.Getcommand(Param);
           20: Begin

                  // First we need to parse the list of conductors to find None values
                  // Then we need to redefine Nconds to reallocate
                  actualNConds := 0;
                  actualNPhases := 0;
                  // Line constants type defined by the first found valid conductor
                  AuxParser[ActorID].CmdString := Parser[ActorID].StrValue;
                  FCondsUser := Parser[ActorID].StrValue;
                  For i := 1 to FNConds Do
                  begin
                      AuxParser[ActorID].NextParam;
                      if CompareText(AuxParser[ActorID].StrValue, 'None')=0 then continue;
                      if CompareText(AuxParser[ActorID].StrValue, '')=0 then continue;
                      actualNConds := actualNConds + 1;
                      if i <= FNPhases then actualNPhases := actualNPhases + 1;
                  end;

                  j := 0;
                  if FNConds <> NConds then
                  begin
                    NConds := actualNConds;   // allocates
                    Nphases := actualNPhases;
                    LineSpacingClass[ActorID].SetActive(FSpacingType);
                    ActiveLineSpacingObj := LineSpacingClass[ActorID].GetActiveObj;
                    FLastUnit := ActiveLineSpacingObj.Units;
                    FEquivalentSpacing := ActiveLineSpacingObj.EquivalentSpacing;
                    AuxParser[ActorID].CmdString := Parser[ActorID].StrValue;
                    for i:=1 to ActiveLineSpacingObj.NWires do begin
                      AuxParser[ActorID].NextParam;
                      if CompareText(AuxParser[ActorID].StrValue, 'None')=0 then continue;
                      if CompareText(AuxParser[ActorID].StrValue, '')=0 then continue;
                      j := j + 1;
                      if not ActiveLineSpacingObj.EquivalentSpacing then
                      begin
                        FX^[j] := ActiveLineSpacingObj.Xcoord[i];
                        FY^[j] := ActiveLineSpacingObj.Ycoord[i];
                        FUnits^[j] := FLastUnit;
                      end;
                    end;
                    if ActiveLineSpacingObj.EquivalentSpacing then
                    begin
                      FEqDist[1] := ActiveLineSpacingObj.EqDistPhPh;
                      FEqDist[2] := ActiveLineSpacingObj.EqDistPhN;
                      FEqDist[3] := ActiveLineSpacingObj.AvgHeightPh;
                      FEqDist[4] := ActiveLineSpacingObj.AvgHeightN;
                    end;

                  end;

                  istart := 1;
                  istop := FNConds;

                  // Line constants type defined by the first found valid conductor
                  AuxParser[ActorID].CmdString := Parser[ActorID].StrValue;
                  for i := istart to istop do begin
                    AuxParser[ActorID].NextParam;
                    while CompareText(AuxParser[ActorID].StrValue, 'None')=0 do
                    begin
                      AuxParser[ActorID].NextParam;
                    end;
                    if AuxParser[ActorID].StrValue = '' then continue;

                    CondClass := '';
                    CondName := '';
                    dotpos := Pos('.', AuxParser[ActorID].StrValue);
                    CASE dotpos OF
                       0:begin
                        DoSimpleMsg ('You must define the conductor class for all the valid conductors in the "conductors" array (LineGeometry.'+name+').', 10103);
                        exit;
                       end;
                    ELSE Begin
                         CondClass := Copy(AuxParser[ActorID].StrValue, 1, dotpos-1);
                         CondName  := Copy(AuxParser[ActorID].StrValue, dotpos+1, Length(AuxParser[ActorID].StrValue));
                         End;
                    End;
                    if LowerCase(CondClass) = 'wiredata' then
                    begin
                      if i <= NPhases then ChangeLineConstantsType(Overhead);
                      break;
                    end
                    else if LowerCase(CondClass) = 'cndata' then
                    begin
                      if i <= NPhases then ChangeLineConstantsType(ConcentricNeutral);
                      break;
                    end
                    else if LowerCase(CondClass) = 'tsdata' then
                    begin
                      if i <= NPhases then ChangeLineConstantsType(TapeShield);
                      break;
                    end
                    else
                    begin
                      DoSimpleMsg ('You must use valid conductor classes (wiredata, cndata, tsdata) for all the conductors in the "conductors" array (LineGeometry.'+name+').', 10103);
                      exit;
                    end;

                  end;


                  AuxParser[ActorID].CmdString := Parser[ActorID].StrValue;
                  for i := istart to istop do begin
                    AuxParser[ActorID].NextParam; // ignore any parameter name  not expecting any
                    while CompareText(AuxParser[ActorID].StrValue, 'None')=0 do
                    begin
                      AuxParser[ActorID].NextParam;
                    end;
                    if AuxParser[ActorID].StrValue = '' then continue;

                    CondClass := '';
                    CondName := '';
                    dotpos := Pos('.', AuxParser[ActiveActor].StrValue);
                    CASE dotpos OF
                       0:begin
                        DoSimpleMsg ('You must define the conductor class for all the valid conductors in the "conductors" array (LineGeometry.'+name+').', 10103);
                        exit;
                       end;
                    ELSE Begin
                         CondClass := Copy(AuxParser[ActiveActor].StrValue, 1, dotpos-1);
                         CondName  := Copy(AuxParser[ActiveActor].StrValue, dotpos+1, Length(AuxParser[ActiveActor].StrValue));
                         End;
                    End;
                    if LowerCase(CondClass) = 'wiredata' then WireDataClass[ActorID].Code := CondName
                    else if LowerCase(CondClass) = 'cndata' then CNDataClass[ActorID].code := CondName
                    else if LowerCase(CondClass) = 'tsdata' then TSDataClass[ActorID].code := CondName;
                    FCondName[i] := CondName;

                    if Assigned(ActiveConductorDataObj) then begin
                      FWireData^[i] := ActiveConductorDataObj;
                      if (i=1) then begin
                        If (ActiveConductorDataObj.NormAmps > 0.0) and (Normamps = 0.0) Then Normamps  := ActiveConductorDataObj.NormAmps;
                        If (ActiveConductorDataObj.Emergamps > 0.0) and (Emergamps = 0.0) Then Emergamps := ActiveConductorDataObj.EmergAmps;
                        If (ActiveConductorDataObj.NumAmpRatings > 1) and (NumAmpRatings = 1) Then NumAmpRatings  := ActiveConductorDataObj.NumAmpRatings;
                        If (length(ActiveConductorDataObj.AmpRatings) > 1) and (length(AmpRatings) = 1)
                          Then Begin
                            setlength(AmpRatings,NumAmpRatings);
                            AmpRatings  := ActiveConductorDataObj.AmpRatings;
                          End;
                      end;
                    end else
                      DoSimpleMsg('Conductor Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103)
                  end
               End;
         ELSE
           // Inherited parameters
           ClassEdit(ActiveLineGeometryObj, Parampointer - NumPropsThisClass)
         END;

         {Set defaults}
         CASE ParamPointer OF

            2: If (FNPhases > FNconds) then FNPhases := FNConds;
            3: If (ActiveCond < 1) or (ActiveCond > FNconds) Then DoSimpleMsg('Illegal cond= specification in Line Geometry:'+CRLF+Parser[ActorID].cmdstring, 10102);
            4,13,14: Begin
                if ParamPointer=4 then
                  WireDataClass[ActorID].code := Param
                else if ParamPointer=13 then
                  CNDataClass[ActorID].code := Param
                else
                  TSDataClass[ActorID].Code := Param;
                If Assigned(ActiveConductorDataObj) Then Begin
                  FWireData^[ActiveCond] := ActiveConductorDataObj;
                  {Default the current ratings for this geometry to the rating of the first conductor}
                  If (ActiveCond = 1) then  Begin
                    If (ActiveConductorDataObj.NormAmps > 0.0) and (Normamps = 0.0) Then Normamps  := ActiveConductorDataObj.NormAmps;
                    If (ActiveConductorDataObj.Emergamps > 0.0) and (Emergamps = 0.0) Then Emergamps := ActiveConductorDataObj.EmergAmps;
                    If (ActiveConductorDataObj.NumAmpRatings > 1) and (NumAmpRatings = 1) Then NumAmpRatings  := ActiveConductorDataObj.NumAmpRatings;
                    If (length(ActiveConductorDataObj.AmpRatings) > 1) and (length(AmpRatings) = 1)
                      Then Begin
                        setlength(AmpRatings,NumAmpRatings);
                        AmpRatings  := ActiveConductorDataObj.AmpRatings;
                      End;
                  End;
                 End
                 Else if ParamPointer=4 then
                   DoSimpleMsg('WireData Object "' + param + '" not defined. Must be previously defined.', 10103)
                 else if ParamPointer=13 then
                   DoSimpleMsg('CNData Object "' + param + '" not defined. Must be previously defined.', 10103)
                 else
                   DoSimpleMsg('TSData Object "' + param + '" not defined. Must be previously defined.', 10103);
               End;
         END;

         Case ParamPointer of
            1,4..7,11..16,20: DataChanged := TRUE;
         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     END;

  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineGeometry.MakeLike(Const LineName:String):Integer;
VAR
   OtherLineGeometry: TLineGeometryObj;
   i                : Integer;

BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherLineGeometry := Find(LineName);
   IF OtherLineGeometry<>Nil THEN
   WITH ActiveLineGeometryObj DO BEGIN
       NConds       := OtherLineGeometry.NWires;   // allocates
       FNphases     := OtherLineGeometry.FNphases;
       FSpacingType := OtherLineGeometry.FSpacingType;
       FEquivalentSpacing := OtherLineGeometry.FEquivalentSpacing;
       For i := 1 to FNConds Do FPhaseChoice^[i]  := OtherLineGeometry.FPhaseChoice^[i];
       For i := 1 to FNConds Do FCondName^[i]     := OtherLineGeometry.FCondName^[i];
       For i := 1 to FNConds Do FWireData^[i]     := OtherLineGeometry.FWireData^[i];
       For i := 1 to FNConds Do FX^[i]            := OtherLineGeometry.FX^[i];
       For i := 1 to FNConds Do FY^[i]            := OtherLineGeometry.FY^[i];
       For i := 1 to 4 Do FEqDist[i]            := OtherLineGeometry.FEqDist[i];
       For i := 1 to FNConds Do FUnits^[i]        := OtherLineGeometry.FUnits^[i];
       FLastUnit := OtherLineGeometry.FLastUnit; // Useful if template geometry uses a spacing
       DataChanged := TRUE;
       NormAmps    := OtherLineGeometry.NormAmps;
       EmergAmps   := OtherLineGeometry.EmergAmps;
       Freduce     := OtherLineGeometry.Freduce;

       UpdateLineGeometryData(activecircuit[ActiveActor].solution.Frequency );

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherLineGeometry.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in LineGeometry MakeLike: "' + LineName + '" Not Found.', 102);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineGeometry.Init(Handle:Integer; ActorID : Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TLineGeometry.Init', -1);
   Result := 0;
END;

Function TLineGeometry.Get_Code:String;  // Returns active line code string

BEGIN

  Result := TLineGeometryObj(ElementList.Active).Name;

END;

Procedure TLineGeometry.Set_Code(const Value:String);  // sets the  active LineGeometry
VAR
  LineGeometryObj:TLineGeometryObj;
BEGIN

    ActiveLineGeometryObj := Nil;
    LineGeometryObj       := ElementList.First;
    WHILE LineGeometryObj<>Nil DO BEGIN

       IF CompareText(LineGeometryObj.Name, Value)=0 THEN BEGIN
          ActiveLineGeometryObj := LineGeometryObj;
          Exit;
       END;

       LineGeometryObj := ElementList.Next;
    END;

    DoSimpleMsg('LineGeometry: "' + Value + '" not Found.', 103);

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineGeometry Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



constructor TLineGeometryObj.Create(ParClass:TDSSClass; const LineGeometryName:String);

BEGIN
     Inherited Create(ParClass);
     Name                 := LowerCase(LineGeometryName);
     DSSObjType           := ParClass.DSSClassType;

      DataChanged         := TRUE;

      FPhaseChoice        := nil;
      FCondName           := nil;
      FWireData           := nil;
      FX                  := nil;
      FY                  := nil;
      //FEqDist             := nil;
      FEquivalentSpacing  := FALSE;
      Funits              := nil;
      FLineData           := Nil;
      FSpacingType        := '';
      FCondsUser          := '';

(* was causing unnecessary allocations (was leaving dangling memory)
      Nconds      := 3;  // Allocates terminals
      FNphases    := 3;
*)
      FNconds             := 0;
      FNPhases            := 0;
//       ActiveCond  := 1;
      FActiveCond         := 1;
      FLastUnit           := UNITS_FT;
      Normamps            := 0.0;
      EmergAmps           := 0.0;
      FLineType           := 1;  // Default to OH  Line

      FReduce             := FALSE;
     {Initialize dynamic array for ratings}
      NumAmpRatings       :=  1;
      setlength(AmpRatings,NumAmpRatings);
      AmpRatings[0]       :=  NormAmps;

     InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineGeometryObj.Destroy;
BEGIN

    if Assigned(FLineData) then
      FreeAndNil(FLineData);
    FreeStringArray(FCondName, FnConds);
    Reallocmem(Fwiredata, 0);
    Reallocmem(FY, 0);
    Reallocmem(FX, 0);
    Reallocmem(Funits, 0);
    Reallocmem(FPhaseChoice, 0);
    SetLength(FEqDist, 0);

    Inherited destroy;
END;



PROCEDURE TLineGeometryObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i,j :Integer;
   cond_type :Integer;
   conductor_array :String;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
    Begin
       For i := 1 to 2 Do  Begin
         Writeln(F,'~ ',PropertyName^[i],'=',GetPropertyValue(i));
       End;
       If not FEquivalentSpacing then
       begin
         // Avoid spacing and wire arrays as the information has been dumped
         // already on each of the conductor positions.
         For j := 1 to FNConds Do Begin
           ActiveCond := j;
           Writeln(F,'~ ',PropertyName^[3],'=',GetPropertyValue(3));

           if FWireData^[FActiveCond] is TCNDataObj then cond_type := 13 // cncable
           else if FWireData^[FActiveCond] is TTSDataObj then cond_type := 14  // tscable
           else cond_type := 4;  // wire

           Writeln(F,'~ ',PropertyName^[cond_type],'=',GetPropertyValue(cond_type));
           Writeln(F,'~ ',PropertyName^[5],'=',GetPropertyValue(5));
           Writeln(F,'~ ',PropertyName^[6],'=',GetPropertyValue(6));
           Writeln(F,'~ ',PropertyName^[7],'=',GetPropertyValue(7));
         End;
       end
       else
       begin
          Writeln(F,'~ ',PropertyName^[11],'=',GetPropertyValue(11));
          if FCondsUser <> '' then  // Use the saved conductors array string as it will include None values that would get ignored otherwise.
            Writeln(F,'~ ', PropertyName^[20], '=[',FCondsUser,']')
          else
          begin
            conductor_array:= '';
            For j := 1 to FNConds Do Begin
              if FWireData^[j] is TCNDataObj then conductor_array := conductor_array + 'cndata.' + FCondName^[j] + ','  // cncable
              else if FWireData^[j] is TTSDataObj then conductor_array := conductor_array + 'tsdata.' + FCondName^[j] + ','  // tscable
              else conductor_array := conductor_array + 'wiredata.' + FCondName^[j] + ',';  // wire
            end;
            Writeln(F,'~ ', PropertyName^[20], '=[',conductor_array,']');
          end;

       end;



       For i := 8 to 10 Do  Begin
         Writeln(F,'~ ',PropertyName^[i],'=',GetPropertyValue(i));
       End;

       For i := 17 to 19 Do  Begin

         Writeln(F,'~ ',PropertyName^[i],'=',GetPropertyValue(i));
       End;

    End;

end;

function TLineGeometryObj.GetPropertyValue(Index: Integer): String;
var
  j,
  i       : Integer;
{Return Property Value for Active index}

begin

  CASE Index OF
      1   : Result       := Format('%d',[FNConds]);
      2   : Result       := Format('%d',[FNphases]);
      3   : Result       := Format('%d',[FActiveCond]);
      4,13,14: Result := FCondName^[FActiveCond];
      5   : Result       := Format('%-g',[FX^[FActiveCond]]);
      6   : Result       := Format('%-g',[FY^[FActiveCond]]);
      7   : Result       := LineUnitsStr(FUnits^[FActiveCond]);
      8   : Result       := Format('%-g',[NormAmps]);
      9   : Result       := Format('%-g',[EmergAmps]);
      12,15,16: Begin
                  Result := '[';
                  for i:= 1 to FNConds do Result := Result + FCondName^[i] + ' ';
                  Result := Result + ']';
                End;
      17  : Result := inttostr(NumAmpRatings);
      18  : Begin
              Result   :=  '[';
              for  j:= 1 to NumAmpRatings do
                Result :=  Result + floattoStrf(AmpRatings[j-1],ffgeneral,8,4) + ',';
              Result   :=  Result + ']';
            End;
      19: Result := LineTypeList.Get(FLineType);
      20: Begin  // Similar to 12,15,16 but with conductor data class prepended
            Result := '[';
            for i:= 1 to FNConds do Result := Result + FCondName^[i] + ' ';
            Result := Result + ']';
          End;
   ELSE
     // Inherited parameters
     Result     := Inherited GetPropertyValue(Index);
   END;

end;

function TLineGeometryObj.Get_FX(i:Integer) : Double;
begin
  If i <= FNConds Then Result := FX^[i] Else Result := 0.0;
end;

function TLineGeometryObj.Get_FY(i:Integer) : Double;
begin
  If i <= FNConds Then Result := FY^[i] Else Result := 0.0;
end;

function TLineGeometryObj.Get_FUnits(i:Integer) : Integer;
begin
  If i <= FNConds Then Result := FUnits^[i] Else Result := 0;
end;

function TLineGeometryObj.Get_ConductorName(i:Integer) : String;
begin
  If i <= FNConds Then Result := FCondName^[i] Else Result := '';
end;

function TLineGeometryObj.Get_ConductorData(i:Integer) : TConductorDataObj;
begin
  If i <= FNConds Then Result := FWireData^[i] Else Result := nil;
end;

function TLineGeometryObj.get_Nconds: Integer;
begin
     If Freduce Then Result := FNPhases Else Result :=FNConds;
end;

function TLineGeometryObj.Get_PhaseChoice(i: Integer): ConductorChoice;
begin
     Result := FPhaseChoice^[i];
end;

function TLineGeometryObj.Get_RhoEarth: Double;
begin
     Result := FLineData.rhoearth;
end;

function TLineGeometryObj.Get_EpsRMedium: Double;
begin
     Result := FLineData.epsrmedium;
end;

function TLineGeometryObj.Get_HeightOffset: Double;
begin
     Result := FLineData.heightOffset; // returned in user defined height units
end;

function TLineGeometryObj.Get_HeightUnit: Integer;
begin
     Result := FLineData.userHeightUnit;
end;

function TLineGeometryObj.Get_YCmatrix(f, Lngth: double;
  Units: Integer): Tcmatrix;
begin
    Result := Nil;
    If DataChanged Then UpdateLineGeometryData(f);
    If Not SolutionAbort Then Result := FLineData.YCMatrix[f, Lngth, Units];
end;

function TLineGeometryObj.Get_Zmatrix(f, Lngth: double;
  Units: Integer): Tcmatrix;
begin
   Result := Nil;
   If DataChanged Then UpdateLineGeometryData(f);
   If Not SolutionAbort Then Result := FLineData.ZMatrix[F, Lngth, Units];
end;

procedure TLineGeometryObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '3';
     PropertyValue[2]  :=  '3';
     PropertyValue[3]  :=  '1';
     PropertyValue[4]  :=  '';
     PropertyValue[5]  :=  '0';
     PropertyValue[6]  :=  '32';
     PropertyValue[7]  :=  'ft';
     PropertyValue[8]  :=  '0';
     PropertyValue[9]  :=  '0';
     PropertyValue[17] :=  '1'; // 1 season
     PropertyValue[18] :=  '[400]'; // 1 rating
     PropertyValue[19] :=  'OH'; // 1 rating

    inherited  InitPropertyValues(NumPropsThisClass);

end;


procedure TLineGeometryObj.SaveWrite(var F: TextFile);
{ Override standard SaveWrite}
{Linegeometry structure not conducive to standard means of saving}
var
   TempStr  : String;
   strPhaseChoice: String;
   j,
   iprop    :Integer;
   i        :Integer;

begin
   {Write only properties that were explicitly set in the
   final order they were actually set}
   iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
   If iProp > 0 then  Writeln(F);

   While iProp >0 Do
   Begin
      With ParentClass Do

        CASE RevPropertyIdxMap^[iProp] of
            3,11,12:  Begin   // if cond=, spacing, or wires were ever used write out arrays ...
                   For i := 1 to Fnconds Do
                   Begin
                       case PhaseChoice[i] of
                            Overhead: strPhaseChoice := 'wire';
                            ConcentricNeutral: strPhaseChoice := 'cncable';
                            TapeShield: strPhaseChoice := 'tscable';
                       else
                            strPhaseChoice := 'wire';
                       end;
                       Writeln(F, Format('~ Cond=%d %s=%s X=%.7g h=%.7g units=%s',
                                        [i, strPhaseChoice, FCondName^[i], FX^[i], FY^[i], LineUnitsStr(FUnits^[i]) ]));
                   End;
                End;
            4..7: {do Nothing}; // Ignore these properties;
            8: Writeln(F, Format('~ normamps=%.4g', [NormAmps]));
            9: Writeln(F, Format('~ emergamps=%.4g', [EmergAmps]));
            10: If FReduce then  Writeln(F, '~ Reduce=Yes');
            13..14:;   {do Nothing} // Ignore these properties;
            18: Begin
                  TempStr   :=  '[';
                  for  j:= 1 to NumAmpRatings do
                    TempStr :=  TempStr + floattoStrf(AmpRatings[j-1],ffgeneral,8,4) + ',';
                  TempStr   :=  TempStr + ']';
                  Writeln(F,'ratings=' + TempStr);
                End;
            19: Writeln(F, '~ LineType=%.4g' + LineTypeList.Get(FLineType));
        ELSE
          Writeln(F,Format('~ %s=%s', [PropertyName^[RevPropertyIdxMap^[iProp]],CheckForBlanks(PropertyValue[iProp])] ));
        END;
      iProp := GetNextPropertySet(iProp);
   End;



end;

procedure TLineGeometryObj.set_ActiveCond(const Value: Integer);
begin
  If Value>0 Then If Value <=FNconds Then Begin
     FActiveCond := Value;
     If Funits^[FactiveCond] = -1 Then Funits^[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
  End;
end;

procedure TLineGeometryObj.ChangeLineConstantsType (newPhaseChoice: ConductorChoice);
var
  newLineData: TLineConstants;
  needNew: Boolean;
begin
  newLineData := nil;
  needNew := False;
  if newPhaseChoice <> FPhaseChoice^[ActiveCond] then needNew := True;
  if not Assigned (FLineData) then
    needNew := True
  else if FNConds <> FLineData.Nconductors then
    needNew := True;

  if needNew then
    case newPhaseChoice of
      Overhead          : newLineData := TOHLineConstants.Create(FNConds);
      ConcentricNeutral : newLineData := TCNTSLineConstants.Create(FNConds);
      TapeShield        : newLineData := TCNTSLineConstants.Create(FNConds);
    end;

  if Assigned(newLineData) then begin
    if Assigned(FLineData) then begin
      newLineData.Nphases   := FLineData.Nphases;
      newLineData.rhoearth  := FLineData.rhoearth;
      newLineData.epsrmedium  := FLineData.epsrmedium;
      FreeAndNil(FLineData);
    end;
    FLineData := newLineData;
  end;
  FPhaseChoice^[ActiveCond] := newPhaseChoice;
end;

procedure TLineGeometryObj.set_Nconds(const Value: Integer);
Var i:Integer;
begin
  If Assigned(FCondName) Then  FreestringArray(FCondName, FNConds);  // dispose of old allocation

  FNconds := Value;
  If Assigned(FLineData) Then FreeAndNil(FLineData);

  {Allocations}
  Reallocmem( FWireData, Sizeof(FWireData^[1]) *FNconds);
  Reallocmem( FX,        Sizeof(FX^[1])        *FNconds);
  Reallocmem( FY,        Sizeof(FY^[1])        *FNconds);
  SetLength( FEqDist, 5 );
  Reallocmem( FUnits,    Sizeof(Funits^[1])    *FNconds);
  Reallocmem( FPhaseChoice,    Sizeof(FPhaseChoice^[1])    *FNconds);

  For i := 1 to FNconds Do
  Begin
    ActiveCond := i;
    ChangeLineConstantsType(Overhead);    // works on activecond
  End;

  FCondName := AllocStringArray(FNconds);
  {Initialize Allocations}
  For i := 1 to FNconds Do
  Begin
    FWireData^[i]  := Nil;
    FX^[i]         := 0.0;
    FY^[i]         := 0.0;
    FUnits^[i]     := -1;  // default to ft
  end;
//  For i := 1 to FNconds Do FPhaseChoice^[i] := Unknown;   // This was defined previously (ChangeLineConstantsType)
                                                            // and overrides previous allocations

  For i := 1 to 4 Do
  Begin
    FEqDist[i]         := 0.0;
  end;

  FLastUnit := UNITS_FT;

end;

procedure TLineGeometryObj.set_Nphases(const Value: Integer);
begin
  FNphases          := Value;
  FLineData.Nphases := Value;
end;

procedure TLineGeometryObj.Set_RhoEarth(const Value: Double);
begin
  FLineData.RhoEarth :=Value;
end;

procedure TLineGeometryObj.Set_EpsRMedium(const Value: Double);
begin
  FLineData.epsrmedium := Value;
end;

procedure TLineGeometryObj.Set_HeightOffset(const Value: Double);
begin
  FLineData.heightOffset := Value;
end;

procedure TLineGeometryObj.Set_HeightUnit(const Value: Integer);
begin
  FLineData.userHeightUnit := Value;
end;

procedure TLineGeometryObj.UpdateLineGeometryData(f:Double);
Var
  i               : Integer;
  LineGeomErrMsg  : String;
  cnd             : TCNDataObj;
  tsd             : TTSDataObj;
begin

  FLineData.EquivalentSpacing := FEquivalentSpacing;
  if FEquivalentSpacing then
  begin
    // Always four elements: EqDistPhPh, EqDistPhN, AvgHeightPh, AvgHeightN
    FLineData.EqDist[1, FLastUnit] := FEqDist[1];
    FLineData.EqDist[2, FLastUnit] := FEqDist[2];
    FLineData.EqDist[3, FLastUnit] := FEqDist[3] + FLineData.heightOffset * To_Meters(FLineData.userHeightUnit) * From_Meters(FLastUnit);
    FLineData.EqDist[4, FLastUnit] := FEqDist[4] + FLineData.heightOffset * To_Meters(FLineData.userHeightUnit) * From_Meters(FLastUnit);
  end;
  For i := 1 to FNconds Do Begin
    if not FEquivalentSpacing then
    begin
      FLineData.X[i, Funits^[i]] := FX^[i];
      FLineData.Y[i, Funits^[i]] := FY^[i] + FLineData.heightOffset * To_Meters(FLineData.userHeightUnit) * From_Meters(Funits^[i]);
    end;

    FLineData.radius[i, FWireData^[i].RadiusUnits] := FWireData^[i].Radius;
    FLineData.capradius[i, FWireData^[i].RadiusUnits] := FWireData^[i].capRadius;
    FLineData.GMR[i, FWireData^[i].GMRUnits]       := FWireData^[i].GMR;
    FLineData.Rdc[i, FWireData^[i].ResUnits]       := FWireData^[i].Rdc;
    FLineData.Rac[i, FWireData^[i].ResUnits]       := FWireData^[i].Rac;
    if (FWireData^[i] is TCNDataObj) then begin
      with (FLineData as TCNTSLineConstants) do begin
        cnd := (FWireData^[i] as TCNDataObj);
        CondType[i] := 1;  //CN
        EpsR[i] := cnd.EpsR;
        InsLayer[i, cnd.RadiusUnits] := cnd.InsLayer;
        DiaIns[i, cnd.RadiusUnits] := cnd.DiaIns;
        DiaCable[i, cnd.RadiusUnits] := cnd.DiaCable;
        kStrand[i] := cnd.NStrand;
        DiaStrand[i, cnd.RadiusUnits] := cnd.DiaStrand;
        GmrStrand[i, cnd.GMRUnits] := cnd.GmrStrand;
        RStrand[i, cnd.ResUnits] := cnd.RStrand;
        Semicon[i] := cnd.Semicon;
      end;
    end else if (FWireData^[i] is TTSDataObj) then
      begin
        with (FLineData as TCNTSLineConstants) do
        begin
          tsd                           := (FWireData^[i] as TTSDataObj);
          CondType[i]                   := 2;  //TS
          EpsR[i]                       := tsd.EpsR;
          InsLayer[i, tsd.RadiusUnits]  := tsd.InsLayer;
          DiaIns[i, tsd.RadiusUnits]    := tsd.DiaIns;
          DiaCable[i, tsd.RadiusUnits]  := tsd.DiaCable;
          DiaShield[i, tsd.RadiusUnits] := tsd.DiaShield;
          TapeLayer[i, tsd.RadiusUnits] := tsd.TapeLayer;
          TapeLap[i] := tsd.TapeLap;
        end;
      end;
  End;

  FLineData.Nphases := FNphases;
  DataChanged := FALSE;

  {Before we calc, check for bad conductor definitions}
  if FLineData.ConductorsInSameSpace(LineGeomErrMsg) then Begin
    Raise ELineGeometryProblem.Create('Error in LineGeometry.'+Name+': '+LineGeomErrMsg);
    SolutionAbort := TRUE;
  End Else Begin
    FLineData.Calc(f);
    If FReduce Then FLineData.Reduce; // reduce out neutrals
  End;
end;

procedure TLineGeometryObj.LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray);
var
  i, j, actualNConds, actualNPhases: Integer;
  newPhaseChoice: ConductorChoice;
begin
  // check the actual number of existing positions with conductors before allocating
  actualNConds := 0;
  actualNPhases := 0;
  For i := 1 to Spc.NWires Do
  begin
      if Wires^[i] <> nil then
      begin
        actualNConds := actualNConds + 1;
        if i <= Spc.Nphases then actualNPhases := actualNPhases + 1;
      end;

  end;
  
  NConds := actualNConds;   // allocates
  FNphases := actualNPhases;
  FSpacingType := Spc.Name;
  if FNConds > FNPhases then FReduce := True;

  newPhaseChoice := Overhead;
  for i := 1 to Spc.NWires Do begin
    if Wires[i] = nil then continue;
    if Wires[i] is TCNDataObj then newPhaseChoice := ConcentricNeutral;
    if Wires[i] is TTSDataObj then newPhaseChoice := TapeShield;
  end;
  ChangeLineConstantsType (newPhaseChoice);

  j := 0;
  For i := 1 to Spc.NWires Do
  begin
    if Wires^[i] <> nil then
    begin
      j := j + 1;
      FCondName^[j] := Wires^[i].Name;
      FWireData^[j] := Wires^[i];
      if not Spc.EquivalentSpacing then
      begin
        FX^[j] := Spc.Xcoord[i];
        FY^[j] := Spc.Ycoord[i];
        FUnits^[j] := Spc.Units;
      end;
      if ((Wires^[i].NormAmps < NormAmps) or (NormAmps = 0)) and (j <= FNPhases) then
      begin
        NormAmps    := Wires^[i].NormAmps;
        EmergAmps   := Wires^[i].EmergAmps;
      end;
    end;

  end;
  if Spc.EquivalentSpacing then
  begin
    FEqDist[1] := Spc.EqDistPhPh;
    FEqDist[2] := Spc.EqDistPhN;
    FEqDist[3] := Spc.AvgHeightPh;
    FEqDist[4] := Spc.AvgHeightN;
    FLastUnit := Spc.Units;
  end;

  FEquivalentSpacing := Spc.EquivalentSpacing;

  DataChanged := TRUE;
                                   
  // UpdateLineGeometryData will be called when we get the impedance matrix for the line.
  // No need to call it one here because this function has already set DataChanged and also
  // LoadSpacingAndWires is only ever called from TLineObj.FMakeZFromSpacing which retrieves Z
  // after calling it.

end;

end.
