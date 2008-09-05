unit LineCode;

interface

{The Linecode object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LineCode Class.
 This sets the active Linecode object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.

 }

USES
   Command, DSSClass, DSSObject, UcMatrix;


TYPE

   TLineCode = class(TDSSClass)
     private
       SymComponentsChanged:Boolean;
       MatrixChanged:Boolean;

       Function Get_Code:String;  // Returns active line code string
       Procedure Set_Code(const Value:String);  // sets the  active linecode

       Procedure SetZ1Z0(i:Integer; Value:Double);
       Procedure SetUnits(Const s:String);  // decode units specification

       Procedure DoMatrix(i:Integer);  // set impedances as matrices
       
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const LineName:String):Integer;  Override;
     public
       
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       // Set this property to point ActiveLineCodeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TLineCodeObj = class(TDSSObject)
     private

        Procedure Set_NPhases(Value:Integer);
        Procedure DoKronReduction;

      public
        FNPhases:Integer;

        SymComponentsModel,
        ReduceByKron:Boolean;

        Z,         // Base Frequency Series Z matrix
        Zinv,
        YC:    TCMatrix;  // Shunt capacitance matrix at Base frequency.

        BaseFrequency:Double;

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
        rho:   Double;

        Units:Integer;  {See LineUnits}

        constructor Create(ParClass:TDSSClass; const LineCodeName:String);
        destructor Destroy; override;
        Property NumPhases:Integer Read FNPhases Write Set_Nphases;
        Procedure CalcMatricesFromZ1Z0;

        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

VAR
   ActiveLineCodeObj:TLineCodeObj;

implementation

USES  ParserDel,  DSSGlobals, Sysutils, Ucomplex, Arraydef, Utilities, LineUnits;

Const      NumPropsThisClass = 21;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLineCode.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name := 'LineCode';
     DSSClassType := DSS_OBJECT;
     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TLineCode.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineCode.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     PropertyName[1] := 'nphases';
     PropertyName[2] := 'r1';
     PropertyName[3] := 'x1';
     PropertyName[4] := 'r0';
     PropertyName[5] := 'x0';
     PropertyName[6] := 'c1';
     PropertyName[7] := 'c0';
     PropertyName[8] := 'units';
     PropertyName[9] := 'rmatrix';
     PropertyName[10] := 'xmatrix';
     PropertyName[11] := 'cmatrix';
     PropertyName[12] := 'baseFreq';
     PropertyName[13] := 'normamps';
     PropertyName[14] := 'emergamps';
     PropertyName[15] := 'faultrate';
     PropertyName[16] := 'pctperm';
     PropertyName[17] := 'repair';
     PropertyName[18] := 'Kron';
     PropertyName[19] := 'Rg';
     PropertyName[20] := 'Xg';
     PropertyName[21] := 'rho';


     PropertyHelp[1] := 'Number of phases in the line this line code data represents.  Setting this property reinitializes the line code.  Impedance matrix is reset for default symmetrical component.';
     PropertyHelp[2] := 'Positive-sequence Resistance, ohms per unit length.  See also Rmatrix.';
     PropertyHelp[3] := 'Positive-sequence Reactance, ohms per unit length.  See also Xmatrix';
     PropertyHelp[4] := 'Zero-sequence Resistance, ohms per unit length.';
     PropertyHelp[5] := 'Zero-sequence Reactance, ohms per unit length.';
     PropertyHelp[6] := 'Positive-sequence capacitance, nf per unit length. See also Cmatrix.';
     PropertyHelp[7] := 'Zero-sequence capacitance, nf per unit length.';
     PropertyHelp[8] := 'One of (ohms per ...) {none|mi|km|kft|m|me|ft|in|cm}.  Default is none; assumes units agree with length units' +
                    'given in Line object';
     PropertyHelp[9] := 'Resistance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. '+
                     'May be used to specify the impedance of any line configuration.  For balanced line models, you may '+
                     'use the standard symmetrical component data definition instead.';
     PropertyHelp[10] := 'Reactance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. '+
                     'May be used to specify the impedance of any line configuration.  For balanced line models, you may '+
                     'use the standard symmetrical component data definition instead.';
     PropertyHelp[11] := 'Nodal Capacitance matrix, lower triangle, nf per unit length.Order of the matrix is the number of phases. '+
                     'May be used to specify the shunt capacitance of any line configuration.  For balanced line models, you may '+
                     'use the standard symmetrical component data definition instead.';
     PropertyHelp[12] := 'Frequency at which impedances are specified.';
     PropertyHelp[13] := 'Normal ampere limit on line.  This is the so-called Planning Limit. It may also be '+
                     'the value above which load will have to be dropped in a contingency.  Usually about '+
                     '75% - 80% of the emergency (one-hour) rating.';
     PropertyHelp[14] := 'Emergency ampere limit on line (usually one-hour rating).';
     PropertyHelp[15] := 'Number of faults per unit length per year.';
     PropertyHelp[16] := 'Percentage of the faults that become permanent.';
     PropertyHelp[17] := 'Hours to repair.';
     PropertyHelp[18] := 'Kron = Y/N. Default=N.  Perform Kron reduction on the impedance matrix after it is formed, reducing order by 1. ' +
                         'Do this only on initial definition after matrices are defined. Ignored for symmetrical components.';
     PropertyHelp[19] := 'Carson earth return resistance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. Default=0';
     PropertyHelp[20] := 'Carson earth return reactance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. Default=0';
     PropertyHelp[21] := 'Default=100 meter ohms.  Earth resitivity used to compute earth correction factor.';


     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineCode.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit Do
   Begin
    ActiveDSSObject := TLineCodeObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
   End;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineCode.SetUnits(Const s:String);
// decodes the units string and sets the Units variable

BEGIN
      ActiveLineCodeObj.Units := GetUnitsCode(S);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineCode.SetZ1Z0(i:Integer; Value:Double);
// set symmetrical component impedances and a flag to indicate they were changed
BEGIN

   SymComponentsChanged := True;
   

   WITH ActiveLineCodeObj DO
   Begin
     SymComponentsModel := TRUE;
     CASE i OF
       1: R1 := Value;
       2: X1 := Value;
       3: R0 := Value;
       4: X0 := Value;
       5: C1 := Value;
       6: C0 := Value;
     ELSE
     END;
   End;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineCode.DoMatrix(i:Integer);

VAR
    OrderFound, Norder,j:Integer;
    MatBuffer:pDoubleArray;
    Zvalues:pComplexArray;
    Factor:Double;

BEGIN
   WITH ActiveLineCodeObj DO BEGIN
     MatrixChanged := True;
     MatBuffer := Allocmem(Sizeof(double)*FNphases*FNphases);
     OrderFound := Parser.ParseAsSymMatrix(FNphases, MatBuffer);

     If OrderFound>0 THEN    // Parse was successful
     CASE i OF
       1: BEGIN    {R}
            ZValues := Z.GetValuesArrayPtr(Norder);
            IF Norder=FNphases THEN
            FOR j := 1 to FNphases*FNphases DO ZValues^[j].Re := MatBuffer^[j];
          END;
       2: BEGIN   {X}
            ZValues := Z.GetValuesArrayPtr(Norder);
            IF Norder=FNphases THEN
            FOR j := 1 to FNphases*FNphases DO ZValues^[j].im := MatBuffer^[j];
          END;
       3: BEGIN    {YC Matrix}
            Factor := TwoPi * BaseFrequency  * 1.0e-9;
            ZValues := YC.GetValuesArrayPtr(Norder);
            IF Norder=FNphases THEN
            FOR j := 1 to FNphases*FNphases DO ZValues^[j].im := Factor*MatBuffer^[j];
          END;
     ELSE                
     END;

     Freemem(MatBuffer, Sizeof(double)*FNphases*FNphases);
   END;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineCode.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveLineCodeObj := ElementList.Active;
  ActiveDSSObject := ActiveLineCodeObj;
  SymComponentsChanged := False;
  MatrixChanged := False;
  ActiveLineCodeObj.ReduceByKron := FALSE;  // Allow all matrices to be computed it raw form

  WITH ActiveLineCodeObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 101);
            1: Numphases := Parser.IntValue;  // Use property value to force reallocations
            2: SetZ1Z0(1, Parser.Dblvalue);  {R1}
            3: SetZ1Z0(2, Parser.Dblvalue);  {X0}
            4: SetZ1Z0(3, Parser.Dblvalue);  {R1}
            5: SetZ1Z0(4, Parser.Dblvalue);  {X0}
            6: SetZ1Z0(5, Parser.Dblvalue * 1.0e-9); {C1}   // Convert from nano to farads
            7: SetZ1Z0(6, Parser.Dblvalue * 1.0e-9); {C0}
            8: SetUnits(Param);
            9: {Rmatrix} DoMatrix(1);
           10: {Xmatrix} DoMatrix(2);
           11: {Cmatrix} DoMatrix(3);
           12: BaseFrequency := Parser.DblValue;
           13: NormAmps      := Parser.Dblvalue;
           14: EmergAmps     := Parser.Dblvalue;
           15: FaultRate     := Parser.Dblvalue;
           16: PctPerm       := Parser.Dblvalue;
           17: HrsToRepair   := Parser.Dblvalue;
           18: ReduceByKron  := InterpretYesNo(Param);
           19: Rg := Parser.DblValue;
           20: Xg := Parser.DblValue;
           21: rho := Parser.DblValue;
         ELSE
           ClassEdit(ActiveLineCodeObj, Parampointer - NumPropsThisClass)
         END;

         CASE ParamPointer OF
             9..11: SymComponentsModel := FALSE;
         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END;

     IF SymComponentsModel THEN CalcMatricesFromZ1Z0;
     IF ReduceByKron and Not SymComponentsModel Then DoKronReduction;
     IF MatrixChanged THEN BEGIN
        Zinv.Copyfrom(Z);
        Zinv.Invert;
     END;
  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineCode.MakeLike(Const LineName:String):Integer;
VAR
   OtherLineCode:TLineCodeObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherLineCode := Find(LineName);
   IF OtherLineCode<>Nil THEN
   WITH ActiveLineCodeObj DO BEGIN

       IF FNPhases <> OtherLineCode.FNphases THEN BEGIN
         FNphases := OtherLineCode.FNphases;

         IF Z<>nil    THEN Z.Free;
         IF Zinv<>nil THEN Zinv.Free;
         IF Yc<>nil   THEN Yc.Free;

         Z    := TCmatrix.CreateMatrix(FNphases);
         Zinv := TCMatrix.CreateMatrix(FNphases);
         Yc   := TCMatrix.CreateMatrix(FNphases);
       END;

       Z.CopyFrom(OtherLineCode.Z);
       Zinv.CopyFrom(OtherLineCode.Zinv);
       Yc.CopyFrom(OtherLineCode.Yc);
       BaseFrequency := OtherLineCode.BaseFrequency;
       R1 := OtherLineCode.R1;
       X1 := OtherLineCode.X1;
       R0 := OtherLineCode.R0;
       X0 := OtherLineCode.X0;
       C1 := OtherLineCode.C1;
       C0 := OtherLineCode.C0;
       Rg := OtherLineCode.Rg;
       Xg := OtherLineCode.Xg;
       rho := OtherLineCode.rho;
       NormAmps := OtherLineCode.NormAmps;
       EmergAmps := OtherLineCode.EmergAmps;
       FaultRate := OtherLineCode.FaultRate;
       PctPerm := OtherLineCode.PctPerm;
       HrsToRepair := OtherLineCode.HrsToRepair;

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherLineCode.PropertyValue[i];
       Result := 1;
   END
   ELSE  DoSimpleMsg('Error in Line MakeLike: "' + LineName + '" Not Found.', 102);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLineCode.Init(Handle:Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TLineCode.Init', -1);
   REsult := 0;
END;

Function TLineCode.Get_Code:String;  // Returns active line code string

BEGIN

  Result := TlineCodeObj(ElementList.Active).Name;

END;

Procedure TLineCode.Set_Code(const Value:String);  // sets the  active linecode
VAR
  LineCodeObj:TLineCodeObj;
BEGIN

    ActiveLineCodeObj := Nil;
    LineCodeObj := ElementList.First;
    WHILE LineCodeObj<>Nil DO BEGIN

       IF CompareText(LineCodeObj.Name, Value)=0 THEN BEGIN
          ActiveLineCodeObj := LineCodeObj;
          Exit;
       END;

       LineCodeObj := ElementList.Next;
    END;

    DoSimpleMsg('Linecode: "' + Value + '" not Found.', 103);

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineCode Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLineCodeObj.Create(ParClass:TDSSClass; const LineCodeName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(LineCodeName);
     DSSObjType := ParClass.DSSClassType;

     FNPhases := 3;  // Directly set conds and phases
     R1 := 0.0580;  //ohms per 1000 ft
     X1 := 0.1206;
     R0 := 0.1784;
     X0 := 0.4047;
     C1 := 3.4e-9;  // nf per 1000ft
     C0 := 1.6e-9;
     Z    := nil;
     Zinv := nil;
     Yc   := nil;
     Basefrequency := 60.0;
     Units := UNITS_NONE;  // default to none  (no conversion)
     Normamps := 400.0;
     EmergAmps := 600.0;
     PctPerm := 20.0;
     FaultRate := 0.1;

     Rg := 0.0;
     Xg := 0.0;
     rho := 100.0;

     SymComponentsModel := TRUE;
     ReduceByKron := FALSE;
     CalcMatricesFromZ1Z0;  // put some reasonable values in

     InitPropertyValues(0);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineCodeObj.Destroy;
BEGIN
    Z.Free;
    Zinv.Free;
    Yc.Free;
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineCodeObj.Set_NPhases(Value:Integer);
// Set the number of phases and reallocate phase-sensitive arrays
// Need to preserve values in Z matrices

BEGIN
    If Value>0 THEN BEGIN
      IF FNphases <> Value THEN BEGIN    // If size is no different, we don't need to do anything
        FNPhases := Value;
        // Put some reasonable values in these matrices
        CalcMatricesFromZ1Z0;  // reallocs matrices
      END;
    END;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLineCodeObj.CalcMatricesFromZ1Z0;
VAR
   Zs,Zm,Ys, Ym, Ztemp:Complex;
   i,j:Integer;
   Yc1, Yc0, OneThird: double;

BEGIN
    IF Z<>nil    THEN Z.Free;
    IF Zinv<>nil THEN Zinv.Free;
    IF Yc<>nil   THEN Yc.Free;

    // For a line, nphases = ncond, for now
    Z    := TCmatrix.CreateMatrix(FNphases);
    Zinv := TCMatrix.CreateMatrix(FNphases);
    Yc   := TCMatrix.CreateMatrix(FNphases);

    OneThird := 1.0/3.0;  // Do this to get more precision in next few statements

    Ztemp := CmulReal(cmplx(R1,X1),2.0);
    Zs := CmulReal(CAdd(Ztemp, Cmplx(R0, X0)), OneThird);
    Zm := CmulReal(Csub(cmplx(R0, X0), Cmplx(R1, X1)), OneThird);

    Yc1 := TwoPi * BaseFrequency * C1;
    Yc0 := TwoPi * BaseFrequency * C0;

    Ys := CMulReal(Cadd(CMulReal(Cmplx(0.0, Yc1), 2.0), Cmplx(0.0, Yc0)), OneThird);
    Ym := CmulReal(Csub(cmplx(0.0, Yc0), Cmplx(0.0, Yc1)), OneThird);

    FOR i := 1 to FNphases DO BEGIN
       Z.SetElement(i,i, Zs);
       Yc.SetElement(i,i, Ys);
       FOR j := 1 to i-1 DO BEGIN
           Z.SetElemsym(i,j, Zm);
           Yc.SetElemsym(i,j, Ym);
       END;
    END;
    Zinv.Copyfrom(Z);
    Zinv.Invert;
END;

PROCEDURE TLineCodeObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i,j :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
    Begin

        Writeln(F,'~ ',PropertyName^[1],'=',FNphases:0);
        Writeln(F,'~ ',PropertyName^[2],'=',R1:0:5);
        Writeln(F,'~ ',PropertyName^[3],'=',X1:0:5);
        Writeln(F,'~ ',PropertyName^[4],'=',R0:0:5);
        Writeln(F,'~ ',PropertyName^[5],'=',X0:0:5);
        Writeln(F,'~ ',PropertyName^[6],'=',C1 * 1.0e9:0:5);
        Writeln(F,'~ ',PropertyName^[7],'=',C0 * 1.0e9:0:5);
        Writeln(F,'~ ',PropertyName^[8],'=',PropertyValue[8]);
        Write(F,'~ ',PropertyName^[9],'=','"');
           FOR i := 1 to FNPhases DO Begin
             FOR j := 1 to FNphases DO Begin
                 Write(F, Z.GetElement(i,j).re:0:5,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');
        Write(F,'~ ',PropertyName^[10],'=','"');
           FOR i := 1 to FNPhases DO Begin
             FOR j := 1 to FNphases DO Begin
                 Write(F, Z.GetElement(i,j).im:0:5,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');
        Write(F,'~ ',PropertyName^[11],'=','"');
           FOR i := 1 to FNPhases DO Begin
             FOR j := 1 to FNphases DO Begin
                 Write(F, (Yc.GetElement(i,j).im/TwoPi/BaseFrequency * 1.e9):0:2,' ');
             End;
             Write(F,'|');
           End;
           Writeln(F,'"');


         For i := 12 to NumProperties Do
         Begin
            Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
         End;

     End;

end;

procedure TLineCodeObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '3'; // 'nphases';
     PropertyValue[2] :=  '.058'; // 'r1';
     PropertyValue[3] :=  '.1206'; // 'x1';
     PropertyValue[4] :=  '0.1784'; // 'r0';
     PropertyValue[5] :=  '0.4047'; // 'x0';
     PropertyValue[6] :=  '3.4'; // 'c1';
     PropertyValue[7] :=  '1.6'; // 'c0';
     PropertyValue[8] :=  'none'; // 'units';
     PropertyValue[9] :=  ''; // 'rmatrix';
     PropertyValue[10] := ''; // 'xmatrix';
     PropertyValue[11] := ''; // 'cmatrix';
     PropertyValue[12] := '60'; // 'baseFreq';
     PropertyValue[13] :=  '400'; // 'normamps';
     PropertyValue[14] :=  '600'; // 'emergamps';
     PropertyValue[15] :=  '0.1'; // 'faultrate';
     PropertyValue[16] :=  '20'; // 'pctperm';
     PropertyValue[17] :=  '3'; // 'Hrs to repair';
     PropertyValue[18] :=  'N'; // 'Kron';
     PropertyValue[19] :=  '0'; // 'Rg';
     PropertyValue[20] :=  '0'; // 'Xg';
     PropertyValue[21] :=  '100'; // 'rho';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TLineCodeObj.DoKronReduction;
Var
        NewZ, NewYC : TcMatrix;

begin

        NewZ := Z.Kron ;       // Perform Kron Reductions into temp space
        NewYC := YC.Kron ;

        // Reallocate into smaller space   if Kron was successful


        If NewZ<>Nil Then Begin
            Numphases :=NewZ.order;

            // Get rid of Z and YC and replace

            Z.Free;
            YC.Free;

            Z := NewZ;
            YC := NewYC;
        End;

end;



end.
