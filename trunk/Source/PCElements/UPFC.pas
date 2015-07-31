unit UPFC;
{
  ----------------------------------------------------------
  Copyright (c) 2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 7-6-2015  Created from VSOURCE 

}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, Spectrum, Loadshape;

TYPE
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TUPFC = CLASS(TPCClass)
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherSource:STring):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TUPFCObj = class(TPCElement)
     private
        VRef    : Double; //Expected vooltage in the output (only magnitude)
        pf      : Double; //Expected power factor (under revision)
        Xs      : Double; //Impedance of the series Xfmr
        Sr0     : Complex; //Shift register for controller 1
        Vbin    : Complex; // Voltage at the input of the device
        Vbout   : Complex; // Voltage at the output of the device
        Tol1    : Double;   //Tolerance (dead band) specified for the controller 1
        ERR0    : Double;   //Error controller 1
        ZBase   : Double;
        Freq    : Double;
        Amps    : Double;
        Angle   : Double;
        SrcFrequency:Double;
        FphaseShift  :Double;
        ModeUPFC    : Integer;

        ScanType     : Integer;
        SequenceType : Integer;

        ShapeFactor  : Complex;
        ShapeIsActual: Boolean;
//        Procedure GetVterminalForSource;

        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);
        PROCEDURE CalcYearlyMult(Hr:double);
        Function GetinputCurr:Complex;
        Function GetOutputCurr:Complex;
        Function CalcUPFCPowers:Complex;

      public

        Z     : TCmatrix;  // Base Frequency Series Z matrix
        Zinv  : TCMatrix;
        VMag  : Double;

        constructor Create(ParClass:TDSSClass; const SourceName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;
        Procedure CalcYPrim; Override;

        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;
       Procedure GetCurrents(Curr: pComplexArray);Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        PROCEDURE InitPropertyValues(ArrayOffset:Integer); Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean); Override;
        FUNCTION  GetPropertyValue(Index:Integer):String;Override;

        // New functions

   End;

VAR
    ActiveUPFCObj:TUPFCObj;
    UPFC_class:TUPFC;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Dynamics, Utilities, Sysutils, Command;

Const NumPropsThisClass = 12;

Var CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TUPFC.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name   := 'UPFC';
     DSSClassType := PC_ELEMENT + UPFC_ELEMENT;  // UPFC  is PC Element

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
     UPFC_class := Self;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TUPFC.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TUPFC.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names
     PropertyName[1] := 'bus1';
     PropertyName[2] := 'bus2';
     PropertyName[3] := 'refkv';
     PropertyName[4] := 'pf';
     PropertyName[5] := 'frequency';
     PropertyName[6] := 'phases';
     PropertyName[7] := 'Xs';
     PropertyName[8] := 'Tol0';
     PropertyName[9] := 'Tol1';
     PropertyName[10]:= 'Enabled';
     PropertyName[11]:= 'Mode';

     // define Property help values
     PropertyHelp[1] := 'Name of bus to which the input terminal (1) is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3' +CRLF+CRLF+
                        'The UPFC object is a two-terminal voltage source (thevenin equivalent).';
     PropertyHelp[2] := 'Name of bus to which the output terminal (2) is connected.'+CRLF+'bus2=busname'+CRLF+'bus2=busname.1.2.3' +CRLF+CRLF+
                        'The UPFC object is a two-terminal voltage source (thevenin equivalent).' ;
     PropertyHelp[3] := 'Base Voltage expected at the output of the UPFC'+ CRLF +
                        '"refkv=0.24"';
     PropertyHelp[4] := 'Power factor expected';
     PropertyHelp[5] := 'UPFC working frequency.  Defaults to system default base frequency.';
     PropertyHelp[6] := 'Number of phases.  Defaults to 3.';
     PropertyHelp[7] := 'Impedance of the series transformer of the UPFC';
     PropertyHelp[8] := 'Tolerance in percentage for the series PI controller'+CRLF+
                        'Tol0=0.02 is the format used to define 2% tolerance (Default)';
     PropertyHelp[9] := 'Tolerance in percentage for the shunt PI controller'+CRLF+
                        'Tol0=0.02 is the format used to define 2% tolerance (Default)';
     PropertyHelp[10]:= 'Inherited property from ActiveObject';
     PropertyHelp[11]:= 'Integer used to define the control mode of the UPFC: 0=Off, 1=Voltage regulator, 2=Phase angle regulator';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Name of harmonic spectrum for this source.  Default is "defaultUPFC", which is defined when the DSS starts.';

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TUPFC.NewObject(const ObjName:String):Integer;
Begin
    // Make a new voltage source and add it to UPFC class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TUPFCObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TUPFC.Edit:Integer;
VAR
   ParamPointer : Integer;
   ParamName,
   Param        : String;
   ZTemp        : Complex;

Begin
  // continue parsing with contents of Parser
  ActiveUPFCObj            := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveUPFCObj;

  Result := 0;

  WITH ActiveUPFCObj DO Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param     := Parser.StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "UPFC.'+Name+'"', 320);
            1: SetBus(1,param);  // special handling of Bus 1
            2: SetBus(2,param);     // special handling of Bus 2
            3: VRef     := Parser.DblValue; // kv Output reference
            4: pf       := Parser.DblValue; // pu
            5: Freq     := Parser.DblValue; // Freq
            6: Begin
                 Nphases   := Parser.Intvalue; // num phases
                 NConds    := Fnphases;  // Force Reallocation of terminal info
               End;
            7: Xs       := Parser.DblValue; // Xs
            9: Tol1     := Parser.DblValue; // Tolerance Ctrl 2
            10:enabled  := InterpretYesNo(Param);
            11:ModeUPFC := Parser.IntValue;

         ELSE
            ClassEdit(ActiveUPFCObj, ParamPointer - NumPropsThisClass)
         End;

         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     RecalcElementData;
     YPrimInvalid := True;
  End;

End;

//----------------------------------------------------------------------------
Function TUPFC.MakeLike(Const OtherSource:String):Integer;
VAR
   OtherUPFC :TUPFCObj;
   i :Integer;

Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherUPFC := Find(OtherSource);
   IF OtherUPFC<>Nil THEN
   WITH ActiveUPFCObj DO Begin

       IF Fnphases <> OtherUPFC.Fnphases THEN Begin
           Nphases := OtherUPFC.Fnphases;
           NConds  := Fnphases;  // Forces reallocation of terminal stuff

           Yorder := Fnconds * Fnterms;
           YPrimInvalid := True;

           IF Z<>nil    THEN Z.Free;
           IF Zinv<>nil THEN Zinv.Free;

           Z    := TCmatrix.CreateMatrix(Fnphases);
           Zinv := TCMatrix.CreateMatrix(Fnphases);
       End;

       Z.CopyFrom(OtherUPFC.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
       VRef      := OtherUPFC.VRef;
       pf        := OtherUPFC.pf;
       Xs        := OtherUPFC.Xs;
       Tol1      := OtherUPFC.Tol1;
       ZBase     := OtherUPFC.ZBase;
       Freq      := OtherUPFC.Freq;
       Enabled   := OtherUPFC.Enabled;
       ModeUPFC  := OtherUPFC.ModeUPFC;

       ClassMakeLike(OtherUPFC);

       For i := 1 to ParentClass.NumProperties Do FPropertyValue[i] := OtherUPFC.FPropertyValue[i];
       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in UPFC MakeLike: "' + OtherSource + '" Not Found.', 322);

End;

//----------------------------------------------------------------------------
Function TUPFC.Init(Handle:Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TUPFC.Init', -1);
   Result := 0;
End;

//=============================================================================
Constructor TUPFCObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

     Nphases  := 1;
     Fnconds  := 2;
     Nterms   := 2;   // A 2-terminal device
     Z        := nil;
     Zinv     := nil;
     {Basefrequency := 60.0;} // set in base class
     VRef     := 0.24;
     pf       := 1.0;
     Xs       := 0.7540; // Xfmr series inductace 2e-3 H
     Sr0      := Cmplx(0,0);
     Tol1     := 0.02;
     Freq     := 60.0;
     enabled  := True;
     ModeUPFC := 1;

     InitPropertyValues(0);


     Yorder := Fnterms * Fnconds;
     RecalcElementData;

End;


//=============================================================================
Destructor TUPFCObj.Destroy;
Begin
    Z.Free;
    Zinv.Free;

    Inherited Destroy;
End;

//=============================================================================
Procedure TUPFCObj.RecalcElementData;
VAR
   Zs, Zm, Z1, Z2, Z0     : Complex;
   Value, Value1, Value2 : Complex;
   Calpha1, Calpha2      : Complex;
   i, j    : Integer;

   Factor : Double;

//   Rs, Xs, Rm, Xm : Double;

   Begin
    IF Z    <> nil THEN Z.Free;
    IF Zinv <> nil THEN Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z    := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    If   FNPhases = 1 THEN Factor := 1.0 ELSE Factor := SQRT3;


    {Update property Value array}
     { Don't change a specified value; only computed ones}

         Z1 := Cmplx(0, Xs);
 {        Z2 := Cmplx(0, Xs); //We will see if these are necessary later
         Z0 := Cmplx(0, Xs);
 }
         // Diagonals  (all the same)
         Value  := Z1;   // Z1 + Z2 + Z0
         FOR i := 1 to Fnphases  Do Z.SetElement(i, i, Value);

         // Off-Diagonals - Apparently there are no off-Diagonals

   Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

End;

//=============================================================================
Procedure TUPFCObj.CalcYPrim;

Var
   Value :Complex;
   i, j  :Integer;
   FreqMultiplier:Double;

Begin

 // Build only YPrim Series
     IF YPrimInvalid THEN Begin
       IF YPrim_Series <> nil Then YPrim_Series.Free;
       YPrim_Series := TcMatrix.CreateMatrix(Yorder);
       IF YPrim <> nil Then YPrim.Free;
       YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Series.Clear;
          YPrim.Clear;
     End;

     FYprimFreq := ActiveCircuit.Solution.Frequency  ;
     FreqMultiplier := FYprimFreq / BaseFrequency;

     { Put in Series RL Adjusted for frequency }
     For i := 1 to Fnphases Do Begin
         For j := 1 to Fnphases Do Begin
           Value    := Z.GetElement(i, j);
           Value.im := Value.im * FreqMultiplier;  {Modify from base freq}
           Zinv.SetElement(i, j, value);
         End;
     End;

     Zinv.Invert;  {Invert in place}

     If Zinv.InvertError>0 Then
      Begin       {If error, put in Large series conductance}
        DoErrorMsg('TUPFCObj.CalcYPrim', 'Matrix Inversion Error for UPFC "' + Name + '"',
                   'Invalid impedance specified. Replaced with small resistance.', 325);
        Zinv.Clear;
        For i := 1 to Fnphases Do Zinv.SetElement(i, i, Cmplx(1.0/EPSILON, 0.0));
      End;

   // YPrim_Series.CopyFrom(Zinv);

     For i := 1 to FNPhases do Begin
       For j := 1 to FNPhases do Begin
          Value := Zinv.GetElement(i, j);
          YPrim_series.SetElement(i, j, Value);
          YPrim_series.SetElement(i + FNPhases, j + FNPhases, Value);
          //YPrim_series.SetElemsym(i + FNPhases, j, CNegate(Value))
          YPrim_series.SetElement(i, j+Fnphases, Cnegate(Value));
          YPrim_series.SetElement(i+Fnphases, j, Cnegate(Value));
       End;
     End;

     YPrim.CopyFrom(YPrim_Series);
     
     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
     Inherited CalcYPrim;

     YPrimInvalid := False;

End;

//=============================================================================

//===========================================================================

Function TUPFCObj.InjCurrents:Integer;

Begin

   GetInjCurrents(InjCurrent);

{This is source injection}

   Result := Inherited InjCurrents; // Add into system array

End;

//===========================================================================
//Taken from ISources due to the kind of model
//===========================================================================
//Calculates the output current for the UPFC device
{
        Vbin   Xs  Vbout
     <---*--=======--*--->
         |           |
 I input ^           ^ I output
         |           |
}

Function TUPFCObj.GetoutputCurr:Complex;

VAr
   Error:Double;
   TError:Double;
   Vpolar:polar;
   Ipolar:polar;
   VTemp:complex;
   Itemp:complex;

Begin

  TRY
    WITH ActiveCircuit.Solution Do

    case ModeUPFC of
        0:  Itemp:=cmplx(0,0); //UPFC off
        1:  Begin              //UPFC as a voltage regulator
              Vpolar:=ctopolar(Vbout);
              Error:=1-abs(Vpolar.mag/(VRef*1000));
              if Error > Tol1 then
                Begin
                  VTemp   :=  csub(Vbout,Vbin);
                  Vpolar  :=  ctopolar(Vbin);
                  TError  :=  (VRef*1000)-Vpolar.mag;
                  if TError >= 24.0 then TError:=24;
                  Vpolar  :=  topolar(TError,Vpolar.ang);
                  VTemp   :=  csub(ptocomplex(Vpolar),VTemp); //Calculates Vpq
                  Itemp   :=  cdiv(VTemp,cmplx(0,Xs));
                  SR0     :=  ITemp;
                End
                else ITemp:=SR0;
            end;
        2:  Itemp:=cmplx(0,0); //UPFC phase angle regulator
    end;
    Result := Itemp;
  EXCEPT
      DoSimpleMsg('Error computing current for Isource.'+Name+'. Check specification. Aborting.', 334);
      IF In_Redirect Then Redirect_Abort := TRUE;
  END;
End;
//============================================================================

Function TUPFCObj.CalcUPFCPowers:Complex;

VAr
   IUPFC:complex;

Begin
      IUPFC:=cdiv(csub(Vbin,Vbout),cmplx(0,Xs));
      Result := cmul(Vbin,conjg(IUPFC));
End;


//============================================================================
//Calculates the input current to absorb reactive power from UPFC
{
        Vbin   Xs  Vbout
     <---*--=======--*--->
         |           |
 I input ^           ^ I output
         |           |
}

Function TUPFCObj.GetinputCurr:Complex;

VAr
   Power,Currin:complex;
   S,QIdeal:double;
Begin

  TRY
    WITH ActiveCircuit.Solution Do
  {Get first Phase Current}
      case ModeUPFC of
          0:  CurrIn:=cmplx(0,0);
          1:  begin
                  CurrIn:=cnegate(SR0);
              end;
          2: Begin
                  Power:=CalcUPFCPowers;
                  S:=abs(Power.re)/pf;
                  QIdeal:=Power.im-sqrt(1-pf*pf)*S;   //This is the expected compensating reactive power
                  CurrIn:=conjg(cdiv(cmplx(0,QIdeal),Vbin)); //Q in terms of current  *** conjg
            End;
      end;
      if True then begin  // The UPFC is active and the control action takes place

      end
      else begin
        CurrIn:=cmplx(0,0);
      end;
      Result := CurrIn;
  EXCEPT
      DoSimpleMsg('Error computing current for Isource.'+Name+'. Check specification. Aborting.', 334);
      IF In_Redirect Then Redirect_Abort := TRUE;
  END;

End;
//===========================================================================
Procedure TUPFCObj.GetInjCurrents(Curr:pComplexArray);

{Fill Up an array of injection currents}

VAR
   i:Integer;
Begin

     WITH ActiveCircuit.solution DO  Begin
        for i := 1 to fnphases do
        begin
        Vbin  :=  NodeV^[NodeRef^[i]];           //Gets voltage at the input of UPFC Cond i
        Vbout :=  NodeV^[NodeRef^[i+fnphases]]; //Gets voltage at the output of UPFC Cond i
//      these functions were modified to follow the UPFC Dynamic
//      (Different from VSource)
        Curr^[i+fnphases]:= GetoutputCurr;
        Curr^[i] := GetinputCurr;
        end;
     End;
End;

//===========================================================================
Procedure TUPFCObj.GetCurrents(Curr: pComplexArray);

VAR
   i:Integer;

Begin
  TRY
   WITH    ActiveCircuit.Solution
   DO Begin
     //FOR i := 1 TO (Nterms * NConds) DO Vtemp^[i] := V^[NodeRef^[i]];
     // This is safer    12/7/99
       FOR     i := 1 TO Yorder DO  Vterminal^[i] := NodeV^[NodeRef^[i]];

//       YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

       GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
      // Add Together  with yprim currents
//       FOR i := 1 TO Yorder DO Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);
//       Alternative for incorporating the calculated currents
        FOR i := 1 TO Yorder DO Curr^[i] := ComplexBuffer^[i];
   End;  {With}
  EXCEPT
    On E: Exception
    Do DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element.', 327);
  End;

End;


//=============================================================================
Procedure TUPFCObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i,j:Integer;
   c:Complex;

Begin
    Inherited DumpProperties(F,Complete);

    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete Then Begin
        Writeln(F);
        Writeln(F,'BaseFrequency=',BaseFrequency:0:1);
        Writeln(F,'VMag=',VMag:0:2);
        Writeln(F,'Z Matrix=');
        FOR i := 1 to Fnphases DO Begin
          FOR j := 1 to i DO Begin
              c := Z.GetElement(i,j);
              Write(F, Format('%.8g +j %.8g ',[C.re, C.im ]));
          End;
          Writeln(F);
        End;
    End;

End;


//=============================================================================
procedure TUPFCObj.InitPropertyValues(ArrayOffset: Integer);
begin

     {PropertyValue Allocated in DSSObject.Create}
     PropertyValue[1]  := GetBus(1);
     PropertyValue[2]  := GetBus(2);
     PropertyValue[3]  := '0.24';
     PropertyValue[4]  := '1';
     PropertyValue[5]  := Format('%d',[Round(ActiveCircuit.Fundamental)]);
     PropertyValue[6]  := '3';
     PropertyValue[7]  := '0.7540';  // 2mH inductance
     PropertyValue[8]  := '0.02';
     PropertyValue[9]  := '0.02';
     PropertyValue[10] := 'True';
     PropertyValue[11] := 'False';

     inherited  InitPropertyValues(NumPropsThisClass);

end;

//=============================================================================
function TUPFCObj.GetPropertyValue(Index: Integer): String;
begin
        Case Index of
          1 : Result  := GetBus(1);
          2 : Result  := GetBus(2);
          3 : Result  := Format('%-.5g',[VRef]);
          4 : Result  := Format('%-.5g',[pf]);
          5 : Result := Format('%-.5g',[Freq]);
          7 : Result := Format('%-.5g',[Xs]);
          9 : Result := Format('%-.5g',[Tol1]);
          10: If Enabled  Then Result:='True' Else Result := 'False';

        Else
          Result := Inherited GetPropertyValue(Index);
        End;
end;


//----------------------------------------------------------------------------
Procedure TUPFCObj.CalcDailyMult(Hr:Double);

Begin
{     IF DailyShapeObj <> Nil THEN
       Begin
         ShapeFactor   := DailyShapeObj.GetMult(Hr);
         ShapeIsActual := DailyShapeObj.UseActual;
       End
     ELSE ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;  // Default to no daily variation}
End;


//----------------------------------------------------------------------------
Procedure TUPFCObj.CalcDutyMult(Hr:double);

Begin
{     IF DutyShapeObj <> Nil THEN
       Begin
           ShapeFactor   := DutyShapeObj.GetMult(Hr);
           ShapeIsActual := DutyShapeObj.UseActual;
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified}
End;

//----------------------------------------------------------------------------
Procedure TUPFCObj.CalcYearlyMult(Hr:double);

Begin
{Yearly curve is assumed to be hourly only}
{     IF   YearlyShapeObj<>Nil THEN Begin
           ShapeFactor   := YearlyShapeObj.GetMult(Hr);
           ShapeIsActual := YearlyShapeObj.UseActual;
     End
     ELSE ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;   // Defaults to no variation}
End;

//=============================================================================
procedure TUPFCObj.MakePosSequence;

Var
        S:String;
begin
 {
        S :='Phases=1 ';
        S := S + Format('BasekV=%-.5g ', [kVbase/SQRT3]);
        S := S + Format('R1=%-.5g ', [R1]);
        S := S + Format('X1=%-.5g ', [X1]);

        Parser.CmdString := S;
        Edit;

        inherited;
 }
end;



initialization

   CDOUBLEONE := CMplx(1.0, 1.0);
end.
