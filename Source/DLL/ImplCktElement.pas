unit ImplCktElement;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   5-17-00 Fixed bug in SeqCurrents and SeqPowers with location of Reallocmem
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TCktElement = class(TAutoObject, ICktElement)
  protected
    function Get_BusNames: OleVariant; safecall;
    function Get_Name: WideString; safecall;
    function Get_NumConductors: Integer; safecall;
    function Get_NumPhases: Integer; safecall;
    function Get_NumTerminals: Integer; safecall;
    function Get_Properties(Index: OleVariant): IDSSProperty; safecall;
    procedure Set_BusNames(Value: OleVariant); safecall;
    
    function Get_Currents: OleVariant; safecall;
    function Get_Voltages: OleVariant; safecall;
    function Get_EmergAmps: Double; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Losses: OleVariant; safecall;
    function Get_NormalAmps: Double; safecall;
    function Get_PhaseLosses: OleVariant; safecall;
    function Get_Powers: OleVariant; safecall;
    function Get_SeqCurrents: OleVariant; safecall;
    function Get_SeqPowers: OleVariant; safecall;
    function Get_SeqVoltages: OleVariant; safecall;
    procedure Close(Term, Phs: Integer); safecall;
    procedure Open(Term, Phs: Integer); safecall;
    procedure Set_EmergAmps(Value: Double); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_NormalAmps(Value: Double); safecall;
    function IsOpen(Term, Phs: Integer): WordBool; safecall;
    function Get_AllPropertyNames: OleVariant; safecall;
    function Get_NumProperties: Integer; safecall;
    function Get_Residuals: OleVariant; safecall;
    function Get_Yprim: OleVariant; safecall;
  end;

implementation

uses ComServ, DSSClassDefs, DSSGlobals, UComplex, Sysutils,
     PDElement, MathUtil, ImplGlobals, Variants;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_BusNames: OleVariant;

Var
   i :Integer;
begin

     If ActiveCircuit <> Nil Then
     Begin
       With ActiveCircuit Do Begin
         Result := VarArrayCreate([0, ActiveCktElement.Nterms-1], varOleStr);
         For i := 1 to  ActiveCktElement.Nterms Do Begin
             Result[i-1] := ActiveCktElement.GetBus(i);
         End;
       End;
     End
     Else
         Result := VarArrayCreate([0, 0], varOleStr);

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Name: WideString;
begin
   If ActiveCircuit <> Nil Then
      WITH ActiveCircuit.ActiveCktElement DO
      Begin
        Result := ParentClass.Name + '.' + Name;
      End
   Else
      Result := '';
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_NumConductors: Integer;
begin
   If ActiveCircuit <> Nil Then
        Result := ActiveCircuit.ActiveCktElement.NConds
   Else Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_NumPhases: Integer;
begin
   If ActiveCircuit <> Nil Then
        Result := ActiveCircuit.ActiveCktElement.NPhases
   Else Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_NumTerminals: Integer;
begin
   If ActiveCircuit <> Nil Then
        Result := ActiveCircuit.ActiveCktElement.NTerms
   Else Result := 0;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Properties(Index: OleVariant): IDSSProperty;

Var
   Str:String;
   i :Integer;
begin

  If ActiveCircuit <> Nil Then
  Begin

     Case (Vartype(Index) and VarTypeMask) of
         VarSmallint, VarInteger: FPropIndex := Integer(Index) + 1;    // INdex is zero based to match arrays
         VarOleStr:
           Begin
              FPropClass := ActiveDSSObject.ParentClass;
              FPropIndex := 0;
              Str := Index;
              If FPropClass <> Nil Then
               With FPropClass Do
               For i := 1 to NumProperties Do Begin
                   If CompareText(Str, PropertyName^[i]) = 0 Then Begin
                       FPropIndex := i;
                       Break;
                   End;
               End;
           End;
     Else
         DoSimpleMsg('Illegal Var Type Passed to Properties Interface: '+ Format('$%x',[VarType(Index)]), 5011);
     End;

  End;

  // DoSimpleMsg('Properties: FPropIndex ='+ IntToStr(FPropIndex));

  Result := FDSSProperty As IDSSProperty;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Set_BusNames(Value: OleVariant);
Var
   i :Integer;
   Count, Low :Integer;
begin

     If ActiveCircuit <> Nil Then
     Begin
       With ActiveCircuit Do Begin
         Low := VarArrayLowBound(Value, 1);
         Count := VarArrayHighBound(Value, 1) - Low + 1;
         If Count >  ActiveCktElement.NTerms Then Count := ActiveCktElement.NTerms;
         For i := 1 to Count Do Begin
             ActiveCktElement.SetBus(i, Value[i-1 + Low]);
         End;
       End;
     End;
end;



{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Currents: OleVariant;
VAR
  cBuffer: pComplexArray;
  NValues, iV ,i: Integer;

Begin
  If ActiveCircuit <> Nil Then
     WITH ActiveCircuit.ActiveCktElement DO
     Begin
         NValues := NConds*NTerms;
         Result := VarArrayCreate([0, 2*NValues-1], varDouble);
         cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
         GetCurrents(cBuffer);
         iV :=0;
         For i := 1 to  NValues DO
         Begin
            Result[iV] := cBuffer^[i].re;
            Inc(iV);
            Result[iV] := cBuffer^[i].im;
            Inc(iV);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Voltages: OleVariant;

// Bus Voltages at active terminal

VAR
  numcond, i,n,iV:Integer;
  Volts:Complex;

Begin

// Return voltages for all terminals

     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement DO
        Begin
         numcond := NConds*Nterms;
         Result := VarArrayCreate([0, 2*numcond-1], varDouble);
         // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
         iV :=0;
         FOR i := 1 to  numcond DO
         Begin
            n := ActiveCktElement.NodeRef^[i];
            Volts := Solution.NodeV^[n]; // ok if =0
            Result[iV] := Volts.re;
            Inc(iV);
            Result[iV] := Volts.im;
            Inc(iV);
         End;
        End;
      End
    ELSE Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_EmergAmps: Double;
begin
     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
         Begin
             With ActiveCktElement As TPDElement Do Result := EmergAmps ;
         End
         Else Result := 0.0;
     End;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Enabled: WordBool;
Begin

   If ActiveCircuit <> Nil Then
      Result := ActiveCircuit.ActiveCktElement.Enabled
   Else
       Result := False;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Losses: OleVariant;

Var
   LossValue :complex;
begin

     IF ActiveCircuit <> Nil THEN
      WITH ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        Begin
         Result    := VarArrayCreate([0, 1], varDouble);
         LossValue := ActiveCktElement.Losses;
         Result[0] := LossValue.re;
         Result[1] := LossValue.im;
        End;
      End
    ELSE Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_NormalAmps: Double;
begin
     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
         Begin
             With ActiveCktElement As TPDElement Do Result := NormAmps ;
         End
         Else Result := 0.0;
     End;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_PhaseLosses: OleVariant;

// Returns Phase losses in kW, kVar

VAR
  cBuffer:pComplexArray;
  NValues,  i, iV : Integer;

Begin


 IF ActiveCircuit <> Nil THEN

  WITH ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NPhases;
      Result := VarArrayCreate([0, 2*NValues-1], varDouble);
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GetPhaseLosses( NValues, cBuffer);
      iV :=0;
      For i := 1 to  NValues DO Begin
         Result[iV] := cBuffer^[i].re*0.001;
         Inc(iV);
         Result[iV] := cBuffer^[i].im*0.001;
         Inc(iV);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_Powers: OleVariant;

// Return complex kW, kvar in each conductor for each terminal

VAR
  cBuffer:pComplexArray;
  NValues, i,iV : Integer;

Begin

 IF ActiveCircuit <> Nil THEN
  WITH ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NConds*Nterms;
      Result := VarArrayCreate([0, 2*NValues-1], varDouble);
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GetPhasePower(cBuffer);
      iV :=0;
      For i := 1 to  NValues DO Begin
         Result[iV] := cBuffer^[i].re*0.001;
         Inc(iV);
         Result[iV] := cBuffer^[i].im*0.001;
         Inc(iV);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_SeqCurrents: OleVariant;

// All sequence currents of active ciruit element
// returns magnitude only.

VAR
  Nvalues,i,j,k, iV:Integer;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin
   IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       If ActiveCktElement<>Nil THEN
       WITH ActiveCktElement DO
       Begin
        Result := VarArrayCreate([0, 3*NTerms-1], varDouble);
        IF NPhases <> 3
        THEN Begin
           IF (Nphases = 1) and PositiveSequence
           THEN Begin
              NValues := NConds*NTerms;
              cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
              GetCurrents(cBuffer);

              For i := 0 to  3*NTerms-1 DO Result[i] := 0.0;   // Initialize Result
              iV := 1;
              {Put only phase 1 quantities in Pos seq}
              FOR j := 1 to NTerms Do
              Begin
                  k := (j-1)*NConds;
                  Result[iV] := Cabs(cBuffer^[k+1]);
                  Inc(iV, 3);
              End;
              Reallocmem(cBuffer,0);
           END
           ELSE  For i := 0 to  3*NTerms-1 DO Result[i] := -1.0;  // Signify n/A

        END
        ELSE Begin
          iV := 0;
          NValues := NConds * NTerms;
          cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
          GetCurrents(cBuffer);
          For j := 1 to NTerms Do
            Begin
              k := (j-1)*NConds;
              For i := 1 to  3 DO
              Begin
                Iph[i] := cBuffer^[k+i];
              End;
              Phase2SymComp(@Iph, @I012);
              For i := 1 to 3 DO
              Begin
                Result[iV] := Cabs(I012[i]);
                Inc(iV);
              End;
            End;
          Reallocmem(cBuffer,0);
        End;
       End;
     End
   ELSE Result := VarArrayCreate([0, 0], varDouble);

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_SeqPowers: OleVariant;


// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

VAR
  Nvalues,i,j,k,n, icount:Integer;
  S:Complex;
  VPh, V012 : Array[1..3] of Complex;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin

 IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO Begin
      Result := VarArrayCreate([0, 2*3*NTerms-1], varDouble);
      IF NPhases<>3
      THEN  Begin
           IF (Nphases = 1) and PositiveSequence
           THEN Begin
              NValues := NConds*NTerms;
              cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
              GetCurrents(cBuffer);

              For i := 0 to  2*3*NTerms-1 DO Result[i] := 0.0;   // Initialize Result
              iCount := 2;  // Start with kW1
              {Put only phase 1 quantities in Pos seq}
              FOR j := 1 to NTerms Do
              Begin
                  k := (j-1)*NConds;
                  n := NodeRef^[k+1];
                  Vph[1] := Solution.NodeV^[n];  // Get voltage at node
                  S := Cmul(Vph[1], conjg(cBuffer^[k+1]));   // Computer power per phase
                  Result[icount] := S.re*0.003; // 3-phase kW conversion
                  inc(icount);
                  Result[icount] := S.im*0.003; // 3-phase kvar conversion
                  inc(icount, 6);
              End;
              Reallocmem(cBuffer,0);
           END

           ELSE  For i := 0 to  2*3*NTerms-1 DO Result[i] := -1.0;  // Signify n/A
      END
      ELSE Begin
        NValues := NConds*NTerms;
        cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
        GetCurrents(cBuffer);
        icount := 0;
        FOR j := 1 to NTerms Do Begin
         k :=(j-1)*NConds;
         FOR i := 1 to  3 DO Begin
            n := NodeRef^[i+k];
            Vph[i]  := Solution.NodeV^[n];
         End;
         For i := 1 to  3 DO Begin
           Iph[i] := cBuffer^[k+i];
         End;
         Phase2SymComp(@Iph, @I012);
         Phase2SymComp(@Vph, @V012);
         For i := 1 to 3 DO  Begin
           S := Cmul(V012[i], conjg(I012[i]));
           Result[icount] := S.re*0.003; // 3-phase kW conversion
           inc(icount);
           Result[icount] := S.im*0.003; // 3-phase kW conversion
           inc(icount);
         End;
        End;
        Reallocmem(cBuffer,0);
      End;
     End;
   End
 ELSE Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function TCktElement.Get_SeqVoltages: OleVariant;
// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal
// 0, 1, 2 sequence  (0, +, -)

VAR
  Nvalues,i,j,k,n, iV:Integer;
  VPh, V012 : Array[1..3] of Complex;
  S:String;

Begin
  n := -1; // unassigned flags for exception message
  Nvalues := -1;
  IF   ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
     TRY
      Nvalues := NPhases;
      Result := VarArrayCreate([0, 3*NTerms-1], varDouble);
      IF Nvalues<>3
      THEN Begin
           IF (Nphases = 1) and PositiveSequence
           THEN Begin

              For i := 0 to  3*NTerms-1 DO Result[i] := 0.0;   // Initialize Result
              iV := 1;
              {Put only phase 1 quantities in Pos seq}
              FOR j := 1 to NTerms Do
              Begin
                  k := (j - 1) * NConds;
                  Vph[1] := Solution.NodeV^[NodeRef^[1 + k]];
                  Result[iV] := Cabs(Vph[1]);
                  Inc(iV, 3);
              End;
           END

           ELSE     For i := 0 to  3*NTerms-1 DO Result[i] := -1.0;  // Signify n/A
      End
      ELSE Begin
       iV := 0;

       FOR j := 1 to NTerms Do
       Begin

          k :=(j-1)*NConds;
          FOR i := 1 to  3 DO
          Begin
             Vph[i]  := Solution.NodeV^[NodeRef^[i+k]];
          End;
          Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             Result[iV] := Cabs(V012[i]);
             Inc(iV);
          End;

       End;
      End;

      EXCEPT
         On E:Exception Do
         Begin
            S:= E.message + CRLF +
                'Element='+ ActiveCktElement.Name + CRLF+
                'Nvalues=' + IntToStr(NValues) + CRLF +
                'NTerms=' + IntToStr(NTerms) + CRLF +
                'NConds =' + IntToStr(NConds) + CRLF +
                'noderef=' + IntToStr(N) ;
            DoSimpleMsg(S, 5012);
          End;
      END;
     End
     Else
         Result := VarArrayCreate([0, 0], varDouble);  // Disabled

   End
  ELSE Result := VarArrayCreate([0, 0], varDouble);


end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Close(Term, Phs: Integer);

Begin

   IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
      If ActiveCktElement<>nil THEN
      WITH ActiveCktElement DO
      Begin
        ActiveTerminal := Terminals^[Term];
        Closed[Phs] := TRUE;
      End;
   End;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Open(Term, Phs: Integer);

Begin
   IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
      If ActiveCktElement<>nil THEN
      WITH ActiveCktElement DO
      Begin
        ActiveTerminal := Terminals^[Term];
        Closed[Phs] := FALSE;
      End;
   End;

end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Set_EmergAmps(Value: Double);

begin

     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
         Begin
             With ActiveCktElement As TPDElement Do EmergAmps := Value;
         End;  {Else Do Nothing}
     End;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Set_Enabled(Value: WordBool);
begin
   If ActiveCircuit <> Nil Then
      ActiveCircuit.ActiveCktElement.Enabled := Value;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
procedure TCktElement.Set_NormalAmps(Value: Double);
begin
     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         If (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT Then
         Begin
             With ActiveCktElement As TPDElement Do NormAmps := Value;
         End;  {Else Do Nothing}
     End;
end;


function TCktElement.IsOpen(Term, Phs: Integer): WordBool;

Var
   i  :Integer;

begin
     If ActiveCircuit <> Nil Then
     With ActiveCircuit Do
     Begin
         With ActiveCktElement Do ActiveTerminal := Terminals^[Term];
         If Phs=0 Then // At least one must be open
         Begin
             Result := False;
             For i := 1 to ActiveCktElement.NConds Do
                 If not ActiveCktElement.Closed[i] Then
                 Begin
                    Result :=  True;
                    Exit;
                 End;
         End
         Else // Check a specific phase or conductor
         Begin
             Result := Not ActiveCktElement.Closed[Phs];
         End;
     End;
end;

function TCktElement.Get_AllPropertyNames: OleVariant;

VAR
   k:Integer;
begin
  Result := VarArrayCreate([0, 0], varOleStr);
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     Begin
          WITH ParentClass Do
          Begin
              Result := VarArrayCreate([0, NumProperties-1], varOleStr);
              For k := 1 to NumProperties DO Begin
                  Result[k-1] := PropertyName^[k];
              End;
          End;
     End
   End;
end;

function TCktElement.Get_NumProperties: Integer;
begin
  Result := 0;
  IF ActiveCircuit <> Nil THEN
   WITH ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     Begin
          Result := ParentClass.NumProperties ;
     End
   End;


end;

function TCktElement.Get_Residuals: OleVariant;
VAR
  cBuffer       :pComplexArray;
  iV ,i, j, k   :Integer;
  cResid        :Complex;

Begin

  If ActiveCircuit <> Nil Then
     WITH ActiveCircuit.ActiveCktElement DO
     Begin
         Result := VarArrayCreate([0, 2*NTerms-1], varDouble);    // 2 values per terminal
         cBuffer := Allocmem(sizeof(cBuffer^[1])*Yorder);
         GetCurrents(cBuffer);
         iV :=0;
         For i := 1 to  NTerms DO
         Begin
            cResid := CZERO;
            k :=0;
            For j := 1 to Nconds Do Begin
                inc(k);
                Caccum(cResid, CBuffer^[k]);
            End;
            Result[iV] := Cabs(cResid);
            Inc(iV);
            Result[iV] := CDang(cResid);
            Inc(iV);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     Result := VarArrayCreate([0, 0], varDouble);

end;

function TCktElement.Get_Yprim: OleVariant;
{ Return the YPrim matrix for this element }

Var
   iV      : Integer;
   i       : Integer;
   NValues : Integer;
   cValues : pComplexArray;

begin
   IF ActiveCircuit = nil Then Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE With ActiveCircuit Do
      If ActiveCktElement<>Nil THEN
      WITH ActiveCktElement Do  Begin
          NValues := SQR(Yorder);
          cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
          If cValues=Nil Then Begin   // check for unassigned array
                            Result := VarArrayCreate([0, 0], varDouble);  // just return null array
                            Exit;  // Get outta here
                         End;
          Result := VarArrayCreate( [0, 2*NValues -1], varDouble);  // Make variant array
          iV := 0;

          FOR i := 1 to  NValues DO  Begin    // Plunk the values in the variant array
              Result[iV] := cValues^[i].re;
              Inc(iV);
              Result[iV] := cValues^[i].im;
              Inc(iV);
          End;
      End
      ELSE Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

initialization
  TAutoObjectFactory.Create(ComServer, TCktElement, Class_CktElement, ciInternal, tmApartment);
end.
