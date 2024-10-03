unit ImplReactors;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TReactors = class(TAutoObject, IReactors)
  Protected
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_kV: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    function Get_kvar: Double; safecall;
    procedure Set_kvar(Value: Double); safecall;
    function Get_LCurve: WideString; safecall;
    procedure Set_LCurve(const Value: WideString); safecall;
    function Get_lmH: Double; safecall;
    function Get_Parallel: WordBool; safecall;
    procedure Set_lmH(Value: Double); safecall;
    procedure Set_Parallel(Value: WordBool); safecall;
    function Get_R: Double; safecall;
    procedure Set_R(Value: Double); safecall;
    function Get_RCurve: WideString; safecall;
    procedure Set_RCurve(const Value: WideString); safecall;
    function Get_Rmatrix: OleVariant; safecall;
    procedure Set_Rmatrix(Value: OleVariant); safecall;
    function Get_Rp: Double; safecall;
    function Get_X: Double; safecall;
    procedure Set_Rp(Value: Double); safecall;
    procedure Set_X(Value: Double); safecall;
    function Get_Xmatrix: OleVariant; safecall;
    procedure Set_Xmatrix(Value: OleVariant); safecall;
    function Get_Z: OleVariant; safecall;
    procedure Set_Z(Value: OleVariant); safecall;
    function Get_Z0: OleVariant; safecall;
    function Get_Z1: OleVariant; safecall;
    function Get_Z2: OleVariant; safecall;
    procedure Set_Z0(Value: OleVariant); safecall;
    procedure Set_Z1(Value: OleVariant); safecall;
    procedure Set_Z2(Value: OleVariant); safecall;
    function Get_AllNames: OleVariant; safecall;
end;

implementation

uses ComServ, DSSGlobals, Reactor, Variants, Pointerlist, Sysutils;

function TReactors.Get_First: Integer;
Var
  Elem:  TReactorObj;
  pList: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin

    If ReactorClass[ActiveActor].ElementList.ListSize > 0 Then
    Begin
      pList := ReactorClass[ActiveActor].ElementList;
      Elem := pList.First;
      Repeat
        If Elem.Enabled
        Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
          Result := 1;
        End
        Else Elem := pList.Next;
      Until (Result = 1) or (Elem = nil);
    End
    Else
        Result := 0;  // signify no more

  End;


end;

function TReactors.Get_Next: Integer;
Var
  Elem:   TReactorObj;
  pList:  TPointerList;

Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin

    If ReactorClass[ActiveActor].ElementList.ListSize > 0 Then
    Begin
      pList := ReactorClass[ActiveActor].ElementList;
      Elem := pList.First;
      Repeat
        If Elem.Enabled
        Then
        Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
          Result := pList.ActiveIndex;
        End
        Else Elem := pList.Next;
      Until (Result > 0) or (Elem = nil);
    End
    Else
      Result := 0;  // signify no more

  End;

end;

function TReactors.Get_Count: Integer;
begin
  If Assigned(ActiveCircuit[ActiveActor]) Then
    Result := ReactorClass[ActiveActor].ElementList.ListSize;
end;

function TReactors.Get_Name: WideString;
Var
  Elem:  TReactorObj;

begin
  Result := '';
  Elem := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.Name
  End;

end;

procedure TReactors.Set_Name(const Value: WideString);
VAR
    activesave  :integer;
    Elem        :TReactorObj;
    S           : String;
    Found       :Boolean;
    pList       :TPointerList;
Begin

  IF ActiveCircuit[ActiveActor] <> NIL
  THEN
  Begin      // Search list of Storages in active circuit for name
    If ReactorClass[ActiveActor].ElementList.ListSize > 0 Then
    Begin
      S           := Value;  // Convert to Pascal String
      Found       := FALSE;
      pList       := ReactorClass[ActiveActor].ElementList;
      activesave  :=  pList.ActiveIndex;
      Elem        := pList.First;
      While Elem <> NIL Do
      Begin
        IF (CompareText(Elem.Name, S) = 0)
        THEN
        Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
          Found := TRUE;
          Break;
        End;
        Elem := pList.Next;
      End;
      IF NOT Found
      THEN
      Begin
        DoSimpleMsg('Reactor "'+S+'" Not Found in Active Circuit.', 20003);
        Elem := pList.Get(activesave);    // Restore active Storage
        ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
      End;
    End;
  End;

end;

function TReactors.Get_kV: Double;
Var
  Elem  :  TReactorObj;

begin
  Result  := 0.0;
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.kVNominal;
  End;

end;

procedure TReactors.Set_kV(Value: Double);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.kVNominal := Value;
  End;

end;

function TReactors.Get_kvar: Double;
Var
  Elem  :  TReactorObj;

begin
  Result  := 0.0;
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.kvarNominal;
  End;

end;

procedure TReactors.Set_kvar(Value: Double);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.kvarNominal := Value;
  End;

end;

function TReactors.Get_LCurve: WideString;
Var
  Elem  :  TReactorObj;

begin
  Result  := '';
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.LCurve;
  End;

end;

procedure TReactors.Set_LCurve(const Value: WideString);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.LCurve := Value;
  End;
end;

function TReactors.Get_lmH: Double;
Var
  Elem  :  TReactorObj;

begin
  Result  := 0.0;
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.LNominal * 1e3;
  End;

end;

function TReactors.Get_Parallel: WordBool;
Var
  Elem  :  TReactorObj;

begin
  Result  := false;
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.IsParallel;
  End;

end;

procedure TReactors.Set_lmH(Value: Double);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.LNominal := Value / 1e3;
  End;

end;

procedure TReactors.Set_Parallel(Value: WordBool);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.IsParallel := Value;
  End;

end;

function TReactors.Get_R: Double;
Var
  Elem  :  TReactorObj;

begin
  Result  := 0.0;
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.R;
  End;

end;

procedure TReactors.Set_R(Value: Double);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.R := Value;
  End;

end;

function TReactors.Get_RCurve: WideString;
Var
  Elem  :  TReactorObj;

begin
  Result  := '';
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.RCurve;
  End;

end;

procedure TReactors.Set_RCurve(const Value: WideString);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.RCurve := Value;
  End;

end;

function TReactors.Get_Rmatrix: OleVariant;
VAR
  Elem  :  TReactorObj;
  i     :Integer;

begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  Begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil THEN
    Begin
        WITH Elem DO
        Begin
          if RMatrix <> nil then
          Begin
            Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
            FOR i := 1 to Sqr(NPhases) DO
            Begin
                 Result[i - 1] :=  Rmatrix[i];
            End;
          End;
        End;
    End;
  End;

end;

procedure TReactors.Set_Rmatrix(Value: OleVariant);
VAR
  Elem    :  TReactorObj;
  idx,
  i,
  j,
  k       :Integer;

begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil Then
    Begin
      WITH Elem DO
      Begin
        Reallocmem(Rmatrix,Sizeof(double) *  NPhases *  NPhases);
        idx := 1;;  
        k := VarArrayLowBound(Value, 1);
        j := VarArrayHighBound(Value, 1);
        FOR i := k to j DO
        Begin
           Rmatrix[idx] := Value[i];
           Inc(idx);
        End;
        YprimInvalid[ActiveActor] := True;
      End;
    End;
  end;

end;

function TReactors.Get_Rp: Double;
Var
  Elem  :  TReactorObj;

begin
  Result  := 0.0;
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.Rp;
  End;

end;

function TReactors.Get_X: Double;
Var
  Elem  :  TReactorObj;

begin
  Result  := 0.0;
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Result := Elem.X;
  End;

end;

procedure TReactors.Set_Rp(Value: Double);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.Rp := Value;
  End;

end;

procedure TReactors.Set_X(Value: Double);
Var
  Elem  :  TReactorObj;

begin
  Elem    := ReactorClass[ActiveActor].GetActiveObj;
  if Elem <> nil then
  Begin
    Elem.X := Value;
  End;

end;

function TReactors.Get_Xmatrix: OleVariant;
VAR
  Elem  :  TReactorObj;
  i     :Integer;

begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  Begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil THEN
    Begin
      WITH Elem DO
      Begin
        if Xmatrix <> nil then
        Begin
          Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
          FOR i := 1 to Sqr(NPhases) DO
          Begin
               Result[i - 1] :=  Xmatrix[i];
          End;
        End;
      End;
    End;
  End;

end;

procedure TReactors.Set_Xmatrix(Value: OleVariant);
VAR
  Elem    :  TReactorObj;
  idx,
  i,
  j,
  k       :Integer;

begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil Then
    Begin
      WITH Elem DO
      Begin
        Reallocmem(Xmatrix,Sizeof(double) *  NPhases *  NPhases);
        idx := 1;;  
        k := VarArrayLowBound(Value, 1);
        j := VarArrayHighBound(Value, 1);
        FOR i := k to j DO
        Begin
           Xmatrix[idx] := Value[i];
           Inc(idx);
        End;
        YprimInvalid[ActiveActor] := True;
      End;
    End;
  end;

end;

function TReactors.Get_Z: OleVariant;
VAR
  Elem  :  TReactorObj;
  i     :Integer;

begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  Begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil THEN
    Begin
      WITH Elem DO
      Begin
        Result := VarArrayCreate([0, 1], varDouble);
        Result[0] := Z.re;
        Result[1] := Z.im;
      End;
    End;
  End;

end;

procedure TReactors.Set_Z(Value: OleVariant);
VAR
  Elem    :  TReactorObj;
  i,
  k       :Integer;

begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil Then
    Begin
      WITH Elem DO
      Begin
        k := VarArrayLowBound(Value, 1);
        Z.re  := Value[k];
        Z.im  := Value[k + 1];
        YprimInvalid[ActiveActor] := True;
      End;
    End;
  end;

end;

function TReactors.Get_Z0: OleVariant;
VAR
  Elem  :  TReactorObj;
  i     :Integer;

begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  Begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil THEN
    Begin
      WITH Elem DO
      Begin
        Result := VarArrayCreate([0, 1], varDouble);
        Result[0] := Z0.re;
        Result[1] := Z0.im;
      End;
    End;
  End;

end;

function TReactors.Get_Z1: OleVariant;
VAR
  Elem  :  TReactorObj;
  i     :Integer;

begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  Begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil THEN
    Begin
      WITH Elem DO
      Begin
        Result := VarArrayCreate([0, 1], varDouble);
        Result[0] := Z1.re;
        Result[1] := Z1.im;
      End;
    End;
  End;

end;

function TReactors.Get_Z2: OleVariant;
VAR
  Elem  :  TReactorObj;
  i     :Integer;

begin
  Result := VarArrayCreate([0, 0], varDouble);
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  Begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil THEN
    Begin
      WITH Elem DO
      Begin
        Result := VarArrayCreate([0, 1], varDouble);
        Result[0] := Z2.re;
        Result[1] := Z2.im;
      End;
    End;
  End;

end;

procedure TReactors.Set_Z0(Value: OleVariant);
VAR
  Elem    :  TReactorObj;
  i,
  k       :Integer;

begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil Then
    Begin
      WITH Elem DO
      Begin
        k := VarArrayLowBound(Value, 1);
        Z0.re  := Value[k];
        Z0.im  := Value[k + 1];
        YprimInvalid[ActiveActor] := True;
      End;
    End;
  end;

end;

procedure TReactors.Set_Z1(Value: OleVariant);
VAR
  Elem    :  TReactorObj;
  i,
  k       :Integer;

begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil Then
    Begin
      WITH Elem DO
      Begin
        k := VarArrayLowBound(Value, 1);
        Z1.re  := Value[k];
        Z1.im  := Value[k + 1];
        YprimInvalid[ActiveActor] := True;
      End;
    End;
  end;

end;

procedure TReactors.Set_Z2(Value: OleVariant);
VAR
  Elem    :  TReactorObj;
  i,
  k       :Integer;

begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN
  begin
    Elem    := ReactorClass[ActiveActor].GetActiveObj;
    If Elem <> nil Then
    Begin
      WITH Elem DO
      Begin
        k := VarArrayLowBound(Value, 1);
        Z2.re  := Value[k];
        Z2.im  := Value[k + 1];
        YprimInvalid[ActiveActor] := True;
      End;
    End;
  end;

end;

function TReactors.Get_AllNames: OleVariant;
Var
  Elem    :  TReactorObj;
  pList   :  TPointerList;
  k       :  Integer;

Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
    WITH ActiveCircuit[ActiveActor] DO
    Begin

      If ReactorClass[ActiveActor].ElementList.ListSize > 0 Then
      Begin
        pList := ReactorClass[ActiveActor].ElementList;
        VarArrayRedim(result, pList.ListSize - 1);
        k:=0;
        Elem := pList.First;
        WHILE Elem<>Nil DO
        Begin
          Result[k] := Elem.Name;
          Inc(k);
          Elem := pList.Next;
        End;
      End;

    End;
  End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TReactors, CLASS_Reactors,
    ciInternal, tmApartment);

end.
