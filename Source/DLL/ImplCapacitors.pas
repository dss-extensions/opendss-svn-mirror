unit ImplCapacitors;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TCapacitors = class(TAutoObject, ICapacitors)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_IsDelta: WordBool; safecall;
    function Get_kV: Double; safecall;
    function Get_kvar: Double; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_NumSteps: Integer; safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    procedure Set_kV(Value: Double); safecall;
    procedure Set_kvar(Value: Double); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_NumSteps(Value: Integer); safecall;

  end;

implementation

uses ComServ, DSSGlobals, Capacitor, Variants, SysUtils, PointerList;

function ActiveCapacitor: TCapacitorObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.ShuntCapacitors.Active;
end;

function TCapacitors.Get_AllNames: OleVariant;
Var
  elem: TCapacitorObj;
  lst: TPointerList;
  k: Integer;
Begin
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
    lst := ShuntCapacitors;
    Result := VarArrayCreate([0, lst.ListSize-1], varOleStr);
    k:=0;
    elem := lst.First;
    WHILE elem<>Nil DO Begin
      Result[k] := elem.Name;
      Inc(k);
      elem := lst.Next;
    End;
  End ELSE Result := VarArrayCreate([0, 0], varOleStr);
end;

function TCapacitors.Get_First: Integer;
Var
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.ShuntCapacitors;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit.ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;

function TCapacitors.Get_IsDelta: WordBool;
var
  elem: TCapacitorObj;
begin
  Result := FALSE;
  elem := ActiveCapacitor;
  if elem <> nil then
    if elem.Connection > 0 then Result := TRUE;
end;

function TCapacitors.Get_kV: Double;
var
  elem: TCapacitorObj;
begin
  Result := 0.0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.NomKV;
end;

function TCapacitors.Get_kvar: Double;
var
  elem: TCapacitorObj;
begin
  Result := 0.0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.Totalkvar;
end;

function TCapacitors.Get_Name: WideString;
Var
  elem: TCapacitorObj;
Begin
  Result := '';
  elem := ActiveCapacitor;
  If elem <> Nil Then Result := elem.Name;
end;

function TCapacitors.Get_Next: Integer;
Var
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.ShuntCapacitors;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit.ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;

function TCapacitors.Get_NumSteps: Integer;
var
  elem: TCapacitorObj;
begin
  Result := 0;
  elem := ActiveCapacitor;
  if elem <> nil then Result := elem.NumSteps;
end;

procedure TCapacitors.Set_IsDelta(Value: WordBool);
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then elem.Connection := Integer (Value);
end;

procedure TCapacitors.Set_kV(Value: Double);
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then elem.NomKV := Value;
end;

procedure TCapacitors.Set_kvar(Value: Double);
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then elem.Totalkvar := Value;
end;

procedure TCapacitors.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TCapacitorObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.ShuntCapacitors;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit.ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('Capacitor "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit.ActiveCktElement := elem;
    End;
  End;
end;

procedure TCapacitors.Set_NumSteps(Value: Integer);
var
  elem: TCapacitorObj;
begin
  elem := ActiveCapacitor;
  if elem <> nil then elem.NumSteps := Value;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TCapacitors, Class_Capacitors,
    ciInternal, tmApartment);
end.
