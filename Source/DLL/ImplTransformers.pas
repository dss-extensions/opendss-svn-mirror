unit ImplTransformers;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TTransformers = class(TAutoObject, ITransformers)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_IsDelta: WordBool; safecall;
    function Get_kV: Double; safecall;
    function Get_kVA: Double; safecall;
    function Get_MaxTap: Double; safecall;
    function Get_MinTap: Double; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_NumTaps: Integer; safecall;
    function Get_NumWindings: Integer; safecall;
    function Get_R: Double; safecall;
    function Get_Rneut: Double; safecall;
    function Get_Tap: Double; safecall;
    function Get_Wdg: Integer; safecall;
    function Get_XfmrCode: WideString; safecall;
    function Get_Xhl: Double; safecall;
    function Get_Xht: Double; safecall;
    function Get_Xlt: Double; safecall;
    function Get_Xneut: Double; safecall;
    procedure Set_IsDelta(Value: WordBool); safecall;
    procedure Set_kV(Value: Double); safecall;
    procedure Set_kVA(Value: Double); safecall;
    procedure Set_MaxTap(Value: Double); safecall;
    procedure Set_MinTap(Value: Double); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_NumTaps(Value: Integer); safecall;
    procedure Set_NumWindings(Value: Integer); safecall;
    procedure Set_R(Value: Double); safecall;
    procedure Set_Rneut(Value: Double); safecall;
    procedure Set_Tap(Value: Double); safecall;
    procedure Set_Wdg(Value: Integer); safecall;
    procedure Set_XfmrCode(const Value: WideString); safecall;
    procedure Set_Xhl(Value: Double); safecall;
    procedure Set_Xht(Value: Double); safecall;
    procedure Set_Xlt(Value: Double); safecall;
    procedure Set_Xneut(Value: Double); safecall;

  end;

implementation

uses ComServ, DSSGlobals, Transformer, Variants, SysUtils, PointerList;

function ActiveTransformer: TTransfObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.Transformers.Active;
end;

function TTransformers.Get_AllNames: OleVariant;
Var
  elem: TTransfObj;
  lst: TPointerList;
  k: Integer;
Begin
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
    lst := Transformers;
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

function TTransformers.Get_First: Integer;
Var
  elem: TTransfObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.Transformers;
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

function TTransformers.Get_IsDelta: WordBool;
var
  elem: TTransfObj;
begin
  Result := FALSE;
  elem := ActiveTransformer;
  if elem <> nil then
    if elem.WdgConnection[elem.ActiveWinding] > 0 then Result := TRUE;
end;

function TTransformers.Get_kV: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Winding^[elem.ActiveWinding].kvll;
end;

function TTransformers.Get_kVA: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgKVA[elem.ActiveWinding];
end;

function TTransformers.Get_MaxTap: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Maxtap[elem.ActiveWinding];
end;

function TTransformers.Get_MinTap: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.Mintap[elem.ActiveWinding];
end;

function TTransformers.Get_Name: WideString;
Var
  elem: TTransfObj;
Begin
  Result := '';
  If ActiveCircuit <> Nil Then Begin
    elem := ActiveCircuit.Transformers.Active;
    If elem <> Nil Then Result := elem.Name;
  End;
end;

function TTransformers.Get_Next: Integer;
Var
  elem: TTransfObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.Transformers;
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

function TTransformers.Get_NumTaps: Integer;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.NumTaps[elem.ActiveWinding];
end;

function TTransformers.Get_NumWindings: Integer;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.NumberOfWindings;
end;

function TTransformers.Get_R: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgResistance[elem.ActiveWinding];
end;

function TTransformers.Get_Rneut: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgRneutral[elem.ActiveWinding];
end;

function TTransformers.Get_Tap: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.PresentTap[elem.ActiveWinding];
end;

function TTransformers.Get_Wdg: Integer;
var
  elem: TTransfObj;
begin
  Result := 0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.ActiveWinding;
end;

function TTransformers.Get_XfmrCode: WideString;
var
  elem: TTransfObj;
begin
  Result := '';
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XfmrCode;
end;

function TTransformers.Get_Xhl: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XhlVal;
end;

function TTransformers.Get_Xht: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XhtVal;
end;

function TTransformers.Get_Xlt: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then Result := elem.XltVal;
end;

function TTransformers.Get_Xneut: Double;
var
  elem: TTransfObj;
begin
  Result := 0.0;
  elem := ActiveTransformer;
  if elem <> nil then
    Result := elem.WdgXneutral[elem.ActiveWinding];
end;

procedure TTransformers.Set_IsDelta(Value: WordBool);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].Connection := Integer (Value);
end;

procedure TTransformers.Set_kV(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].kvll := Value;
end;

procedure TTransformers.Set_kVA(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].kva := Value;
end;

procedure TTransformers.Set_MaxTap(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].MaxTap := Value;
end;

procedure TTransformers.Set_MinTap(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].MinTap := Value;
end;

procedure TTransformers.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TTransfObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.Transformers;
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
      DoSimpleMsg('Transformer "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit.ActiveCktElement := elem;
    End;
  End;
end;

procedure TTransformers.Set_NumTaps(Value: Integer);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].NumTaps := Value;
end;

procedure TTransformers.Set_NumWindings(Value: Integer);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.SetNumWindings (Value);
end;

procedure TTransformers.Set_R(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].Rpu := 0.01 * Value;
end;

procedure TTransformers.Set_Rneut(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].Rneut := Value;
end;

procedure TTransformers.Set_Tap(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].puTap := Value;
end;

procedure TTransformers.Set_Wdg(Value: Integer);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    if (value > 0) and (value <= elem.NumberOfWindings) then
      elem.ActiveWinding := Value;
end;

procedure TTransformers.Set_XfmrCode(const Value: WideString);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.XfmrCode := Value;
end;

procedure TTransformers.Set_Xhl(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    if elem.NumberOfWindings > 1 then elem.XhlVal := Value;
end;

procedure TTransformers.Set_Xht(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    if elem.NumberOfWindings > 1 then elem.XhtVal := Value;
end;

procedure TTransformers.Set_Xlt(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    if elem.NumberOfWindings > 1 then elem.XltVal := Value;
end;

procedure TTransformers.Set_Xneut(Value: Double);
var
  elem: TTransfObj;
begin
  elem := ActiveTransformer;
  if elem <> nil then
    elem.Winding^[elem.ActiveWinding].Xneut := Value;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TTransformers, Class_Transformers,
    ciInternal, tmApartment);
end.
