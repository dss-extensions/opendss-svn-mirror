unit ImplRegControls;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TRegControls = class(TAutoObject, IRegControls)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_CTPrimary: Double; safecall;
    function Get_Delay: Double; safecall;
    function Get_First: Integer; safecall;
    function Get_ForwardBand: Double; safecall;
    function Get_ForwardR: Double; safecall;
    function Get_ForwardVreg: Double; safecall;
    function Get_ForwardX: Double; safecall;
    function Get_IsInverseTime: WordBool; safecall;
    function Get_IsReversible: WordBool; safecall;
    function Get_MaxTapChange: Double; safecall;
    function Get_MonitoredBus: WideString; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_PTratio: Double; safecall;
    function Get_ReverseBand: Double; safecall;
    function Get_ReverseR: Double; safecall;
    function Get_ReverseVreg: Double; safecall;
    function Get_ReverseX: Double; safecall;
    function Get_TapDelay: Double; safecall;
    function Get_TapWinding: Integer; safecall;
    function Get_Transformer: WideString; safecall;
    function Get_VoltageLimit: Double; safecall;
    function Get_Winding: Integer; safecall;
    procedure Set_CTPrimary(Value: Double); safecall;
    procedure Set_Delay(Value: Double); safecall;
    procedure Set_ForwardBand(Value: Double); safecall;
    procedure Set_ForwardR(Value: Double); safecall;
    procedure Set_ForwardVreg(Value: Double); safecall;
    procedure Set_ForwardX(Value: Double); safecall;
    procedure Set_IsInverseTime(Value: WordBool); safecall;
    procedure Set_IsReversible(Value: WordBool); safecall;
    procedure Set_MaxTapChange(Value: Double); safecall;
    procedure Set_MonitoredBus(const Value: WideString); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_PTratio(Value: Double); safecall;
    procedure Set_ReverseBand(Value: Double); safecall;
    procedure Set_ReverseR(Value: Double); safecall;
    procedure Set_ReverseVreg(Value: Double); safecall;
    procedure Set_ReverseX(Value: Double); safecall;
    procedure Set_TapDelay(Value: Double); safecall;
    procedure Set_TapWinding(Value: Integer); safecall;
    procedure Set_Transformer(const Value: WideString); safecall;
    procedure Set_VoltageLimit(Value: Double); safecall;
    procedure Set_Winding(Value: Integer); safecall;

  end;

implementation

uses ComServ, DSSGlobals, ControlElem, RegControl, Variants, SysUtils, PointerList;

function ActiveRegControl: TRegControlObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.RegControls.Active;
end;

function TRegControls.Get_AllNames: OleVariant;
Var
  elem: TRegControlObj;
  lst: TPointerList;
  k: Integer;
Begin
  IF ActiveCircuit <> Nil THEN WITH ActiveCircuit DO Begin
    lst := RegControls;
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

function TRegControls.Get_CTPrimary: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.CT;
end;

function TRegControls.Get_Delay: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.InitialDelay;
end;

function TRegControls.Get_First: Integer;
Var
  elem: TRegControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then begin
    lst := ActiveCircuit.RegControls;
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

function TRegControls.Get_ForwardBand: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.BandVoltage;
end;

function TRegControls.Get_ForwardR: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.LineDropR;
end;

function TRegControls.Get_ForwardVreg: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.TargetVoltage;
end;

function TRegControls.Get_ForwardX: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.LineDropX;
end;

function TRegControls.Get_IsInverseTime: WordBool;
var
  elem: TRegControlObj;
begin
  Result := FALSE;
  elem := ActiveRegControl;
  if elem <> nil then
    if elem.IsInverseTime then Result := TRUE;
end;

function TRegControls.Get_IsReversible: WordBool;
var
  elem: TRegControlObj;
begin
  Result := FALSE;
  elem := ActiveRegControl;
  if elem <> nil then
    if elem.UseReverseDrop then Result := TRUE;
end;

function TRegControls.Get_MaxTapChange: Double;   // TODO - change to Integer
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.MaxTapChange;
end;

function TRegControls.Get_MonitoredBus: WideString;
var
  elem: TRegControlObj;
begin
  Result := '';
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.ControlledBusName;
end;

function TRegControls.Get_Name: WideString;
var
  elem: TRegControlObj;
begin
  Result := '';
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.Name;
end;

function TRegControls.Get_Next: Integer;
Var
  elem: TRegControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit <> Nil Then Begin
    lst := ActiveCircuit.RegControls;
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

function TRegControls.Get_PTratio: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.PT;
end;

function TRegControls.Get_ReverseBand: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.RevBandVoltage;
end;

function TRegControls.Get_ReverseR: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.RevLineDropR;
end;

function TRegControls.Get_ReverseVreg: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.RevTargetVoltage;
end;

function TRegControls.Get_ReverseX: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.RevLineDropX;
end;

function TRegControls.Get_TapDelay: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.SubsequentDelay;
end;

function TRegControls.Get_TapWinding: Integer;
var
  elem: TRegControlObj;
begin
  Result := 0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.TrWinding;  // has the taps
end;

function TRegControls.Get_Transformer: WideString;
var
  elem: TRegControlObj;
begin
  Result := '';
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.Transformer.Name;
end;

function TRegControls.Get_VoltageLimit: Double;
var
  elem: TRegControlObj;
begin
  Result := 0.0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.VoltageLimit;
end;

function TRegControls.Get_Winding: Integer;
var
  elem: TRegControlObj;
begin
  Result := 0;
  elem := ActiveRegControl;
  if elem <> nil then Result := elem.ElementTerminal;  // monitored winding
end;

procedure TRegControls.Set_CTPrimary(Value: Double);
begin

end;

procedure TRegControls.Set_Delay(Value: Double);
begin

end;

procedure TRegControls.Set_ForwardBand(Value: Double);
begin

end;

procedure TRegControls.Set_ForwardR(Value: Double);
begin

end;

procedure TRegControls.Set_ForwardVreg(Value: Double);
begin

end;

procedure TRegControls.Set_ForwardX(Value: Double);
begin

end;

procedure TRegControls.Set_IsInverseTime(Value: WordBool);
begin

end;

procedure TRegControls.Set_IsReversible(Value: WordBool);
begin

end;

procedure TRegControls.Set_MaxTapChange(Value: Double);
begin

end;

procedure TRegControls.Set_MonitoredBus(const Value: WideString);
begin

end;

procedure TRegControls.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TRegControlObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit <> NIL THEN Begin
    lst := ActiveCircuit.RegControls;
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
      DoSimpleMsg('RegControl "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit.ActiveCktElement := elem;
    End;
  End;
end;

procedure TRegControls.Set_PTratio(Value: Double);
begin

end;

procedure TRegControls.Set_ReverseBand(Value: Double);
begin

end;

procedure TRegControls.Set_ReverseR(Value: Double);
begin

end;

procedure TRegControls.Set_ReverseVreg(Value: Double);
begin

end;

procedure TRegControls.Set_ReverseX(Value: Double);
begin

end;

procedure TRegControls.Set_TapDelay(Value: Double);
begin

end;

procedure TRegControls.Set_TapWinding(Value: Integer);
begin

end;

procedure TRegControls.Set_Transformer(const Value: WideString);
begin

end;

procedure TRegControls.Set_VoltageLimit(Value: Double);
begin

end;

procedure TRegControls.Set_Winding(Value: Integer);
begin

end;

initialization
  TAutoObjectFactory.Create(ComServer, TRegControls, Class_RegControls,
    ciInternal, tmApartment);
end.
