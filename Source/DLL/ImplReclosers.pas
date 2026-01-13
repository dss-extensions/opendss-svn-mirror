unit ImplReclosers;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TReclosers = class(TAutoObject, IReclosers)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_MonitoredTerm: Integer; safecall;
    procedure Set_MonitoredTerm(Value: Integer); safecall;
    function Get_SwitchedObj: WideString; safecall;
    procedure Set_SwitchedObj(const Value: WideString); safecall;
    function Get_MonitoredObj: WideString; safecall;
    function Get_SwitchedTerm: Integer; safecall;
    procedure Set_MonitoredObj(const Value: WideString); safecall;
    procedure Set_SwitchedTerm(Value: Integer); safecall;
    function Get_NumFast: Integer; safecall;
    function Get_RecloseIntervals: OleVariant; safecall;
    function Get_Shots: Integer; safecall;
    procedure Set_NumFast(Value: Integer); safecall;
    procedure Set_Shots(Value: Integer); safecall;
    function Get_GndInst: Double; safecall;
    function Get_PhInst: Double; safecall;
    procedure Set_GndInst(Value: Double); safecall;
    procedure Set_PhInst(Value: Double); safecall;
    procedure Close; safecall;
    procedure Open; safecall;
    function Get_idx: Integer; safecall;
    procedure Set_idx(Value: Integer); safecall;
    function Get_NormalState: OleVariant; safecall;
    function Get_State: OleVariant; safecall;
    procedure Set_NormalState(Value: OleVariant); safecall;
    procedure Reset; safecall;
    procedure Set_State(Value: OleVariant); safecall;
    function Get_GndFastPickup: Double; safecall;
    function Get_GndSlowPickup: Double; safecall;
    function Get_PhFastPickup: Double; safecall;
    function Get_PhSlowPickup: Double; safecall;
    procedure Set_GndFastPickup(Value: Double); safecall;
    procedure Set_GndSlowPickup(Value: Double); safecall;
    procedure Set_PhFastPickup(Value: Double); safecall;
    procedure Set_PhSlowPickup(Value: Double); safecall;
    function Get_InterruptingRating: Double; safecall;
    function Get_RatedCurrent: Double; safecall;
    procedure Set_InterruptingRating(Value: Double); safecall;
    procedure Set_RatedCurrent(Value: Double); safecall;

  end;

implementation

uses ComServ, Executive, Sysutils, ControlElem, Recloser, PointerList, Variants, DSSGlobals, DSSClassDefs;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('recloser.%s.%s=%s', [TRecloserObj(RecloserClass.GetActiveObj).Name, parm, val]);
  DSSExecutive[ActiveActor].Command := cmd;
end;

function TReclosers.Get_AllNames: OleVariant;
Var
  elem: TRecloserObj;
  pList: TPointerList;
  k: Integer;
Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
        If RecloserClass.ElementList.ListSize > 0 then
        Begin
          pList := RecloserClass.ElementList;
          VarArrayRedim(Result, pList.ListSize -1);
          k:=0;
          elem := pList.First;
          WHILE elem<>Nil DO Begin
              Result[k] := elem.Name;
              Inc(k);
              elem := pList.next;
          End;
        End;
    End;
end;

function TReclosers.Get_Count: Integer;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := RecloserClass.ElementList.ListSize;
end;

function TReclosers.Get_First: Integer;
Var
   pElem : TRecloserObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := RecloserClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := RecloserClass.ElementList.ActiveIndex;
          End
          Else pElem := RecloserClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;

function TReclosers.Get_Name: WideString;
Var
  elem: TRecloserObj;
Begin
  Result := '';
  elem := RecloserClass.GetActiveObj;
  If elem <> Nil Then Result := elem.Name;
end;

function TReclosers.Get_Next: Integer;
Var
   pElem : TRecloserObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := RecloserClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := RecloserClass.ElementList.ActiveIndex;
          End
          Else pElem := RecloserClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;

procedure TReclosers.Set_Name(const Value: WideString);
// Set element active by name

begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If RecloserClass.SetActive(Value) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := RecloserClass.ElementList.Active;
          End
          Else Begin
              DoSimpleMsg('Recloser "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;
end;

function TReclosers.Get_MonitoredObj: WideString;
var
  elem: TRecloserObj;
begin
  Result := '';
  elem := RecloserClass.GetActiveObj;
  if elem <> nil then Result := elem.MonitoredElementName;
end;

procedure TReclosers.Set_MonitoredObj(const Value: WideString);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.GetActiveObj;
  if elem <> nil then Set_parameter('monitoredObj', Value);
end;

function TReclosers.Get_MonitoredTerm: Integer;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.GetActiveObj;
  if elem <> nil then Result := elem.MonitoredElementTerminal;
end;

procedure TReclosers.Set_MonitoredTerm(Value: Integer);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.GetActiveObj;
  if elem <> nil then Set_parameter('monitoredterm', IntToStr(Value));
end;

function TReclosers.Get_SwitchedObj: WideString;
var
  elem: TRecloserObj;
begin
  Result := '';
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.ElementName;
end;

procedure TReclosers.Set_SwitchedObj(const Value: WideString);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.GetActiveObj;
  if elem <> nil then Set_parameter('SwitchedObj', Value);
end;

function TReclosers.Get_SwitchedTerm: Integer;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.GetActiveObj;
  if elem <> nil then Result := elem.ElementTerminal;
end;

procedure TReclosers.Set_SwitchedTerm(Value: Integer);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.GetActiveObj;
  if elem <> nil then Set_parameter('SwitchedTerm', IntToStr(Value));
end;

function TReclosers.Get_NumFast: Integer;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.NumFast;
end;

function TReclosers.Get_RecloseIntervals: OleVariant;
// return reclose intervals in seconds
Var
  elem: TRecloserObj;
  i, k: Integer;
Begin
  Result := VarArrayCreate([0, 0], varDouble);
  Result[0] := -1.0;
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
      elem := RecloserClass.ElementList.Active;
      If elem <> Nil Then
      Begin
        VarArrayRedim(Result, elem.NumReclose-1);
        k:=0;
        for i := 1 to elem.NumReclose  do
        Begin
            Result[k] := elem.RecloseIntervals ^[i];
            Inc(k);
        End;
      End;
  End;
end;

function TReclosers.Get_Shots: Integer;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.NumReclose + 1;
end;

procedure TReclosers.Set_NumFast(Value: Integer);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('numfast', IntToStr(Value));
end;

procedure TReclosers.Set_Shots(Value: Integer);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('shots', IntToStr(Value));
end;

function TReclosers.Get_GndInst: Double;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.GndInst;
end;

function TReclosers.Get_PhInst: Double;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.PhInst;
end;

procedure TReclosers.Set_GndInst(Value: Double);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('GndInst', Format('%.g',[Value]));
end;

procedure TReclosers.Set_PhInst(Value: Double);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('PhInst', Format('%.g',[Value]));
end;

procedure TReclosers.Close;
var
  elem: TRecloserObj;
  i: Integer;
begin
  elem := RecloserClass.ElementList.Active;
  for i := 1 to elem.ControlledElement.NPhases do elem.States[i] := CTRL_CLOSE // Close all phases
end;

procedure TReclosers.Open;
var
  elem: TRecloserObj;
  i: Integer;
begin
  elem := RecloserClass.ElementList.Active;
  for i := 1 to elem.ControlledElement.NPhases do elem.States[i] := CTRL_OPEN // Open all phases
end;

function TReclosers.Get_idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> Nil then
       Result := RecloserClass.ElementList.ActiveIndex
    else Result := 0;
end;

procedure TReclosers.Set_idx(Value: Integer);
Var
    pRecloser:TRecloserObj;
begin
    if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pRecloser := RecloserClass.Elementlist.Get(Value);
        If pRecloser <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pRecloser;
    End;
end;

function TReclosers.Get_NormalState: OleVariant;
Var
    i :Integer;
    pRecloser:TRecloserObj;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
       pRecloser := RecloserClass.GetActiveObj;
       If pRecloser <> Nil Then
       Begin
          Result := VarArrayCreate([0, pRecloser.ControlledElement.NPhases-1], varOleStr);
          For i := 1 to pRecloser.ControlledElement.NPhases Do Begin
             if pRecloser.NormalStates[i] = CTRL_CLOSE then Result[i-1] := 'closed' else Result[i-1] := 'open';
          End;
       End;
     End
     Else
         Result := VarArrayCreate([0, 0], varOleStr);
end;

function TReclosers.Get_State: OleVariant;
Var
    i :Integer;
    pRecloser:TRecloserObj;
begin

    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
       pRecloser := RecloserClass.GetActiveObj;
       If pRecloser <> Nil Then
       Begin
          Result := VarArrayCreate([0, pRecloser.ControlledElement.NPhases-1], varOleStr);
          For i := 1 to pRecloser.ControlledElement.NPhases Do Begin
             if pRecloser.States[i] = CTRL_CLOSE then Result[i-1] := 'closed' else Result[i-1] := 'open';
          End;
       End;
     End
     Else
         Result := VarArrayCreate([0, 0], varOleStr);
end;

procedure TReclosers.Reset;
Var
    pRecloser:TRecloserObj;
begin
      if ActiveCircuit[ActiveActor] <> Nil then   Begin
        pRecloser := RecloserClass.ElementList.Active;
        If pRecloser <> Nil Then pRecloser.Reset(ActiveActor);
    End;
end;

procedure TReclosers.Set_NormalState(Value: OleVariant);
Var
    i :Integer;
    Count, Low :Integer;
    pRecloser:TRecloserObj;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
         pRecloser := RecloserClass.GetActiveObj;
         If pRecloser <> Nil Then
         Begin
            Low := VarArrayLowBound(Value, 1);
            Count := VarArrayHighBound(Value, 1) - Low + 1;
            If Count >  pRecloser.ControlledElement.NPhases Then Count := pRecloser.ControlledElement.NPhases;
            For i := 1 to Count Do Begin
                case LowerCase(Value[i-1 + Low])[1] of
                  'o': pRecloser.NormalStates[i] := CTRL_OPEN;
                  'c': pRecloser.NormalStates[i] := CTRL_CLOSE;
                end;
            End;
         End;

     End;
end;

procedure TReclosers.Set_State(Value: OleVariant);
Var
    i :Integer;
    Count, Low :Integer;
    pRecloser:TRecloserObj;
begin
    If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
         pRecloser := RecloserClass.GetActiveObj;
         If pRecloser <> Nil Then
         Begin
            Low := VarArrayLowBound(Value, 1);
            Count := VarArrayHighBound(Value, 1) - Low + 1;
            If Count >  pRecloser.ControlledElement.NPhases Then Count := pRecloser.ControlledElement.NPhases;
            For i := 1 to Count Do Begin
                case LowerCase(Value[i-1 + Low])[1] of
                  'o': pRecloser.States[i] := CTRL_OPEN;
                  'c': pRecloser.States[i] := CTRL_CLOSE;
                end;
            End;
         End;

     End;
end;

function TReclosers.Get_GndFastPickup: Double;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.GndFastPickup;
end;

function TReclosers.Get_GndSlowPickup: Double;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.GndSlowPickup;
end;

function TReclosers.Get_PhFastPickup: Double;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.PhFastPickup;
end;

function TReclosers.Get_PhSlowPickup: Double;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.PhSlowPickup;
end;

procedure TReclosers.Set_GndFastPickup(Value: Double);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('GndFastPickup', Format('%.g',[Value]));
end;

procedure TReclosers.Set_GndSlowPickup(Value: Double);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('GndSlowPickup', Format('%.g',[Value]));
end;

procedure TReclosers.Set_PhFastPickup(Value: Double);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('PhFastPickup', Format('%.g',[Value]));
end;

procedure TReclosers.Set_PhSlowPickup(Value: Double);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('PhSlowPickup', Format('%.g',[Value]));
end;

function TReclosers.Get_InterruptingRating: Double;
var
  elem: TRecloserObj;
begin
  Result := 0;
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.InterruptingRating;
end;

function TReclosers.Get_RatedCurrent: Double;
Var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Result := elem.RatedCurrent
  else Result := -1.0;
end;

procedure TReclosers.Set_InterruptingRating(Value: Double);
var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('InterruptingRating', Format('%.g',[Value]));
end;

procedure TReclosers.Set_RatedCurrent(Value: Double);
Var
  elem: TRecloserObj;
begin
  elem := RecloserClass.ElementList.Active;
  if elem <> nil then Set_parameter('RatedCurrent', Format('%.8g ',[Value]));
end;

initialization
  TAutoObjectFactory.Create(ComServer, TReclosers, Class_Reclosers,
    ciInternal, tmApartment);
end.
