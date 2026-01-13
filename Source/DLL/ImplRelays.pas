unit ImplRelays;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TRelays = class(TAutoObject, IRelays)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        procedure Close; SAFECALL;
        procedure Open; SAFECALL;
        procedure Reset; SAFECALL;
        function Get_State: Olevariant; SAFECALL;
        procedure Set_State(Value: Olevariant); SAFECALL;
        function Get_NormalState: Olevariant; SAFECALL;
        procedure Set_NormalState(Value: Olevariant); SAFECALL;

    end;

implementation

uses
    ComServ,
    Executive,
    Relay,
    ControlElem,
    Circuit,
    DSSGlobals,
    Sysutils,
    Pointerlist,
    Variants;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := false;  // Reset for commands entered from outside
    cmd := Format('Relay.%s.%s=%s', [TRelayObj(RelayClass.GetActiveObj).Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function TRelays.Get_AllNames: Olevariant;
var
    elem: TRelayObj;
    pList: TPointerList;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        if RelayClass.ElementList.ListSize > 0 then
        begin
            pList := RelayClass.ElementList;
            VarArrayRedim(Result, pList.ListSize - 1);
            k := 0;
            elem := pList.First;
            while elem <> nil do
            begin
                Result[k] := elem.Name;
                Inc(k);
                elem := pList.next;
            end;
        end;
    end;

end;

function TRelays.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
        Result := RelayClass.ElementList.ListSize;
end;

function TRelays.Get_First: Integer;
var
    pElem: TRelayObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pElem := RelayClass.ElementList.First;
        if pElem <> nil then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := RelayClass.ElementList.Next;
            until (Result = 1) or (pElem = nil);
    end;
end;


function TRelays.Get_Name: Widestring;
var
    elem: TRelayObj;
begin
    Result := '';
    elem := RelayClass.GetActiveObj;
    if elem <> nil then
        Result := elem.Name;
end;

function TRelays.Get_Next: Integer;
var
    pElem: TRelayObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pElem := RelayClass.ElementList.Next;
        if pElem <> nil then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := RelayClass.ElementList.ActiveIndex;
                end
                else
                    pElem := RelayClass.ElementList.Next;
            until (Result > 0) or (pElem = nil);
    end;
end;

procedure TRelays.Set_Name(const Value: Widestring);
// Set element active by name

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        if RelayClass.SetActive(Value) then
        begin
            ActiveCircuit[ActiveActor].ActiveCktElement := RelayClass.ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('Relay "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;

function TRelays.Get_MonitoredObj: Widestring;
var
    elem: TRelayObj;
begin
    Result := '';
    elem := RelayClass.GetActiveObj;
    if elem <> nil then
        Result := elem.MonitoredElementName;
end;

procedure TRelays.Set_MonitoredObj(const Value: Widestring);
var
    elem: TRelayObj;
begin
    elem := RelayClass.GetActiveObj;
    if elem <> nil then
        Set_parameter('monitoredObj', Value);
end;

function TRelays.Get_MonitoredTerm: Integer;
var
    elem: TRelayObj;
begin
    Result := 0;
    elem := RelayClass.GetActiveObj;
    if elem <> nil then
        Result := elem.MonitoredElementTerminal;
end;

function TRelays.Get_SwitchedObj: Widestring;
var
    elem: TRelayObj;
begin
    Result := '';
    elem := RelayClass.ElementList.Active;
    if elem <> nil then
        Result := elem.ElementName;

end;


procedure TRelays.Set_MonitoredTerm(Value: Integer);
var
    elem: TRelayObj;
begin
    elem := RelayClass.GetActiveObj;
    if elem <> nil then
        Set_parameter('monitoredterm', IntToStr(Value));

end;

procedure TRelays.Set_SwitchedObj(const Value: Widestring);
var
    elem: TRelayObj;
begin
    elem := RelayClass.GetActiveObj;
    if elem <> nil then
        Set_parameter('SwitchedObj', Value);

end;

function TRelays.Get_SwitchedTerm: Integer;
var
    elem: TRelayObj;
begin
    Result := 0;
    elem := RelayClass.ElementList.Active;
    if elem <> nil then
        Result := elem.ElementTerminal;
end;

procedure TRelays.Set_SwitchedTerm(Value: Integer);
var
    elem: TRelayObj;
begin
    elem := RelayClass.GetActiveObj;
    if elem <> nil then
        Set_parameter('SwitchedTerm', IntToStr(Value));
end;

function TRelays.Get_idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> nil then
        Result := RelayClass.ElementList.ActiveIndex
    else
        Result := 0;
end;

procedure TRelays.Set_idx(Value: Integer);
var
    pRelay: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pRelay := Relayclass.Elementlist.Get(Value);
        if pRelay <> nil then
            ActiveCircuit[ActiveActor].ActiveCktElement := pRelay;
    end;
end;

procedure TRelays.Close;
var
    elem: TRelayObj;
    i: Integer;
begin
    elem := RelayClass.ElementList.Active;
    for i := 1 to elem.ControlledElement.NPhases do
        elem.States[i] := CTRL_CLOSE // Close all phases
end;

procedure TRelays.Open;
var
    elem: TRelayObj;
    i: Integer;
begin
    elem := RelayClass.ElementList.Active;
    for i := 1 to elem.ControlledElement.NPhases do
        elem.States[i] := CTRL_OPEN // Open all phases
end;

procedure TRelays.Reset;
var
    pRelay: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pRelay := RelayClass.ElementList.Active;
        if pRelay <> nil then
            pRelay.Reset(ActiveActor);
    end;
end;

function TRelays.Get_State: Olevariant;
var
    i: Integer;
    pRelay: TRelayObj;
begin

    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pRelay := RelayClass.ElementList.Active;
        if pRelay <> nil then
        begin
            Result := VarArrayCreate([0, pRelay.ControlledElement.NPhases - 1], varOleStr);
            for i := 1 to pRelay.ControlledElement.NPhases do
            begin
                if pRelay.States[i] = CTRL_CLOSE then
                    Result[i - 1] := 'closed'
                else
                    Result[i - 1] := 'open';
            end;
        end
    end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

procedure TRelays.Set_State(Value: Olevariant);
var
    i: Integer;
    Count, Low: Integer;
    pRelay: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pRelay := RelayClass.ElementList.Active;
        if pRelay <> nil then
        begin
            Low := VarArrayLowBound(Value, 1);
            Count := VarArrayHighBound(Value, 1) - Low + 1;
            if Count > pRelay.ControlledElement.NPhases then
                Count := pRelay.ControlledElement.NPhases;
            for i := 1 to Count do
            begin
                case LowerCase(Value[i - 1 + Low])[1] of
                    'o':
                        pRelay.States[i] := CTRL_OPEN;
                    'c':
                        pRelay.States[i] := CTRL_CLOSE;
                end;
            end;

        end
    end;
end;

function TRelays.Get_NormalState: Olevariant;
var
    i: Integer;
    pRelay: TRelayObj;
begin
    Result := dssActionNone;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pRelay := RelayClass.ElementList.Active;
        if pRelay <> nil then
        begin
            Result := VarArrayCreate([0, pRelay.ControlledElement.NPhases - 1], varOleStr);
            for i := 1 to pRelay.ControlledElement.NPhases do
            begin
                if pRelay.NormalStates[i] = CTRL_CLOSE then
                    Result[i - 1] := 'closed'
                else
                    Result[i - 1] := 'open';
            end;
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

procedure TRelays.Set_NormalState(Value: Olevariant);
var
    i: Integer;
    Count, Low: Integer;
    pRelay: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Low := VarArrayLowBound(Value, 1);
        Count := VarArrayHighBound(Value, 1) - Low + 1;
        if Count > pRelay.ControlledElement.NPhases then
            Count := pRelay.ControlledElement.NPhases;
        for i := 1 to Count do
        begin
            case LowerCase(Value[i - 1 + Low])[1] of
                'o':
                    pRelay.NormalStates[i] := CTRL_OPEN;
                'c':
                    pRelay.NormalStates[i] := CTRL_CLOSE;
            end;
        end;
    end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TRelays, Class_Relays,
        ciInternal, tmApartment);
end.
