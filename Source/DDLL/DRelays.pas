unit DRelays;

interface

function RelaysI(mode: Longint; arg: Longint): Longint; CDECL;
function RelaysS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure RelaysV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    Executive,
    Relay,
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
    DSSExecutive.Command := cmd;
end;

function RelaysI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pElem: TRelayObj;
    elem: TRelayObj;
    pRelay: TRelayObj;

begin
    Result := 0;          // Default return value
    case mode of
        0:
        begin  // Relays.Count
            Result := 0;
            if ActiveCircuit[ActiveActor] <> nil then
                Result := RelayClass.ElementList.ListSize;
        end;
        1:
        begin  // Relays.First
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
        2:
        begin  // Relays.Next
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
        3:
        begin  // Relays.MonitoredTerm read
            Result := 0;
            elem := RelayClass.GetActiveObj;
            if elem <> nil then
                Result := elem.MonitoredElementTerminal;
        end;
        4:
        begin  // Relays.MonitoredTerm write
            elem := RelayClass.GetActiveObj;
            if elem <> nil then
                Set_parameter('monitoredterm', IntToStr(arg));
        end;
        5:
        begin  // Relays.SwitchedTerm read
            Result := 0;
            elem := RelayClass.ElementList.Active;
            if elem <> nil then
                Result := elem.ElementTerminal;
        end;
        6:
        begin  // Relays.SwitchedTerm read
            elem := RelayClass.GetActiveObj;
            if elem <> nil then
                Set_parameter('SwitchedTerm', IntToStr(arg));
        end;
        7:
        begin  // Relays.Idx read
            if ActiveCircuit[ActiveActor] <> nil then
                Result := RelayClass.ElementList.ActiveIndex
            else
                Result := 0;
        end;
        8:
        begin  // Relays.Idx write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pRelay := Relayclass.Elementlist.Get(arg);
                if pRelay <> nil then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pRelay;
            end;
        end
    else
        Result := -1;
    end;
end;

//****************************String type properties****************************
function RelaysS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TRelayObj;

begin
    Result := Pansichar(Ansistring(''));    // Default return value
    case mode of
        0:
        begin   // Relays.Name read
            Result := Pansichar(Ansistring(''));
            elem := RelayClass.GetActiveObj;
            if elem <> nil then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin   // Relays.Name write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                if RelayClass.SetActive(Widestring(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := RelayClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Relay "' + Widestring(arg) + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end;
        2:
        begin   // Relays.MonitoredObj read
            Result := Pansichar(Ansistring(''));
            elem := RelayClass.GetActiveObj;
            if elem <> nil then
                Result := Pansichar(Ansistring(elem.MonitoredElementName));
        end;
        3:
        begin   // Relays.MonitoredObj write
            elem := RelayClass.GetActiveObj;
            if elem <> nil then
                Set_parameter('monitoredObj', Widestring(arg));
        end;
        4:
        begin   // Relays.SwitchedObj read
            Result := Pansichar(Ansistring(''));
            elem := RelayClass.ElementList.Active;
            if elem <> nil then
                Result := Pansichar(Ansistring(elem.ElementName));
        end;
        5:
        begin   // Relays.SwitchedObj write
            elem := RelayClass.GetActiveObj;
            if elem <> nil then
                Set_parameter('SwitchedObj', Widestring(arg));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//****************************Variant type properties****************************
procedure RelaysV(mode: Longint; out arg: Variant); CDECL;

var
    elem: TRelayObj;
    pList: TPointerList;
    k: Integer;

begin
    case mode of
        0:
        begin  // Relays.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                if RelayClass.ElementList.ListSize > 0 then
                begin
                    pList := RelayClass.ElementList;
                    VarArrayRedim(arg, pList.ListSize - 1);
                    k := 0;
                    elem := pList.First;
                    while elem <> nil do
                    begin
                        arg[k] := elem.Name;
                        Inc(k);
                        elem := pList.next;
                    end;
                end;
            end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
