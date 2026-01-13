unit DReclosers;

interface

function ReclosersI(mode: Longint; arg: Longint): Longint; CDECL;
function ReclosersF(mode: Longint; arg: Double): Double; CDECL;
function ReclosersS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure ReclosersV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    Executive,
    Sysutils,
    ControlElem,
    Recloser,
    PointerList,
    Variants,
    DSSGlobals,
    DSSClassDefs;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := false;  // Reset for commands entered from outside
    cmd := Format('recloser.%s.%s=%s', [TRecloserObj(RecloserClass.GetActiveObj).Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function ReclosersI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pElem: TRecloserObj;
    elem: TRecloserObj;
    pRecloser: TRecloserObj;
    i: Integer;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // Reclosers.Count
            Result := 0;
            if ActiveCircuit[ActiveActor] <> nil then
                Result := RecloserClass.ElementList.ListSize;
        end;
        1:
        begin  // Reclosers.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pElem := RecloserClass.ElementList.First;
                if pElem <> nil then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := 1;
                        end
                        else
                            pElem := RecloserClass.ElementList.Next;
                    until (Result = 1) or (pElem = nil);
            end;
        end;
        2:
        begin  // Reclosers.Next
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pElem := RecloserClass.ElementList.Next;
                if pElem <> nil then
                    repeat
                        if pElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                            Result := RecloserClass.ElementList.ActiveIndex;
                        end
                        else
                            pElem := RecloserClass.ElementList.Next;
                    until (Result > 0) or (pElem = nil);
            end;
        end;
        3:
        begin  // Reclosers.MonitoredTerm read
            Result := 0;
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
                Result := elem.MonitoredElementTerminal;
        end;
        4:
        begin  // Reclosers.MonitoredTerm write
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
                Set_parameter('monitoredterm', IntToStr(arg));
        end;
        5:
        begin  // Reclosers.SwitchedTerm read
            Result := 0;
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
                Result := elem.ElementTerminal;
        end;
        6:
        begin  // Reclosers.SwitchedTerm write
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
                Set_parameter('SwitchedTerm', IntToStr(arg));
        end;
        7:
        begin  // Reclosers.NumFast read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            ;
            if elem <> nil then
                Result := elem.NumFast;
        end;
        8:
        begin  // Reclosers.NumFast write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('numfast', IntToStr(arg));
        end;
        9:
        begin  // Reclosers.Shots read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            ;
            if elem <> nil then
                Result := elem.NumReclose + 1;
        end;
        10:
        begin  // Reclosers.Shots write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('shots', IntToStr(arg));
        end;
        11:
        begin  // Recloser.Open
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
            begin
                for i := 1 to elem.ControlledElement.NPhases do
                    elem.States[i] := CTRL_OPEN // Open all phases
            end;
        end;
        12:
        begin  // Reclosers.Close
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
            begin
                for i := 1 to elem.ControlledElement.NPhases do
                    elem.States[i] := CTRL_CLOSE // Close all phases
            end;
        end;
        13:
        begin // Reclosers.Idx read
            if ActiveCircuit[ActiveActor] <> nil then
                Result := RecloserClass.ElementList.ActiveIndex
            else
                Result := 0;
        end;
        14:
        begin // Reclosers.Idx write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pRecloser := RecloserClass.Elementlist.Get(arg);
                if pRecloser <> nil then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pRecloser;
            end;
        end;
        15:
        begin  // Reclosers.Reset
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                elem.Reset(ActiveActor);
        end
    else
        Result := -1;
    end;
end;

//********************Floating point type properties******************************
function ReclosersF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TRecloserObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Reclosers.PhFastPickup read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := elem.PhFastPickup;
        end;
        1:
        begin  // Reclosers.PhFastPickup write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('PhFastPickup', Format('%.g', [arg]));
        end;
        2:
        begin  // Reclosers.PhInst read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := elem.PhInst;
        end;
        3:
        begin  // Reclosers.PhInst write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('PhInst', Format('%.g', [arg]));
        end;
        4:
        begin  // Reclosers.GndFastPickup read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := elem.GndFastPickup;
        end;
        5:
        begin  // Reclosers.GndFastPickup write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('GndFastPickup', Format('%.g', [arg]));
        end;
        6:
        begin  // Reclosers.GndInst read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := elem.GndInst;
        end;
        7:
        begin  // Reclosers.GndInst write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('GndInst', Format('%.g', [arg]));
        end;
        8:
        begin  // Reclosers.PhSlowPickup read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := elem.PhSlowPickup;
        end;
        9:
        begin  // Reclosers.PhSlowPickup write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('PhSlowPickup', Format('%.g', [arg]));
        end;
        10:
        begin  // Reclosers.GndSlowPickup read
            Result := 0;
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := elem.GndSlowPickup;
        end;
        11:
        begin  // Reclosers.GndSlowPickup write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('GndSlowPickup', Format('%.g', [arg]));
        end;
        12:
        begin  // Reclosers.RatedCurrent read
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := elem.RatedCurrent
            else
                Result := -1.0;
        end;
        13:
        begin  // Reclosers.RatedCurrent write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('RatedCurrent', Format('%.8g ', [arg]));
        end;
        14:
        begin  // Reclosers.InterruptingRating read
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := elem.InterruptingRating
            else
                Result := -1.0;
        end;
        15:
        begin  // Reclosers.InterruptingRating write
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Set_parameter('InterruptingRating', Format('%.8g ', [arg]));
        end
    else
        Result := -1.0;
    end;
end;

//********************String type properties******************************
function ReclosersS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TRecloserObj;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // Reclosers.Name read
            Result := Pansichar(Ansistring(''));
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // Reclosers.Name write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                if RecloserClass.SetActive(String(arg)) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := RecloserClass.ElementList.Active;
                end
                else
                begin
                    DoSimpleMsg('Recloser "' + arg + '" Not Found in Active Circuit.', 77003);
                end;
            end;
        end;
        2:
        begin  // Reclosers.MonitoredObj read
            Result := Pansichar(Ansistring(''));
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
                Result := Pansichar(Ansistring(elem.MonitoredElementName));
        end;
        3:
        begin  // Reclosers.MonitoredObj write
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
                Set_parameter('monitoredObj', String(arg));
        end;
        4:
        begin  // Reclosers.SwitchedObj read
            Result := Pansichar(Ansistring(''));
            elem := RecloserClass.ElementList.Active;
            if elem <> nil then
                Result := Pansichar(Ansistring(elem.ElementName));
        end;
        5:
        begin  // Reclosers.SwitchedObj write
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
                Set_parameter('SwitchedObj', String(arg));
        end;
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//********************Variant type properties******************************
procedure ReclosersV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TRecloserObj;
    pList: TPointerList;
    k, i: Integer;
    S: String;

begin
    case mode of
        0:
        begin  // Reclosers.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                if RecloserClass.ElementList.ListSize > 0 then
                begin
                    pList := RecloserClass.ElementList;
                    elem := pList.First;
                    while elem <> nil do
                    begin
                        WriteStr2Array(elem.Name);
                        WriteStr2Array(Char(0));
                        elem := pList.next;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // Reclosers.RecloseIntervals
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                elem := RecloserClass.ElementList.Active;
                if elem <> nil then
                begin
                    setlength(myDBLArray, elem.NumReclose);
                    k := 0;
                    for i := 1 to elem.NumReclose do
                    begin
                        myDBLArray[k] := elem.RecloseIntervals^[i];
                        Inc(k);
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        2:
        begin  // Reclosers.State read
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := RecloserClass.GetActiveObj;
                if Elem <> nil then
                begin
                    for i := 1 to elem.ControlledElement.Nphases do
                    begin
                        if elem.States[i] = CTRL_CLOSE then
                            WriteStr2Array('closed')
                        else
                            WriteStr2Array('open');
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        3:
        begin  // Reclosers.State write
            myType := 4;          // String
            k := 0;
            elem := RecloserClass.GetActiveObj;
            if elem <> nil then
            begin

                for i := 1 to elem.ControlledElement.NPhases do
                begin
                    S := BArray2Str(myPointer, k);
                    if S = '' then
                        break
                    else
                    begin
                        case LowerCase(S)[1] of
                            'o':
                                elem.States[i] := CTRL_OPEN;
                            'c':
                                elem.States[i] := CTRL_CLOSE;
                        end;
                    end;
                end;
            end;
            mySize := k;
        end;
        4:
        begin  // Reclosers.NormalState read
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := RecloserClass.GetActiveObj;
                if Elem <> nil then
                begin
                    for i := 1 to elem.ControlledElement.Nphases do
                    begin
                        if elem.NormalStates[i] = CTRL_CLOSE then
                            WriteStr2Array('closed')
                        else
                            WriteStr2Array('open');
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        5:
        begin  // Reclosers.NormalState write
            elem := RecloserClass.GetActiveObj;
            k := 0;
            if elem <> nil then
            begin
      // allocate space based on number of phases of controlled device
                for i := 1 to elem.ControlledElement.NPhases do
                begin
                    S := BArray2Str(myPointer, k);
                    if S = '' then
                        break
                    else
                    begin
                        case LowerCase(S)[1] of
                            'o':
                                elem.NormalStates[i] := CTRL_OPEN;
                            'c':
                                elem.NormalStates[i] := CTRL_CLOSE;
                        end;
                    end;
                end;
            end;
            mySize := k;
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;
end;

end.
