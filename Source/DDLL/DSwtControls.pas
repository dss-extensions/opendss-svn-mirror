unit DSwtControls;

interface

function SwtControlsI(mode: Longint; arg: Longint): Longint; CDECL;
function SwtControlsF(mode: Longint; arg: Double): Double; CDECL;
function SwtControlsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure SwtControlsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    ControlElem,
    SwtControl,
    Variants,
    SysUtils,
    PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
    Result := nil;
    if ActiveCircuit[ActiveActor] <> nil then
        Result := ActiveCircuit[ActiveActor].SwtControls.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := false;  // Reset for commands entered from outside
    cmd := Format('swtcontrol.%s.%s=%s', [ActiveSwtControl.Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function SwtControlsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    elem: TSwtControlObj;
    lst: TPointerList;
    i: Integer;

begin
    Result := 0;      // Default return value
    case mode of
        0:
        begin  // SwtControls.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                lst := ActiveCircuit[ActiveActor].SwtControls;
                elem := lst.First;
                if elem <> nil then
                begin
                    repeat
                        if elem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                            Result := 1;
                        end
                        else
                            elem := lst.Next;
                    until (Result = 1) or (elem = nil);
                end;
            end;
        end;
        1:
        begin  // SwtControls.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                lst := ActiveCircuit[ActiveActor].SwtControls;
                elem := lst.Next;
                if elem <> nil then
                begin
                    repeat
                        if elem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                            Result := lst.ActiveIndex;
                        end
                        else
                            elem := lst.Next;
                    until (Result > 0) or (elem = nil);
                end
            end;
        end;
        2:
        begin  // SwtControls.IsLocked read
            Result := 0;
            elem := ActiveSwtControl;
            if elem <> nil then
            begin
                if elem.IsLocked then
                    Result := 1;
            end;
        end;
        3:
        begin  // SwtControls.IsLocked write
            if arg = 1 then
                Set_Parameter('Lock', 'y')
            else
                Set_Parameter('Lock', 'n');
        end;
        4:
        begin  // SwtControls.SwitchedTerm read
            Result := 0;
            elem := ActiveSwtControl;
            if elem <> nil then
                Result := elem.ElementTerminal;
        end;
        5:
        begin   // SwtControls.SwitchedTerm write
            Set_Parameter('SwitchedTerm', IntToStr(arg));
        end;
        6:
        begin  // SwtControls.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].SwtControls.ListSize;
        end;
        7:
        begin  // SwtControls.Open
            elem := ActiveSwtControl;
            if elem <> nil then
            begin
                for i := 1 to elem.ControlledElement.NPhases do
                    elem.States[i] := CTRL_OPEN // Open all phases
            end;
        end;
        8:
        begin  // SwtControls.Close
            elem := ActiveSwtControl;
            if elem <> nil then
            begin
                for i := 1 to elem.ControlledElement.NPhases do
                    elem.States[i] := CTRL_CLOSE // Close all phases
            end;
        end;
        9:
        begin  // SwtControls.Reset
            elem := ActiveSwtControl;
            if elem <> nil then
            begin
                elem.Locked := false;
                elem.Reset(ActiveActor);
            end;
        end;
    else
        Result := -1;
    end;
end;

//************************************Floating point type properties****************
function SwtControlsF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TSwtControlObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // SwtControls.Delay read
            Result := 0.0;
            elem := ActiveSwtControl;
            if elem <> nil then
                Result := elem.TimeDelay;
        end;
        1:
        begin  // SwtControls.Delay write
            Set_Parameter('Delay', FloatToStr(arg));
        end
    else
        Result := -1.0;
    end;
end;

//************************************String type properties************************
function SwtControlsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    elem: TSwtControlObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    lst: TPointerList;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin  // SwtControls.Name read
            Result := Pansichar(Ansistring(''));
            elem := ActiveSwtControl;
            if elem <> nil then
                Result := Pansichar(Ansistring(elem.Name));
        end;
        1:
        begin  // SwtControls.Name write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                lst := ActiveCircuit[ActiveActor].SwtControls;
                S := String(arg);  // Convert to Pascal String
                Found := false;
                ActiveSave := lst.ActiveIndex;
                elem := lst.First;
                while elem <> nil do
                begin
                    if (CompareText(elem.Name, S) = 0) then
                    begin
                        ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                        Found := true;
                        Break;
                    end;
                    elem := lst.Next;
                end;
                if not Found then
                begin
                    DoSimpleMsg('SwtControl "' + S + '" Not Found in Active Circuit.', 5003);
                    elem := lst.Get(ActiveSave);    // Restore active Load
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                end;
            end;
        end;
        2:
        begin  // SwtControl.SwitchedObj read
            Result := Pansichar(Ansistring(''));
            elem := ActiveSwtControl;
            if elem <> nil then
                Result := Pansichar(Ansistring(elem.ElementName));
        end;
        3:
        begin  // SwtControl.SwitchedObj write
            Set_Parameter('SwitchedObj', String(arg));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//******************************Variant type properties*****************************
procedure SwtControlsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    elem: TSwtControlObj;
    lst: TPointerList;
    k,
    i: Integer;
    S: String;

begin
    case mode of
        0:
        begin  // SwtControls.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if SwtControls.ListSize > 0 then
                    begin
                        lst := SwtControls;
                        elem := lst.First;
                        while elem <> nil do
                        begin
                            WriteStr2Array(elem.Name);
                            WriteStr2Array(Char(0));
                            elem := lst.Next;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // SwtControls.State read
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ActiveSwtControl;
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
        2:
        begin  // SwtControls.State write
            myType := 4;          // String
            k := 0;
            elem := ActiveSwtControl;
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
        3:
        begin  // SwtControls.NormalState read
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ActiveSwtControl;
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
        4:
        begin  // SwtControls.NormalState write
            elem := ActiveSwtControl;
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
