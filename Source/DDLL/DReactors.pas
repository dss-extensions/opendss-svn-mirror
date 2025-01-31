unit DReactors;

interface

function ReactorsI(mode: Longint; arg: Longint): Longint; CDECL;
function ReactorsF(mode: Longint; arg: Double): Double; CDECL;
function ReactorsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure ReactorsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    {$IFNDEF FPC_DLL}
    ComServ,
    {$ENDIF}
    DSSGlobals,
    Reactor,
    Variants,
    Pointerlist,
    Sysutils;


// Wrapper for concentating all the integer-based IO for the Reactor Obj mimicking COM
function ReactorsI(mode: Longint; arg: Longint): Longint; CDECL;
var
    Elem: TReactorObj;
    pList: TPointerList;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin           // Reactors.First
            if ActiveCircuit[ActiveActor] <> nil then
            begin

                if ReactorClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    pList := ReactorClass[ActiveActor].ElementList;
                    Elem := pList.First;
                    repeat
                        if Elem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
                            Result := 1;
                        end
                        else
                            Elem := pList.Next;
                    until (Result = 1) or (Elem = nil);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin           // Reactors.Next
            Result := 0;  // signify no more
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                if ReactorClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    pList := ReactorClass[ActiveActor].ElementList;
                    Elem := pList.Next;
                    if Elem <> nil then
                    begin
                        repeat
                            if Elem.Enabled then
                            begin
                                ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
                                Result := pList.ActiveIndex;
                            end
                            else
                                Elem := pList.Next;
                        until (Result > 0) or (Elem = nil);
                    end;
                end;

            end;
        end;
        2:
        begin            // Reactors.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ReactorClass[ActiveActor].ElementList.ListSize;
        end;
        3:
        begin            // Parallel.read
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                if Elem.IsParallel then
                    Result := 1;
            end;
        end;
        4:
        begin            // Parallel.write
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.IsParallel := arg > 0;
            end;
        end
    else
        Result := -1; // The user is asking for the wrong command
    end;
end;

// Wrapper for concentating all the double-based IO for the Reactor Obj mimicking COM
function ReactorsF(mode: Longint; arg: Double): Double; CDECL;
var
    Elem: TReactorObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin           // Reactor.kV Read
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Elem.kVNominal;
            end;
        end;
        1:
        begin           // Reactor.kV Write
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.kVNominal := arg;
            end;
        end;
        2:
        begin           // Reactor.kvar Read
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Elem.kvarNominal;
            end;
        end;
        3:
        begin           // Reactor.kvar Write
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.kvarNominal := arg;
            end;
        end;
        4:
        begin           // Reactor.lmH Read
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Elem.LNominal * 1e3;
            end;
        end;
        5:
        begin           // Reactor.lmH Write
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.LNominal := arg / 1e3;
            end;
        end;
        6:
        begin           // Reactor.R Read
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Elem.R;
            end;
        end;
        7:
        begin           // Reactor.R Write
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.R := arg;
            end;
        end;
        8:
        begin           // Reactor.Rp Read
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Elem.Rp;
            end;
        end;
        9:
        begin           // Reactor.Rp Write
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.Rp := arg;
            end;
        end;
        10:
        begin           // Reactor.X Read
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Elem.X;
            end;
        end;
        11:
        begin           // Reactor.X Write
            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.X := arg;
            end;
        end
    else
        Result := -1.0; // We got the wrong command
    end;

end;

// Wrapper for concentating all the string-based IO for the Reactor Obj mimicking COM
function ReactorsS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
var
    Elem: TReactorObj;
    k: Integer;
    pList: TPointerList;
    activesave: Integer;
    S: String;
    Found: Boolean;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin                   // Reactor.Name Read

            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Pansichar(Ansistring(Elem.Name));
            end;

        end;
        1:
        begin                   // Reactor.Name Write

            if ActiveCircuit[ActiveActor] <> nil then
            begin      // Search list of Storages in active circuit for name
                if ReactorClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := false;
                    pList := ReactorClass[ActiveActor].ElementList;
                    activesave := pList.ActiveIndex;
                    Elem := pList.First;
                    while Elem <> nil do
                    begin
                        if (CompareText(Elem.Name, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
                            Found := true;
                            Break;
                        end;
                        Elem := pList.Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Reactor "' + S + '" Not Found in Active Circuit.', 20003);
                        Elem := pList.Get(activesave);    // Restore active Storage
                        ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
                    end;
                end;
            end;

        end;
        2:
        begin                   // Reactor.LCurve Read

            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Pansichar(Ansistring(Elem.LCurve));
            end;

        end;
        3:
        begin                   // Reactor.LCurve Write

            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.LCurve := String(arg);
            end;

        end;
        4:
        begin                   // Reactor.RCurve Read

            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Result := Pansichar(Ansistring(Elem.RCurve));
            end;

        end;
        5:
        begin                   // Reactor.RCurve Write

            Elem := ReactorClass[ActiveActor].GetActiveObj;
            if Elem <> nil then
            begin
                Elem.RCurve := String(arg);
            end;

        end;
    end;

end;

// Wrapper for concentating all the array-like IO structures for the Reactor Obj mimicking COM
procedure ReactorsV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    Elem: TReactorObj;
    idx,
    i,
    k: Integer;
    pList: TPointerList;
    PDouble: ^Double;

begin
    case mode of
        0:
        begin                   // Reactors.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                with ActiveCircuit[ActiveActor] do
                begin

                    if ReactorClass[ActiveActor].ElementList.ListSize > 0 then
                    begin
                        pList := ReactorClass[ActiveActor].ElementList;

                        k := 0;
                        Elem := pList.First;
                        while Elem <> nil do
                        begin
                            WriteStr2Array(Elem.Name);
                            WriteStr2Array(Char(0));
                            Inc(k);
                            Elem := pList.Next;
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
        begin                   // Reactors.RMatrix read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        if RMatrix <> nil then
                        begin
                            setlength(myDBLArray, Sqr(Nphases));
                            for i := 1 to Sqr(NPhases) do
                            begin
                                myDBLArray[i - 1] := Rmatrix[i];
                            end;
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        2:
        begin                   // Reactors.RMatrix write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        Reallocmem(Rmatrix, Sizeof(Double) * NPhases * NPhases);
                        for i := 1 to Sqr(NPhases) do
                        begin
                            PDouble := myPointer;
                            Rmatrix[i] := PDouble^;
                            Inc(k);
                            inc(Pbyte(myPointer), 8);
                        end;
                        YprimInvalid[ActiveActor] := true;
                    end;
                end;
            end;
            mySize := k;
        end;
        3:
        begin                   // Reactors.XMatrix read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        if XMatrix <> nil then
                        begin
                            setlength(myDBLArray, Sqr(Nphases));
                            for i := 1 to Sqr(NPhases) do
                            begin
                                myDBLArray[i - 1] := Xmatrix[i];
                            end;
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        4:
        begin                   // Reactors.XMatrix write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        Reallocmem(Xmatrix, Sizeof(Double) * NPhases * NPhases);
                        for i := 1 to Sqr(NPhases) do
                        begin
                            PDouble := myPointer;
                            Xmatrix[i] := PDouble^;
                            Inc(k);
                            inc(Pbyte(myPointer), 8);
                        end;
                        YprimInvalid[ActiveActor] := true;
                    end;
                end;
            end;
            mySize := k;
        end;
        5:
        begin                   // Reactors.Z read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        setlength(myDBLArray, 2);
                        myDBLArray[0] := Z.re;
                        myDBLArray[1] := Z.im;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        6:
        begin                   // Reactors.Z write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        PDouble := myPointer;
                        Z.re := PDouble^;
                        inc(Pbyte(myPointer), 8);
                        PDouble := myPointer;
                        Z.im := PDouble^;
                        YprimInvalid[ActiveActor] := true;
                        k := 2;
                    end;
                end;
            end;
            mySize := k;
        end;
        7:
        begin                   // Reactors.Z0 read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        setlength(myDBLArray, 2);
                        myDBLArray[0] := Z0.re;
                        myDBLArray[1] := Z0.im;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        8:
        begin                   // Reactors.Z0 write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        PDouble := myPointer;
                        Z0.re := PDouble^;
                        Inc(k);
                        inc(Pbyte(myPointer), 8);
                        PDouble := myPointer;
                        Z0.im := PDouble^;
                        YprimInvalid[ActiveActor] := true;
                        k := 2;
                    end;
                end;
            end;
            mySize := k;
        end;
        9:
        begin                   // Reactors.Z1 read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        setlength(myDBLArray, 2);
                        myDBLArray[0] := Z1.re;
                        myDBLArray[1] := Z1.im;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        10:
        begin                   // Reactors.Z1 write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        PDouble := myPointer;
                        Z1.re := PDouble^;
                        inc(Pbyte(myPointer), 8);
                        PDouble := myPointer;
                        Z1.im := PDouble^;
                        YprimInvalid[ActiveActor] := true;
                        k := 2;
                    end;
                end;
            end;
            mySize := k;
        end;
        11:
        begin                   // Reactors.Z2 read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        setlength(myDBLArray, 2);
                        myDBLArray[0] := Z2.re;
                        myDBLArray[1] := Z2.im;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        12:
        begin                   // Reactors.Z2 write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                Elem := ReactorClass[ActiveActor].GetActiveObj;
                if Elem <> nil then
                begin
                    with Elem do
                    begin
                        PDouble := myPointer;
                        Z2.re := PDouble^;
                        inc(Pbyte(myPointer), 8);
                        PDouble := myPointer;
                        Z2.im := PDouble^;
                        YprimInvalid[ActiveActor] := true;
                        k := 2;
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
