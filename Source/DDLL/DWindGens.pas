unit DWindGens;

interface

function WindGensI(mode: Longint; arg: Longint): Longint; CDECL;
function WindGensF(mode: Longint; arg: Double): Double; CDECL;
function WindGensS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure WindGensV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    {$IFNDEF FPC_DLL}
    ComServ,
    {$ENDIF}
    DSSGlobals,
    WindGen,
    Variants,
    Pointerlist,
    Sysutils;

// Wrapper for concentating all the integer-based IO for the WindGen Obj mimicking COM
function WindGensI(mode: Longint; arg: Longint): Longint; CDECL;
var
    WindGenElem: TWindGenObj;
    pList: TPointerList;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin           // WindGens.First
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                if WindGenClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    pList := WindGenClass[ActiveActor].ElementList;
                    WindGenElem := pList.First;
                    repeat
                        if WindGenElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
                            Result := 1;
                        end
                        else
                            WindGenElem := pList.Next;
                    until (Result = 1) or (WindGenElem = nil);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin           // WindGens.Next
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                if WindGenClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    pList := WindGenClass[ActiveActor].ElementList;
                    WindGenElem := pList.First;
                    repeat
                        if WindGenElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
                            Result := pList.ActiveIndex;
                        end
                        else
                            WindGenElem := pList.Next;
                    until (Result > 0) or (WindGenElem = nil);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin           // WindGens.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := WindGenClass[ActiveActor].ElementList.ListSize;
        end;
        3:
        begin           // WindGens.Idx Read
            if ActiveCircuit[ActiveActor] <> nil then
                Result := WindGenClass[ActiveActor].ElementList.ActiveIndex
        end;
        4:
        begin           // WindGens.Idx Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                WindGenElem := WindGenClass[ActiveActor].ElementList.Get(arg);
                if WindGenElem <> nil then
                    ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
            end;
        end;
        5:
        begin           // WindGens.N_WTG Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.N_WTG
            end;
        end;
        6:
        begin           // WindGens.N_WTG Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.N_WTG := arg
            end;
        end;
        7:
        begin           // WindGens.NPoles Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := Trunc(WindGenElem.WindGenVars.Poles)
            end;
        end;
        8:
        begin           // WindGens.NPoles Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.Poles := arg
            end;
        end;
        9:
        begin           // WindGens.QFlag Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.QFlg
            end;
        end;
        10:
        begin           // WindGens.QFlag Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.QFlg := arg
            end;
        end;
        11:
        begin           // WindGens.QMode Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.QMode
            end;
        end;
        12:
        begin           // WindGens.QMode Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.QMode := arg
            end;
        end
    else
        Result := -1; // The user is asking for the wrong command
    end;

end;


// Wrapper for concentating all the double-based IO for the WindGen Obj mimicking COM
function WindGensF(mode: Longint; arg: Double): Double; CDECL;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin           // WindGens.Ag Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindGenVars.ag
            end;
        end;
        1:
        begin           // WindGens.Ag Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.ag := arg
            end;
        end;
        2:
        begin           // WindGens.Cp Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindGenVars.Cp
            end;
        end;
        3:
        begin           // WindGens.Cp Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.Cp := arg
            end;
        end;
        4:
        begin           // WindGens.kV Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.PresentkV
            end;
        end;
        5:
        begin           // WindGens.kV Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.PresentkV := arg
            end;
        end;
        6:
        begin           // WindGens.kVA Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindGenVars.kVArating
            end;
        end;
        7:
        begin           // WindGens.kVA Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.kVArating := arg;
                WindGenElem.WindModelDyn.EditProp(13, floattostr(arg));
            end;
        end;
        8:
        begin           // WindGens.kvar Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.Presentkvar
            end;
        end;
        9:
        begin           // WindGens.kvar Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.Presentkvar := arg
            end;
        end;
        10:
        begin           // WindGens.kW Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.PresentkW
            end;
        end;
        11:
        begin           // WindGens.kW Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.PresentkW := arg
            end;
        end;
        12:
        begin           // WindGens.Lamda Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindGenVars.Lamda
            end;
        end;
        13:
        begin           // WindGens.Lamda Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.Lamda := arg
            end;
        end;
        14:
        begin           // WindGens.pd Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindGenVars.pd
            end;
        end;
        15:
        begin           // WindGens.pd Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.pd := arg
            end;
        end;
        16:
        begin           // WindGens.PF Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.PFNominal
            end;
        end;
        17:
        begin           // WindGens.PF Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.PFNominal := arg
            end;
        end;
        18:
        begin           // WindGens.Pss Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.Pss
            end;
        end;
        19:
        begin           // WindGens.Pss Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.Pss := arg
            end;
        end;
        20:
        begin           // WindGens.Qss Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.Qss
            end;
        end;
        21:
        begin           // WindGens.Qss Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.Qss := arg
            end;
        end;
        22:
        begin           // WindGens.Rad Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindGenVars.Rad
            end;
        end;
        23:
        begin           // WindGens.Rad Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.Rad := arg
            end;
        end;
        24:
        begin           // WindGens.RThev Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.Rthev
            end;
        end;
        25:
        begin           // WindGens.RThev Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.Rthev := arg
            end;
        end;
        26:
        begin           // WindGens.VCutOut Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindGenVars.VCutout
            end;
        end;
        27:
        begin           // WindGens.VCutOut Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.VCutout := arg
            end;
        end;
        28:
        begin           // WindGens.VCutIn Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindGenVars.VCutin
            end;
        end;
        29:
        begin           // WindGens.VCutIn Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindGenVars.VCutin := arg
            end;
        end;
        30:
        begin           // WindGens.Vss Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.Vss
            end;
        end;
        31:
        begin           // WindGens.Vss Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.Vss := arg
            end;
        end;
        32:
        begin           // WindGens.WindSpeed Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.vwind
            end;
        end;
        33:
        begin           // WindGens.WindSpeed Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.vwind := arg
            end;
        end;
        34:
        begin           // WindGens.XThev Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := WindGenElem.WindModelDyn.Xthev
            end;
        end;
        35:
        begin           // WindGens.XThev Write
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                WindGenElem.WindModelDyn.Xthev := arg
            end;
        end
    else
        Result := -1.0; // We got the wrong command
    end;

end;

// Wrapper for concentating all the string-based IO for the WindGen Obj mimicking COM
function WindGensS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
var
    WindGenElem: TWindGenObj;
    k: Integer;
    pList: TPointerList;
    activesave: Integer;
    S: String;
    Found: Boolean;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin                   // WindGen.Name Read
            WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
            if WindGenElem <> nil then
            begin
                Result := Pansichar(Ansistring(WindGenElem.Name))
            end;
        end;
        1:
        begin                   // WindGen.Name Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin      // Search list of Storages in active circuit for name
                if WindGenClass[ActiveActor].ElementList.ListSize > 0 then
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := false;
                    pList := WindGenClass[ActiveActor].ElementList;
                    activesave := pList.ActiveIndex;
                    WindGenElem := pList.First;
                    while WindGenElem <> nil do
                    begin
                        if (CompareText(WindGenElem.Name, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
                            Found := true;
                            Break;
                        end;
                        WindGenElem := pList.Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('WindGen "' + S + '" Not Found in Active Circuit.', 20003);
                        WindGenElem := pList.Get(activesave);    // Restore active Storage
                        ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
                    end;
                end;
            end;
        end;
    end;
end;

// Wrapper for concentating all the array-like IO structures for the Windgen Obj mimicking COM
procedure WindGensV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    WindGenElem: TWindGenObj;
    k: Integer;
    pList: TPointerList;

begin
    case mode of
        0:
        begin                   // WindGen.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                with ActiveCircuit[ActiveActor] do
                begin

                    if WindGenClass[ActiveActor].ElementList.ListSize > 0 then
                    begin
                        pList := WindGenClass[ActiveActor].ElementList;
                        k := 0;
                        WindGenElem := pList.First;
                        while WindGenElem <> nil do
                        begin
                            WriteStr2Array(WindGenElem.Name);
                            WriteStr2Array(Char(0));
                            Inc(k);
                            WindGenElem := pList.Next;
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
        begin                   // WindGen.RegisterNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            for k := 0 to NumWGenRegisters - 1 do
            begin
                WriteStr2Array(WindGenClass[ActiveActor].RegisterNames[k + 1]);
                WriteStr2Array(Char(0));
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        2:
        begin                   // WindGen.RegisterValues
            myType := 2;      // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
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
