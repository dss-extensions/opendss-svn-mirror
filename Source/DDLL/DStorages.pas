unit DStorages;

interface

function StoragesI(mode: Longint; arg: Longint): Longint; CDECL;
function StoragesF(mode: Longint; arg: Double): Double; CDECL;
function StoragesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure StoragesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    {$IFNDEF FPC_DLL}
    ComServ,
    {$ENDIF}
    Storage,
    Variants,
    PointerList,
    DSSGlobals,
    CktElement,
    SysUtils;

// Wrapper for concentating all the integer-based IO for the Storage Obj mimicking COM
function StoragesI(mode: Longint; arg: Longint): Longint; CDECL;
var
    pStorageElem: TStorageObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin           // Storages.First
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.First;
                if pStorageElem <> nil then
                begin
                    repeat
                        if pStorageElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
                            Result := 1;
                        end
                        else
                            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
                    until (Result = 1) or (pStorageElem = nil);
                end
            end;
        end;
        1:
        begin           // Storages.Next
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
                if pStorageElem <> nil then
                begin
                    repeat
                        if pStorageElem.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
                            Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex;
                        end
                        else
                            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
                    until (Result > 0) or (pStorageElem = nil);
                end
            end;
        end;
        2:
        begin           // Storages.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].StorageElements.ListSize;
        end;
        3:
        begin           // Storages.Idx read
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex
        end;
        4:
        begin           // Storages.Idx write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Get(arg);
                if pStorageElem <> nil then
                    ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
            end;
        end;
        5:
        begin           // Storages.State Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorageElem <> nil then
                begin
                    Result := pStorageElem.StorageState;
                end;
            end;
        end;
        6:
        begin           // Storages.State Write
          {  Legal States
           STORE_CHARGING    = -1;
           STORE_IDLING      =  0;
           STORE_DISCHARGING =  1;
          }
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorageElem <> nil then
                begin
                    pStorageElem.StorageState := arg;
                end;
            end;
        end;
        7:
        begin           // Storages.ControlMode Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorageElem <> nil then
                begin
                    if pStorageElem.GFM_mode then
                        Result := 1
                end;
            end;
        end;
        8:
        begin           // Storages.ControlMode Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorageElem <> nil then
                begin
                    pStorageElem.GFM_mode := false;
                    if arg <> 0 then
                        pStorageElem.GFM_mode := true
                end;
            end;
        end;
        9:
        begin           // Storages.SafeMode
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorageElem <> nil then
                begin
                    if pStorageElem.myDynVars.SafeMode then
                        Result := 1;
                end;
            end;
        end;
        10:
        begin           // Storages.VarFollowInverter Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorageElem <> nil then
                begin
                    if pStorageElem.FVarFollowInverter then
                        Result := 1;
                end;
            end;
        end;
        11:
        begin           // Storages.VarFollowInverter Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorageElem <> nil then
                begin
                    pStorageElem.FVarFollowInverter := false;
                    if arg <> 0 then
                        pStorageElem.FVarFollowInverter := true
                end;
            end;
        end
    else
    begin
        Result := -1; // Just sent the wrong command
    end;
    end;
end;

// Wrapper for concentating all the double-based IO for the Storage Obj mimicking COM
function StoragesF(mode: Longint; arg: Double): Double; CDECL;
var
    pStorage: TStorageObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin           // Storages.puSOC Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.Storagevars.kWhStored / pStorage.StorageVars.kWhRating;
                end;
            end;
        end;
        1:
        begin           // Storages.puSOC Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.Storagevars.kWhStored := pStorage.StorageVars.kWhRating * arg;
                end;
            end;
        end;
        2:
        begin           // Storages.AmpLimit Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.myDynVars.ILimit;
                end;
            end;
        end;
        3:
        begin           // Storages.AmpLimit Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.myDynVars.ILimit := arg;
                end;
            end;
        end;
        4:
        begin           // Storages.AmpLimitGain Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.myDynVars.VError;
                end;
            end;
        end;
        5:
        begin           // Storages.AmpLimitGain Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.myDynVars.VError := arg;
                end;
            end;
        end;
        6:
        begin            // Storages.ChargeTrigger Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.ChargeTrigger;
                end;
            end;
        end;
        7:
        begin            // Storages.ChargeTrigger Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.ChargeTrigger := arg;
                end;
            end;
        end;
        8:
        begin            // Storages.DisChargeTrigger Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.DisChargeTrigger;
                end;
            end;
        end;
        9:
        begin            // Storages.DisChargeTrigger Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.DisChargeTrigger := arg;
                end;
            end;
        end;
        10:
        begin            // Storages.EffCharge Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.pctChargeEff;
                end;
            end;
        end;
        11:
        begin            // Storages.EffCharge Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.pctChargeEff := arg;
                end;
            end;
        end;
        12:
        begin            // Storages.EffDisCharge Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.pctDischargeEff;
                end;
            end;
        end;
        13:
        begin            // Storages.EffDisCharge Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.pctDischargeEff := arg;
                end;
            end;
        end;
        14:
        begin            // Storages.kP Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.myDynVars.kP * 1e3;
                end;
            end;
        end;
        15:
        begin            // Storages.kP Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.myDynVars.kP := arg / 1e3;
                end;
            end;
        end;
        16:
        begin            // Storages.kV Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.PresentkV;
                end;
            end;
        end;
        17:
        begin            // Storages.kV Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.PresentkV := arg;
                end;
            end;
        end;
        18:
        begin            // Storages.kVA Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.StorageVars.FkVArating;
                end;
            end;
        end;
        19:
        begin            // Storages.kVA Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.StorageVars.FkVArating := arg;
                end;
            end;
        end;
        20:
        begin            // Storages.kvar Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.kvarRequested;
                end;
            end;
        end;
        21:
        begin            // Storages.kvar Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.kvarRequested := arg;
                end;
            end;
        end;
        22:
        begin            // Storages.kVDC Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.myDynVars.RatedVDC / 1e3;
                end;
            end;
        end;
        23:
        begin             // Storages.kVDC Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.myDynVars.RatedVDC := arg * 1e3;
                end;
            end;
        end;
        24:
        begin            // Storages.kW Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.kW;
                end;
            end;
        end;
        25:
        begin            // Storages.kW Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.kW := arg;
                end;
            end;
        end;
        26:
        begin           // Storages.kWhRated Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.StorageVars.kWhrating;
                end;
            end;
        end;
        27:
        begin           // Storages.kWhRated Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.StorageVars.kWhrating := arg;
                end;
            end;
        end;
        28:
        begin           // Storages.kWRated Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.StorageVars.kWrating;
                end;
            end;
        end;
        29:
        begin           // Storages.kWRated Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.StorageVars.kWrating := arg;
                end;
            end;
        end;
        30:
        begin           // Storages.LimitCurrent Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    if pStorage.IsCurrentLimited then
                        Result := 1;
                end;
            end;
        end;
        31:
        begin           // Storages.LimitCurrent Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.IsCurrentLimited := false;
                    if arg <> 0 then
                        pStorage.IsCurrentLimited := true;
                end;
            end;
        end;
        32:
        begin           // Storages.PF Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.PFnominal;
                end;
            end;
        end;
        33:
        begin           // Storages.PF Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.PFnominal := arg;
                end;
            end;
        end;
        34:
        begin           // Storages.PITol Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.myDynVars.CtrlTol * 100;
                end;
            end;
        end;
        35:
        begin           // Storages.PITol Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.myDynVars.CtrlTol := arg / 100;
                end;
            end;
        end;
        36:
        begin           // Storages.SafeVoltage Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.myDynVars.SMThreshold;
                end;
            end;
        end;
        37:
        begin           // Storages.SafeVoltage Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.myDynVars.SMThreshold := arg;
                end;
            end;
        end;
        38:
        begin            // Storages.TimeChargeTrig Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := pStorage.ChargeTime;
                end;
            end;
        end;
        39:
        begin            // Storages.TimeChargeTrig Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    pStorage.ChargeTime := arg;
                end;
            end;
        end
    else
        Result := -1.0; // The user sent the wrong command code
    end;
end;

// Wrapper for concentating all the string-based IO for the Storage Obj mimicking COM
function StoragesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
var
    pStorage: TStorageObj;
    activesave: Integer;
    S: String;
    Found: Boolean;

begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin                   // Storages.Name Read
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
                if pStorage <> nil then
                begin
                    Result := Pansichar(Ansistring(pStorage.Name));
                end
            end;
        end;
        1:
        begin                   // Storages.Name Write
            if ActiveCircuit[ActiveActor] <> nil then
            begin      // Search list of Storages in active circuit for name
                with ActiveCircuit[ActiveActor].StorageElements do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := false;
                    ActiveSave := ActiveIndex;
                    pStorage := First;
                    while pStorage <> nil do
                    begin
                        if (CompareText(pStorage.Name, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pStorage;
                            Found := true;
                            Break;
                        end;
                        pStorage := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Storage "' + S + '" Not Found in Active Circuit.', 5003);
                        pStorage := Get(ActiveSave);    // Restore active Storage
                        ActiveCircuit[ActiveActor].ActiveCktElement := pStorage;
                    end;
                end;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

// Wrapper for concentating all the array-like IO structures for the Storage Obj mimicking COM
procedure StoragesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    StorageElem: TStorageObj;
    k: Integer;

begin
    case mode of
        0:
        begin                   // Storages.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if StorageElements.ListSize > 0 then
                    begin
                        k := 0;
                        StorageElem := StorageElements.First;
                        while StorageElem <> nil do
                        begin
                            WriteStr2Array(StorageElem.Name);
                            WriteStr2Array(Char(0));
                            Inc(k);
                            StorageElem := StorageElements.Next;
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
        begin                   // Storages.RegisterNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            for k := 0 to NumStorageRegisters - 1 do
            begin
                WriteStr2Array(StorageClass[ActiveActor].RegisterNames[k + 1]);
                WriteStr2Array(Char(0));
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        2:
        begin                   // Storages.RegisterValues
            myType := 2;      // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> nil then
            begin
                StorageElem := TStorageObj(ActiveCircuit[ActiveActor].ActiveCktElement);
                if StorageElem <> nil then
                begin
                    setlength(myDBLArray, NumStorageRegisters);
                    for k := 0 to numStorageRegisters - 1 do
                    begin
                        myDBLArray[k] := StorageElem.Registers[k + 1];
                    end;
                end
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
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
