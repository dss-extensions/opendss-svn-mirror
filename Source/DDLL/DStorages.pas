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
    CktElement;

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

        end;
    end;
end;

// Wrapper for concentating all the string-based IO for the Storage Obj mimicking COM
function StoragesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
begin

end;

// Wrapper for concentating all the array-like IO structures for the Storage Obj mimicking COM
procedure StoragesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
begin

end;

end.
