unit ImplStorages;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TStorages = class(TAutoObject, IStorages)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_State: Integer; SAFECALL;
        procedure Set_State(Value: Integer); SAFECALL;
        function Get_puSOC: Double; SAFECALL;
        procedure Set_puSOC(Value: Double); SAFECALL;
        function Get_AmpLimit: Double; SAFECALL;
        function Get_AmpLimitGain: Double; SAFECALL;
        function Get_ChargeTrigger: Double; SAFECALL;
        function Get_ControlMode: Integer; SAFECALL;
        function Get_DischargeTrigger: Double; SAFECALL;
        function Get_EffCharge: Double; SAFECALL;
        function Get_EffDischarge: Double; SAFECALL;
        function Get_Kp: Double; SAFECALL;
        function Get_kV: Double; SAFECALL;
        function Get_kVA: Double; SAFECALL;
        function Get_kvar: Double; SAFECALL;
        function Get_kVDC: Double; SAFECALL;
        function Get_kW: Double; SAFECALL;
        function Get_kWhRated: Double; SAFECALL;
        function Get_kWRated: Double; SAFECALL;
        function Get_LimitCurrent: Double; SAFECALL;
        function Get_PF: Double; SAFECALL;
        function Get_PITol: Double; SAFECALL;
        function Get_SafeMode: Integer; SAFECALL;
        function Get_SafeVoltage: Double; SAFECALL;
        function Get_TimeChargeTrig: Double; SAFECALL;
        function Get_VarFollowInverter: Integer; SAFECALL;
        procedure Set_AmpLimit(Value: Double); SAFECALL;
        procedure Set_AmpLimitGain(Value: Double); SAFECALL;
        procedure Set_ChargeTrigger(Value: Double); SAFECALL;
        procedure Set_ControlMode(Value: Integer); SAFECALL;
        procedure Set_DischargeTrigger(Value: Double); SAFECALL;
        procedure Set_EffCharge(Value: Double); SAFECALL;
        procedure Set_EffDischarge(Value: Double); SAFECALL;
        procedure Set_Kp(Value: Double); SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        procedure Set_kVA(Value: Double); SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        procedure Set_kVDC(Value: Double); SAFECALL;
        procedure Set_kW(Value: Double); SAFECALL;
        procedure Set_kWhRated(Value: Double); SAFECALL;
        procedure Set_kWRated(Value: Double); SAFECALL;
        procedure Set_LimitCurrent(Value: Double); SAFECALL;
        procedure Set_PF(Value: Double); SAFECALL;
        procedure Set_PITol(Value: Double); SAFECALL;
        procedure Set_SafeVoltage(Value: Double); SAFECALL;
        procedure Set_TimeChargeTrig(Value: Double); SAFECALL;
        procedure Set_VarFollowInverter(Value: Integer); SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Storage,
    Variants,
    Sysutils;

function TStorages.Get_AllNames: Olevariant;
var
    StorageElem: TStorageObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        with ActiveCircuit[ActiveActor] do
        begin
            if StorageElements.ListSize > 0 then
            begin
                VarArrayRedim(result, StorageElements.ListSize - 1);
                k := 0;
                StorageElem := StorageElements.First;
                while StorageElem <> nil do
                begin
                    Result[k] := StorageElem.Name;
                    Inc(k);
                    StorageElem := StorageElements.Next;
                end;
            end;
        end;
    end;

end;

function TStorages.Get_RegisterNames: Olevariant;
var
    k: Integer;

begin
    Result := VarArrayCreate([0, NumStorageRegisters - 1], varOleStr);
    for k := 0 to NumStorageRegisters - 1 do
    begin
        Result[k] := StorageClass[ActiveActor].RegisterNames[k + 1];
    end;
end;

function TStorages.Get_RegisterValues: Olevariant;
var
    StorageElem: TStorageObj;
    k: Integer;
begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        StorageElem := TStorageObj(ActiveCircuit[ActiveActor].StorageElements.Active);
        if StorageElem <> nil then
        begin
            Result := VarArrayCreate([0, numStorageRegisters - 1], varDouble);
            for k := 0 to numStorageRegisters - 1 do
            begin
                Result[k] := StorageElem.Registers[k + 1];
            end;
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);
    end
    else
    begin
        Result := VarArrayCreate([0, 0], varDouble);
    end;
end;

function TStorages.Get_First: Integer;
var
    pStorageElem: TStorageObj;

begin

    Result := 0;
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

function TStorages.Get_Next: Integer;
var
    pStorageElem: TStorageObj;

begin
    Result := 0;
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

function TStorages.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].StorageElements.ListSize;
end;

function TStorages.Get_idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> nil then
        Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex
    else
        Result := 0;
end;

procedure TStorages.Set_idx(Value: Integer);
var
    pStorage: TStorageObj;
begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Get(Value);
        if pStorage <> nil then
            ActiveCircuit[ActiveActor].ActiveCktElement := pStorage;
    end;

end;

function TStorages.Get_Name: Widestring;
var
    pStorage: TStorageObj;

begin
    Result := '';
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.Name;
        end
        else
            Result := '';  // signify no name
    end;

end;

procedure TStorages.Set_Name(const Value: Widestring);
var
    activesave: Integer;
    StorageElem: TStorageObj;
    S: String;
    Found: Boolean;
begin

    if ActiveCircuit[ActiveActor] <> nil then
    begin      // Search list of Storages in active circuit for name
        with ActiveCircuit[ActiveActor].StorageElements do
        begin
            S := Value;  // Convert to Pascal String
            Found := false;
            ActiveSave := ActiveIndex;
            StorageElem := First;
            while StorageElem <> nil do
            begin
                if (CompareText(StorageElem.Name, S) = 0) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := StorageElem;
                    Found := true;
                    Break;
                end;
                StorageElem := Next;
            end;
            if not Found then
            begin
                DoSimpleMsg('Storage "' + S + '" Not Found in Active Circuit.', 5003);
                StorageElem := Get(ActiveSave);    // Restore active Storage
                ActiveCircuit[ActiveActor].ActiveCktElement := StorageElem;
            end;
        end;
    end;

end;

function TStorages.Get_State: Integer;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.StorageState;
        end;
    end;

end;

procedure TStorages.Set_State(Value: Integer);
{  Legal States
     STORE_CHARGING    = -1;
     STORE_IDLING      =  0;
     STORE_DISCHARGING =  1;
}
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.StorageState := Value;
        end;
    end;

end;

function TStorages.Get_puSOC: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.Storagevars.kWhStored / pStorage.StorageVars.kWhRating;
        end;
    end;

end;

procedure TStorages.Set_puSOC(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.Storagevars.kWhStored := pStorage.StorageVars.kWhRating * Value;
        end;
    end;

end;

function TStorages.Get_AmpLimit: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.myDynVars.ILimit;
        end;
    end;

end;

function TStorages.Get_AmpLimitGain: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.myDynVars.VError;
        end;
    end;

end;

function TStorages.Get_ChargeTrigger: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.ChargeTrigger;
        end;
    end;

end;

function TStorages.Get_ControlMode: Integer;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            if pStorage.GFM_mode then
                Result := 1
        end;
    end;

end;

function TStorages.Get_DischargeTrigger: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.DisChargeTrigger;
        end;
    end;

end;

function TStorages.Get_EffCharge: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.pctChargeEff;
        end;
    end;

end;

function TStorages.Get_EffDischarge: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.pctDischargeEff;
        end;
    end;

end;

function TStorages.Get_Kp: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.myDynVars.kP * 1e3;
        end;
    end;

end;

function TStorages.Get_kV: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.PresentkV;
        end;
    end;

end;

function TStorages.Get_kVA: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.StorageVars.FkVArating;
        end;
    end;

end;

function TStorages.Get_kvar: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.kvarRequested;
        end;
    end;

end;

function TStorages.Get_kVDC: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.myDynVars.RatedVDC / 1e3;
        end;
    end;

end;

function TStorages.Get_kW: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.kW;
        end;
    end;

end;

function TStorages.Get_kWhRated: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.StorageVars.kWhrating;
        end;
    end;

end;

function TStorages.Get_kWRated: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.StorageVars.kWrating;
        end;
    end;

end;

function TStorages.Get_LimitCurrent: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
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

function TStorages.Get_PF: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.PFnominal;
        end;
    end;

end;

function TStorages.Get_PITol: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.myDynVars.CtrlTol * 100;
        end;
    end;

end;

function TStorages.Get_SafeMode: Integer;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            if pStorage.myDynVars.SafeMode then
                Result := 1;
        end;
    end;

end;

function TStorages.Get_SafeVoltage: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.myDynVars.SMThreshold;
        end;
    end;

end;

function TStorages.Get_TimeChargeTrig: Double;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            Result := pStorage.ChargeTime;
        end;
    end;

end;

function TStorages.Get_VarFollowInverter: Integer;
var
    pStorage: TStorageObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            if pStorage.FVarFollowInverter then
                Result := 1;
        end;
    end;

end;

procedure TStorages.Set_AmpLimit(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.myDynVars.ILimit := Value;
        end;
    end;

end;

procedure TStorages.Set_AmpLimitGain(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.myDynVars.VError := Value;
        end;
    end;

end;

procedure TStorages.Set_ChargeTrigger(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.ChargeTrigger := Value;
        end;
    end;

end;

procedure TStorages.Set_ControlMode(Value: Integer);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.GFM_mode := false;
            if Value <> 0 then
                pStorage.GFM_mode := true
        end;
    end;

end;

procedure TStorages.Set_DischargeTrigger(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.DisChargeTrigger := value;
        end;
    end;

end;

procedure TStorages.Set_EffCharge(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.pctChargeEff := Value;
        end;
    end;

end;

procedure TStorages.Set_EffDischarge(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.pctDischargeEff := Value;
        end;
    end;

end;

procedure TStorages.Set_Kp(Value: Double);
var
    pStorage: TStorageObj;

begin

    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.myDynVars.kP := Value / 1e3;
        end;
    end;

end;

procedure TStorages.Set_kV(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.PresentkV := Value;
        end;
    end;

end;

procedure TStorages.Set_kVA(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.StorageVars.FkVArating := Value;
        end;
    end;

end;

procedure TStorages.Set_kvar(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.kvarRequested := Value;
        end;
    end;

end;

procedure TStorages.Set_kVDC(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.myDynVars.RatedVDC := Value * 1e3;
        end;
    end;

end;

procedure TStorages.Set_kW(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.kW := Value;
        end;
    end;

end;

procedure TStorages.Set_kWhRated(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.StorageVars.kWhrating := Value;
        end;
    end;

end;

procedure TStorages.Set_kWRated(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.StorageVars.kWrating := Value;
        end;
    end;


end;

procedure TStorages.Set_LimitCurrent(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.IsCurrentLimited := false;
            if Value <> 0 then
                pStorage.IsCurrentLimited := true;
        end;
    end;

end;

procedure TStorages.Set_PF(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.PFnominal := Value;
        end;
    end;

end;

procedure TStorages.Set_PITol(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.myDynVars.CtrlTol := value / 100;
        end;
    end;

end;

procedure TStorages.Set_SafeVoltage(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.myDynVars.SMThreshold := Value;
        end;
    end;

end;

procedure TStorages.Set_TimeChargeTrig(Value: Double);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.ChargeTime := Value;
        end;
    end;

end;

procedure TStorages.Set_VarFollowInverter(Value: Integer);
var
    pStorage: TStorageObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
        if pStorage <> nil then
        begin
            pStorage.FVarFollowInverter := false;
            if Value <> 0 then
                pStorage.FVarFollowInverter := true
        end;
    end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TStorages, Class_Storages,
        ciInternal, tmApartment);
end.
