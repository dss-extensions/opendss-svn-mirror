unit ImplWindGens;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TWindGens = class(TAutoObject, IWindGens)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_RegisterNames: Olevariant; SAFECALL;
        function Get_RegisterValues: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_Idx: Integer; SAFECALL;
        procedure Set_Idx(Value: Integer); SAFECALL;
        function Get_Ag: Double; SAFECALL;
        procedure Set_Ag(Value: Double); SAFECALL;
        function Get_Cp: Double; SAFECALL;
        procedure Set_Cp(Value: Double); SAFECALL;
        function Get_kV: Double; SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        function Get_kVA: Double; SAFECALL;
        function Get_kvar: Double; SAFECALL;
        function Get_kW: Double; SAFECALL;
        procedure Set_kVA(Value: Double); SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        procedure Set_kW(Value: Double); SAFECALL;
        function Get_Lamda: Double; SAFECALL;
        function Get_N_WTG: Integer; SAFECALL;
        function Get_NPoles: Integer; SAFECALL;
        function Get_pd: Double; SAFECALL;
        function Get_PF: Double; SAFECALL;
        function Get_PSS: Double; SAFECALL;
        function Get_QFlag: Integer; SAFECALL;
        function Get_QMode: Integer; SAFECALL;
        function Get_QSS: Double; SAFECALL;
        function Get_Rad: Double; SAFECALL;
        function Get_RThev: Double; SAFECALL;
        function Get_VCutIn: Double; SAFECALL;
        function Get_Vss: Double; SAFECALL;
        function Get_WindSpeed: Double; SAFECALL;
        function Get_XThev: Double; SAFECALL;
        procedure Set_Lamda(Value: Double); SAFECALL;
        procedure Set_N_WTG(Value: Integer); SAFECALL;
        procedure Set_NPoles(Value: Integer); SAFECALL;
        procedure Set_pd(Value: Double); SAFECALL;
        procedure Set_PF(Value: Double); SAFECALL;
        procedure Set_PSS(Value: Double); SAFECALL;
        procedure Set_QFlag(Value: Integer); SAFECALL;
        procedure Set_QMode(Value: Integer); SAFECALL;
        procedure Set_QSS(Value: Double); SAFECALL;
        procedure Set_Rad(Value: Double); SAFECALL;
        procedure Set_RThev(Value: Double); SAFECALL;
        procedure Set_VCutIn(Value: Double); SAFECALL;
        procedure Set_Vss(Value: Double); SAFECALL;
        procedure Set_WindSpeed(Value: Double); SAFECALL;
        procedure Set_XThev(Value: Double); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_VCutOut: Double; SAFECALL;
        procedure Set_VCutOut(Value: Double); SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    WindGen,
    Variants,
    Pointerlist,
    Sysutils;

function TWindGens.Get_AllNames: Olevariant;
var
    WindGenElem: TWindGenObj;
    pList: TPointerList;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        with ActiveCircuit[ActiveActor] do
        begin

            if WindGenClass[ActiveActor].ElementList.ListSize > 0 then
            begin
                pList := WindGenClass[ActiveActor].ElementList;
                VarArrayRedim(result, pList.ListSize - 1);
                k := 0;
                WindGenElem := pList.First;
                while WindGenElem <> nil do
                begin
                    Result[k] := WindGenElem.Name;
                    Inc(k);
                    WindGenElem := pList.Next;
                end;
            end;

        end;
    end;

end;

function TWindGens.Get_RegisterNames: Olevariant;
var
    k: Integer;

begin

    Result := VarArrayCreate([0, NumWGenRegisters - 1], varOleStr);
    for k := 0 to NumWGenRegisters - 1 do
    begin
        Result[k] := WindGenClass[ActiveActor].RegisterNames[k + 1];
    end;

end;

function TWindGens.Get_RegisterValues: Olevariant;
var
    WindGenElem: TWindGenObj;
    k: Integer;
begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin

        WindGenElem := TWindGenObj(ActiveCircuit[ActiveActor].ActiveCktElement);
        if WindGenElem <> nil then
        begin
            Result := VarArrayCreate([0, NumWGenRegisters - 1], varDouble);
            for k := 0 to NumWGenRegisters - 1 do
            begin
                Result[k] := WindGenElem.Registers[k + 1];
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

function TWindGens.Get_First: Integer;
var
    WindGenElem: TWindGenObj;
    pList: TPointerList;

begin

    Result := 0;
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

function TWindGens.Get_Next: Integer;
var
    WindGenElem: TWindGenObj;
    pList: TPointerList;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> nil then
    begin

        if WindGenClass[ActiveActor].ElementList.ListSize > 0 then
        begin
            pList := WindGenClass[ActiveActor].ElementList;
            WindGenElem := pList.Next;
            repeat
                if WindGenElem <> nil then
                begin
                    if WindGenElem.Enabled then
                    begin
                        ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
                        Result := pList.ActiveIndex;
                    end
                    else
                        WindGenElem := pList.Next;
                end
            until (Result > 0) or (WindGenElem = nil);
        end
        else
            Result := 0;  // signify no more

    end;
end;

function TWindGens.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := WindGenClass[ActiveActor].ElementList.ListSize;
end;

function TWindGens.Get_Idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> nil then
        Result := WindGenClass[ActiveActor].ElementList.ActiveIndex
    else
        Result := 0;
end;

procedure TWindGens.Set_Idx(Value: Integer);
var
    WindGenElem: TWindGenObj;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin

        WindGenElem := WindGenClass[ActiveActor].ElementList.Get(Value);
        if WindGenElem <> nil then
            ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;

    end;

end;

function TWindGens.Get_Ag: Double; SAFECALL;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0.0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindGenVars.ag
    end;

end;

procedure TWindGens.Set_Ag(Value: Double); SAFECALL;
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.ag := Value
    end;

end;

function TWindGens.Get_Cp: Double; SAFECALL;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0.0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindGenVars.Cp
    end;

end;

procedure TWindGens.Set_Cp(Value: Double); SAFECALL;
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.Cp := Value
    end;

end;

function TWindGens.Get_kV: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0.0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.PresentkV
    end;

end;

procedure TWindGens.Set_kV(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.PresentkV := Value
    end;

end;

function TWindGens.Get_kVA: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0.0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindGenVars.kVArating
    end;

end;

function TWindGens.Get_kvar: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0.0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.Presentkvar
    end;

end;

function TWindGens.Get_kW: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0.0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.PresentkW
    end;

end;

procedure TWindGens.Set_kVA(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.kVArating := Value;
        WindGenElem.WindModelDyn.EditProp(13, floattostr(Value));
    end;

end;

procedure TWindGens.Set_kvar(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.Presentkvar := Value
    end;

end;

procedure TWindGens.Set_kW(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.PresentkW := Value
    end;

end;

function TWindGens.Get_Lamda: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0.0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindGenVars.Lamda
    end;

end;

function TWindGens.Get_N_WTG: Integer;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.N_WTG
    end;

end;

function TWindGens.Get_NPoles: Integer;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := Trunc(WindGenElem.WindGenVars.Poles)
    end;

end;

function TWindGens.Get_pd: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindGenVars.pd
    end;

end;

function TWindGens.Get_PF: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.PFNominal
    end;

end;

function TWindGens.Get_PSS: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.Pss
    end;

end;

function TWindGens.Get_QFlag: Integer;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.QFlg
    end;

end;

function TWindGens.Get_QMode: Integer;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.QMode
    end;

end;

function TWindGens.Get_QSS: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.Qss
    end;

end;

function TWindGens.Get_Rad: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindGenVars.Rad
    end;

end;

function TWindGens.Get_RThev: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.Rthev
    end;

end;

function TWindGens.Get_VCutOut: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindGenVars.VCutout
    end;

end;

function TWindGens.Get_VCutIn: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindGenVars.VCutin
    end;

end;

function TWindGens.Get_Vss: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.Vss
    end;

end;

function TWindGens.Get_WindSpeed: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.vwind
    end;

end;

function TWindGens.Get_XThev: Double;
var
    WindGenElem: TWindGenObj;

begin
    Result := 0;
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.WindModelDyn.Xthev
    end;

end;

procedure TWindGens.Set_Lamda(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.Lamda := Value
    end;

end;

procedure TWindGens.Set_N_WTG(Value: Integer);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.N_WTG := Value
    end;

end;

procedure TWindGens.Set_NPoles(Value: Integer);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.Poles := Value
    end;

end;

procedure TWindGens.Set_pd(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.pd := Value
    end;

end;

procedure TWindGens.Set_PF(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.PFNominal := Value
    end;

end;


procedure TWindGens.Set_PSS(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.Pss := Value
    end;

end;

procedure TWindGens.Set_QFlag(Value: Integer);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.QFlg := Value
    end;

end;

procedure TWindGens.Set_QMode(Value: Integer);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.QMode := Value
    end;

end;

procedure TWindGens.Set_QSS(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.Qss := Value
    end;

end;

procedure TWindGens.Set_Rad(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.Rad := Value
    end;

end;

procedure TWindGens.Set_RThev(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.Rthev := Value
    end;

end;

procedure TWindGens.Set_VCutOut(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.VCutout := Value
    end;

end;

procedure TWindGens.Set_VCutIn(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindGenVars.VCutin := Value
    end;

end;

procedure TWindGens.Set_Vss(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.Vss := Value
    end;

end;

procedure TWindGens.Set_WindSpeed(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.vwind := Value
    end;

end;

procedure TWindGens.Set_XThev(Value: Double);
var
    WindGenElem: TWindGenObj;

begin
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        WindGenElem.WindModelDyn.Xthev := Value
    end;

end;

function TWindGens.Get_Name: Widestring;
var
    WindGenElem: TWindGenObj;

begin
    Result := '';
    WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
    if WindGenElem <> nil then
    begin
        Result := WindGenElem.Name
    end;

end;

procedure TWindGens.Set_Name(const Value: Widestring);
var
    activesave: Integer;
    WindgenElem: TWindGenObj;
    S: String;
    Found: Boolean;
    pList: TPointerList;
begin

    if ActiveCircuit[ActiveActor] <> nil then
    begin      // Search list of Storages in active circuit for name
        if WindGenClass[ActiveActor].ElementList.ListSize > 0 then
        begin
            S := Value;  // Convert to Pascal String
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


initialization
    TAutoObjectFactory.Create(ComServer, TWindGens, CLASS_WindGens,
        ciInternal, tmApartment);
end.
