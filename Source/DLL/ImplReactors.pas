unit ImplReactors;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TReactors = class(TAutoObject, IReactors)
    PROTECTED
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_kV: Double; SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        function Get_kvar: Double; SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        function Get_LCurve: Widestring; SAFECALL;
        procedure Set_LCurve(const Value: Widestring); SAFECALL;
        function Get_lmH: Double; SAFECALL;
        function Get_Parallel: Wordbool; SAFECALL;
        procedure Set_lmH(Value: Double); SAFECALL;
        procedure Set_Parallel(Value: Wordbool); SAFECALL;
        function Get_R: Double; SAFECALL;
        procedure Set_R(Value: Double); SAFECALL;
        function Get_RCurve: Widestring; SAFECALL;
        procedure Set_RCurve(const Value: Widestring); SAFECALL;
        function Get_Rmatrix: Olevariant; SAFECALL;
        procedure Set_Rmatrix(Value: Olevariant); SAFECALL;
        function Get_Rp: Double; SAFECALL;
        function Get_X: Double; SAFECALL;
        procedure Set_Rp(Value: Double); SAFECALL;
        procedure Set_X(Value: Double); SAFECALL;
        function Get_Xmatrix: Olevariant; SAFECALL;
        procedure Set_Xmatrix(Value: Olevariant); SAFECALL;
        function Get_Z: Olevariant; SAFECALL;
        procedure Set_Z(Value: Olevariant); SAFECALL;
        function Get_Z0: Olevariant; SAFECALL;
        function Get_Z1: Olevariant; SAFECALL;
        function Get_Z2: Olevariant; SAFECALL;
        procedure Set_Z0(Value: Olevariant); SAFECALL;
        procedure Set_Z1(Value: Olevariant); SAFECALL;
        procedure Set_Z2(Value: Olevariant); SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Reactor,
    Variants,
    Pointerlist,
    Sysutils;

function TReactors.Get_First: Integer;
var
    Elem: TReactorObj;
    pList: TPointerList;
begin
    Result := 0;
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

function TReactors.Get_Next: Integer;
var
    Elem: TReactorObj;
    pList: TPointerList;

begin
    Result := 0;
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
        end
        else
            Result := 0;  // signify no more

    end;

end;

function TReactors.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ReactorClass[ActiveActor].ElementList.ListSize;
end;

function TReactors.Get_Name: Widestring;
var
    Elem: TReactorObj;

begin
    Result := '';
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.Name
    end;

end;

procedure TReactors.Set_Name(const Value: Widestring);
var
    activesave: Integer;
    Elem: TReactorObj;
    S: String;
    Found: Boolean;
    pList: TPointerList;
begin

    if ActiveCircuit[ActiveActor] <> nil then
    begin      // Search list of Storages in active circuit for name
        if ReactorClass[ActiveActor].ElementList.ListSize > 0 then
        begin
            S := Value;  // Convert to Pascal String
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

function TReactors.Get_kV: Double;
var
    Elem: TReactorObj;

begin
    Result := 0.0;
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.kVNominal;
    end;

end;

procedure TReactors.Set_kV(Value: Double);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.kVNominal := Value;
    end;

end;

function TReactors.Get_kvar: Double;
var
    Elem: TReactorObj;

begin
    Result := 0.0;
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.kvarNominal;
    end;

end;

procedure TReactors.Set_kvar(Value: Double);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.kvarNominal := Value;
    end;

end;

function TReactors.Get_LCurve: Widestring;
var
    Elem: TReactorObj;

begin
    Result := '';
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.LCurve;
    end;

end;

procedure TReactors.Set_LCurve(const Value: Widestring);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.LCurve := Value;
    end;
end;

function TReactors.Get_lmH: Double;
var
    Elem: TReactorObj;

begin
    Result := 0.0;
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.LNominal * 1e3;
    end;

end;

function TReactors.Get_Parallel: Wordbool;
var
    Elem: TReactorObj;

begin
    Result := false;
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.IsParallel;
    end;

end;

procedure TReactors.Set_lmH(Value: Double);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.LNominal := Value / 1e3;
    end;

end;

procedure TReactors.Set_Parallel(Value: Wordbool);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.IsParallel := Value;
    end;

end;

function TReactors.Get_R: Double;
var
    Elem: TReactorObj;

begin
    Result := 0.0;
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.R;
    end;

end;

procedure TReactors.Set_R(Value: Double);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.R := Value;
    end;

end;

function TReactors.Get_RCurve: Widestring;
var
    Elem: TReactorObj;

begin
    Result := '';
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.RCurve;
    end;

end;

procedure TReactors.Set_RCurve(const Value: Widestring);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.RCurve := Value;
    end;

end;

function TReactors.Get_Rmatrix: Olevariant;
var
    Elem: TReactorObj;
    i: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                if RMatrix <> nil then
                begin
                    Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
                    for i := 1 to Sqr(NPhases) do
                    begin
                        Result[i - 1] := Rmatrix[i];
                    end;
                end;
            end;
        end;
    end;

end;

procedure TReactors.Set_Rmatrix(Value: Olevariant);
var
    Elem: TReactorObj;
    idx,
    i,
    j,
    k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                Reallocmem(Rmatrix, Sizeof(Double) * NPhases * NPhases);
                idx := 1;
                ;
                k := VarArrayLowBound(Value, 1);
                j := VarArrayHighBound(Value, 1);
                for i := k to j do
                begin
                    Rmatrix[idx] := Value[i];
                    Inc(idx);
                end;
                YprimInvalid[ActiveActor] := true;
            end;
        end;
    end;

end;

function TReactors.Get_Rp: Double;
var
    Elem: TReactorObj;

begin
    Result := 0.0;
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.Rp;
    end;

end;

function TReactors.Get_X: Double;
var
    Elem: TReactorObj;

begin
    Result := 0.0;
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Result := Elem.X;
    end;

end;

procedure TReactors.Set_Rp(Value: Double);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.Rp := Value;
    end;

end;

procedure TReactors.Set_X(Value: Double);
var
    Elem: TReactorObj;

begin
    Elem := ReactorClass[ActiveActor].GetActiveObj;
    if Elem <> nil then
    begin
        Elem.X := Value;
    end;

end;

function TReactors.Get_Xmatrix: Olevariant;
var
    Elem: TReactorObj;
    i: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                if Xmatrix <> nil then
                begin
                    Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
                    for i := 1 to Sqr(NPhases) do
                    begin
                        Result[i - 1] := Xmatrix[i];
                    end;
                end;
            end;
        end;
    end;

end;

procedure TReactors.Set_Xmatrix(Value: Olevariant);
var
    Elem: TReactorObj;
    idx,
    i,
    j,
    k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                Reallocmem(Xmatrix, Sizeof(Double) * NPhases * NPhases);
                idx := 1;
                ;
                k := VarArrayLowBound(Value, 1);
                j := VarArrayHighBound(Value, 1);
                for i := k to j do
                begin
                    Xmatrix[idx] := Value[i];
                    Inc(idx);
                end;
                YprimInvalid[ActiveActor] := true;
            end;
        end;
    end;

end;

function TReactors.Get_Z: Olevariant;
var
    Elem: TReactorObj;
    i: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                Result := VarArrayCreate([0, 1], varDouble);
                Result[0] := Z.re;
                Result[1] := Z.im;
            end;
        end;
    end;

end;

procedure TReactors.Set_Z(Value: Olevariant);
var
    Elem: TReactorObj;
    i,
    k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                k := VarArrayLowBound(Value, 1);
                Z.re := Value[k];
                Z.im := Value[k + 1];
                YprimInvalid[ActiveActor] := true;
            end;
        end;
    end;

end;

function TReactors.Get_Z0: Olevariant;
var
    Elem: TReactorObj;
    i: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                Result := VarArrayCreate([0, 1], varDouble);
                Result[0] := Z0.re;
                Result[1] := Z0.im;
            end;
        end;
    end;

end;

function TReactors.Get_Z1: Olevariant;
var
    Elem: TReactorObj;
    i: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                Result := VarArrayCreate([0, 1], varDouble);
                Result[0] := Z1.re;
                Result[1] := Z1.im;
            end;
        end;
    end;

end;

function TReactors.Get_Z2: Olevariant;
var
    Elem: TReactorObj;
    i: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                Result := VarArrayCreate([0, 1], varDouble);
                Result[0] := Z2.re;
                Result[1] := Z2.im;
            end;
        end;
    end;

end;

procedure TReactors.Set_Z0(Value: Olevariant);
var
    Elem: TReactorObj;
    i,
    k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                k := VarArrayLowBound(Value, 1);
                Z0.re := Value[k];
                Z0.im := Value[k + 1];
                YprimInvalid[ActiveActor] := true;
            end;
        end;
    end;

end;

procedure TReactors.Set_Z1(Value: Olevariant);
var
    Elem: TReactorObj;
    i,
    k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                k := VarArrayLowBound(Value, 1);
                Z1.re := Value[k];
                Z1.im := Value[k + 1];
                YprimInvalid[ActiveActor] := true;
            end;
        end;
    end;

end;

procedure TReactors.Set_Z2(Value: Olevariant);
var
    Elem: TReactorObj;
    i,
    k: Integer;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        begin
            with Elem do
            begin
                k := VarArrayLowBound(Value, 1);
                Z2.re := Value[k];
                Z2.im := Value[k + 1];
                YprimInvalid[ActiveActor] := true;
            end;
        end;
    end;

end;

function TReactors.Get_AllNames: Olevariant;
var
    Elem: TReactorObj;
    pList: TPointerList;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        with ActiveCircuit[ActiveActor] do
        begin

            if ReactorClass[ActiveActor].ElementList.ListSize > 0 then
            begin
                pList := ReactorClass[ActiveActor].ElementList;
                VarArrayRedim(result, pList.ListSize - 1);
                k := 0;
                Elem := pList.First;
                while Elem <> nil do
                begin
                    Result[k] := Elem.Name;
                    Inc(k);
                    Elem := pList.Next;
                end;
            end;

        end;
    end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TReactors, CLASS_Reactors,
        ciInternal, tmApartment);

end.
