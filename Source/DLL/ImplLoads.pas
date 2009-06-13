unit ImplLoads;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TLoads = class(TAutoObject, ILoads)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_idx: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    procedure Set_idx(Value: Integer); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_kV: Double; safecall;
    function Get_kvar: Double; safecall;
    function Get_kW: Double; safecall;
    function Get_PF: Double; safecall;
    procedure Set_kV(Value: Double); safecall;
    procedure Set_kvar(Value: Double); safecall;
    procedure Set_kW(Value: Double); safecall;
    procedure Set_PF(Value: Double); safecall;

  end;

implementation

uses ComServ,DSSGlobals, Load, Variants, SysUtils;

function TLoads.Get_AllNames: OleVariant;
Var
  LoadElem:TLoadObj;
  k:Integer;

Begin
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       Result := VarArrayCreate([0, Loads.ListSize-1], varOleStr);
       k:=0;
       LoadElem := Loads.First;
       WHILE LoadElem<>Nil DO  Begin
          Result[k] := LoadElem.Name;
          Inc(k);
          LoadElem := Loads.Next;
       End;
     End
    ELSE Result := VarArrayCreate([0, 0], varOleStr);

end;

function TLoads.Get_First: Integer;
Var
   pLoad:TLoadObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pLoad := ActiveCircuit.Loads.First;
        If pLoad <> Nil Then
        Begin
          Repeat
            If pLoad.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pLoad;
              Result := 1;
            End
            Else pLoad := ActiveCircuit.Loads.Next;
          Until (Result = 1) or (pLoad = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TLoads.Get_idx: Integer;
begin
    if ActiveCircuit <> Nil then
       Result := ActiveCircuit.Loads.ActiveIndex
    else Result := 0;
end;

function TLoads.Get_Name: WideString;
Var
   pLoad:TLoadObj;

Begin
   Result := '';
   If ActiveCircuit <> Nil Then
   Begin
        pLoad := ActiveCircuit.Loads.Active;
        If pLoad <> Nil Then
          Result := pLoad.Name
        Else
            Result := '';  // signify no name
   End;

end;

function TLoads.Get_Next: Integer;
Var
   pLoad:TLoadObj;

Begin
   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pLoad := ActiveCircuit.Loads.Next;
        If pLoad <> Nil Then
        Begin
          Repeat
            If pLoad.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pLoad;
              Result := ActiveCircuit.Loads.ActiveIndex;
            End
            Else pLoad := ActiveCircuit.Loads.Next;
          Until (Result > 0) or (pLoad = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

procedure TLoads.Set_idx(Value: Integer);
Var
    pLoad:TLoadObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pLoad := ActiveCircuit.Loads.Get(Value);
        If pLoad <> Nil Then ActiveCircuit.ActiveCktElement := pLoad;
    End;

end;

procedure TLoads.Set_Name(const Value: WideString);
VAR
    ActiveSave :integer;
    pLoad:TLoadObj;
    S: String;
    Found :Boolean;
Begin

  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of Loads in active circuit for name
     WITH ActiveCircuit.Loads DO
       Begin
           S := Value;  // Convert to Pascal String
           Found := FALSE;
           ActiveSave := ActiveIndex;
           pLoad := First;
           While pLoad <> NIL Do
           Begin
              IF (CompareText(pLoad.Name, S) = 0)
              THEN Begin
                  ActiveCircuit.ActiveCktElement := pLoad;
                  Found := TRUE;
                  Break;
              End;
              pLoad := Next;
           End;
           IF NOT Found
           THEN Begin
               DoSimpleMsg('Load "'+S+'" Not Found in Active Circuit.', 5003);
               pLoad := Get(ActiveSave);    // Restore active Load
               ActiveCircuit.ActiveCktElement := pLoad;
           End;
       End;
  End;

end;

function TLoads.Get_kV: Double;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kVLoadBase;
             End;
         End;
   End;

end;

function TLoads.Get_kvar: Double;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kvarBase;
             End;
         End;
   End;
end;

function TLoads.Get_kW: Double;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kWBase;
             End;
         End;
   End;
end;

function TLoads.Get_PF: Double;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).PFNominal;
             End;
         End;
   End;
end;

procedure TLoads.Set_kV(Value: Double);
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).kVLoadBase := Value;
                  TLoadObj(Active).UpdateVoltageBases;  // side effectes
             End;
         End;
   End;
end;

procedure TLoads.Set_kvar(Value: Double);
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).kvarBase := Value;
                  TLoadObj(Active).LoadSpecType := 1;
             End;
         End;
   End;
end;

procedure TLoads.Set_kW(Value: Double);
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).kWBase := Value;
                  TLoadObj(Active).LoadSpecType := 0;
             End;
         End;
   End;
end;

procedure TLoads.Set_PF(Value: Double);
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).PFNominal := Value;
             End;
         End;
   End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TLoads, Class_Loads,
    ciInternal, tmApartment);
end.
