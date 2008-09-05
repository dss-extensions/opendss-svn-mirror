unit ImplMeters;

{
   1-12-00  Modified first..next to return only enabled Meters
   7/19/01 Added Totals
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TMeters = class(TAutoObject, IMeters)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_First: Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_RegisterNames: OleVariant; safecall;
    function Get_RegisterValues: OleVariant; safecall;
    procedure Reset; safecall;
    procedure ResetAll; safecall;
    procedure Sample; safecall;
    procedure Save; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Totals: OleVariant; safecall;
    { Protected declarations }
  end;

implementation

uses ComServ,
     EnergyMeter,
     DSSGlobals,
     SysUtils,
     Variants;

function TMeters.Get_AllNames: OleVariant;
Var
  MeterElem:TEnergyMeterObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     Begin
       VarArrayRedim(Result, EnergyMeters.ListSize-1);
       k:=0;
       MeterElem := EnergyMeters.First;
       WHILE MeterElem<>Nil DO
       Begin
          Result[k] := MeterElem.Name;
          Inc(k);
          MeterElem := EnergyMeters.Next;
       End;
     End;

end;

function TMeters.Get_First: Integer;
Var
   pMeter:TEnergyMeterObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.First;
        If pMeter <> Nil Then
        Begin
          Repeat
            If pMeter.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pMeter;
              Result := 1;
            End
            Else  pMeter := ActiveCircuit.EnergyMeters.Next;
          Until (Result = 1) or (pMeter = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TMeters.Get_Name: WideString;
Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.Active;
        If pMeter <> Nil Then
          Result := pMeter.name;
   End;
end;

function TMeters.Get_Next: Integer;

Var
   pMeter:TEnergyMeterObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.next;
        If pMeter <> Nil Then
        Begin
          Repeat
            If pMeter.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pMeter;
              Result := ActiveCircuit.EnergyMeters.ActiveIndex;
            End
            Else  pMeter := ActiveCircuit.EnergyMeters.next;
          Until (Result > 0) or (pMeter = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TMeters.Get_RegisterNames: OleVariant;

Var
    EnergyMeterClass:TEnergyMeter;
    k :integer;

Begin
    EnergyMeterClass := DssClassList.Get(Classnames.Find('energymeter'));
    Result := VarArrayCreate([0, NumEMRegisters - 1], varOleStr);
    For k := 0 to  NumEMRegisters - 1  Do Begin
       Result[k] := EnergyMeterClass.RegisterNames[k + 1];
    End;
end;

function TMeters.Get_RegisterValues: OleVariant;

Var
   Meter :TEnergyMeterObj;
   k     :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        Meter :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If Meter <> Nil Then
        Begin
            Result := VarArrayCreate([0, numEMRegisters-1], varDouble);
            FOR k := 0 to numEMRegisters-1 DO
            Begin
                Result[k] := Meter.Registers[k+1];
            End;
        End
        Else
            Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;

end;

procedure TMeters.Reset;
Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.Active;
        If pMeter <> Nil Then pMeter.ResetRegisters;
   End;

end;

procedure TMeters.ResetAll;
VAR
    Mon:TEnergyMeter;
    DevClassIndex:Integer;

Begin
     DevClassIndex := ClassNames.Find('energymeter');
     IF DevClassIndex>0 THEN Begin
      Mon := DSSClassList.Get(DevClassIndex);
      Mon.ResetAll;
     End;
end;

procedure TMeters.Sample;

Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.Active;
        If pMeter <> Nil Then
          pMeter.TakeSample;
   End;

end;

procedure TMeters.Save;

Var
   pMeter:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeter := ActiveCircuit.EnergyMeters.Active;
        If pMeter <> Nil Then
          pMeter.SaveRegisters;
   End;

end;

procedure TMeters.Set_Name(const Value: WideString);
VAR
    activesave :integer;
    Mtr:TEnergyMeterObj;
    S: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of EnergyMeters in active circuit for name
       WITH ActiveCircuit.EnergyMeters DO
         Begin
             S := Value;  // Convert to Pascal String
             Found := FALSE;
             ActiveSave := ActiveIndex;
             Mtr := First;
             While Mtr <> NIL Do
             Begin
                IF (CompareText(Mtr.Name, S) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := Mtr;
                    Found := TRUE;
                    Break;
                End;
                Mtr := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('EnergyMeter "'+S+'" Not Found in Active Circuit.', 5005);
                 Mtr := Get(ActiveSave);    // Restore active Mtrerator
                 ActiveCircuit.ActiveCktElement := Mtr;
             End;
         End;
  End;

end;

function TMeters.Get_Totals: OleVariant;
Var
   i:Integer;
   
begin

     If ActiveCircuit <> Nil Then With ActiveCircuit Do Begin
          TotalizeMeters;
          Result := VarArrayCreate([0, NumEMRegisters-1], varDouble);
          For i := 1 to NumEMregisters Do Begin
               Result[i-1] := RegisterTotals[i];
          End;
     End
     Else Begin
          Result := VarArrayCreate([0, 0], varDouble);
     End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TMeters, Class_Meters,
    ciInternal, tmApartment);
end.
