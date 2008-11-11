unit ImplMeters;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
    function Get_Peakcurrent: OleVariant; safecall;
    procedure Set_Peakcurrent(Value: OleVariant); safecall;
    function Get_CalcCurrent: OleVariant; safecall;
    procedure Set_CalcCurrent(Value: OleVariant); safecall;
    function Get_AllocFactors: OleVariant; safecall;
    procedure Set_AllocFactors(Value: OleVariant); safecall;
    function Get_MeteredElement: WideString; safecall;
    function Get_MeteredTerminal: Integer; safecall;
    procedure Set_MeteredElement(const Value: WideString); safecall;
    procedure Set_MeteredTerminal(Value: Integer); safecall;
    { Protected declarations }
  end;

implementation

uses ComServ,
     EnergyMeter,
     DSSGlobals,
     SysUtils,
     ucomplex,
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
   pMeterObj:TEnergyMeterObj;

Begin

   If ActiveCircuit <> Nil Then
   Begin
        pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then   Result := pMeterObj.name;
   End;
end;

function TMeters.Get_Next: Integer;

Var
   pMeterObj :TEnergyMeterObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pMeterObj := ActiveCircuit.EnergyMeters.next;
        If pMeterObj <> Nil Then
        Begin
          Repeat   // Find an Enabled Meter
            If pMeterObj.Enabled  Then Begin
              ActiveCircuit.ActiveCktElement := pMeterObj;
              Result := ActiveCircuit.EnergyMeters.ActiveIndex;
            End
            Else  pMeterObj := ActiveCircuit.EnergyMeters.next;
          Until (Result > 0) or (pMeterObj = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;

function TMeters.Get_RegisterNames: OleVariant;

Var
    pMeterObj :TEnergyMeterObj;
    k :integer;

Begin
    pMeterObj := TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
    if Assigned(pMeterObj) then  Begin
      Result := VarArrayCreate([0, NumEMRegisters - 1], varOleStr);
      For k := 0 to  NumEMRegisters - 1  Do Begin
         Result[k] := pMeterObj.RegisterNames[k + 1];
      End;
    End
    Else Result := VarArrayCreate([0, 0], varOleStr); // null array
end;

function TMeters.Get_RegisterValues: OleVariant;

Var
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := VarArrayCreate([0, numEMRegisters-1], varDouble);
            FOR k := 0 to numEMRegisters-1 DO
            Begin
                Result[k] := pMeterObj.Registers[k+1];
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
    pEnergyMeterClass:TEnergyMeter;
    DevClassIndex:Integer;

Begin
     DevClassIndex := ClassNames.Find('energymeter');
     IF DevClassIndex>0 THEN Begin
        pEnergyMeterClass := DSSClassList.Get(DevClassIndex);
        pEnergyMeterClass.ResetAll;
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
    pMeterObj:TEnergyMeterObj;
    TestStr: String;
    Found :Boolean;
Begin


  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of EnergyMeters in active circuit for name
       WITH ActiveCircuit.EnergyMeters DO
         Begin
             TestStr := Value;  // Convert to Pascal String for testing
             Found := FALSE;
             ActiveSave := ActiveIndex;
             pMeterObj := First;
             While pMeterObj <> NIL Do
             Begin
                IF (CompareText(pMeterObj.Name, TestStr) = 0)
                THEN Begin
                    ActiveCircuit.ActiveCktElement := pMeterObj;
                    Found := TRUE;
                    Break;
                End;
                pMeterObj := Next;
             End;
             IF NOT Found
             THEN Begin
                 DoSimpleMsg('EnergyMeter "'+TestStr+'" Not Found in Active Circuit.', 5005);
                 pMeterObj := Get(ActiveSave);    // Restore active Meter
                 ActiveCircuit.ActiveCktElement := pMeterObj;
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
          For i := 1 to NumEMregisters Do Result[i-1] := RegisterTotals[i];
     End
     Else Begin
          Result := VarArrayCreate([0, 0], varDouble);
     End;

end;

function TMeters.Get_Peakcurrent: OleVariant;
Var
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := pMeterObj.SensorCurrent^[k+1];
        End
        Else Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;

end;

procedure TMeters.Set_Peakcurrent(Value: OleVariant);
Var
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.SensorCurrent^[i] := Value[k];
               inc(k);
            End;
        End;
   End;

end;

function TMeters.Get_CalcCurrent: OleVariant;
Var
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := Cabs(pMeterObj.CalculatedCurrent^[k+1]);
        End
        Else Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;

end;

procedure TMeters.Set_CalcCurrent(Value: OleVariant);
Var
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.CalculatedCurrent^[i] := cmplx(Value[k], 0.0);   // Just set the real part
               inc(k);
            End;
        End;
   End;

end;

function TMeters.Get_AllocFactors: OleVariant;
Var
   pMeterObj :TEnergyMeterObj;
   k         :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := VarArrayCreate([0, pMeterObj.NPhases -1], varDouble);
            FOR k := 0 to pMeterObj.NPhases-1 DO  Result[k] := pMeterObj.PhsAllocationFactor^[k+1];
        End
        Else Result := VarArrayCreate([0, 0], varDouble);
   End
   ELSE Begin
        Result := VarArrayCreate([0, 0], varDouble);
   End;

end;

procedure TMeters.Set_AllocFactors(Value: OleVariant);
Var
   pMeterObj :TEnergyMeterObj;
   k, i      :Integer;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            k := VarArrayLowBound(Value, 1);   // get starting index for Value array
            FOR i := 1 to pMeterObj.NPhases DO Begin
               pMeterObj.PhsAllocationFactor^[i] := Value[k];   // Just set the real part
               inc(k);
            End;
        End;
   End;

end;

function TMeters.Get_MeteredElement: WideString;
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            Result := pMeterObj.ElementName;
        End
        Else Result := '';
   End
   ELSE Begin
        Result := '';
   End;

end;

function TMeters.Get_MeteredTerminal: Integer;
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
     Begin
          pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
          If pMeterObj <> Nil Then
            Begin
                Result := pMeterObj.MeteredTerminal;
            End
          Else Result := 0;
     End
   ELSE Begin
        Result := 0;
   End;

end;

procedure TMeters.Set_MeteredElement(const Value: WideString);
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            pMeterObj.elementName := Value;
            pMeterObj.MeteredElementChanged := TRUE;
            pMeterObj.RecalcElementData;
        End;
   End;

end;

procedure TMeters.Set_MeteredTerminal(Value: Integer);
Var
   pMeterObj :TEnergyMeterObj;
Begin

// First make sure active circuit element is a meter
   IF ActiveCircuit <> Nil THEN
   Begin
        pMeterObj :=  TEnergyMeterObj(ActiveCircuit.EnergyMeters.Active);
        If pMeterObj <> Nil Then
        Begin
            pMeterObj.MeteredTerminal := Value;
            pMeterObj.MeteredElementChanged := TRUE;
            pMeterObj.RecalcElementData;
        End;
   End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TMeters, Class_Meters,
    ciInternal, tmApartment);
end.
