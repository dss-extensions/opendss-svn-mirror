unit CableConstants;

interface

Uses Arraydef, Ucmatrix, Ucomplex, LineUnits, LineConstants;

TYPE

TCableConstants = class(TLineConstants)
  private
    FEpsR       :pDoubleArray;
    FInsLayer   :pDoubleArray;
    FDiaIns     :pDoubleArray;
    FDiaCable   :pDoubleArray;

    function Get_EpsR(i: Integer): Double;
    function Get_InsLayer(i, units: Integer): Double;
    function Get_DiaIns(i, units: Integer): Double;
    function Get_DiaCable(i, units: Integer): Double;

    procedure Set_EpsR(i: Integer; const Value: Double);
    procedure Set_InsLayer(i, units: Integer; const Value: Double);
    procedure Set_DiaIns(i, units: Integer; const Value: Double);
    procedure Set_DiaCable(i, units: Integer; const Value: Double);
  protected

  public
    Constructor Create(NumConductors:Integer);
    Destructor Destroy;  Override;

    Property EpsR[i:Integer]:Double            Read Get_EpsR     Write Set_EpsR;
    Property InsLayer[i, units:Integer]:Double Read Get_InsLayer Write Set_InsLayer;
    Property DiaIns[i, units:Integer]:Double   Read Get_DiaIns   Write Set_DiaIns;
    Property DiaCable[i, units:Integer]:Double Read Get_DiaCable Write Set_DiaCable;
end;

implementation

uses SysUtils;

function TCableConstants.Get_EpsR(i: Integer): Double;
begin
  Result := FEpsR^[i];
end;

function TCableConstants.Get_InsLayer(i, units: Integer): Double;
begin
  Result := FInsLayer^[i] * From_Meters(Units);
end;

function TCableConstants.Get_DiaIns(i, units: Integer): Double;
begin
  Result := FDiaIns^[i] * From_Meters(Units);
end;

function TCableConstants.Get_DiaCable(i, units: Integer): Double;
begin
  Result := FDiaCable^[i] * From_Meters(Units);
end;

procedure TCableConstants.Set_EpsR(i: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FEpsR^[i] := Value;
end;

procedure TCableConstants.Set_InsLayer(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FInsLayer^[i] := Value * To_Meters(units);
end;

procedure TCableConstants.Set_DiaIns(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaIns^[i] := Value * To_Meters(units);
end;

procedure TCableConstants.Set_DiaCable(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaCable^[i] := Value * To_Meters(units);
end;

constructor TCableConstants.Create( NumConductors: Integer);
begin
  inherited Create (NumConductors);
  FEpsR:= Allocmem(Sizeof(FEpsR^[1])*FNumConds);
  FInsLayer:= Allocmem(Sizeof(FInsLayer^[1])*FNumConds);
  FDiaIns:= Allocmem(Sizeof(FDiaIns^[1])*FNumConds);
  FDiaCable:= Allocmem(Sizeof(FDiaCable^[1])*FNumConds);
end;

destructor TCableConstants.Destroy;
begin
  Reallocmem(FEpsR, 0);
  Reallocmem(FInsLayer, 0);
  Reallocmem(FDiaIns, 0);
  Reallocmem(FDiaCable, 0);
  inherited;
end;

initialization

end.
