unit CNLineConstants;

interface

Uses Arraydef, Ucmatrix, Ucomplex, LineUnits, LineConstants, CableConstants;

TYPE

TCNLineConstants = class(TCableConstants)
  private
    FkStrand    :pIntegerArray;
    FDiaStrand  :pDoubleArray;
    FGmrStrand  :pDoubleArray;
    FRStrand    :pDoubleArray;

    function Get_kStrand(i: Integer): Integer;
    function Get_DiaStrand(i, units: Integer): Double;
    function Get_GmrStrand(i, units: Integer): Double;
    function Get_RStrand(i, units: Integer): Double;

    procedure Set_kStrand(i: Integer; const Value: Integer);
    procedure Set_DiaStrand(i, units: Integer; const Value: Double);
    procedure Set_GmrStrand(i, units: Integer; const Value: Double);
    procedure Set_RStrand(i, units: Integer; const Value: Double);
  protected

  public
    Function  ConductorsInSameSpace(var ErrorMessage:String):Boolean;override;
    Procedure Calc(f:double);override;

    Constructor Create(NumConductors:Integer);
    Destructor Destroy;  Override;

    Property kStrand[i:Integer]:Integer         Read Get_kStrand    Write Set_kStrand;
    Property DiaStrand[i, units:Integer]:Double Read Get_DiaStrand  Write Set_DiaStrand;
    Property GmrStrand[i, units:Integer]:Double Read Get_GmrStrand  Write Set_GmrStrand;
    Property RStrand[i, units:Integer]:Double   Read Get_RStrand    Write Set_RStrand;
end;

implementation

uses SysUtils;

function TCNLineConstants.Get_kStrand(i: Integer): Integer;
begin
  Result := FkStrand^[i];
end;

function TCNLineConstants.Get_DiaStrand(i, units: Integer): Double;
begin
  Result := FDiaStrand^[i] * From_Meters(Units);
end;

function TCNLineConstants.Get_GmrStrand(i, units: Integer): Double;
begin
  Result := FGmrStrand^[i] * From_Meters(Units);
end;

function TCNLineConstants.Get_RStrand(i, units: Integer): Double;
begin
  Result := FRStrand^[i] * From_Meters(Units);
end;

procedure TCNLineConstants.Set_kStrand(i: Integer; const Value: Integer);
begin
  If (i>0) and (i<=FNumConds) Then FkStrand^[i] := Value;
end;

procedure TCNLineConstants.Set_DiaStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaStrand^[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.Set_GmrStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FGmrStrand^[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.Set_RStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FRStrand^[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.Calc(f: double);
{Compute base Z and YC matrices in ohms/m for this frequency and earth impedance}
Var
  Zi, Zspacing:Complex;
  PowerFreq:Boolean;
  Lfactor:Complex;
  i,j:Integer;
  Dij, Dijp, Pfactor:Double;
  ReducedSize :Integer;
begin
  Frequency := f;  // this has side effects

  If assigned(FZreduced) Then Begin ReducedSize := FZreduced.order; FZreduced.Free;  End Else ReducedSize := 0;
  If assigned(FYCreduced) Then FYCreduced.Free;
  FZreduced := Nil;
  FYCreduced := Nil;

  FZmatrix.Clear;
  FYCMatrix.Clear;

  {For less than 1 kHz use GMR to better match published data}
  LFactor := Cmplx(0.0, Fw*mu0/twopi );
  If  (f < 1000.0)and(f > 40.0) Then PowerFreq:= TRUE Else PowerFreq:= FALSE;

  {Self Impedances}
  For i := 1 to FNumConds Do Begin
    Zi := Get_Zint(i);
    If PowerFreq Then Begin // for less than 1 kHz, use published GMR
      Zi.im := 0.0;
      Zspacing := CmulReal(Lfactor, ln( 1.0/FGMR^[i] ));  // use GMR
    End Else Begin
      Zspacing := CmulReal(Lfactor, ln( 1.0/Fradius^[i] ));
    End;
    FZmatrix.SetElement(i, i, Cadd(Zi, Cadd( Zspacing, Get_Ze(i,i) ) ) );
  End;

  {Mutual IMpedances}
  For i := 1 to FNumConds Do Begin
    For j := 1 to i-1 Do Begin
      Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]));
      FZmatrix.SetElemSym(i, j, Cadd(Cmulreal(Lfactor, ln(1.0/Dij)), Get_Ze(i,j)));
    End;
  End;

  {Capacitance Matrix}
  Pfactor := -1.0/ twopi / e0 / Fw; // include frequency

  {Construct P matrix and then invert}

  For i := 1 to FnumConds Do Begin
    FYCMatrix.SetElement(i, i, cmplx(0.0, pfactor * ln(2.0*abs(Fy^[i])/Fradius^[i])));
  End;

  For i := 1 to FNumConds Do Begin
    For j := 1 to i-1 Do Begin
      Dij  := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]));
      Dijp := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]+Fy^[j])); // distance to image j
      FYCMatrix.SetElemSym(i, j, cmplx(0.0, pfactor * ln(Dijp/Dij)));
    End;
  End;

  FYCMatrix.Invert; // now should be nodal C matrix

  If ReducedSize>0 Then Kron(ReducedSize);  // Was reduced so reduce again to same size

  {Else the Zmatrix is OK as last computed}
  FRhoChanged := FALSE;
end;

function TCNLineConstants.ConductorsInSameSpace( var ErrorMessage: String): Boolean;
var
  i,j   :Integer;
  Dij   :Double;
begin
  Result := FALSE;

  For i := 1 to FNumConds do Begin
    if (FY^[i] >= 0.0) then Begin
      Result := TRUE;
      ErrorMessage :=
        Format('CN cable %d height must be < 0. ', [ i ]);
      Exit
    End;
  End;

  For i := 1 to FNumConds do Begin
    for j := i+1 to FNumConds do Begin
      Dij := Sqrt(SQR(FX^[i] - FX^[j]) + SQR(FY^[i] - FY^[j]));
      if (Dij < (Fradius^[i]+Fradius^[j])) then Begin
        Result := TRUE;
        ErrorMessage := Format('CN conductors %d and %d occupy the same space.', [i, j ]);
        Exit;
      End;
    End;
  End;
end;

constructor TCNLineConstants.Create( NumConductors: Integer);
begin
  inherited Create (NumConductors);
  FkStrand:= Allocmem(Sizeof(FkStrand^[1])*FNumConds);
  FDiaStrand:= Allocmem(Sizeof(FDiaStrand^[1])*FNumConds);
  FGmrStrand:= Allocmem(Sizeof(FGmrStrand^[1])*FNumConds);
  FRStrand:= Allocmem(Sizeof(FRStrand^[1])*FNumConds);
end;

destructor TCNLineConstants.Destroy;
begin
  Reallocmem(FkStrand, 0);
  Reallocmem(FDiaStrand, 0);
  Reallocmem(FGmrStrand, 0);
  Reallocmem(FRStrand, 0);
  inherited;
end;

initialization

end.
