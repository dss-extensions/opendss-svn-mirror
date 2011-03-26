unit TSLineConstants;

interface

Uses Arraydef, Ucmatrix, Ucomplex, LineUnits, LineConstants, CableConstants;

TYPE

TTSLineConstants = class(TCableConstants)
  private
    FDiaShield: pDoubleArray;
    FTapeLayer: pDoubleArray;
    FTapeLap:   pDoubleArray;

    function Get_DiaShield(i, units: Integer): Double;
    function Get_TapeLayer(i, units: Integer): Double;
    function Get_TapeLap(i: Integer): Double;

    procedure Set_DiaShield(i, units: Integer; const Value: Double);
    procedure Set_TapeLayer(i, units: Integer; const Value: Double);
    procedure Set_TapeLap(i: Integer; const Value: Double);
  protected

  public
    Function  ConductorsInSameSpace(var ErrorMessage:String):Boolean;override;
    Procedure Calc(f:double);override;

    Constructor Create(NumConductors:Integer);
    Destructor Destroy;  Override;

    Property DiaShield[i, units:Integer]:Double Read Get_DiaShield  Write Set_DiaShield;
    Property TapeLayer[i, units:Integer]:Double Read Get_TapeLayer  Write Set_TapeLayer;
    Property TapeLap[i:Integer]:Double          Read Get_TapeLap    Write Set_TapeLap;
end;

implementation

uses SysUtils;

function TTSLineConstants.Get_DiaShield(i, units: Integer): Double;
begin
  Result := FDiaShield^[i] * From_Meters(Units);
end;

function TTSLineConstants.Get_TapeLayer(i, units: Integer): Double;
begin
  Result := FTapeLayer^[i] * From_Meters(Units);
end;

function TTSLineConstants.Get_TapeLap(i: Integer): Double;
begin
  Result := FTapeLap^[i];
end;

procedure TTSLineConstants.Set_DiaShield(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaShield^[i] := Value * To_Meters(units);
end;

procedure TTSLineConstants.Set_TapeLayer(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FTapeLayer^[i] := Value * To_Meters(units);
end;

procedure TTSLineConstants.Set_TapeLap(i: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FTapeLap^[i] := Value;
end;

procedure TTSLineConstants.Calc(f: double);
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

function TTSLineConstants.ConductorsInSameSpace( var ErrorMessage: String): Boolean;
var
  i,j   :Integer;
  Dij   :Double;
begin
  Result := FALSE;

  For i := 1 to FNumConds do Begin
    if (FY^[i] >= 0.0) then Begin
      Result := TRUE;
      ErrorMessage :=
        Format('TS cable %d height must be < 0. ', [ i ]);
      Exit
    End;
  End;

  For i := 1 to FNumConds do Begin
    for j := i+1 to FNumConds do Begin
      Dij := Sqrt(SQR(FX^[i] - FX^[j]) + SQR(FY^[i] - FY^[j]));
      if (Dij < (Fradius^[i]+Fradius^[j])) then Begin
        Result := TRUE;
        ErrorMessage := Format('TS conductors %d and %d occupy the same space.', [i, j ]);
        Exit;
      End;
    End;
  End;
end;

constructor TTSLineConstants.Create( NumConductors: Integer);
begin
  inherited Create (NumConductors);
  FDiaShield:= Allocmem(Sizeof(FDiaShield^[1])*FNumConds);
  FTapeLayer:= Allocmem(Sizeof(FTapeLayer^[1])*FNumConds);
  FTapeLap:= Allocmem(Sizeof(FTapeLap^[1])*FNumConds);
end;

destructor TTSLineConstants.Destroy;
begin
  Reallocmem(FDiaShield, 0);
  Reallocmem(FTapeLayer, 0);
  Reallocmem(FTapeLap, 0);
  inherited;
end;

initialization

end.
