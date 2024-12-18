unit CNTSLineConstants;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

Uses Arraydef, Ucmatrix, Ucomplex, LineUnits, LineConstants, CableConstants;

TYPE

TCNTSLineConstants = class(TCableConstants)
  private

    // For All
    FCondType    :pIntegerArray; // Use as 1: CN, 2: TS, 3: Bare wire

    // For CN
    FkStrand    :pIntegerArray;
    FDiaStrand  :pDoubleArray;
    FGmrStrand  :pDoubleArray;
    FRStrand    :pDoubleArray;
    FSemicon    :Array of boolean;  // Attention!: zero based index for this type of array

    // For TS
    FDiaShield: pDoubleArray;
    FTapeLayer: pDoubleArray;
    FTapeLap:   pDoubleArray;

    // For All
    function Get_CondType(i: Integer): Integer;

    // For CN
    function Get_kStrand(i: Integer): Integer;
    function Get_DiaStrand(i, units: Integer): Double;
    function Get_GmrStrand(i, units: Integer): Double;
    function Get_RStrand(i, units: Integer): Double;
    function Get_Semicon(i: Integer): Boolean;

    // For TS
    function Get_DiaShield(i, units: Integer): Double;
    function Get_TapeLayer(i, units: Integer): Double;
    function Get_TapeLap(i: Integer): Double;

    // For All
    procedure Set_CondType(i: Integer; const Value: Integer);

    // For CN
    procedure Set_kStrand(i: Integer; const Value: Integer);
    procedure Set_DiaStrand(i, units: Integer; const Value: Double);
    procedure Set_GmrStrand(i, units: Integer; const Value: Double);
    procedure Set_RStrand(i, units: Integer; const Value: Double);
    procedure Set_Semicon(i: Integer; const Value: Boolean);

    // For TS
    procedure Set_DiaShield(i, units: Integer; const Value: Double);
    procedure Set_TapeLayer(i, units: Integer; const Value: Double);
    procedure Set_TapeLap(i: Integer; const Value: Double);
  protected

  public
    Procedure Calc(f:double);override;

    Constructor Create(NumConductors:Integer);
    Destructor Destroy;  Override;

    // For All
    Property CondType[i:Integer]:Integer         Read Get_CondType    Write Set_CondType;

    // For CN
    Property kStrand[i:Integer]:Integer         Read Get_kStrand    Write Set_kStrand;
    Property DiaStrand[i, units:Integer]:Double Read Get_DiaStrand  Write Set_DiaStrand;
    Property GmrStrand[i, units:Integer]:Double Read Get_GmrStrand  Write Set_GmrStrand;
    Property RStrand[i, units:Integer]:Double   Read Get_RStrand    Write Set_RStrand;
    Property Semicon[i:Integer]:Boolean Read Get_Semicon    Write Set_Semicon;

    // For TS
    Property DiaShield[i, units:Integer]:Double Read Get_DiaShield  Write Set_DiaShield;
    Property TapeLayer[i, units:Integer]:Double Read Get_TapeLayer  Write Set_TapeLayer;
    Property TapeLap[i:Integer]:Double          Read Get_TapeLap    Write Set_TapeLap;
end;

implementation

uses SysUtils, Math, Utilities;

Const
  // For TS
  RhoTS:double = 2.3718e-8;  // for copper tape shield



// For All
function TCNTSLineConstants.Get_CondType(i: Integer): Integer;
begin
  Result := FCondType^[i];
end;

procedure TCNTSLineConstants.Set_CondType(i: Integer; const Value: Integer);
begin
  If (i>0) and (i<=FNumConds) Then FCondType^[i] := Value;
end;

// For CN
function TCNTSLineConstants.Get_kStrand(i: Integer): Integer;
begin
  Result := FkStrand^[i];
end;

function TCNTSLineConstants.Get_Semicon(i: Integer): Boolean;
begin
  Result := FSemicon[i - 1];
end;

function TCNTSLineConstants.Get_DiaStrand(i, units: Integer): Double;
begin
  Result := FDiaStrand^[i] * From_Meters(Units);
end;

function TCNTSLineConstants.Get_GmrStrand(i, units: Integer): Double;
begin
  Result := FGmrStrand^[i] * From_Meters(Units);
end;

function TCNTSLineConstants.Get_RStrand(i, units: Integer): Double;
begin
  Result := FRStrand^[i] * From_Per_Meter(Units);
end;

procedure TCNTSLineConstants.Set_kStrand(i: Integer; const Value: Integer);
begin
  If (i>0) and (i<=FNumConds) Then FkStrand^[i] := Value;
end;

procedure TCNTSLineConstants.Set_Semicon(i: Integer; const Value: Boolean);
begin
  If (i>0) and (i<=FNumConds) Then FSemicon[i - 1] := Value;
end;

procedure TCNTSLineConstants.Set_DiaStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaStrand^[i] := Value * To_Meters(units);
end;

procedure TCNTSLineConstants.Set_GmrStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FGmrStrand^[i] := Value * To_Meters(units);
end;

procedure TCNTSLineConstants.Set_RStrand(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FRStrand^[i] := Value * To_Per_Meter(units);
end;


// For TS
function TCNTSLineConstants.Get_DiaShield(i, units: Integer): Double;
begin
  Result := FDiaShield^[i] * From_Meters(Units);
end;

function TCNTSLineConstants.Get_TapeLayer(i, units: Integer): Double;
begin
  Result := FTapeLayer^[i] * From_Meters(Units);
end;

function TCNTSLineConstants.Get_TapeLap(i: Integer): Double;
begin
  Result := FTapeLap^[i];
end;

procedure TCNTSLineConstants.Set_DiaShield(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FDiaShield^[i] := Value * To_Meters(units);
end;

procedure TCNTSLineConstants.Set_TapeLayer(i, units: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FTapeLayer^[i] := Value * To_Meters(units);
end;

procedure TCNTSLineConstants.Set_TapeLap(i: Integer; const Value: Double);
begin
  If (i>0) and (i<=FNumConds) Then FTapeLap^[i] := Value;
end;



procedure TCNTSLineConstants.Calc(f: double);
{Compute base Z and YC matrices in ohms/m for this frequency and earth impedance}
Var
  Zi, Zspacing:  Complex;
  PowerFreq:     Boolean;
  Lfactor:       Complex;
  i, j:          Integer;
  Dij, Yfactor:  Double;
  ReducedSize:   Integer;
  N, idxi, idxj: Integer;
  Zmat, Ztemp:   TCMatrix;
  Denom, RadIn, RadOut:  Double;

  // For CN
  ResCN, RadCN, RadStrand:  Double;
  GmrCN:         Double;

  // For TS
  ResTS:         Double;
  GmrTS:         Double;

  {$IFDEF ANDREA}
  {****} DumpFile : TextFile;
  {$ENDIF}
begin
  Frequency := f;  // this has side effects

  If assigned(FZreduced) Then Begin
    ReducedSize := FZreduced.order;
    FZreduced.Free;
  End Else
    ReducedSize := 0;
  If assigned(FYCreduced) Then FYCreduced.Free;
  FZreduced := Nil;
  FYCreduced := Nil;

  FZmatrix.Clear;
  FYCMatrix.Clear;

  // add concentric neutrals or tape shields to the end of conductor list; they are always reduced
  N := FNumConds + FNumPhases;
  Zmat := TCMatrix.CreateMatrix(N);

  {For less than 1 kHz use GMR to better match published data}
  LFactor := Cmplx(0.0, Fw*mu0/twopi );
  If  (f < 1000.0)and(f > 40.0) Then PowerFreq:= TRUE Else PowerFreq:= FALSE;

  // Self Impedances - CN/TS cores and bare neutrals
  For i := 1 to FNumConds Do Begin
    Zi := Get_Zint(i);
    If PowerFreq Then Begin // for less than 1 kHz, use published GMR
      Zi.im := 0.0;
      Zspacing := CmulReal(Lfactor, ln( 1.0/FGMR^[i] ));  // use GMR
    End Else Begin
      Zspacing := CmulReal(Lfactor, ln( 1.0/Fradius^[i] ));
    End;
    Zmat.SetElement(i, i, Cadd(Zi, Cadd( Zspacing, Get_Ze(i,i))));
  End;

  // CN/TS self impedances
  for i := 1 to FNumPhases do begin
    if FCondType^[i] = 1 then begin  // CN
      ResCN := FRstrand^[i] / FkStrand^[i];
      RadCN := 0.5 * (FDiaCable^[i] - FDiaStrand^[i]);
      GmrCN := Power (FGmrStrand^[i] * FkStrand^[i] * Power(RadCN, FkStrand^[i] - 1.0),
                      1.0 / FkStrand^[i]);
      Zspacing := CMulReal (Lfactor, ln(1.0/GmrCN));
      Zi := cmplx (ResCN, 0.0);
      idxi := i + FNumConds;
      Zmat.SetElement(idxi, idxi, Cadd(Zi, Cadd (Zspacing, Get_Ze (i, i))));
    end
    else if FCondType^[i] = 2 then begin  // TS
      ResTS := 0.3183 * RhoTS / (FDiaShield^[i]*FTapeLayer^[i]*sqrt(50.0/(100.0-FTapeLap^[i])));
      GmrTS := 0.5 * (FDiaShield^[i] - FTapeLayer^[i]);  // per Kersting, to center of TS
      Zspacing := CMulReal (Lfactor, ln(1.0/GmrTS));
      Zi := cmplx (ResTS, 0.0);
      idxi := i + FNumConds;
      Zmat.SetElement(idxi, idxi, Cadd(Zi, Cadd (Zspacing, Get_Ze (i, i))));
    end;
  End;

  // Mutual Impedances - between CN/TS cores and bare neutrals
  For i := 1 to FNumConds Do Begin
    For j := 1 to i-1 Do Begin
      if not FEquivalentSpacing then Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]))
      else
      begin
        if ((j <= FNumPhases) and (i > FNumPhases)) then Dij := FEqDist[2 - 1] // EqDistPhN
        else Dij := FEqDist[1 - 1];  // EqDistPhPh (including N-N conductorss)
      end;
      Zmat.SetElemSym(i, j, Cadd(Cmulreal(Lfactor, ln(1.0/Dij)), Get_Ze(i,j)));
    End;
  End;

  // Mutual Impedances - CN/TS to other CN/TS, cores, and bare neutrals
  For i := 1 to FNumPhases Do Begin
    idxi := i + FNumConds;
    For j := 1 to i-1 Do Begin  // CN/TS to other CN/TS
      idxj := j + FNumConds;
      if not FEquivalentSpacing then Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]))
      else
      begin
        if ((j <= FNumPhases) and (i > FNumPhases)) then Dij := FEqDist[2 - 1] // EqDistPhN
        else Dij := FEqDist[1 - 1];  // EqDistPhPh (including N-N conductorss)
      end;
      Zmat.SetElemSym(idxi, idxj, Cadd(Cmulreal(Lfactor, ln(1.0/Dij)), Get_Ze(i,j)));
    End;
    for j := 1 to FNumConds do begin // CN/TS to cores and bare neutrals
      idxj := j;
      if FCondType^[i] = 1 then begin  // CN
        RadCN := 0.5 * (FDiaCable^[i] - FDiaStrand^[i]);
        if i = j then begin // CN to its own phase core
          Dij := RadCN;
        end else begin // CN to another phase or bare neutral
          if not FEquivalentSpacing then Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]))
          else
          begin
            if ((i <= FNumPhases) and (j > FNumPhases)) then Dij := FEqDist[2 - 1] // EqDistPhN
            else Dij := FEqDist[1 - 1];  // EqDistPhPh (including N-N conductorss)
          end;
          Dij := Power (Power(Dij, FkStrand^[i]) - Power(RadCN, FkStrand^[i]),
                1.0 / FkStrand^[i]);
        end;
      end
      else if FCondType^[i] = 2 then begin  // TS
        GmrTS := 0.5 * (FDiaShield^[i] - FTapeLayer^[i]);  // per Kersting, to center of TS
        if i = j then begin // TS to its own phase core
          Dij := GmrTS;
        end else begin // TS to another phase or bare neutral
          if not FEquivalentSpacing then Dij := sqrt(sqr(Fx^[i]-Fx^[j]) + sqr(Fy^[i]-Fy^[j]))
          else
          begin
            if ((i <= FNumPhases) and (j > FNumPhases)) then Dij := FEqDist[2 - 1] // EqDistPhN
            else Dij := FEqDist[1 - 1];  // EqDistPhPh (including N-N conductorss)
          end;
        end;
      end;
      Zmat.SetElemSym(idxi, idxj, Cadd(Cmulreal(Lfactor, ln(1.0/Dij)), Get_Ze(i,j)));
    end;
  End;

  {$IFDEF ANDREA}
//***** Special for Andrea to see 6x6 matrix before it is reduced
  Assignfile(DumpFile, 'CNData-1.txt');
  Rewrite(Dumpfile);
  Writeln(DumpFile, 'Before Reduction');
  DumpComplexMatrix(DumpFile, Zmat);
//*****
  {$ENDIF}

  // reduce out the CN
  while Zmat.Order > FNumConds do begin
    Ztemp := Zmat.Kron(Zmat.Order);
    Zmat.Free;
    Zmat := Ztemp;
  end;
  FZMatrix.CopyFrom(Zmat);
  Zmat.Free;

  {$IFDEF ANDREA}
//*****    Special for Andrea
  Writeln(DumpFile, 'After Reduction');
  DumpComplexMatrix(DumpFile, FZMatrix);
  CloseFile(DumpFile);
  FireOffEditor('CNData-1.txt');
//*****
  {$ENDIF}

  // for shielded cables, build the capacitance matrix directly
  // assumes the insulation may lie between semicon layers
  for i := 1 to FNumPhases do begin
    Yfactor := twopi * e0 * FEpsR^[i] * Fw; // includes frequency so C==>Y
    RadOut := 0.5 * FDiaIns^[i];
    RadIn := RadOut - FInsLayer^[i];
    if FCondType^[i] = 1 then begin  // CN
      if FSemicon[i - 1] then begin
        // Semicon layer (default)
        Denom := ln(RadOut / RadIn);
      end else begin
        // No semicon layer (Synergi and Kersting/Kerestes' book)
        RadCN := 0.5 * (FDiaCable^[i] - FDiaStrand^[i]);
        RadStrand := 0.5 * FDiaStrand^[i];
        Denom := ln(RadCN / RadIn) - (1 / FkStrand^[i]) * ln(FkStrand^[i] * RadStrand / RadCN);
      end;
    end
    else if FCondType^[i] = 2 then begin  // TS
      Denom := ln (RadOut / RadIn);
    end;
    FYCMatrix.SetElement(i, i, cmplx(0.0, Yfactor / Denom));
  end;

  If ReducedSize>0 Then Kron(ReducedSize);  // Was reduced so reduce again to same size

  {Else the Zmatrix is OK as last computed}
  FRhoChanged := FALSE;
end;


constructor TCNTSLineConstants.Create( NumConductors: Integer);
begin
  inherited Create (NumConductors);
  // For All
  FCondType:= Allocmem(Sizeof(FCondType^[1])*FNumConds);

  // For CN
  FkStrand:= Allocmem(Sizeof(FkStrand^[1])*FNumConds);
  FDiaStrand:= Allocmem(Sizeof(FDiaStrand^[1])*FNumConds);
  FGmrStrand:= Allocmem(Sizeof(FGmrStrand^[1])*FNumConds);
  FRStrand:= Allocmem(Sizeof(FRStrand^[1])*FNumConds);
  SetLength(FSemicon, FNumConds);

  // For TS
  FDiaShield:= Allocmem(Sizeof(FDiaShield^[1])*FNumConds);
  FTapeLayer:= Allocmem(Sizeof(FTapeLayer^[1])*FNumConds);
  FTapeLap:= Allocmem(Sizeof(FTapeLap^[1])*FNumConds);
end;

destructor TCNTSLineConstants.Destroy;
begin
  // For All
  Reallocmem(FCondType, 0);

  // For CN
  Reallocmem(FkStrand, 0);
  Reallocmem(FDiaStrand, 0);
  Reallocmem(FGmrStrand, 0);
  Reallocmem(FRStrand, 0);
  SetLength(FSemicon, 0);

  // For TS
  Reallocmem(FDiaShield, 0);
  Reallocmem(FTapeLayer, 0);
  Reallocmem(FTapeLap, 0);

  inherited;
end;

end.

