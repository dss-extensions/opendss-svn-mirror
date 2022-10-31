unit InvDynamics;

interface

uses
    PCElement,
    Circuit,
    UComplex,
    mathutil,
    Utilities,
    UcMatrix;

type
  {Structure for hosting data and solving for each inverter based element}
    TInvDynamicVars = packed record
        Vgrid: array of Polar;     // Grid voltage at the point of connection per phase
        dit,                                        // Current's first derivative per phase
        it,                                         // Current's integration per phase
        itHistory,                                  // Shift register for it
        m: array of Double;    // Average duty cycle per phase
        iMaxPPhase,
        kP,                                         // PI controller gain
        CtrlTol,                                    // Control loop tolerance
        SMThreshold,                                // Voltage threshold for entering into safe mode
        RatedVDC,                                   // Rated DC voltage at the inverter's input
        LS,                                         // Series inductance, careful, it cannot be 0 in dyn mode
        RS,                                         // Series resistance (filter)
        BasekV,                                     // BAse kV depending on the number of phases
        MaxVS,                                      // Max Voltage at the inverter terminal to safely operate the inverter
        MinVS,                                      // Min Voltage at the inverter terminal to safely operate the inverter
        MinAmps,                                    // Min amps required for exporting energy
        ISP: Double;             // Current setpoint according to the actual DER kW
        Discharging,                                // To verify if the storage device is discharging
        SafeMode: Boolean;            // To indicate weather the Inverter has entered into safe mode

        function Get_InvDynValue(myindex: Integer): Double;
        function Get_InvDynName(myindex: Integer): String;
        procedure Set_InvDynValue(myindex: Integer; myValue: Double);
        procedure SolveDynamicStep(i, ActorID: Integer; PICtrl: PPICtrl);
        procedure SolveModulation(i, ActorID: Integer; PICtrl: PPICtrl);
        function DoGFM_Mode(ActorID, NPhases: Integer): Boolean;
        procedure CalcGFMYprim(ActorID, NPhases: Integer; YMatrix: pTcMatrix);
        procedure CalcGFMVoltage(ActorID, NPhases: Integer; x: pComplexArray);

    end;

var
    NumInvDynVars: Integer = 9;

implementation

uses
    DSSGlobals;

  // Returns the value of the given state variable using the index lo localize it
function TInvDynamicVars.Get_InvDynValue(myindex: Integer): Double;
begin
    case myindex of
        0:
            if length(Vgrid) > 0 then
                Result := Vgrid[0].mag
            else
                Result := 0;
        1:
            if length(dit) > 0 then
                Result := dit[0]
            else
                Result := 0;
        2:
            if length(it) > 0 then
                Result := it[0]
            else
                Result := 0;
        3:
            if length(ithistory) > 0 then
                Result := ithistory[0]
            else
                Result := 0;
        4:
            Result := RatedVDC;
        5:
            if length(m) > 0 then
                Result := m[0]
            else
                Result := 0;
        6:
            Result := ISP;
        7:
            Result := LS;
        8:
            Result := iMaxPPhase;
    else
        Result := 0;
    end;

end;

  // Sets the value for the state variable indicated in the index
procedure TInvDynamicVars.Set_InvDynValue(myindex: Integer; myValue: Double);
begin
    case myindex of
        0: ;  // Read only
        1:
            dit[0] := myValue;
        2:
            it[0] := myValue;
        3:
            ithistory[0] := myValue;
        4:
            RatedVDC := myValue;
        5:
            m[0] := myValue;
        6: ; // Read only
        7:
            LS := myValue;
        8:
            iMAxPPhase := myValue;
    else
      // Do nothing
    end;

end;

  // Returns the name of the state variable located by the given index
function TInvDynamicVars.Get_InvDynName(myindex: Integer): String;
begin
    case myindex of
        0:
            Result := 'Grid voltage';
        1:
            Result := 'di/dt';
        2:
            Result := 'it';
        3:
            Result := 'it History';
        4:
            Result := 'Rated VDC';
        5:
            Result := 'Avg duty cycle';
        6:
            Result := 'Target (Amps)';
        7:
            Result := 'Series L';
        8:
            Result := 'Max. Amps (phase)';
    else
        Result := 'Unknown variable';
    end;

end;

  // Solves the derivative term of the differential equation to be integrated
procedure TInvDynamicVars.SolveDynamicStep(i, ActorID: Integer; PICtrl: PPICtrl);
var
    myDCycle,
    iDelta,
    iErrorPct,
    iError: Double;

begin
    with ActiveCircuit[ActorID].Solution do
    begin
        SolveModulation(i, ActorID, PICtrl);
        dit[i] := ((m[i] * RatedVDC) - (RS * it[i]) - Vgrid[i].mag) / LS;  // Solves derivative
    end;
end;

  // Calculates and stores the averaged modulation factor for controlling the inverter output
procedure TInvDynamicVars.SolveModulation(i, ActorID: Integer; PICtrl: PPICtrl);
var
    myDCycle,
    iDelta,
    iErrorPct,
    iError: Double;

begin
    with ActiveCircuit[ActorID].Solution do
    begin
        if (DynaVars.IterationFlag <> 0) then
        begin                                                     // duty cycle at time h
            iError := (ISP - it[i]);                          // Only recalculated on the second iter
            iErrorPct := iError / ISP;
            if Abs(iErrorPct) > CtrlTol then
            begin
                iDelta := PICtrl^.SolvePI(IError);
                myDCycle := m[i] + iDelta;
                if Vgrid[i].mag > MinVS then
                begin
                    if SafeMode then
                    begin
               //Coming back from safe operation, need to boost duty cycle
                        m[i] := ((RS * it[i]) + Vgrid[i].mag) / RatedVDC;
                        SafeMode := false;
                    end
                    else
                    if (myDCycle <= 1) and (myDCycle > 0) then
                        m[i] := myDCycle;
                end
                else
                begin
                    m[i] := 0;
                    SafeMode := true;
                end;
            end;
        end;

    end;
end;
//---------------------------------------------------------------------------------------
//|             Calculates the current phasors to match with the target (v)             |
//---------------------------------------------------------------------------------------
function TInvDynamicVars.DoGFM_Mode(ActorID, NPhases: Integer): Boolean;
var
    j,
    i: Integer;
    myError: Double;

begin
    // Checks for initialization
    if length(it) < NPhases then
    begin
      // If enters here is because probably this is the first iteration
        setlength(it, NPhases);  // Used to correct the magnitude
        setlength(dit, NPhases); // Used to correct the phase angle
      // Initializes the currents vector for phase and angle
        for j := 1 to NPhases do
        begin
            it[j - 1] := 0;
            dit[j - 1] := (j - 1) * (TwoPi / -3);
        end;
    end;

    with ActiveCircuit[ActorID].Solution do
    begin
        iMaxPPhase := ISP / BasekV;
        for i := 1 to NPhases do
        begin
            myError := 1 - (Vgrid[i - 1].mag / BasekV);
            if Discharging then
            begin
          // Corrects the magnitude
                it[i - 1] := it[i - 1] + (myError * (ISP / BasekV) * kP * 1000);
          // Checks if the IBR is out of the saturaton point
                if it[i - 1] > (ISP / BasekV) then
                    it[i - 1] := (ISP / BasekV);

          // Corrects the phase angle
                myError := (((i - 1) * TwoPi / -3) - Vgrid[i - 1].ang);
                dit[i - 1] := dit[i - 1] + myError;
                Vgrid[i - 1].ang := dit[i - 1];  // Saves at the Vgrid register for future use
            end
            else
            if not Discharging then
                Result := false;
        end;
    end;
end;

//---------------------------------------------------------------------------------------
//| Calculates the equivalent short circuit impedance for the inverter operating in GFM |
//| Similar to the calculation user for VSource, replacing some variables with constants|
//---------------------------------------------------------------------------------------
procedure TInvDynamicVars.CalcGFMYprim(ActorID, NPhases: Integer; YMatrix: pTcMatrix);
var
    Z: TcMatrix;
    Zs,
    Zm: Complex;
    a,
    b,
    c,
    R0,
    X0,
    X1,
    R1,
    R2,
    X2,
    Isc1,
    Xs,
    Rs,
    Rm,
    Xm: Double;
    i,
    j: Integer;
begin
    Z := TCmatrix.CreateMatrix(YMatrix^.Order);

    X1 := (Sqr(BasekV) / ISP) / Sqrt(1.0 + 0.0625);
    R1 := X1 / 4; // Uses defaults
    R2 := R1;     // default Z2 = Z1
    X2 := X1;
    Isc1 := (ISP * 1000.0 / (sqrt(3) * BasekV)) / NPhases;
  //  Compute R0, X0
    a := 10;
    b := (4.0 * (R1 + X1 * 3));
    c := (4.0 * (R1 * R1 + X1 * X1) - SQR((sqrt(3) * BasekV * 1000.0) / Isc1));
    R0 := QuadSolver(a, b, c);
    X0 := R0 * 3;
    // for Z matrix
    Xs := (2.0 * X1 + X0) / 3.0;
    Rs := (2.0 * R1 + R0) / 3.0;
    Rm := (R0 - R1) / 3.0;
    Xm := (X0 - X1) / 3.0;
    Zs := cmplx(Rs, Xs);
    Zm := cmplx(Rm, Xm);

    for i := 1 to NPhases do
    begin
        Z.SetElement(i, i, Zs);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
        end;
    end;

    Z.Invert();
    YMatrix^.CopyFrom(Z);
end;
//---------------------------------------------------------------------------------------
//|   Calculates the voltage magnitudes and angles for facilitating GFM control mode    |
//---------------------------------------------------------------------------------------
procedure TInvDynamicVars.CalcGFMVoltage(ActorID, NPhases: Integer; x: pComplexArray);
var
    refAngle: Double;
    i: Integer;
begin
    refAngle := 0;
    for i := 1 to NPhases do
        x^[i] := pdegtocomplex(BasekV, (360.0 + refAngle - ((i - 1) * 360.0) / NPhases));
end;

end.
