unit InvDynamics;

interface

uses
    PCElement,
    Circuit,
    UComplex,
    mathutil;

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
        SafeMode: Boolean;            // To indicate weather the Inverter has entered into safe mode

        function Get_InvDynValue(myindex: Integer): Double;
        function Get_InvDynName(myindex: Integer): String;
        procedure Set_InvDynValue(myindex: Integer; myValue: Double);
        procedure SolveDynamicStep(i, ActorID: Integer; PICtrl: PPICtrl);

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
        dit[i] := ((m[i] * RatedVDC) - (RS * it[i]) - Vgrid[i].mag) / LS;  // Solves derivative
    end;
end;

end.
