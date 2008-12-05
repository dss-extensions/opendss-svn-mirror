unit ImplSolution;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  ComObj, ActiveX, OpenDSSEngine_TLB, StdVcl;

type
  TSolution = class(TAutoObject, ISolution)
  protected
    function Get_Frequency: Double; safecall;
    function Get_Hour: Integer; safecall;
    function Get_Iterations: Integer; safecall;
    function Get_LoadMult: Double; safecall;
    function Get_MaxIterations: Integer; safecall;
    function Get_Mode: Integer; safecall;
    function Get_Number: Integer; safecall;
    function Get_Random: Integer; safecall;
    function Get_Seconds: Double; safecall;
    function Get_StepSize: Double; safecall;
    function Get_Tolerance: Double; safecall;
    function Get_Year: Integer; safecall;
    procedure Set_Frequency(Value: Double); safecall;
    procedure Set_Hour(Value: Integer); safecall;
    procedure Set_LoadMult(Value: Double); safecall;
    procedure Set_MaxIterations(Value: Integer); safecall;
    procedure Set_Mode(Mode: Integer); safecall;
    procedure Set_Number(Value: Integer); safecall;
    procedure Set_Random(Random: Integer); safecall;
    procedure Set_Seconds(Value: Double); safecall;
    procedure Set_StepSize(Value: Double); safecall;
    procedure Set_Tolerance(Value: Double); safecall;
    procedure Set_Year(Value: Integer); safecall;
    procedure Solve; safecall;
    function Get_ModeID: WideString; safecall;
    function Get_LoadModel: Integer; safecall;
    procedure Set_LoadModel(Value: Integer); safecall;
    function Get_LDCurve: WideString; safecall;
    procedure Set_LDCurve(const Value: WideString); safecall;
    function Get_pctGrowth: Double; safecall;
    procedure Set_pctGrowth(Value: Double); safecall;
    function Get_AddType: Integer; safecall;
    procedure Set_AddType(Value: Integer); safecall;
    function Get_GenkW: Double; safecall;
    procedure Set_GenkW(Value: Double); safecall;
    function Get_GenPF: Double; safecall;
    procedure Set_GenPF(Value: Double); safecall;
    function Get_Capkvar: Double; safecall;
    procedure Set_Capkvar(Value: Double); safecall;
    function Get_Algorithm: Integer; safecall;
    procedure Set_Algorithm(Value: Integer); safecall;
    function Get_ControlMode: Integer; safecall;
    procedure Set_ControlMode(Value: Integer); safecall;
    function Get_GenMult: Double; safecall;
    procedure Set_GenMult(Value: Double); safecall;
    function Get_DefaultDaily: WideString; safecall;
    function Get_DefaultYearly: WideString; safecall;
    procedure Set_DefaultDaily(const Value: WideString); safecall;
    procedure Set_DefaultYearly(const Value: WideString); safecall;
    function Get_EventLog: OleVariant; safecall;
    function Get_dblHour: Double; safecall;
    procedure Set_dblHour(Value: Double); safecall;
    procedure Set_StepsizeHr(Value: Double); safecall;
    procedure Set_StepsizeMin(Value: Double); safecall;
  end;

implementation

uses ComServ, DSSGlobals, Math, LoadShape, Utilities, Variants;

function TSolution.Get_Frequency: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Frequency
     Else Result := 0.0;
end;

function TSolution.Get_Hour: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.intHour
     Else Result := 0;
end;

function TSolution.Get_Iterations: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Iteration
     Else Result := 0;
end;

function TSolution.Get_LoadMult: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.LoadMultiplier
     Else Result := 0.0;
end;

function TSolution.Get_MaxIterations: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.MaxIterations
     Else Result := 0;
end;

function TSolution.Get_Mode: Integer;
begin
     //If ActiveCircuit <> Nil Then Result := GetSolutionModeID      changed to integer 8/16/00
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Mode
     Else Result := 0;
end;

function TSolution.Get_Number: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.NumberOfTimes
     Else Result := 0;
end;

function TSolution.Get_Random: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.RandomType
     Else Result := 0;
end;

function TSolution.Get_Seconds: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.dynavars.t
     Else Result := 0.0;
end;

function TSolution.Get_StepSize: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.dynavars.h
     Else Result := 0.0;
end;

function TSolution.Get_Tolerance: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.ConvergenceTolerance
     Else Result := 0.0;
end;

function TSolution.Get_Year: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Year
     Else Result := 0;
end;

procedure TSolution.Set_Frequency(Value: Double);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Frequency  := Value;
end;

procedure TSolution.Set_Hour(Value: Integer);
begin
     If ActiveCircuit <> Nil Then With  ActiveCircuit.Solution Do Begin
        intHour  := Value;
        UpdatedblHour;
     End;
end;

procedure TSolution.Set_LoadMult(Value: Double);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.LoadMultiplier  := Value;
end;

procedure TSolution.Set_MaxIterations(Value: Integer);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.MaxIterations  := Value;
end;

procedure TSolution.Set_Mode(Mode: Integer);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Mode := Mode; //InterpretSolveMode(Value);
end;

procedure TSolution.Set_Number(Value: Integer);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.NumberOfTimes  := Value;
end;

procedure TSolution.Set_Random(Random: Integer);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.RandomType := Random;
end;

procedure TSolution.Set_Seconds(Value: Double);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.dynavars.t  := Value;
end;

procedure TSolution.Set_StepSize(Value: Double);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.dynavars.h  := Value;
end;

procedure TSolution.Set_Tolerance(Value: Double);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.ConvergenceTolerance  := Value;
end;

procedure TSolution.Set_Year(Value: Integer);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Year  := Value;
end;

procedure TSolution.Solve;
begin
  IF ActiveCircuit <> Nil THEN ActiveCircuit.Solution.Solve;
end;

function TSolution.Get_ModeID: WideString;
begin
    If ActiveCircuit <> Nil Then Result := GetSolutionModeID
    ELSE Result := '';
end;

function TSolution.Get_LoadModel: Integer;
begin
    If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.LoadModel
    ELSE Result := 0;
end;

procedure TSolution.Set_LoadModel(Value: Integer);
begin

   If ActiveCircuit <> Nil Then  WITH ActiveCircuit.Solution Do Begin
      LoadModel := Value;
      DefaultLoadModel := LoadModel;
   End;

end;

function TSolution.Get_LDCurve: WideString;
begin
     IF ActiveCircuit <> Nil Then Result := ActiveCircuit.LoadDurCurve
     ELSE Result := '';
end;

procedure TSolution.Set_LDCurve(const Value: WideString);
begin
      IF ActiveCircuit <> Nil
      THEN With ActiveCircuit DO
      Begin
            LoadDurCurve    := Value;
            LoadDurCurveObj := LoadShapeClass.Find(LoadDurCurve);
            IF LoadDurCurveObj=NIL THEN
             DoSimpleMsg('Load-Duration Curve not found.', 5001);
      End;

end;

function TSolution.Get_pctGrowth: Double;
begin
     IF ActiveCircuit <> NIL
     THEN With ActiveCircuit DO
     Begin
        Result := (DefaultGrowthRate-1.0)*100.0
     End;
end;

procedure TSolution.Set_pctGrowth(Value: Double);
begin
     IF ActiveCircuit <> NIL
     THEN With ActiveCircuit DO
     Begin
        DefaultGrowthRate := 1.0 + Value/100.0;
        DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Solution.Year-1));
     End;
end;

function TSolution.Get_AddType: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.AddType
     Else Result := 0;
end;

procedure TSolution.Set_AddType(Value: Integer);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.AddType := Value;
end;

function TSolution.Get_GenkW: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.GenkW
     Else Result := 0.0;
end;

procedure TSolution.Set_GenkW(Value: Double);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.GenkW := Value;
end;

function TSolution.Get_GenPF: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.GenPF
     Else Result := 0.0;
end;

procedure TSolution.Set_GenPF(Value: Double);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.GenPF := Value;
end;

function TSolution.Get_Capkvar: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.AutoAddObj.Capkvar
     Else Result := 0.0;
end;

procedure TSolution.Set_Capkvar(Value: Double);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.AutoAddObj.Capkvar := Value;
end;

function TSolution.Get_Algorithm: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.Algorithm
     Else Result := 0;
end;

procedure TSolution.Set_Algorithm(Value: Integer);
begin
     If ActiveCircuit <> Nil Then ActiveCircuit.Solution.Algorithm := Value;
end;

function TSolution.Get_ControlMode: Integer;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.Solution.ControlMode
     Else Result := 0;
end;

procedure TSolution.Set_ControlMode(Value: Integer);
begin
    If ActiveCircuit <> Nil Then With ActiveCircuit.Solution Do Begin
         ControlMode := Value;
         DefaultControlMode := ControlMode;
    End;
end;

function TSolution.Get_GenMult: Double;
begin
     If ActiveCircuit <> Nil Then Result := ActiveCircuit.GenMultiplier
     Else Result := 0.0;
end;

procedure TSolution.Set_GenMult(Value: Double);
begin
    If ActiveCircuit <> Nil Then ActiveCircuit.GenMultiplier := Value;
end;

function TSolution.Get_DefaultDaily: WideString;
begin
     IF   ActiveCircuit <> Nil
     THEN Result := ActiveCircuit.DefaultDailyShapeObj.Name
     ELSE Result := '';
end;

function TSolution.Get_DefaultYearly: WideString;
begin
     IF   ActiveCircuit <> Nil
     THEN Result := ActiveCircuit.DefaultYearlyShapeObj.Name
     ELSE Result := '';
end;

procedure TSolution.Set_DefaultDaily(const Value: WideString);
Var  TestLoadShapeObj :TLoadShapeObj;
begin
     If ActiveCircuit <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass.Find(Value);
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit.DefaultDailyShapeObj  := TestLoadShapeObj;
     END;
end;

procedure TSolution.Set_DefaultYearly(const Value: WideString);
Var  TestLoadShapeObj :TLoadShapeObj;
begin
     If ActiveCircuit <> Nil
     Then
     Begin
           TestLoadShapeObj := LoadShapeClass.Find(Value);
           IF TestLoadShapeObj <> NIL THEN ActiveCircuit.DefaultYearlyShapeObj  := TestLoadShapeObj;
     END;

end;

function TSolution.Get_EventLog: OleVariant;
Var i:Integer;
begin
    If ActiveCircuit <> Nil Then Begin
       Result := VarArrayCreate([0, EventStrings.Count-1], varOleStr);
       For i := 0 to EventStrings.Count-1 Do Begin
          Result[i] := EventStrings.Strings[i]; 
       End;
    END
    Else Result := VarArrayCreate([0,0], varOleStr);;

end;

function TSolution.Get_dblHour: Double;
begin
     If ActiveCircuit <> Nil Then  Begin
        Result := ActiveCircuit.Solution.dblHour;
     End;
end;

procedure TSolution.Set_dblHour(Value: Double);
begin
  If ActiveCircuit <> Nil Then With ActiveCircuit.Solution Do Begin
      intHour := Trunc(Value);
      dblHour := Value;
      Dynavars.t := (Value - intHour) * 3600.0;
  End;
end;

procedure TSolution.Set_StepsizeHr(Value: Double);
begin
  If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.Dynavars.h := Value * 3600.0;
  End;
end;

procedure TSolution.Set_StepsizeMin(Value: Double);
begin

  If ActiveCircuit <> Nil Then Begin
      ActiveCircuit.Solution.Dynavars.h := Value * 60.0;
  End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TSolution, Class_Solution, ciInternal, tmApartment);
end.
