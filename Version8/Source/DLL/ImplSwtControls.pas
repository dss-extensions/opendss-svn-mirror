unit ImplSwtControls;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;



type
  TSwtControls = class(TAutoObject, ISwtControls)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Delay: Double; safecall;
    function Get_First: Integer; safecall;
    function Get_IsLocked: WordBool; safecall;
    function Get_Name: WideString; safecall;
    function Get_Next: Integer; safecall;
    function Get_SwitchedObj: WideString; safecall;
    function Get_SwitchedTerm: Integer; safecall;
    procedure Set_Delay(Value: Double); safecall;
    procedure Set_IsLocked(Value: WordBool); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_SwitchedObj(const Value: WideString); safecall;
    procedure Set_SwitchedTerm(Value: Integer); safecall;
    function Get_Count: Integer; safecall;
    function Get_NormalState: OleVariant; safecall;
    procedure Set_NormalState(Value: OleVariant); safecall;
    function Get_State: OleVariant; safecall;
    procedure Set_State(Value: OleVariant); safecall;
    procedure Open; safecall;
    procedure Close; safecall;
    procedure Reset; safecall;
    function Get_RatedCurrent: Double; safecall;
    procedure Set_RatedCurrent(Value: Double); safecall;

  end;

implementation

uses ComServ, DSSGlobals, Executive, ControlElem, SwtControl, Variants, SysUtils, PointerList;

function ActiveSwtControl: TSwtControlObj;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].SwtControls.Active;
end;

procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit[ActiveActor]) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('swtcontrol.%s.%s=%s', [ActiveSwtControl.Name, parm, val]);
  DSSExecutive[ActiveActor].Command := cmd;
end;


function TSwtControls.Get_AllNames: OleVariant;
Var
  elem: TSwtControlObj;
  lst: TPointerList;
  k: Integer;
Begin
  Result := VarArrayCreate([0, 0], varOleStr);
  Result[0] := 'NONE';
  IF ActiveCircuit[ActiveActor] <> Nil THEN WITH ActiveCircuit[ActiveActor] DO
  If SwtControls.ListSize > 0 Then
  Begin
    lst := SwtControls;
    Result := VarArrayCreate([0, lst.ListSize-1], varOleStr);
    k:=0;
    elem := lst.First;
    WHILE elem<>Nil DO Begin
      Result[k] := elem.Name;
      Inc(k);
      elem := lst.Next;
    End;
  End;
end;

function TSwtControls.Get_Delay: Double;
var
  elem: TSwtControlObj;
begin
  Result := 0.0;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.TimeDelay;
end;

function TSwtControls.Get_First: Integer;
Var
  elem: TSwtControlObj;
  lst:  TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
    elem := lst.First;
    If elem <> Nil Then Begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := 1;
        End
        Else elem := lst.Next;
      Until (Result = 1) or (elem = nil);
    End;
  End;
end;

function TSwtControls.Get_IsLocked: WordBool;
var
  elem: TSwtControlObj;
begin
  Result := FALSE;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.IsLocked;
end;

function TSwtControls.Get_Name: WideString;
var
  elem: TSwtControlObj;
begin
  Result := '';
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.Name;
end;

function TSwtControls.Get_Next: Integer;
Var
  elem: TSwtControlObj;
  lst: TPointerList;
Begin
  Result := 0;
  If ActiveCircuit[ActiveActor] <> Nil Then Begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
    elem := lst.Next;
    if elem <> nil then begin
      Repeat
        If elem.Enabled Then Begin
          ActiveCircuit[ActiveActor].ActiveCktElement := elem;
          Result := lst.ActiveIndex;
        End
        Else elem := lst.Next;
      Until (Result > 0) or (elem = nil);
    End
  End;
end;

function TSwtControls.Get_SwitchedObj: WideString;
var
  elem: TSwtControlObj;
begin
  Result := '';
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.ElementName;
end;

function TSwtControls.Get_SwitchedTerm: Integer;
var
  elem: TSwtControlObj;
begin
  Result := 0;
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.ElementTerminal;
end;

procedure TSwtControls.Set_Delay(Value: Double);
var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
      elem.TimeDelay  := Value;
  end;
end;

procedure TSwtControls.Set_IsLocked(Value: WordBool);
var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
      elem.Locked := Value;
  end;

end;

procedure TSwtControls.Set_Name(const Value: WideString);
var
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  elem: TSwtControlObj;
  lst: TPointerList;
Begin
  IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
    lst := ActiveCircuit[ActiveActor].SwtControls;
    S := Value;  // Convert to Pascal String
    Found := FALSE;
    ActiveSave := lst.ActiveIndex;
    elem := lst.First;
    While elem <> NIL Do Begin
      IF (CompareText(elem.Name, S) = 0) THEN Begin
        ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        Found := TRUE;
        Break;
      End;
      elem := lst.Next;
    End;
    IF NOT Found THEN Begin
      DoSimpleMsg('SwtControl "'+S+'" Not Found in Active Circuit.', 5003);
      elem := lst.Get(ActiveSave);    // Restore active Load
      ActiveCircuit[ActiveActor].ActiveCktElement := elem;
    End;
  End;
end;

procedure TSwtControls.Set_SwitchedObj(const Value: WideString);
begin
  Set_Parameter ('SwitchedObj', Value);
end;

procedure TSwtControls.Set_SwitchedTerm(Value: Integer);
begin
  Set_Parameter ('SwitchedTerm', IntToStr (Value));
end;

function TSwtControls.Get_Count: Integer;
begin
     If Assigned(ActiveCircuit[ActiveActor]) Then
             Result := ActiveCircuit[ActiveActor].SwtControls.ListSize;
end;

function TSwtControls.Get_NormalState: OleVariant;
Var
   i :Integer;
   elem:TSwtControlObj;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
       elem := ActiveSwtControl;
       If elem <> Nil Then
       Begin
          Result := VarArrayCreate([0, elem.ControlledElement.NPhases-1], varOleStr);
          For i := 1 to elem.ControlledElement.NPhases Do Begin
             if elem.NormalStates[i] = CTRL_CLOSE then Result[i-1] := 'closed' else Result[i-1] := 'open';
          End;
       End;
     End
     Else
         Result := VarArrayCreate([0, 0], varOleStr);
end;


procedure TSwtControls.Set_NormalState(Value: OleVariant);
Var
   i :Integer;
   Count, Low :Integer;
   elem :TSwtControlObj;
begin

     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
         elem := ActiveSwtControl;
         If elem <> Nil Then
         Begin
            Low := VarArrayLowBound(Value, 1);
            Count := VarArrayHighBound(Value, 1) - Low + 1;
            If Count >  elem.ControlledElement.NPhases Then Count := elem.ControlledElement.NPhases;
            For i := 1 to Count Do Begin
                case LowerCase(Value[i-1 + Low])[1] of
                  'o': elem.NormalStates[i] := CTRL_OPEN;
                  'c': elem.NormalStates[i] := CTRL_CLOSE;
                end;
            End;
         End;

     End;
end;


function TSwtControls.Get_State: OleVariant;
Var
   i :Integer;
   elem:TSwtControlObj;
begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
       elem := ActiveSwtControl;
       If elem <> Nil Then
       Begin
          Result := VarArrayCreate([0, elem.ControlledElement.NPhases-1], varOleStr);
          For i := 1 to elem.ControlledElement.NPhases Do Begin
             if elem.States[i] = CTRL_CLOSE then Result[i-1] := 'closed' else Result[i-1] := 'open';
          End;
       End;
     End
     Else
         Result := VarArrayCreate([0, 0], varOleStr);
end;


procedure TSwtControls.Set_State(Value: OleVariant);
Var
   i :Integer;
   Count, Low :Integer;
   elem :TSwtControlObj;
begin

     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
         elem := ActiveSwtControl;
         If elem <> Nil Then
         Begin
            Low := VarArrayLowBound(Value, 1);
            Count := VarArrayHighBound(Value, 1) - Low + 1;
            If Count >  elem.ControlledElement.NPhases Then Count := elem.ControlledElement.NPhases;
            For i := 1 to Count Do Begin
                case LowerCase(Value[i-1 + Low])[1] of
                  'o': elem.States[i] := CTRL_OPEN;
                  'c': elem.States[i] := CTRL_CLOSE;
                end;
            End;
         End;

     End;
end;


procedure TSwtControls.Reset;
var
  elem: TSwtControlObj;
begin
  elem   := ActiveSwtControl;
  if elem <> nil then begin
      elem.Locked := FALSE;
      elem.Reset(ActiveActor);
  end;
end;

procedure TSwtControls.Open;
Var
  elem: TSwtControlObj;
  i: Integer;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
    for i := 1 to elem.ControlledElement.NPhases do elem.States[i] := CTRL_OPEN // Open all phases
  end;
end;

procedure TSwtControls.Close;
Var
  elem: TSwtControlObj;
  i: Integer;
begin
  elem := ActiveSwtControl;
  if elem <> nil then begin
    for i := 1 to elem.ControlledElement.NPhases do elem.States[i] := CTRL_CLOSE // Close all phases
  end;
end;

function TSwtControls.Get_RatedCurrent: Double;
Var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then Result := elem.RatedCurrent
  else Result := -1.0;
end;

procedure TSwtControls.Set_RatedCurrent(Value: Double);
Var
  elem: TSwtControlObj;
begin
  elem := ActiveSwtControl;
  if elem <> nil then Set_parameter('RatedCurrent', Format('%.8g ',[Value]));
end;

initialization
  TAutoObjectFactory.Create(ComServer, TSwtControls, Class_SwtControls,
    ciInternal, tmApartment);
end.
