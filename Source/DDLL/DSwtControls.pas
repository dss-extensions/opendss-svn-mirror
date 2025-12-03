unit DSwtControls;

interface

function SwtControlsI(mode: longint; arg: longint): longint; cdecl;
function SwtControlsF(mode: longint; arg: double): double; cdecl;
function SwtControlsS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
procedure SwtControlsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses DSSGlobals, Executive, ControlElem, SwtControl, Variants, SysUtils, PointerList;

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

function SwtControlsI(mode: longint; arg: longint): longint; cdecl;

Var
  elem: TSwtControlObj;
  lst: TPointerList;
  i: Integer;

begin
  Result := 0;      // Default return value
  case mode of
  0: begin  // SwtControls.First
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
  1: begin  // SwtControls.Next
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
  2: begin  // SwtControls.IsLocked read
      Result := 0;
      elem := ActiveSwtControl;
      if elem <> nil then begin
          if elem.IsLocked then Result := 1;
      end;
  end;
  3: begin  // SwtControls.IsLocked write
      If arg = 1 then
        Set_Parameter ('Lock', 'y')
      else
        Set_Parameter ('Lock', 'n');
  end;
  4: begin  // SwtControls.SwitchedTerm read
      Result := 0;
      elem := ActiveSwtControl;
      if elem <> nil then Result := elem.ElementTerminal;
  end;
  5: begin   // SwtControls.SwitchedTerm write
      Set_Parameter ('SwitchedTerm', IntToStr (arg));
  end;
  6: begin  // SwtControls.Count
     If Assigned(ActiveCircuit[ActiveActor]) Then
             Result := ActiveCircuit[ActiveActor].SwtControls.ListSize;
  end;
  7: begin  // SwtControls.Open
    elem := ActiveSwtControl;
    if elem <> nil then begin
      for i := 1 to elem.ControlledElement.NPhases do elem.States[i] := CTRL_OPEN // Open all phases
    end;
  end;
  8: begin  // SwtControls.Close
    elem := ActiveSwtControl;
    if elem <> nil then begin
      for i := 1 to elem.ControlledElement.NPhases do elem.States[i] := CTRL_CLOSE // Close all phases
    end;
  end;
  9: begin  // SwtControls.Reset
      elem   := ActiveSwtControl;
      if elem <> nil then begin
          elem.Locked := FALSE;
          elem.Reset(ActiveActor);
      end;
  end;
  else
      Result:=-1;
  end;
end;

//************************************Floating point type properties****************
function SwtControlsF(mode: longint; arg: double): double; cdecl;

var
  elem: TSwtControlObj;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // SwtControls.Delay read
      Result := 0.0;
      elem := ActiveSwtControl;
      if elem <> nil then Result := elem.TimeDelay;
  end;
  1: begin  // SwtControls.Delay write
      Set_Parameter ('Delay', FloatToStr (arg));
  end;
  2: begin  // SwtControls.RatedCurrent read
    elem := ActiveSwtControl;
    if elem <> nil then Result := elem.RatedCurrent
    else Result := -1.0;
  end;
  3: begin  // SwtControls.RatedCurrent write
    elem := ActiveSwtControl;
    if elem <> nil then Set_parameter('RatedCurrent', Format('%.8g ',[arg]));
  end
  else
      Result:=-1.0;
  end;
end;

//************************************String type properties************************
function SwtControlsS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;

var
  elem: TSwtControlObj;
  ActiveSave : Integer;
  S: String;
  Found :Boolean;
  lst: TPointerList;

begin
   Result := pAnsiChar(AnsiString('')); // Default return value
   case mode of
   0: begin  // SwtControls.Name read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveSwtControl;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.Name));
   end;
   1: begin  // SwtControls.Name write
      IF ActiveCircuit[ActiveActor] <> NIL THEN Begin
        lst := ActiveCircuit[ActiveActor].SwtControls;
        S := string(arg);  // Convert to Pascal String
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
   2: begin  // SwtControl.SwitchedObj read
      Result := pAnsiChar(AnsiString(''));
      elem := ActiveSwtControl;
      if elem <> nil then Result := pAnsiChar(AnsiString(elem.ElementName));
   end;
   3: begin  // SwtControl.SwitchedObj write
      Set_Parameter ('SwitchedObj', string(arg));
   end
   else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
   end;
end;

//******************************Variant type properties*****************************
procedure SwtControlsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

var
  elem: TSwtControlObj;
  lst: TPointerList;
  k,
  i           : Integer;
  S           : String;

begin
  case mode of
  0:begin  // SwtControls.AllNames
      myType  :=  4;        // String
      setlength(myStrArray,0);
      IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
        WITH ActiveCircuit[ActiveActor] DO
        Begin
          If SwtControls.ListSize > 0 Then
          Begin
            lst := SwtControls;
            elem := lst.First;
            WHILE elem<>Nil DO Begin
              WriteStr2Array(elem.Name);
              WriteStr2Array(Char(0));
              elem := lst.Next;
            End;
          End;
        End;
      End;
      if (length(myStrArray) = 0) then
        WriteStr2Array('None');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  1: begin  // SwtControls.State read
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      Elem := ActiveSwtControl;
      If Elem <> nil Then
      Begin
        for i:= 1 to elem.ControlledElement.Nphases DO
        Begin
          if elem.States[i] = CTRL_CLOSE then
            WriteStr2Array('closed')
          else
            WriteStr2Array('open');
          WriteStr2Array(Char(0));
        End;
      End;
    End;
    if (length(myStrArray) = 0) then
      WriteStr2Array('None');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  2: begin  // SwtControls.State write
    myType  :=  4;          // String
    k := 0;
    elem := ActiveSwtControl;
    If elem <> nil Then
    Begin

      for i := 1 to elem.ControlledElement.NPhases do
      Begin
        S := BArray2Str(myPointer, k);
        if S = '' then
          break
        else
        Begin
           case LowerCase(S)[1] of
            'o': elem.States[i] := CTRL_OPEN;
            'c': elem.States[i] := CTRL_CLOSE;
          end;
        End;
      End;
    End;
    mySize  :=  k;
  end;
  3: begin  // SwtControls.NormalState read
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
      Elem := ActiveSwtControl;
      If Elem <> nil Then
      Begin
        for i:= 1 to elem.ControlledElement.Nphases DO Begin
          if elem.NormalStates[i] = CTRL_CLOSE then
            WriteStr2Array('closed')
          else
            WriteStr2Array('open');
          WriteStr2Array(Char(0));
        End;
      End;
    End;
    if (length(myStrArray) = 0) then
      WriteStr2Array('None');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  4: begin  // SwtControls.NormalState write
    elem := ActiveSwtControl;
    k := 0;
    If elem <> nil Then
    Begin
      // allocate space based on number of phases of controlled device
      for i := 1 to elem.ControlledElement.NPhases do
      Begin
        S := BArray2Str(myPointer, k);
        if S = '' then
          break
        else
        Begin
          case LowerCase(S)[1] of
          'o': elem.NormalStates[i] := CTRL_OPEN;
          'c': elem.NormalStates[i] := CTRL_CLOSE;
          end;
        End;
      End;
    End;
    mySize  :=  k;
  end
  else
    Begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    End;
  end;
end;


end.
