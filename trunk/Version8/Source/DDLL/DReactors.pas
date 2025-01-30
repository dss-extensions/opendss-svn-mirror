unit DReactors;

interface

function ReactorsI(mode:longint;arg:longint):longint;cdecl;
function ReactorsF(mode:longint;arg:double):double;cdecl;
function ReactorsS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure ReactorsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses {$IFNDEF FPC_DLL}ComServ, {$ENDIF} DSSGlobals, Reactor, Variants, Pointerlist, Sysutils;


// Wrapper for concentating all the integer-based IO for the Reactor Obj mimicking COM
function ReactorsI(mode:longint;arg:longint):longint;cdecl;
Var
  Elem          :TReactorObj;
  pList         :TPointerList;

Begin
  Result:=0; // Default return value
  case mode of
    0 : Begin           // Reactors.First
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin

            If ReactorClass[ActiveActor].ElementList.ListSize > 0 Then
            Begin
              pList := ReactorClass[ActiveActor].ElementList;
              Elem := pList.First;
              Repeat
                If Elem.Enabled
                Then Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
                  Result := 1;
                End
                Else Elem := pList.Next;
              Until (Result = 1) or (Elem = nil);
            End
            Else
                Result := 0;  // signify no more
          End;
    End;
    1 : Begin           // Reactors.Next
          Result := 0;  // signify no more
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            If ReactorClass[ActiveActor].ElementList.ListSize > 0 Then
            Begin
              pList := ReactorClass[ActiveActor].ElementList;
              Elem := pList.First;
              Repeat
                If Elem.Enabled
                Then
                Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
                  Result := pList.ActiveIndex;
                End
                Else Elem := pList.Next;
              Until (Result > 0) or (Elem = nil);
            End;

           End;
    End;
    2: Begin            // Reactors.Count
          If Assigned(ActiveCircuit[ActiveActor]) Then
            Result := ReactorClass[ActiveActor].ElementList.ListSize;
    End;
    3: Begin            // Parallel.read
      Elem    := ReactorClass[ActiveActor].GetActiveObj;
      if Elem <> nil then
      Begin
        if Elem.IsParallel then
          Result := 1;
      End;
    End;
    4: Begin            // Parallel.write
      Elem    := ReactorClass[ActiveActor].GetActiveObj;
      if Elem <> nil then
      Begin
          Elem.IsParallel := arg > 0;
      End;
    End
    Else
      Result := -1; // The user is asking for the wrong command
  end;
End;

// Wrapper for concentating all the double-based IO for the Reactor Obj mimicking COM
function ReactorsF(mode:longint;arg:double):double;cdecl;
Var
  Elem   :TReactorObj;

Begin
  Result:=0; // Default return value
  case mode of
    0 : Begin           // Reactor.kV Read
      Elem    := ReactorClass[ActiveActor].GetActiveObj;
      if Elem <> nil then
      Begin
        Result := Elem.kVNominal;
      End;
    End;
    1 : Begin           // Reactor.kV Write
        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Elem.kVNominal := arg;
        End;
    End;
    2 : Begin           // Reactor.kvar Read
      Elem    := ReactorClass[ActiveActor].GetActiveObj;
      if Elem <> nil then
      Begin
        Result := Elem.kvarNominal;
      End;
    End;
    3 : Begin           // Reactor.kvar Write
        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Elem.kvarNominal := arg;
        End;
    End;
    4 : Begin           // Reactor.lmH Read
      Elem    := ReactorClass[ActiveActor].GetActiveObj;
      if Elem <> nil then
      Begin
        Result := Elem.LNominal * 1e3;
      End;
    End;
    5 : Begin           // Reactor.lmH Write
        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Elem.LNominal := arg / 1e3;
        End;
    End;
    6 : Begin           // Reactor.R Read
      Elem    := ReactorClass[ActiveActor].GetActiveObj;
      if Elem <> nil then
      Begin
        Result := Elem.R;
      End;
    End;
    7 : Begin           // Reactor.R Write
        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Elem.R := arg;
        End;
    End;
    8 : Begin           // Reactor.Rp Read
      Elem    := ReactorClass[ActiveActor].GetActiveObj;
      if Elem <> nil then
      Begin
        Result := Elem.Rp;
      End;
    End;
    9 : Begin           // Reactor.Rp Write
        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Elem.Rp := arg;
        End;
    End;
    10: Begin           // Reactor.X Read
      Elem    := ReactorClass[ActiveActor].GetActiveObj;
      if Elem <> nil then
      Begin
        Result := Elem.X;
      End;
    End;
    11: Begin           // Reactor.X Write
        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Elem.X := arg;
        End;
    End
    Else
      Result := -1.0; // We got the wrong command
  end;

End;

// Wrapper for concentating all the string-based IO for the Reactor Obj mimicking COM
function ReactorsS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
Var
  Elem          :TReactorObj;
  k             :Integer;
  pList         :TPointerList;
  activesave    :integer;
  S             :String;
  Found         :Boolean;

Begin
  Result:= pAnsiChar(AnsiString('')); // Default return value
  case mode of
    0 : Begin                   // Reactor.Name Read

        Elem := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Result := pAnsiChar(AnsiString(Elem.Name));
        End;

    End;
    1 : Begin                   // Reactor.Name Write

          IF ActiveCircuit[ActiveActor] <> NIL
          THEN
          Begin      // Search list of Storages in active circuit for name
            If ReactorClass[ActiveActor].ElementList.ListSize > 0 Then
            Begin
              S           := string(arg);  // Convert to Pascal String
              Found       := FALSE;
              pList       := ReactorClass[ActiveActor].ElementList;
              activesave  :=  pList.ActiveIndex;
              Elem        := pList.First;
              While Elem <> NIL Do
              Begin
                IF (CompareText(Elem.Name, S) = 0)
                THEN
                Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
                  Found := TRUE;
                  Break;
                End;
                Elem := pList.Next;
              End;
              IF NOT Found
              THEN
              Begin
                DoSimpleMsg('Reactor "'+S+'" Not Found in Active Circuit.', 20003);
                Elem := pList.Get(activesave);    // Restore active Storage
                ActiveCircuit[ActiveActor].ActiveCktElement := Elem;
              End;
            End;
          End;

    End;
    2 : Begin                   // Reactor.LCurve Read

        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Result := pAnsiChar(AnsiString(Elem.LCurve));
        End;

    End;
    3 : Begin                   // Reactor.LCurve Write

        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Elem.LCurve := string(arg);
        End;

    End;
    4 : Begin                   // Reactor.RCurve Read

        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Result := pAnsiChar(AnsiString(Elem.RCurve));
        End;

    End;
    5 : Begin                   // Reactor.RCurve Write

        Elem    := ReactorClass[ActiveActor].GetActiveObj;
        if Elem <> nil then
        Begin
          Elem.RCurve := string(arg);
        End;

    End;
  end;

End;

// Wrapper for concentating all the array-like IO structures for the Reactor Obj mimicking COM
procedure ReactorsV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;
Var
  Elem          :TReactorObj;
  idx,
  i,
  k             :Integer;
  pList         :TPointerList;
  PDouble       : ^Double;

Begin
  case mode of
    0 : Begin                   // Reactors.AllNames
          myType  :=  4;        // String
          setlength(myStrArray,0);
          IF ActiveCircuit[ActiveActor] <> Nil THEN
          Begin
            WITH ActiveCircuit[ActiveActor] DO
            Begin

              If ReactorClass[ActiveActor].ElementList.ListSize > 0 Then
              Begin
                pList := ReactorClass[ActiveActor].ElementList;

                k:=0;
                Elem := pList.First;
                WHILE Elem<>Nil DO
                Begin
                  WriteStr2Array(Elem.Name);
                  WriteStr2Array(Char(0));
                  Inc(k);
                  Elem := pList.Next;
                End;
              End;

            End;
          End;
          if (length(myStrArray) = 0) then
            WriteStr2Array('None');
          myPointer :=  @(myStrArray[0]);
          mySize    :=  Length(myStrArray);
    End;
    1 : Begin                   // Reactors.RMatrix read
        myType  :=  2;        // Double
        setlength(myDBLArray, 1);
        myDBLArray[0] := 0;
        IF ActiveCircuit[ActiveActor] <> NIL THEN
        Begin
          Elem    := ReactorClass[ActiveActor].GetActiveObj;
          If Elem <> nil THEN
          Begin
              WITH Elem DO
              Begin
                if RMatrix <> nil then
                Begin
                  setlength(myDBLArray, Sqr(Nphases));
                  FOR i := 1 to Sqr(NPhases) DO
                  Begin
                       myDBLArray[i - 1] :=  Rmatrix[i];
                  End;
                End;
              End;
          End;
        End;
        myPointer :=  @(myDBLArray[0]);
        mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    End;
    2 : Begin                   // Reactors.RMatrix write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          begin
            Elem    := ReactorClass[ActiveActor].GetActiveObj;
            If Elem <> nil Then
            Begin
              WITH Elem DO
              Begin
                Reallocmem(Rmatrix,Sizeof(double) *  NPhases *  NPhases);
                FOR i := 1 to Sqr(NPhases) DO
                Begin
                  PDouble  :=  myPointer;
                  Rmatrix[i] := PDouble^;
                  Inc(k);
                  inc(PByte(myPointer),8);
                End;
                YprimInvalid[ActiveActor] := True;
              End;
            End;
          end;
          mySize  :=  k;
    End;
    3 : Begin                   // Reactors.XMatrix read
        myType  :=  2;        // Double
        setlength(myDBLArray, 1);
        myDBLArray[0] := 0;
        IF ActiveCircuit[ActiveActor] <> NIL THEN
        Begin
          Elem    := ReactorClass[ActiveActor].GetActiveObj;
          If Elem <> nil THEN
          Begin
              WITH Elem DO
              Begin
                if XMatrix <> nil then
                Begin
                  setlength(myDBLArray, Sqr(Nphases));
                  FOR i := 1 to Sqr(NPhases) DO
                  Begin
                       myDBLArray[i - 1] :=  Xmatrix[i];
                  End;
                End;
              End;
          End;
        End;
        myPointer :=  @(myDBLArray[0]);
        mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    End;
    4 : Begin                   // Reactors.XMatrix write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          begin
            Elem    := ReactorClass[ActiveActor].GetActiveObj;
            If Elem <> nil Then
            Begin
              WITH Elem DO
              Begin
                Reallocmem(Xmatrix,Sizeof(double) *  NPhases *  NPhases);
                FOR i := 1 to Sqr(NPhases) DO
                Begin
                  PDouble  :=  myPointer;
                  Xmatrix[i] := PDouble^;
                  Inc(k);
                  inc(PByte(myPointer),8);
                End;
                YprimInvalid[ActiveActor] := True;
              End;
            End;
          end;
          mySize  :=  k;
    End;
    5 : Begin                   // Reactors.Z read
        myType  :=  2;        // Double
        setlength(myDBLArray, 1);
        myDBLArray[0] := 0;
        IF ActiveCircuit[ActiveActor] <> NIL THEN
        Begin
          Elem    := ReactorClass[ActiveActor].GetActiveObj;
          If Elem <> nil THEN
          Begin
              WITH Elem DO
              Begin
                setlength(myDBLArray, 2);
                myDBLArray[0] :=  Z.re;
                myDBLArray[1] :=  Z.im;
              End;
          End;
        End;
        myPointer :=  @(myDBLArray[0]);
        mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    End;
    6 : Begin                   // Reactors.Z write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          begin
            Elem    := ReactorClass[ActiveActor].GetActiveObj;
            If Elem <> nil Then
            Begin
              WITH Elem DO
              Begin
                PDouble   :=  myPointer;
                Z.re      := PDouble^;
                inc(PByte(myPointer),8);
                PDouble   :=  myPointer;
                Z.im      := PDouble^;
                YprimInvalid[ActiveActor] := True;
                k :=  2;
              End;
            End;
          end;
          mySize  :=  k;
    End;
    7 : Begin                   // Reactors.Z0 read
        myType  :=  2;        // Double
        setlength(myDBLArray, 1);
        myDBLArray[0] := 0;
        IF ActiveCircuit[ActiveActor] <> NIL THEN
        Begin
          Elem    := ReactorClass[ActiveActor].GetActiveObj;
          If Elem <> nil THEN
          Begin
              WITH Elem DO
              Begin
                setlength(myDBLArray, 2);
                myDBLArray[0] :=  Z0.re;
                myDBLArray[1] :=  Z0.im;
              End;
          End;
        End;
        myPointer :=  @(myDBLArray[0]);
        mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    End;
    8 : Begin                   // Reactors.Z0 write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          begin
            Elem    := ReactorClass[ActiveActor].GetActiveObj;
            If Elem <> nil Then
            Begin
              WITH Elem DO
              Begin
                PDouble   :=  myPointer;
                Z0.re      := PDouble^;
                Inc(k);
                inc(PByte(myPointer),8);
                PDouble   :=  myPointer;
                Z0.im      := PDouble^;
                YprimInvalid[ActiveActor] := True;
                k :=  2;
              End;
            End;
          end;
          mySize  :=  k;
    End;
    9 : Begin                   // Reactors.Z1 read
        myType  :=  2;        // Double
        setlength(myDBLArray, 1);
        myDBLArray[0] := 0;
        IF ActiveCircuit[ActiveActor] <> NIL THEN
        Begin
          Elem    := ReactorClass[ActiveActor].GetActiveObj;
          If Elem <> nil THEN
          Begin
              WITH Elem DO
              Begin
                setlength(myDBLArray, 2);
                myDBLArray[0] :=  Z1.re;
                myDBLArray[1] :=  Z1.im;
              End;
          End;
        End;
        myPointer :=  @(myDBLArray[0]);
        mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    End;
    10: Begin                   // Reactors.Z1 write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          begin
            Elem    := ReactorClass[ActiveActor].GetActiveObj;
            If Elem <> nil Then
            Begin
              WITH Elem DO
              Begin
                PDouble   :=  myPointer;
                Z1.re      := PDouble^;
                inc(PByte(myPointer),8);
                PDouble   :=  myPointer;
                Z1.im      := PDouble^;
                YprimInvalid[ActiveActor] := True;
                k :=  2;
              End;
            End;
          end;
          mySize  :=  k;
    End;
    11: Begin                   // Reactors.Z2 read
        myType  :=  2;        // Double
        setlength(myDBLArray, 1);
        myDBLArray[0] := 0;
        IF ActiveCircuit[ActiveActor] <> NIL THEN
        Begin
          Elem    := ReactorClass[ActiveActor].GetActiveObj;
          If Elem <> nil THEN
          Begin
              WITH Elem DO
              Begin
                setlength(myDBLArray, 2);
                myDBLArray[0] :=  Z2.re;
                myDBLArray[1] :=  Z2.im;
              End;
          End;
        End;
        myPointer :=  @(myDBLArray[0]);
        mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    End;
    12: Begin                   // Reactors.Z2 write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          begin
            Elem    := ReactorClass[ActiveActor].GetActiveObj;
            If Elem <> nil Then
            Begin
              WITH Elem DO
              Begin
                PDouble   :=  myPointer;
                Z2.re      := PDouble^;
                inc(PByte(myPointer),8);
                PDouble   :=  myPointer;
                Z2.im      := PDouble^;
                YprimInvalid[ActiveActor] := True;
                k :=  2;
              End;
            End;
          end;
          mySize  :=  k;
    End
    else
    begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;
  end;

End;

end.
