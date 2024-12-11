unit pyControl;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2025, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  The pyControl is n interface for controllers developped in Python.
  THis structure follows the black box operational principle,
  where it is expected for the Python code to consider some basic
  structure to be compatible with control operations.
}

{$HINTS OFF}
INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     utilities, PointerList, Classes,System.Generics.Collections, Windows, SysUtils;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TpyControl = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const pyControlName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit(ActorID : Integer):Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
  TpyControlObj = class(TControlElem)
    private

      pyScript              : String;

    public
      constructor Create(ParClass:TDSSClass; const pyControlName:String);
      destructor Destroy; override;

      PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model
      PROCEDURE RecalcElementData(ActorID : Integer); Override;
      PROCEDURE CalcYPrim(ActorID : Integer); Override;    // Always Zero for a UPFCControl
      PROCEDURE Sample(ActorID : Integer);  Override;    // Sample control quantities and set action times in Control Queue
      PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
      PROCEDURE Reset(ActorID : Integer); Override;  // Reset to initial defined state
      PROCEDURE GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; // Get present value of terminal Curr
      PROCEDURE GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents
      PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

  end;


VAR
    ActivepyControlObj:TpyControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, uCmatrix, MathUtil, Math, PipeServerInstance, Winapi.ShellAPI;

CONST

    NumPropsThisClass = 1;
    PIPE_FORMAT = '\\%s\pipe\%s'; // \\ServerName\pipe\PipeName
    PIPE_TIMEOUT = 5000;
    BUFSIZE = 10000;


{--------------------------------------------------------------------------}
constructor TpyControl.Create;  // Creates superstructure for all UPFCControl objects
Begin
     Inherited Create;

     Class_name   := 'pyControl';
     DSSClassType := DSSClassType + PY_CONTROLLER;

     DefineProperties;

     CommandList := TCommandList.Create(PropertyName, NumProperties);
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TpyControl.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TpyControl.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName^[1] := 'pyScript';


     PropertyHelp^[1] := 'This is the path to the controller script, it is expected to be a python script (*.py).';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TpyControl.NewObject(const ObjName:String):Integer;
Begin
    // Make a new UPFCControl and add it to UPFCControl class list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TpyControlObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TpyControl.Edit(ActorID : Integer):Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

Begin

  // continue parsing WITH contents of Parser
  ActivepyControlObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActivepyControlObj;

  Result := 0;

  WITH ActivepyControlObj Do Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 364);
            1: Begin
                if SysUtils.FileExists(Param) then
                Begin
                  pyScript := Param;
                End
                Else
                  DoSimpleMsg('The given path "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '" does not exist', 36400);

               End
         ELSE
           // Inherited parameters
           ClassEdit( ActivepyControlObj, ParamPointer - NumPropsthisClass)
         End;
         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     END;

  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TpyControl.MakeLike(const pyControlName:String):Integer;
VAR
   OtherpyControl:TpyControlObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this UPFCControl name in the present collection}
   OtherpyControl := Find(pyControlName);
   IF OtherpyControl<>Nil THEN
   WITH ActivepyControlObj Do Begin

        NPhases := OtherpyControl.Fnphases;
        NConds  := OtherpyControl.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherpyControl.ElementName;
        ControlledElement := OtherpyControl.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherpyControl.MonitoredElement;  // Pointer to target circuit element

        ElementTerminal   := OtherpyControl.ElementTerminal;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherpyControl.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in pyControl MakeLike: "' + pyControlName + '" Not Found.', 370);

End;




{==========================================================================}
{                    TpyControlObj                                           }
{==========================================================================}
{--------------------------------------------------------------------------}
constructor TpyControlObj.Create(ParClass:TDSSClass; const pyControlName:String);

Begin
  Inherited Create(ParClass);

  Name                := LowerCase(pyControlName);
  DSSObjType          := ParClass.DSSClassType;

End;

destructor TpyControlObj.Destroy;
Begin
     ElementName := '';
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TpyControlObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex :Integer;

Begin
{Check for existence of monitored element}

  Devindex := GetCktElementIndex(ElementName); // Global function
  IF   DevIndex>0  THEN
  Begin
    MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
    IF ElementTerminal > MonitoredElement.Nterms THEN
    Begin
      DoErrorMsg('pyControl: "' + Name + '"',
                     'Terminal no. "' +'" does not exist.',
                     'Re-specify terminal no.', 371);
    End
    ELSE
    Begin
     // Sets name of i-th terminal's connected bus in pyControl's buslist
      Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    End;
  End
  ELSE
    DoSimpleMsg('Monitored Element in pyControl.'+Name+ ' does not exist:"'+ElementName+'"', 372);
End;

{--------------------------------------------------------------------------}
procedure TpyControlObj.MakePosSequence(ActorID : Integer);
begin
  if MonitoredElement <> Nil then
  begin
    Nphases := ControlledElement.NPhases;
    Nconds := FNphases;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
  end;
  inherited;
end;

{--------------------------------------------------------------------------}
PROCEDURE TpyControlObj.CalcYPrim(ActorID : Integer);
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;

{--------------------------------------------------------------------------}
PROCEDURE TpyControlObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TpyControlObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TpyControlObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
  Inherited DumpProperties(F,Complete);

  WITH ParentClass Do
    For i := 1 to NumProperties Do
    Begin
      Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
    End;

    If Complete THEN
    Begin
      Writeln(F);
    End;

End;

{--------------------------------------------------------------------------}
PROCEDURE TpyControlObj.DoPendingAction;
VAR
  LPipeName   : String;
  pyargs,
  pyExec      : String;
  ServerH     : TPipeServerInstance;
  pHandle     : THandle;
  Wait4py     : TThreadedQueue<Integer>;
  i           : integer;
  _SEInfo     : TShellExecuteInfo;

begin

    // Queue to wait for the py code to finish
    Wait4py := TThreadedQueue<Integer>.Create(20, 1000, INFINITE);
    // Now launch the py program if exists
    pyExec := pyPath + '\python.exe';

    if SysUtils.FileExists(pyExec) then
    Begin
      LPipeName := Format(PIPE_FORMAT, ['.', 'DSSPipeD_' + IntToStr(ActorID)]);
        // Check whether pipe does exist
      if WaitNamedPipe(PChar(LPipeName), NMPWAIT_WAIT_FOREVER) then // 100 [ms]
        raise Exception.Create('Pipe exists.');
      // Create the pipe
      pHandle := CreateNamedPipe(
        PChar(LPipeName),                                   // Pipe name
        PIPE_ACCESS_DUPLEX,                                 // Read/write access
        PIPE_TYPE_BYTE OR PIPE_READMODE_BYTE OR PIPE_WAIT,  // Message-type pipe; message read mode OR blocking mode //PIPE_NOWAIT
        PIPE_UNLIMITED_INSTANCES,                           // Unlimited instances
        BUFSIZE,                                            // Output buffer size
        BUFSIZE,                                            // Input buffer size
        0,                                                  // Client time-out 50 [ms] default
        nil                                                 // Default security attributes
      );

      pyargs := pyScript + ' d_' + LPipeName;

      // Setup the shell info for executing the py script
      FillChar(_SEInfo, SizeOf(_SEInfo), 0);
      _SEInfo.cbSize := SizeOf(TShellExecuteInfo);
      _SEInfo.lpFile := PChar(pyExec);
      _SEInfo.lpParameters := PChar(pyargs);
      _SEInfo.nShow := SW_HIDE;

      if ShellExecuteEx(@_SEInfo) then
      begin

        Sleep(50);

         // Check if new client is connected
        if not ConnectNamedPipe(pHandle, nil) AND (GetLastError() = ERROR_PIPE_CONNECTED) then
        begin
          ServerH := TPipeServerInstance.Create(1, pHandle,LPipeName, Wait4py);
          i := Wait4py.PopItem();
        end;

      end;
    End;
    CloseHandle(pHandle);
    Wait4py.Destroy();

End;

{--------------------------------------------------------------------------}
PROCEDURE TpyControlObj.Sample(ActorID : Integer);
VAR
  Update      : Boolean;
  LPipeName   : String;
  pyargs,
  pyExec      : String;
  ServerH     : TPipeServerInstance;
  pHandle      : THandle;
  Wait4py     : TThreadedQueue<Integer>;
  _SEInfo     : TShellExecuteInfo;

begin
    Update := False; // Default value
    // Queue to wait for the py code to finish
    Wait4py := TThreadedQueue<Integer>.Create(20, 1000, INFINITE);
    // Now launch the py program if exists
    pyExec := pyPath + '\python.exe';

    if SysUtils.FileExists(pyExec) then
    Begin
      LPipeName := Format(PIPE_FORMAT, ['.', 'DSSPipe_' + IntToStr(ActorID)]);
        // Check whether pipe does exist
      if WaitNamedPipe(PChar(LPipeName), NMPWAIT_WAIT_FOREVER) then // 100 [ms]
        raise Exception.Create('Pipe exists.');
      // Create the pipe
      pHandle := CreateNamedPipe(
        PChar(LPipeName),                                   // Pipe name
        PIPE_ACCESS_DUPLEX,                                 // Read/write access
        PIPE_TYPE_BYTE OR PIPE_READMODE_BYTE OR PIPE_WAIT,  // Message-type pipe; message read mode OR blocking mode //PIPE_NOWAIT
        PIPE_UNLIMITED_INSTANCES,                           // Unlimited instances
        BUFSIZE,                                            // Output buffer size
        BUFSIZE,                                            // Input buffer size
        0,                                                  // Client time-out 50 [ms] default
        nil                                                 // Default security attributes
      );

      pyargs := pyScript + ' s_' + LPipeName;

      // Setup the shell info for executing the py script
      FillChar(_SEInfo, SizeOf(_SEInfo), 0);
      _SEInfo.cbSize := SizeOf(TShellExecuteInfo);
      _SEInfo.lpFile := PChar(pyExec);
      _SEInfo.lpParameters := PChar(pyargs);
      _SEInfo.nShow := SW_HIDE;

      if ShellExecuteEx(@_SEInfo) then
      begin

        Sleep(50);

         // Check if new client is connected
        if not ConnectNamedPipe(pHandle, nil) AND (GetLastError() = ERROR_PIPE_CONNECTED) then
        begin
          ServerH := TPipeServerInstance.Create(1, pHandle, LPipeName, Wait4py);
          Update := (Wait4py.PopItem() > 0);
        end;

      end;

    End;
    CloseHandle(pHandle);
    //Update := False; //---------------------------------------------------------
   {Checks if the controller commands to implement control actions}
    if Update then
    Begin
      With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do
        ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self, ActorID);

    End;
    GlobalResult := '';
    Wait4py.Destroy();
End;

procedure TpyControlObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '""';   //'pyScript';


  inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TpyControlObj.Reset;
begin
  // inherited;

end;

end.
