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
interface

uses
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    ucomplex,
    utilities,
    PointerList,
    Classes,
    System.Generics.Collections,
    Windows,
    SysUtils;

type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TpyControl = class(TControlClass)
    PRIVATE

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const pyControlName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TpyControlObj = class(TControlElem)
    PRIVATE

        pyScript: String;

    PUBLIC
        constructor Create(ParClass: TDSSClass; const pyControlName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;    // Always Zero for a UPFCControl
        procedure Sample(ActorID: Integer); OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset(ActorID: Integer); OVERRIDE;  // Reset to initial defined state
        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE; // Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;   // Returns Injextion currents
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;


var
    ActivepyControlObj: TpyControlObj;

{--------------------------------------------------------------------------}
implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    uCmatrix,
    MathUtil,
    Math,
    PipeServerInstance,
    Winapi.ShellAPI;

const

    NumPropsThisClass = 1;
    PIPE_FORMAT = '\\%s\pipe\%s'; // \\ServerName\pipe\PipeName
    PIPE_TIMEOUT = 5000;
    BUFSIZE = 10000;


{--------------------------------------------------------------------------}
constructor TpyControl.Create;  // Creates superstructure for all UPFCControl objects
begin
    inherited Create;

    Class_name := 'pyControl';
    DSSClassType := DSSClassType + PY_CONTROLLER;

    DefineProperties;

    CommandList := TCommandList.Create(PropertyName, NumProperties);
    CommandList.Abbrev := true;
end;

{--------------------------------------------------------------------------}
destructor TpyControl.Destroy;

begin
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TpyControl.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names

    PropertyName^[1] := 'pyScript';


    PropertyHelp^[1] := 'This is the path to the controller script, it is expected to be a python script (*.py).';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

{--------------------------------------------------------------------------}
function TpyControl.NewObject(const ObjName: String): Integer;
begin
    // Make a new UPFCControl and add it to UPFCControl class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TpyControlObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

{--------------------------------------------------------------------------}
function TpyControl.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing WITH contents of Parser
    ActivepyControlObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActivepyControlObj;

    Result := 0;

    with ActivepyControlObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 364);
                1:
                begin
                    if SysUtils.FileExists(Param) then
                    begin
                        pyScript := Param;
                    end
                    else
                        DoSimpleMsg('The given path "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '" does not exist', 36400);

                end
            else
           // Inherited parameters
                ClassEdit(ActivepyControlObj, ParamPointer - NumPropsthisClass)
            end;
            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

    end;

end;


{--------------------------------------------------------------------------}
function TpyControl.MakeLike(const pyControlName: String): Integer;
var
    OtherpyControl: TpyControlObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this UPFCControl name in the present collection}
    OtherpyControl := Find(pyControlName);
    if OtherpyControl <> nil then
        with ActivepyControlObj do
        begin

            NPhases := OtherpyControl.Fnphases;
            NConds := OtherpyControl.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherpyControl.ElementName;
            ControlledElement := OtherpyControl.ControlledElement;  // Pointer to target circuit element
            MonitoredElement := OtherpyControl.MonitoredElement;  // Pointer to target circuit element

            ElementTerminal := OtherpyControl.ElementTerminal;


            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherpyControl.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in pyControl MakeLike: "' + pyControlName + '" Not Found.', 370);

end;


{==========================================================================}
{                    TpyControlObj                                           }
{==========================================================================}
{--------------------------------------------------------------------------}
constructor TpyControlObj.Create(ParClass: TDSSClass; const pyControlName: String);

begin
    inherited Create(ParClass);

    Name := LowerCase(pyControlName);
    DSSObjType := ParClass.DSSClassType;

end;

destructor TpyControlObj.Destroy;
begin
    ElementName := '';
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TpyControlObj.RecalcElementData(ActorID: Integer);

var
    DevIndex: Integer;

begin
{Check for existence of monitored element}

    Devindex := GetCktElementIndex(ElementName); // Global function
    if DevIndex > 0 then
    begin
        MonitoredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex);
        if ElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg('pyControl: "' + Name + '"',
                'Terminal no. "' + '" does not exist.',
                'Re-specify terminal no.', 371);
        end
        else
        begin
     // Sets name of i-th terminal's connected bus in pyControl's buslist
            Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        end;
    end
    else
        DoSimpleMsg('Monitored Element in pyControl.' + Name + ' does not exist:"' + ElementName + '"', 372);
end;

{--------------------------------------------------------------------------}
procedure TpyControlObj.MakePosSequence(ActorID: Integer);
begin
    if MonitoredElement <> nil then
    begin
        Nphases := ControlledElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    end;
    inherited;
end;

{--------------------------------------------------------------------------}
procedure TpyControlObj.CalcYPrim(ActorID: Integer);
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

{--------------------------------------------------------------------------}
procedure TpyControlObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin

    for i := 1 to Fnconds do
        Curr^[i] := CZERO;

end;

procedure TpyControlObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TpyControlObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    if Complete then
    begin
        Writeln(F);
    end;

end;

{--------------------------------------------------------------------------}
procedure TpyControlObj.DoPendingAction;
var
    LPipeName: String;
    pyargs,
    pyExec: String;
    ServerH: TPipeServerInstance;
    pHandle: THandle;
    Wait4py: TThreadedQueue<Integer>;
    i: Integer;
    _SEInfo: TShellExecuteInfo;

begin

    // Queue to wait for the py code to finish
    Wait4py := TThreadedQueue<Integer>.Create(20, 1000, INFINITE);
    // Now launch the py program if exists
    pyExec := pyPath + '\python.exe';

    if SysUtils.FileExists(pyExec) then
    begin
        LPipeName := Format(PIPE_FORMAT, ['.', 'DSSPipeD_' + IntToStr(ActorID)]);
        // Check whether pipe does exist
        if WaitNamedPipe(Pchar(LPipeName), NMPWAIT_WAIT_FOREVER) then // 100 [ms]
            raise Exception.Create('Pipe exists.');
      // Create the pipe
        pHandle := CreateNamedPipe(
            Pchar(LPipeName),                                   // Pipe name
            PIPE_ACCESS_DUPLEX,                                 // Read/write access
            PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT,  // Message-type pipe; message read mode OR blocking mode //PIPE_NOWAIT
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
        _SEInfo.lpFile := Pchar(pyExec);
        _SEInfo.lpParameters := Pchar(pyargs);
        _SEInfo.nShow := SW_HIDE;

        if ShellExecuteEx(@_SEInfo) then
        begin

            Sleep(50);

         // Check if new client is connected
            if not ConnectNamedPipe(pHandle, nil) and (GetLastError() = ERROR_PIPE_CONNECTED) then
            begin
                ServerH := TPipeServerInstance.Create(1, pHandle, LPipeName, Wait4py);
                i := Wait4py.PopItem();
            end;

        end;
    end;
    CloseHandle(pHandle);
    Wait4py.Destroy();

end;

{--------------------------------------------------------------------------}
procedure TpyControlObj.Sample(ActorID: Integer);
var
    Update: Boolean;
    LPipeName: String;
    pyargs,
    pyExec: String;
    ServerH: TPipeServerInstance;
    pHandle: THandle;
    Wait4py: TThreadedQueue<Integer>;
    _SEInfo: TShellExecuteInfo;

begin
    Update := false; // Default value
    // Queue to wait for the py code to finish
    Wait4py := TThreadedQueue<Integer>.Create(20, 1000, INFINITE);
    // Now launch the py program if exists
    pyExec := pyPath + '\python.exe';

    if SysUtils.FileExists(pyExec) then
    begin
        LPipeName := Format(PIPE_FORMAT, ['.', 'DSSPipe_' + IntToStr(ActorID)]);
        // Check whether pipe does exist
        if WaitNamedPipe(Pchar(LPipeName), NMPWAIT_WAIT_FOREVER) then // 100 [ms]
            raise Exception.Create('Pipe exists.');
      // Create the pipe
        pHandle := CreateNamedPipe(
            Pchar(LPipeName),                                   // Pipe name
            PIPE_ACCESS_DUPLEX,                                 // Read/write access
            PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT,  // Message-type pipe; message read mode OR blocking mode //PIPE_NOWAIT
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
        _SEInfo.lpFile := Pchar(pyExec);
        _SEInfo.lpParameters := Pchar(pyargs);
        _SEInfo.nShow := SW_HIDE;

        if ShellExecuteEx(@_SEInfo) then
        begin

            Sleep(50);

         // Check if new client is connected
            if not ConnectNamedPipe(pHandle, nil) and (GetLastError() = ERROR_PIPE_CONNECTED) then
            begin
                ServerH := TPipeServerInstance.Create(1, pHandle, LPipeName, Wait4py);
                Update := (Wait4py.PopItem() > 0);
            end;

        end;

    end;
    CloseHandle(pHandle);
    //Update := False; //---------------------------------------------------------
   {Checks if the controller commands to implement control actions}
    if Update then
    begin
        with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
            ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self, ActorID);

    end;
    GlobalResult := '';
    Wait4py.Destroy();
end;

procedure TpyControlObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '""';   //'pyScript';


    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TpyControlObj.Reset;
begin
  // inherited;

end;

end.
