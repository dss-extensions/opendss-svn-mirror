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

        LastCMD: String;
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
        function HandlePIPE(ActorID: Integer): Integer;

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
    Math;

const

    NumPropsThisClass = 1;
    PIPE_FORMAT = '\\%s\pipe\%s'; // \\ServerName\pipe\PipeName
    PIPE_TIMEOUT = 5000;
    BUFF_SIZE = 10000;


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
    ReDirFile,
    ParamName,
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
                    ReDirFile := ExpandFileName(Param);
                    if SysUtils.FileExists(ReDirFile) then
                    begin
                        pyScript := ReDirFile;
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

begin
{Do nothing}

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


{--------------------------------------------------------------------------
 Handles the PIPE until the process is done
---------------------------------------------------------------------------}
function TpyControlObj.HandlePIPE(ActorID: Integer): Integer;
var
    POnline: Boolean;
    Written: Cardinal;
    ClientCmd: String;

begin
    POnline := true;
    Result := 0;
    while (pyServer[ActorID] <> INVALID_HANDLE_VALUE) and (POnline) do
    begin

        ClientCmd := Read_From_PyServer(ActorID);

        if ClientCmd = 'closepipe' then
        begin
    // This means that the pyscript is done and we need to close the handler
            if LowerCase(LastCMD) = 'yes' then
            begin
                Result := 1;
            end;
            POnline := false;
        end
        else
        begin
      // The py script is sending commands or something different
            LastCMD := ClientCmd;
            if (LowerCase(LastCMD) <> 'yes') and (LowerCase(LastCMD) <> 'no') then
            begin
                DSSExecutive[ActorID].Command := ClientCmd;
        //Log('Sending message');
                if GlobalResult = '' then
                    GlobalResult := 'OK';

                Write_2_pyServer(GlobalResult, ActorID);
            end
            else
                Write_2_pyServer('OK', ActorID);

        end;
    end;

end;

{--------------------------------------------------------------------------}
procedure TpyControlObj.DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer);
begin

  // Do nothing, this is just for coordination with DSS

end;

{--------------------------------------------------------------------------
 Takes a sampe to determine if the control need to perform a control action
---------------------------------------------------------------------------}
procedure TpyControlObj.Sample(ActorID: Integer);
var
    Update: Boolean;
    pHandle: THandle;

begin

    Update := false; // Default value
 // First, check if the instance's pyServer is running, otherwise do nothing
    if pyServer[ActorID] <> 0 then
    begin

        Write_2_pyServer(pyScript, ActorID);
        Update := (HandlePIPE(ActorID) = 1);

    {Checks if the controller commands to implement control actions}
        if Update then
        begin
      // This action is just to sync DSS with the external control action
            with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
                ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self, ActorID);

        end;
        GlobalResult := '';
    end;
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
