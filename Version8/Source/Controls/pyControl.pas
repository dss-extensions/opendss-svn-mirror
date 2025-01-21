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

      LastCMD      : String;
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
      FUNCTION HandlePIPE(ActorID : Integer): Integer;

  end;


VAR
    ActivepyControlObj:TpyControlObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 1;
    PIPE_FORMAT = '\\%s\pipe\%s'; // \\ServerName\pipe\PipeName
    PIPE_TIMEOUT = 5000;
    BUFF_SIZE = 10000;


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
   ParamPointer : Integer;
   ReDirFile,
   ParamName,
   Param        : String;

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
                ReDirFile := ExpandFileName(Param);
                if SysUtils.FileExists(ReDirFile) then
                Begin
                  pyScript := ReDirFile;
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


{--------------------------------------------------------------------------
 Handles the PIPE until the process is done
---------------------------------------------------------------------------}
FUNCTION TpyControlObj.HandlePIPE(ActorID : Integer): Integer;
var
  POnline       : Boolean;
  Written       : Cardinal;
  ClientCmd     : String;

Begin
  POnline := True;
  Result  := 0;
  while (pyServer[ActorID] <> INVALID_HANDLE_VALUE) and (POnline) do
  begin

    ClientCmd := Read_From_PyServer(ActorID);

    if ClientCmd = 'closepipe' then
    Begin
    // This means that the pyscript is done and we need to close the handler
      if LowerCase(LastCMD) = 'yes' then
      Begin
        Result := 1;
      End;
      POnline := False;
    End
    Else
    Begin
      // The py script is sending commands or something different
      LastCMD := ClientCmd;
      if (LowerCase(LastCMD) <> 'yes') and (LowerCase(LastCMD) <> 'no') then
      Begin
        DSSExecutive[ActiveActor].Command :=  ClientCmd;
        //Log('Sending message');
        if GlobalResult = '' then
          GlobalResult := 'OK';

        Write_2_pyServer(GlobalResult, ActorID);
      End
      Else
        Write_2_pyServer('OK', ActorID);

    End;
  end;

End;

{--------------------------------------------------------------------------}
PROCEDURE TpyControlObj.DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer);
begin

  // Do nothing, this is just for coordination with DSS

End;

{--------------------------------------------------------------------------
 Takes a sampe to determine if the control need to perform a control action
---------------------------------------------------------------------------}
PROCEDURE TpyControlObj.Sample(ActorID : Integer);
VAR
  Update      : Boolean;
  LPipeName   : String;
  pHandle     : THandle;

begin

  Update := False; // Default value
 // First, check if the instance's pyServer is running, otherwise do nothing
  if pyServer[ActorID] <> 0 then
  Begin

    Write_2_pyServer(pyScript, ActorID);
    Update := (HandlePIPE(ActorID) = 1);

    {Checks if the controller commands to implement control actions}
    if Update then
    Begin
      // This action is just to sync DSS with the external control action
      With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do
        ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self, ActorID);

    End;
    GlobalResult := '';
  End;
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
