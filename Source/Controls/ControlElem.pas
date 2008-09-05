unit ControlElem;

interface


USES CktElement, Bus, ucomplex, DSSClass;

TYPE
   TControlElem = class(TCktElement)

   private
       FControlledElement:TCktElement;
       procedure Set_ControlledElement(const Value: TCktElement);  // Pointer to target circuit element

   public

       ElementName:String;
       ElementTerminal:Integer;
       ControlledBusName:String;  // If different than terminal
       ControlledBus: TBus;
       MonitorVariable: String;
       MonitorVarIndex: Integer;
       TimeDelay,
       DblTraceParameter:Double;

       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       PROCEDURE Sample;  Virtual;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code:Integer); Virtual;   // Do the action that is pending from last sample
       PROCEDURE Reset; Virtual;

       Property ControlledElement:TCktElement Read FControlledElement Write Set_ControlledElement;

   end;


implementation

USES
    DSSGlobals, Sysutils;

Constructor TControlElem.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);
    DSSObjType := CTRL_ELEMENT;
    DblTraceParameter := 0.0;
    MonitorVariable := '';
    MonitorVarIndex := 0;
    FControlledElement := Nil;
End;

destructor TControlElem.Destroy;
Begin
   Inherited Destroy;
End;

procedure TControlElem.DoPendingAction;
begin
  // virtual function - should be overridden
  DoSimpleMsg('Programming Error:  Reached base class for DoPendingAction.'+CRLF+'Device: '+DSSClassName+'.'+Name, 460);
end;

procedure TControlElem.Reset;
begin
     DoSimpleMsg('Programming Error: Reached base class for Reset.'+CRLF+'Device: '+DSSClassName+'.'+Name, 461);
end;

procedure TControlElem.Sample;
begin
  // virtual function - should be overridden
  DoSimpleMsg('Programming Error:  Reached base class for Sample.'+CRLF+'Device: '+DSSClassName+'.'+Name, 462);
end;


procedure TControlElem.Set_ControlledElement(const Value: TCktElement);
begin

  Try
  // Check for reassignment
    If Assigned(FControlledElement) Then  FControlledElement.HasControl := FALSE;
  Finally
  FControlledElement := Value;
    If Assigned(FControlledElement) Then With FControlledElement Do Begin
       HasControl := TRUE;
       ControlElement := Self;
    End;
  End;
end;

end.
