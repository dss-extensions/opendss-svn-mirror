unit MeterElement;

Interface

USES CktElement, Bus, ucomplex, DSSClass;

TYPE
   TMeterElement = class(TCktElement)

   public

       ElementName:String;
       MeteredElement:TCktElement;  // Pointer to target circuit element
       MeteredTerminal:Integer;

       constructor Create(ParClass:TDSSClass);
       destructor Destroy; override;

       PROCEDURE TakeSample;  Virtual;    // Sample control quantities and set action times in Control Queue

   end;


implementation

USES
    DSSGlobals, Sysutils;


Constructor TMeterElement.Create(ParClass:TDSSClass);
Begin
    Inherited Create(ParClass);
    DSSObjType := METER_ELEMENT;

    ElementName := '';
    MeteredElement := nil;
    MeteredTerminal := 1;
End;

destructor TMeterElement.Destroy;
Begin
   Inherited Destroy;
End;


procedure TMeterElement.TakeSample;
begin
  // virtual function - should be overridden
  DoSimpleMsg('Programming Error:  Reached base Meterelement class for TakeSample.'+CRLF+'Device: '+Name, 723);
end;


end.
