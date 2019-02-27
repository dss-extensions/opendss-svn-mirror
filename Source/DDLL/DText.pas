unit DText;

interface

function DSSPut_Command(a: Pansichar): Pansichar; CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    Dialogs,
    SysUtils;

function DSSPut_Command(a: Pansichar): Pansichar; CDECL;
begin
    SolutionAbort := false;  // Reset for commands entered from outside
    DSSExecutive[ActiveActor].Command := Widestring(a);  {Convert to String}
    Result := Pansichar(Ansistring(GlobalResult));
end;

end.
