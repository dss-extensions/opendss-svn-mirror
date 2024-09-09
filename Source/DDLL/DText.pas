unit DText;

interface

function DSSPut_Command(a: Pansichar): Pansichar; CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    {$IFNDEF FPC_DLL}
    Dialogs,
    {$ENDIF}
    SysUtils,
    Classes;

function DSSPut_Command(a: Pansichar): Pansichar; CDECL;
var
    CmdList: TStringList;
    i: Integer;
    DSSReply: String;

begin
    SolutionAbort := false;  // Reset for commands entered from outside

    CmdList := TStringList.Create;
    CmdList.clear;
    CmdList.Delimiter := Char(#10);
    CmdList.StrictDelimiter := true;
    CmdList.DelimitedText := String(a);
    DSSReply := '';

    for i := 0 to (CmdList.Count - 1) do
    begin
        DSSExecutive[ActiveActor].Command := CmdList[i];  {Convert to String}
        if not GlobalResult.IsEmpty then
            DSSReply := DSSReply + GlobalResult + Char(#10);
        if ErrorNumber > 0 then
            break;
    end;
    if not DSSReply.IsEmpty then
        DSSReply := Copy(DSSReply, 0, length(DSSReply) - 1);
    CmdList.Free;
    Result := Pansichar(Ansistring(DSSReply));
end;

end.
