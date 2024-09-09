unit DText;

interface

function DSSPut_Command(a:PAnsiChar):PAnsiChar;cdecl;

implementation

uses DSSGlobals, Executive, {$IFNDEF FPC_DLL}Dialogs,{$ENDIF} SysUtils, Classes;

function DSSPut_Command(a:PAnsiChar):PAnsiChar;cdecl;
var
  CmdList : TStringList;
  i       : integer;
  DSSReply: String;

begin
  SolutionAbort := FALSE;  // Reset for commands entered from outside

  CmdList := TStringList.Create;
  CmdList.clear;
  CmdList.Delimiter := Char(#10);
  CmdList.StrictDelimiter := True;
  CmdList.DelimitedText := string(a);
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
  Result:= PAnsiChar(AnsiString(DSSReply));
end;

end.
