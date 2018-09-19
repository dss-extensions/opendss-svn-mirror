unit DDSSExecutive;

interface

function DSSExecutiveI(mode: Longint; arg: Longint): Longint; CDECL;
function DSSExecutiveS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

implementation

uses
    DSSGlobals,
    ExecCommands,
    ExecOptions,
    Executive,
    sysutils;

function DSSExecutiveI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // DSS_executive.NumCommands
            Result := NumExecCommands;
        end;
        1:
        begin  // DSS_executive.NumOptions
            Result := NumExecOptions;
        end
    else
        Result := -1;
    end;
end;

//****************************String type properties******************************
function DSSExecutiveS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    i: Integer;

begin
    Result := Pansichar(Ansistring('0'));// Default return value
    case mode of
        0:
        begin // DSS_Executive.Command
            i := StrToInt(Widestring(arg));
            Result := Pansichar(Ansistring(ExecCommand[i]));
        end;
        1:
        begin // DSS_Executive.Option
            i := StrToInt(Widestring(arg));
            Result := Pansichar(Ansistring(ExecOption[i]));
        end;
        2:
        begin // DSS_Executive.CommandHelp
            i := StrToInt(Widestring(arg));
            Result := Pansichar(Ansistring(CommandHelp[i]));
        end;
        3:
        begin // DSS_Executive.OptionHelp
            i := StrToInt(Widestring(arg));
            Result := Pansichar(Ansistring(OptionHelp[i]));
        end;
        4:
        begin // DSS_Executive.OptionValue
            i := StrToInt(Widestring(arg));
            DSSExecutive.Command := 'get ' + ExecOption[i];
            Result := Pansichar(Ansistring(GlobalResult));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

end.
