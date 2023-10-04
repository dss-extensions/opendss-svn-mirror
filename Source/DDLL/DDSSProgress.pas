unit DDSSProgress;

interface

function DSSProgressI(mode: Longint; arg: Longint): Longint; CDECL;
function DSSProgressS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

implementation

uses
    {$IFDEF FPC_DLL}
CmdForms
    {$ELSE}
    DSSForms
    {$ENDIF}
    ,
    DSSGlobals;

{$IFDEF FPC_DLL}
function DSSProgressI(mode: longint; arg: longint): longint; cdecl;
begin
  Result:=0; // Default return value
  case mode of
  0: begin // DSSProgress.PctProgress
      If NoFormsAllowed Then Exit;
      InitProgressForm;
  end;
  1: begin // DSSProgress.Show()
     If NoFormsAllowed Then Exit;
        InitProgressForm;
        ProgressFormCaption( ' ');
  end;
  2: begin  // DSSProgress.Close()
      If NoFormsAllowed Then Exit;
      ProgressHide;
  end
  else
      Result:=-1;
  end;
end;

function DSSProgressS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
begin
  Result:=pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin // DSSProgress.Caption
     If NoFormsAllowed Then Exit;
     InitProgressForm;
     ProgressCaption (arg);
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not recognized'));
  end;
end;

    {$ELSE}

function DSSProgressI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0; // Default return value
    case mode of
        0:
        begin // DSSProgress.PctProgress
            if NoFormsAllowed then
                Exit;
            InitProgressForm(ActiveActor);
//      ShowPctProgress (arg,ActiveActor);
        end;
        1:
        begin // DSSProgress.Show()
            if NoFormsAllowed then
                Exit;
            InitProgressForm(ActiveActor);
            ProgressFormCaption(' ', ActiveActor);
//        ShowPctProgress(0,ActiveActor);
        end;
        2:
        begin  // DSSProgress.Close()
            if NoFormsAllowed then
                Exit;
            ProgressHide(ActiveActor);
        end
    else
        Result := -1;
    end;
end;

//******************************String type properties*****************************
function DSSProgressS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
begin
    Result := Pansichar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin // DSSProgress.Caption
            if NoFormsAllowed then
                Exit;
            InitProgressForm(ActiveActor);
            ProgressCaption(arg, ActiveActor);
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not recognized'));
    end;
end;
{$ENDIF}
end.
