unit Scriptform;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,contnrs, Menus, ToolWin;

type
  TMainEditForm = class(TForm)
    Editor     :TRichEdit;
    PopupMenu2 :TPopupMenu;
    Do2: TMenuItem;
    Save3: TMenuItem;
    CloseWindow1: TMenuItem;
    OpenSelectedFile1: TMenuItem;
    EditSelectedFile1: TMenuItem;
    FontDialog1: TFontDialog;
    ToolBar1: TToolBar;
    FontBtn: TButton;
    procedure EditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditorSelectionChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditorChange(Sender: TObject);
    procedure Do2Click(Sender: TObject);
    procedure Save3Click(Sender: TObject);
    procedure CloseWindow1Click(Sender: TObject);
    procedure OpenSelectedFile1Click(Sender: TObject);
    procedure EditSelectedFile1Click(Sender: TObject);
    procedure FontDialog1Apply(Sender: TObject; Wnd: HWND);
    procedure FontBtnClick(Sender: TObject);

  private
    { Private declarations }
    Procedure SetFormColor;
    function  Get_HasBeenModified: Boolean;
    procedure Set_HasBeenModified(const Value: Boolean);
    Function  TrimParens(const S:String):String;
    Procedure ExtendSelection;
  public
    { Public declarations }
    cmdList: TStringList;
    line1, line2, col: integer;
    HasFileName, isMainWindow:Boolean;
    procedure UpdateCursorPos;
    function  BuildCommandList: Boolean;
    procedure ExecuteCommandList;
    Procedure ExecuteDSSCommand(Const S:String);
    Procedure UpdateResultform;
    Procedure SaveEditorContents;
    Procedure UpdateSummaryForm;

    Property HasBeenModified:Boolean Read Get_HasBeenModified Write Set_HasBeenModified;
   end;

var
  MainEditForm      :TMainEditForm;
  ActiveScriptForm  :TMainEditForm;
  ScriptWindowList  :TObjectList;
  RecordCommands    :Boolean;

implementation

Uses RichEdit, Executive, DSSGlobals, DSSForms,  Panel,Utilities, MessageForm;

{$R *.DFM}

const
     ModifiedColor =  13434879;

procedure TMainEditForm.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{
  if (Key = VK_RETURN) then begin
    if not (ssCtrl in Shift) then begin
      if Editor.SelLength > 0 then begin
        if BuildCommandList then begin  // execute and then replace selection
          ExecuteCommandList;
          Editor.Undo
          end
        end
      else begin  // execute the current line
        if BuildCommandList then begin
          ExecuteCommandList;
        end
      end
    end
  end;
}
end;



procedure TMainEditForm.UpdateCursorPos;
begin
  line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
    Editor.SelStart);
  line2 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0,
    Editor.SelStart + Editor.SelLength);
  col := (Editor.SelStart -
    SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));
end;

procedure TMainEditForm.EditorSelectionChange(Sender: TObject);
begin
   UpdateCursorPos;
end;

function TMainEditForm.BuildCommandList: Boolean;
var
  i: Integer;
  str: string;
begin
  result := False;
  cmdList.Clear;

  for i := line1 to line2 do begin
    str := Trim (Editor.Lines.Strings[i]);
    if Length(str) > 0 then cmdList.Add (str)
  end;
  if cmdList.Count > 0 then result := True;
end;

procedure TMainEditForm.ExecuteCommandList;
var
   i, imax  :Integer;
begin
  SolutionAbort := FALSE;
  imax := cmdList.Count - 1;
  if imax < 0 then Exit;

  Screen.Cursor := crHourglass;
  for i := 0 to imax do begin
    If Not SolutionAbort Then Begin  // If script involves step that gets aborted, just flush the script
      DSSExecutive.Command := ActiveScriptForm.cmdList.Strings[i];
      If LastCommandWasCompile Then ControlPanel.AddCompiledFile(LastFileCompiled);
    End;
  end;
  Screen.Cursor := crDefault;

  UpdateResultForm;
  UpdateSummaryForm;
end;

Procedure TMainEditForm.ExecuteDSSCommand(Const S:String);
Begin
  SolutionAbort := FALSE;
  DSSExecutive.Command := S;
  If RecordCommands then Editor.Lines.Append(S);
 
  UpdateResultForm;
  UpdateSummaryForm;
End;


procedure TMainEditForm.FormActivate(Sender: TObject);
begin
     ActiveScriptForm := Self;
     SetFormColor;
end;

procedure TMainEditForm.FormCreate(Sender: TObject);
begin
   cmdList := TStringList.Create;
   Editor.Clear;
   UpdateCursorPos;
   HasFileName := FALSE;
   IsMainWindow := FALSE;
end;

procedure TMainEditForm.FormDestroy(Sender: TObject);
begin
  cmdList.Free;
end;

procedure TMainEditForm.UpdateResultform;
begin
     ResultForm.Editor.Clear;
     ResultForm.Editor.Lines.Add(GlobalResult);
end;

procedure TMainEditForm.UpdateSummaryForm;
begin

  With SummaryForm Do
  Begin
      Editor.Clear;
      Editor.Lines.BeginUpdate;

      If ActiveCircuit<>nil Then
        With Editor.Lines Do
        Begin
           IF ActiveCircuit.Issolved Then Add('Status = SOLVED')
           Else Begin
             Editor.SelAttributes.Color := clRed;
             Add('Status = NOT Solved');
             Editor.SelAttributes.Color := clBlack;
           End;
           Add('Solution Mode = ' + GetSolutionModeID);
           Add('Number = ' + IntToStr(ActiveCircuit.Solution.NumberofTimes));
           Add('Load Mult = '+ Format('%5.3f', [ActiveCircuit.LoadMultiplier]));
           Add('Devices = '+ Format('%d', [ActiveCircuit.NumDevices]));
           Add('Buses = ' + Format('%d', [ActiveCircuit.NumBuses]));
           Add('Nodes = ' + Format('%d', [ActiveCircuit.NumNodes]));
           Add('Control Mode =' + GetControlModeID);
           Add('Total Iterations = '+IntToStr(ActiveCircuit.Solution.Iteration));
           Add('Control Iterations = '+IntToStr(ActiveCircuit.Solution.ControlIteration));
           Add('Max Sol Iter = ' +IntToStr(ActiveCircuit.Solution.MostIterationsDone ));
           ControlPanel.Caption := 'DSS Main Control Panel: Active Circuit = ' + ActiveCircuit.Name;
        End
      Else
        With Editor.Lines Do Begin
          Add('No Circuits Defined');
        End;

      {Status ...}

      ControlPanel.UpdateStatus;

      Editor.Lines.EndUpdate;
  End;

end;

procedure TMainEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    IF Self <> MainEditForm Then Begin
       If HasBeenModified Then
       Case MessageDlg('File '+Caption+' has changed.  Save ?', mtConfirmation, [mbYes, mbNo], 0) of
            mrYes: SaveEditorContents;
       Else
       End;

       ScriptWindowList.Remove(Self);
       Action := caFree;
    End;
end;

procedure TMainEditForm.SetFormColor;
begin
  If Editor.Modified Then Editor.Color := ModifiedColor Else Editor.Color := clWindow;
end;

procedure TMainEditForm.EditorChange(Sender: TObject);
begin
      If Editor.Color <> ModifiedColor Then  SetFormColor;
end;

function TMainEditForm.Get_HasBeenModified: Boolean;
begin
        Result := Editor.Modified;
end;

procedure TMainEditForm.Set_HasBeenModified(const Value: Boolean);
begin
    Editor.Modified := Value;
    SetFormColor;
end;

procedure TMainEditForm.Do2Click(Sender: TObject);

begin
   if Editor.SelLength > 0 then begin
      if BuildCommandList then begin // execute selection
          ExecuteCommandList;
      end
    end
    else begin // select and execute current line
{      loc.x := x;
      loc.y := y;
      ret := LoWord (SendMessage (Editor.Handle, EM_CHARFROMPOS, 0, Integer (@loc)));
      line1 := SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, ret);
}
      Line1 := Editor.CaretPos.y;
      line2 := line1;
     { col := (ret - SendMessage(Editor.Handle, EM_LINEINDEX, line1, 0));}
      if BuildCommandList then begin
          ExecuteCommandList;
      end;
      {Editor.SelStart := ret}
    end;

end;

Procedure TMainEditForm.SaveEditorContents;
Var
        Save_Cursor:TCursor;
Begin

  Save_Cursor := Screen.Cursor;

  Screen.Cursor := crHourglass;    { Show hourglass cursor }

  Try

   Try
       Editor.PlainText := True;
       Editor.Lines.SaveToFile (Caption);
       Editor.PlainText := False;
       HasBeenModified := FALSE;
       HasFileName := TRUE;

   Except
       On E:Exception Do DoSimpleMsg('Error saving file: '+E.message, 310);
   End;

  Finally
    Screen.Cursor := Save_Cursor;  { Always restore to normal }
  end;
End;

procedure TMainEditForm.Save3Click(Sender: TObject);
begin
        If Not HasFileName Then  ControlPanel.SaveScriptWindow1Click(Sender)
        Else SaveEditorContents;

end;

procedure TMainEditForm.CloseWindow1Click(Sender: TObject);
begin
        Close;
end;

procedure TMainEditForm.OpenSelectedFile1Click(Sender: TObject);
Var
    TempActiveForm:TMainEditForm;
    FileName:String;
begin
        ExtendSelection;
        If Editor.SelLength>0 Then Begin

        Try
              FileName :=  TrimParens(Trim(Editor.SelText));
              If FileExists(FileName)Then Begin
                TempActiveForm := TMainEditForm.Create(nil);
                TempActiveForm.Editor.Lines.LoadFromFile(FileName);
                TempActiveForm.Caption := ExpandFileName(FileName);
                ScriptWindowList.Add(TempActiveForm);
                ActiveScriptForm := TempActiveForm;
                ActiveScriptForm.HasBeenModified := FALSE;
                ActiveScriptForm.HasFileName := TRUE;
              End
              Else DoSimpleMsg('File "'+Editor.SelText+'" not found in currentdirectory: "'+GetCurrentDir+'"', 311);
        Except
            On E:Exception Do DoSimpleMsg('Error opening new window: '+ E.Message, 312);
        End;
        End;
end;

procedure TMainEditForm.EditSelectedFile1Click(Sender: TObject);
Var
    FileName:String;
begin
        ExtendSelection;

        If Editor.SelLength>0 Then Begin

        Try
              FileName :=  TrimParens(Trim(Editor.SelText));
              If FileExists(FileName)Then  FireOffEditor(FileName)
              Else DoSimpleMsg('File "' + FileName + '" not found in currentdirectory: "'+GetCurrentDir+'"', 313);
        Except
            On E:Exception Do DoSimpleMsg('Error opening Editor: '+ E.Message, 314);
        End;
        End;

end;

function TMainEditForm.TrimParens(const S: String): String;
begin
{Get rid of leading and trailing Parens}
        Result := S;
        Case Result[1] of
          '(': Begin
                Result := Copy(Result, 2, Length(Result)-1);
                If Result[Length(Result)]=')' Then SetLength(Result, Length(Result)-1);
               End;
          '"': Begin
                Result := Copy(Result, 2, Length(Result)-1);
                If Result[Length(Result)]='"' Then SetLength(Result, Length(Result)-1);
               End;
          '''':Begin
                Result := Copy(Result, 2, Length(Result)-1);
                If Result[Length(Result)]='''' Then SetLength(Result, Length(Result)-1);
               End;
          '[': Begin
                Result := Copy(Result, 2, Length(Result)-1);
                If Result[Length(Result)]=']' Then SetLength(Result, Length(Result)-1);
               End;
          '{': Begin
                Result := Copy(Result, 2, Length(Result)-1);
                If Result[Length(Result)]='}' Then SetLength(Result, Length(Result)-1);
               End;
        Else
           {Nada}
        End;

end;

procedure TMainEditForm.ExtendSelection;
Var
    i, LineIdx, Slen:Integer;
    Pos:TPoint;
begin
        If Editor.SelLength=0 Then Begin
          Pos := Editor.CaretPos ;
          LineIdx := Pos.y; // SendMessage(Editor.Handle, EM_EXLINEFROMCHAR, 0, Editor.SelStart);
           // Backup from cursor position until blank found or BOL
           i := Pos.x;
           While (i>0) and Not
             ((Editor.Lines.Strings[LineIdx][i] = ' ') or (Editor.Lines.Strings[LineIdx][i] = '=') )
           Do Begin
              Dec(i);
              Editor.SelStart := Editor.SelStart-1 ;
           End;

           // Now go forward until a blank or EOL
           inc(i);
           slen := Length(Editor.Lines.Strings[LineIdx]);
           While (i <= slen) and (Editor.Lines.Strings[LineIdx][i] <> ' ') Do Begin
            inc(i);
            Editor.SelLength := Editor.SelLength + 1;
           End;

        End;

end;



procedure TMainEditForm.FontDialog1Apply(Sender: TObject; Wnd: HWND);
begin

   Editor.SelAttributes.Assign(TFontDialog(Sender).Font)

end;

procedure TMainEditForm.FontBtnClick(Sender: TObject);
begin
        With FontDialog1 do  Begin
           Options := Options + [fdApplyButton];
           Execute;
        End;
end;

end.
