unit DSSForms;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Panel,
    Classes;

var

    ControlPanelCreated: Boolean;  // signify whether this is the DLL or EXE
    ControlPanel: TControlPanel;

    RebuildHelpForm: Boolean;


procedure CreateControlPanel;
procedure ExitControlPanel;
procedure InitProgressForm(Actor_ID: Integer);
procedure ProgressCaption(const S: String; Actor_ID: Integer);
procedure ProgressFormCaption(const S: String; Actor_ID: Integer);
procedure ProgressHide(Actor_ID: Integer);
procedure ShowControlPanel;
procedure ShowHelpForm;
procedure ShowAboutBox;
procedure ShowPropEditForm;
procedure ShowPctProgress(Count: Integer; Actor_ID: Integer);
procedure ShowMessageForm(S: TStrings);
function DSSMessageDlg(const Msg: String; err: Boolean): Integer;
procedure DSSInfoMessageDlg(const Msg: String);
function GetDSSExeFile: String;
procedure CloseDownForms;
procedure ShowTreeView(const Fname: String);
function MakeChannelSelection(NumFieldsToSkip: Integer; const Filename: String): Boolean;


implementation

uses
    ExecCommands,
    ExecOptions,
    Windows,
    Forms,
    Controls,
    Dialogs,
    DSSGlobals,
    Executive,
    DSSClass,
    ParserDel,
    ProgressForm,
    Helpform,
    PropEdit,
    About,
    Diakoptics,
//          MessageForm,
    ComCtrls,
    TViewer,
    Sysutils,
    FrmCSVchannelSelect,
    System.UITypes,
    ScriptEdit;

procedure InitProgressForm(Actor_ID: Integer);

begin
    // Start up progressform if not already started.
    if (not NoFormsAllowed) and (ActorProgress[Actor_ID] = nil) then
        ActorProgress[Actor_ID] := TProgress.Create(nil);
end;

procedure ShowPctProgress(Count: Integer; Actor_ID: Integer);

begin
    if NoFormsAllowed then
        Exit;      // added RCD 12-5-2010
    ActorProgress[Actor_ID].PctProgress := Count;
    Application.ProcessMessages;
end;

procedure ProgressCaption(const S: String; Actor_ID: Integer);

begin
    if NoFormsAllowed then
        Exit;
    ActorProgress[Actor_ID].Caption := S;
    ActorProgress[Actor_ID].Show;
end;

procedure ProgressFormCaption(const S: String; Actor_ID: Integer);

begin
    if NoFormsAllowed then
        Exit;
    ActorProgress[Actor_ID].FormCaption.Caption := S;
    ActorProgress[Actor_ID].Show;
end;

procedure ProgressHide(Actor_ID: Integer);
begin
    if not NoFormsAllowed and (ActorProgress[Actor_ID] <> nil) then
    begin
        ActorProgress[Actor_ID].Free;
        ActorProgress[Actor_ID] := nil;
    end;
end;

procedure ShowAboutBox;

begin
    if NoFormsAllowed then
        Exit;
    with TAboutBox.Create(nil) do
    try
        ShowModal;
        GlobalResult := VersionString;
    finally
        Free;
    end;

end;


procedure ShowTreeView(const Fname: String);
begin
    if NoFormsAllowed then
        Exit;

    if not Assigned(TViewForm) then
        TViewForm := TTViewForm.Create(nil);

    TViewForm.Left := 0;
    TViewForm.Top := 0;
    TViewForm.TreeView1.Items.Clear;
    TViewForm.ShowFile(Fname);
    TViewForm.Show;
    TViewForm.SetFocus;
end;

function GetDSSExeFile: String;

var
    TheFileName: array[0..MAX_PATH] of Char;

begin

    FillChar(TheFileName, SizeOF(TheFileName), #0);  // Fill it with nulls
    GetModuleFileName(HInstance, TheFileName, SizeOF(TheFileName));
    Result := TheFileName;

    if IsLibrary then
        IsDLL := true;
end;


function DSSMessageDlg(const Msg: String; err: Boolean): Integer;

var
    ScriptEd: TScriptEdit;
    Str: String;

    function IntResult(R: Integer): Integer;
    begin
        if R = mrAbort then
            IntResult := -1
        else
            IntResult := 0;
    end;

begin
    if Length(msg) > 1024 then
        Str := 'Message too long; See Result Form.'
    else
        Str := msg;
    if isDLL then
    begin
        if Err then
            Result := MessageDlg(Str, mtError, [mbOK], 0)
        else
            Result := IntResult(MessageDlg(Str, mtInformation, [mbAbort, mbIgnore], 0))
    end;
//     else
//      ScriptEd.PublishMessage(Str);
    Result := -1;
end;

procedure DSSInfoMessageDlg(const Msg: String);
var
    ScriptEd: TScriptEdit;
    Str: String;
begin

    if length(msg) <= 1024 then
        Str := Msg
    else
        Str := 'Message too long; See Result Form.';
    if isDLL then
    begin
        if length(msg) <= 1024 then
            MessageDlg(Msg, mtInformation, [mbOK], 0)
        else
            MessageDlg('Message too long; See Result Form.', mtInformation, [mbOK], 0);
    end
//     else
//    ScriptEd.PublishMessage(Str);
end;


procedure CreateControlPanel;

begin
    if NoFormsAllowed or isDLL then
        Exit;
    ControlPanel := TControlPanel.Create(nil);
    ControlPanelCreated := true;
    ControlPanel.InitializeForm;
end;

procedure ExitControlPanel;

begin
    if NoFormsAllowed or IsDLL then
        Exit;
    ControlPanel.Exit1Click(nil);
end;

procedure ShowControlPanel;

begin
    if NoFormsAllowed or IsDLL then
        Exit;
    if not ControlPanelCreated then
        CreateControlPanel;
    ControlPanel.Show;
end;

procedure ShowHelpForm;

var
    Param, ParamName: String;


begin
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    if NoFormsAllowed then
        Exit;

     // build tree view WITH nodelist containing data pointing to help strings
     // so that when you click on a node, the help string will come up.

    if HelpFormObj <> nil then   // It's already created.  Let's not do another
    begin
        if RebuildHelpForm then
            HelpFormObj.BuildTreeViewList;
        RebuildHelpForm := false;
        HelpFormObj.Show;
        Exit;
    end;

    if Length(param) = 0 then
    begin
         // Executive help
        HelpFormObj := THelpForm1.Create(nil);
        HelpFormObj.BuildTreeViewList;
        HelpFormObj.Show;
    end;
end;

procedure ShowMessageForm(S: TStrings);

begin
    if NoFormsAllowed then
        Exit;
//          If Not Assigned (MessageForm1) Then MessageForm1 := TMessageForm1.Create(Nil);
//          MessageForm1.Editor.Clear;
//          MessageForm1.Editor.Lines := S;
//          MessageForm1.WindowState := wsNormal;
//          MessageForm1.Show;
    ControlPanel.ResultsEdit.Clear;
    ControlPanel.ResultsEdit.Lines := s;
end;

procedure ShowPropEditForm;

begin
    if NoFormsAllowed then
        Exit;
       // Create Edit form on the fly
    PropEditForm := TPropEditForm.Create(nil);
    PropEditForm.ShowModal;
    PropEditForm.Free;
    PropEditForm := nil;
end;

procedure CloseDownForms;
var
    I: Integer;
begin
    for I := 1 to CPU_Cores do
    begin
        if ActorProgress[I] <> nil then
        begin
            ActorProgress[I].Free;
            ActorProgress[I] := nil;
        end;
    end;

    if HelpFormObj <> nil then
    begin
        HelpFormObj.Free;
        HelpFormObj := nil;
    end;
    if ControlPanelCreated then
    begin
        ControlPanel.Free;
        ControlPanelCreated := false;
    end;
end;

//----------------------------------------------------------------------------
function MakeChannelSelection(NumFieldsToSkip: Integer; const Filename: String): Boolean;
var
    F: TextFile;
    S: String;
    iCounter: Integer;
    i: Integer;
    SaveWhiteSpaceChars: String;

begin
    AssignFile(F, FileName);
    Reset(F);
    Readln(F, S);  // Read first line in file
    CloseFile(F);

    SaveWhiteSpaceChars := AuxParser.Whitespace;
    AuxParser.Whitespace := #9;
    AuxParser.CmdString := S;  // Load up Parser
   // Skip specified number of columns in CSV file
    for i := 1 to NumFieldsToSkip do
        Auxparser.NextParam;
    with ChannelSelectForm.ListBox1 do
    begin
        Clear;
        iCounter := 0;
        repeat
            Auxparser.NextParam;
            S := Auxparser.StrValue;
            if Length(S) > 0 then
            begin
                iCounter := iCounter + 1;
                AddItem(Format('%d. %s', [iCounter, S]), nil);
            end;
        until Length(S) = 0;
    end;
    if ChannelSelectForm.ShowModal = mrOK then
        Result := true
    else
        Result := false;
    AuxParser.Whitespace := SaveWhiteSpaceChars;
end;


initialization

    HelpFormObj := nil;
    Progress := nil;   // Created in Solution and ImplDSSProgress
    ControlPanelCreated := false;
    PropEditForm := nil;
    RebuildHelpForm := true;
    IsMultiThread := true;

finalization

    if PropEditForm <> nil then
        PropEditForm.Free;
    if HelpFormObj <> nil then
        HelpFormObj.Free;
    if IsDLL then
    begin
        if Assigned(Progress) then
            Progress.Free;
        if (ControlPanelCreated) then
            ControlPanel.Free;
        if Assigned(TViewForm) then
            TViewForm.Free;
//    If Assigned(MessageForm1) Then MessageForm1.Free;
    end;

end.
