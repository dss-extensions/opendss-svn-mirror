unit DSSForms;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Panel, Classes;

VAR

   ControlPanelCreated     :Boolean;  // signify whether this is the DLL or EXE
   ControlPanel: TControlPanel;

   RebuildHelpForm:Boolean;


   PROCEDURE CreateControlPanel;
   PROCEDURE ExitControlPanel;
   PROCEDURE InitProgressForm;
   Procedure ProgressCaption(const S:String);
   Procedure ProgressFormCaption(const S:String);
   Procedure ProgressHide;
   PROCEDURE ShowControlPanel;
   PROCEDURE ShowHelpForm ;
   PROCEDURE ShowAboutBox;
   PROCEDURE ShowPropEditForm;
   PROCEDURE ShowPctProgress(Count:Integer);
   Procedure ShowMessageForm(S:TStrings);
   FUNCTION  DSSMessageDlg(const Msg:String;err:boolean):Integer;
   PROCEDURE DSSInfoMessageDlg(const Msg:String);
   FUNCTION  GetDSSExeFile: String;
   PROCEDURE CloseDownForms;
   Procedure ShowTreeView(Const Fname:String);

implementation

Uses      ExecCommands, ExecOptions,
          Windows, Forms, Controls, Dialogs, DSSGlobals,Executive, DSSClass,ParserDel,
          ProgressForm,
          Helpform,
          PropEdit,
          About,
          MessageForm,
          ComCtrls,
          TViewer,
          Sysutils, Registry;


Procedure InitProgressForm;

Begin
    // Start up progressform if not already started.
     If (not NoFormsAllowed) and (Progress=Nil) Then
         Progress := TProgress.Create(Nil);
End;

PROCEDURE ShowPctProgress(Count:Integer);

Begin
     Progress.PctProgress := Count;
     Application.ProcessMessages;
End;

Procedure ProgressCaption(const S:String);

Begin
     If NoFormsAllowed Then Exit;
     Progress.Caption := S;
     Progress.Show;
End;

Procedure ProgressFormCaption(const S:String);

Begin
     If NoFormsAllowed Then Exit;
     Progress.FormCaption.Caption := S;
     Progress.Show;
End;

Procedure ProgressHide;
Begin
     If Not NoFormsAllowed and (Progress <> Nil ) Then Progress.Hide;
End;

Procedure ShowAboutBox;

Begin

 WITH TAboutBox.Create(nil) Do
 Try
     ShowModal;
     GlobalResult := VersionString;
 Finally
      Free;
 End;

End;

Procedure ShowTreeView(Const Fname:String);
Begin
  TViewForm.Left:=0;
  TViewForm.Top := 0;
  TViewForm.TreeView1.Items.Clear;
  TViewForm.ShowFile(Fname);
  TViewForm.Show;
  TViewForm.SetFocus;
end;

FUNCTION GetDSSExeFile: String;

Var
   TheFileName:Array[0..MAX_PATH] of char;

  // Registry: TRegistry;
  // ClassID:String;

Begin

    FillChar(TheFileName, SizeOF(TheFileName), #0);  // Fill it with nulls
    GetModuleFileName(HInstance, TheFileName, SizeOF(TheFileName));
    Result := TheFileName;

    If IsLibrary then IsDLL := TRUE;

(*  OLD WAY

    Result := Application.ExeName;
    If CompareText(ExtractFileName(Result), 'dss.exe') <> 0  Then Begin
        // Find the DLL   file instead
        Registry:=TRegistry.Create;
        Registry.RootKey:=HKEY_CLASSES_ROOT;
        {False because we do not want to create it if it doesn�t exist}
        Registry.OpenKey('\DSSLIB.DSS\CLSID',False);
        ClassID :=Registry.ReadString('');
        Registry.CloseKey;

        Registry.RootKey:=HKEY_LOCAL_MACHINE;
        Registry.OpenKey('\SOFTWARE\Classes\CLSID\'+ClassID +'\InprocServer32', False);
        Result :=Registry.ReadString('');
        Registry.CloseKey;
        Registry.Free;
        IsDLL := TRUE;
    End;
 *)
End;


FUNCTION DSSMessageDlg(const Msg:String;err:boolean):Integer;
       Function IntResult(R:Integer):Integer;
       Begin
           If R = mrAbort then IntResult := -1
           Else IntResult := 0;
       End;
Begin
     If Err Then Result := MessageDlg(Msg, mtError , [mbOK], 0)
     Else Result := IntResult(MessageDlg(Msg, mtInformation , [mbAbort, mbIgnore], 0));
End;

Procedure DSSInfoMessageDlg(const Msg:String);
Begin
    MessageDlg(Msg, mtInformation , [mbOK], 0)
End;




PROCEDURE CreateControlPanel;

Begin
     If NoFormsAllowed then Exit;
     ControlPanel := TControlPanel.Create(Nil);
     ControlPanelCreated := True;
     ControlPanel.InitializeForm;
End;

PROCEDURE ExitControlPanel;

Begin
     If NoFormsAllowed then Exit;
     ControlPanel.Exit1Click(nil);
End;

PROCEDURE ShowControlPanel;

Begin
    If NoFormsAllowed then Exit;
    If Not ControlPanelCreated Then CreateControlPanel;
    ControlPanel.Show;
End;

PROCEDURE ShowHelpForm;

VAR
   Param,ParamName:String;


Begin
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;

     If NoFormsAllowed Then Exit;

     // build tree view WITH nodelist containing data pointing to help strings
     // so that when you click on a node, the help string will come up.

     IF HelpFormObj <> Nil THEN   // It's already created.  Let's not do another
     Begin
          If RebuildHelpForm then HelpFormObj.BuildTreeViewList;
          RebuildHelpForm := FALSE;
          HelpFormObj.Show;
          Exit;
     End;

     IF Length(param)=0 THEN
     Begin
         // Executive help
         HelpFormObj := THelpForm1.Create(Nil);
         HelpFormObj.BuildTreeViewList;
         HelpFormObj.Show;
     End;
End;

Procedure ShowMessageForm(S:TStrings);

Begin
          If NoFormsAllowed Then Exit;
          MessageForm1.Editor.Clear;
          MessageForm1.Editor.Lines := S;
          MessageForm1.WindowState := wsNormal;
          MessageForm1.Show;
End;

Procedure ShowPropEditForm;

Begin
        If NoFormsAllowed Then Exit;
       // Create Edit form on the fly
         PropEditForm := TPropEditForm.Create(NIL);
         PropEditForm.ShowModal;
         PropEditForm.Free;
         PropEditForm := Nil;
End;

Procedure CloseDownForms;

Begin

         If Progress <> Nil Then Begin
            Progress.Free;
            Progress := Nil;
         End;
         
         If HelpFormObj<> Nil Then Begin
             HelpFormObj.Free;
             HelpFormObj := Nil;
         End;
         If ControlPanelCreated Then Begin
             ControlPanel.Free;
             ControlPanelCreated := False;
         End;
End;



initialization

  HelpFormObj := NIL;
  Progress := Nil;   // Created in Solution and ImplDSSProgress
  ControlPanelCreated := FALSE;
  PropEditForm := NIL;
  RebuildHelpForm := True;

finalization

  If PropEditForm <> NIL Then PropEditForm.Free;
  If HelpFormObj <> NIL THEN HelpFormObj.Free;
  If IsDLL Then
  Begin
    If Assigned(Progress) Then Progress.Free;
    If (ControlPanelCreated) THEN ControlPanel.Free;
    If Assigned(TViewForm) Then TViewForm.Free;
    If Assigned(MessageForm1) Then MessageForm1.Free;
  End;

end.
