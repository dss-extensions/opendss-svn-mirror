unit PropEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DSSObject, StdCtrls;

type
  TPropEditForm = class(TForm)
    StringGrid1: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    Function QuotedIfBlanks(const S:String):String;
  private
    { Private declarations }
    Procedure RefreshPropValues;
    Procedure UpdateSelectedRow;
  public
    { Public declarations }
    ObjectBeingEdited:TDSSObject;
  end;

var
  PropEditForm: TPropEditForm;

implementation

{$R *.DFM}

Uses DSSGlobals, Executive, ParserDel;

Var
   SelectedRow : Integer;
   CellEdited  : Boolean;

procedure TPropEditForm.FormCreate(Sender: TObject);
begin

   StringGrid1.ColWidths[0] := 75;
   StringGrid1.ColWidths[1] := 200;
   StringGrid1.Width := 277;
   Width := StringGrid1.Width + 20;
   StringGrid1.DefaultRowHeight := Abs(StringGrid1.Font.Height) + 4;
   StringGrid1.colcount := 2;

end;

Function Min(A, B:integer):Integer;
Begin
    If A < B Then Result := A
    Else Result := B;
End;

procedure TPropEditForm.FormShow(Sender: TObject);

Var
   i:Integer;

begin

   TRY
     ObjectBeingEdited  := ActiveDSSObject;
     Caption := ObjectBeingEdited.ParentClass.name + '.' + ObjectBeingEdited.Name;
     StringGrid1.rowcount := ObjectBeingEdited.ParentClass.NumProperties + 1;
     StringGrid1.Cells[0, 0] := 'Property';
     StringGrid1.Cells[1, 0] := 'Value';

     StringGrid1.Height := StringGrid1.DefaultRowHeight * (StringGrid1.RowCount+2);
     Height := StringGrid1.Height + Button1.Height + 30;
     Button1.Top := Height - 50;
     Button2.Top := Button1.Top;
     With  ObjectBeingEdited.ParentClass Do
     For i := 1 to NumProperties Do Begin
         StringGrid1.Cells[0, i] := ObjectBeingEdited.ParentClass.PropertyName^[i];
         StringGrid1.Cells[1, i] := QuotedIfBlanks(ObjectBeingEdited.GetPropertyValue(PropertyIdxMap[i]));
     End;

     SelectedRow := 0;
     CellEdited  := FALSE;
   EXCEPT
       On E:Exception Do Begin
            DoSimpleMsg('Error attempting to show editing Form: '+E.Message, 143);
            StringGrid1.RowCount := 2;
        End;
   END;
end;

procedure TPropEditForm.RefreshPropValues;

Var  i:Integer;

begin
     With ObjectBeingEdited.ParentClass Do
     For i := 1 to NumProperties Do Begin
         StringGrid1.Cells[1, i] := QuotedIfBlanks(ObjectBeingEdited.GetPropertyValue(PropertyIdxMap[i]));
     End;
end;


procedure TPropEditForm.StringGrid1SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin

   CellEdited := TRUE;  // Just keep track of the fact that the selected cell has been edited
   SelectedRow := Arow;

end;

procedure TPropEditForm.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin

     IF SelectedRow <> 0 THEN
         IF (SelectedRow <> ARow) and CellEdited THEN UpdateSelectedRow;

     SelectedRow := ARow;
     CanSelect := TRUE;
end;

procedure TPropEditForm.StringGrid1KeyPress(Sender: TObject;
  var Key: Char);
begin
   IF (Ord(Key) = 13) and CellEdited THEN UpdateSelectedRow;   // on CR
end;

procedure TPropEditForm.UpdateSelectedRow;
begin
     Parser.CmdString :=  StringGrid1.Cells[0, SelectedRow] + '=' + StringGrid1.Cells[1, SelectedRow];
     ObjectBeingEdited.Edit;
     RefreshPropValues;
     CellEdited := FALSE;
end;

procedure TPropEditForm.Button1Click(Sender: TObject);
begin
     IF CellEdited THEN UpdateSelectedRow;

end;

procedure TPropEditForm.Button2Click(Sender: TObject);
begin
        Close;
end;

function TPropEditForm.QuotedIfBlanks(const S: String): String;
begin
   Result := S;

   If Pos(' ', S)>0 Then Begin
       If S[1] <> '(' Then  // Ignore if already quoted
        Result := '"'+S+'"';
   End;
end;

end.
