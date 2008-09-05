object PropEditForm: TPropEditForm
  Left = 337
  Top = 193
  VertScrollBar.Style = ssHotTrack
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'PropEditForn'
  ClientHeight = 96
  ClientWidth = 205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 200
    Height = 32
    ColCount = 2
    DefaultColWidth = 100
    DefaultRowHeight = 16
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 0
    OnKeyPress = StringGrid1KeyPress
    OnSelectCell = StringGrid1SelectCell
    OnSetEditText = StringGrid1SetEditText
  end
  object Button1: TButton
    Left = 104
    Top = 48
    Width = 81
    Height = 25
    Anchors = [akLeft]
    Caption = '&Update'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 48
    Width = 81
    Height = 25
    Anchors = [akLeft]
    Caption = '&Close'
    TabOrder = 2
    OnClick = Button2Click
  end
end
