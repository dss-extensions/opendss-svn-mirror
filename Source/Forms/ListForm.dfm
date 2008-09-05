object ListBoxForm: TListBoxForm
  Left = 363
  Top = 276
  BorderStyle = bsDialog
  Caption = 'Standard DSS List Form'
  ClientHeight = 232
  ClientWidth = 206
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 16
    Top = 8
    Width = 177
    Height = 169
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object OKBtn: TButton
    Left = 16
    Top = 192
    Width = 57
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 128
    Top = 192
    Width = 65
    Height = 25
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelBtnClick
  end
end
