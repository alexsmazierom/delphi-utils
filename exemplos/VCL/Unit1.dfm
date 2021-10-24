object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1 - ExemploVCL_Deplhi_Utils.d'
  ClientHeight = 324
  ClientWidth = 580
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonUsarStringList: TButton
    Left = 16
    Top = 16
    Width = 137
    Height = 25
    Caption = 'ButtonUsarStringList'
    TabOrder = 0
    OnClick = ButtonUsarStringListClick
  end
  object ButtonUsarFDQuery: TButton
    Left = 16
    Top = 55
    Width = 137
    Height = 25
    Caption = 'ButtonUsarFDQuery'
    TabOrder = 1
    OnClick = ButtonUsarFDQueryClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 305
    Width = 580
    Height = 19
    Panels = <>
    ExplicitLeft = 448
    ExplicitTop = 224
    ExplicitWidth = 0
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'LockingMode=Normal'
      'DriverID=SQLite')
    Left = 280
    Top = 40
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 336
    Top = 96
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 384
    Top = 160
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 288
    Top = 168
  end
end
