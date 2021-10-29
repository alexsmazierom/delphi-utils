object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1 - ExemploVCL_Deplhi_Utils.d'
  ClientHeight = 324
  ClientWidth = 846
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
    Top = 24
    Width = 177
    Height = 25
    Caption = 'TGenericosUtil.Usar TStringList'
    TabOrder = 0
    OnClick = ButtonUsarStringListClick
  end
  object ButtonUsarFDQuery: TButton
    Left = 16
    Top = 63
    Width = 177
    Height = 25
    Caption = 'TGenericosUtil.Usar TFDQuery'
    TabOrder = 1
    OnClick = ButtonUsarFDQueryClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 305
    Width = 846
    Height = 19
    Panels = <>
    ExplicitWidth = 736
  end
  object ButtonIterar: TButton
    Left = 208
    Top = 24
    Width = 209
    Height = 25
    Caption = 'TMetodosAnonimosUtil.Iterar'
    TabOrder = 3
    OnClick = ButtonIterarClick
  end
  object ButtonIterarReverso: TButton
    Left = 208
    Top = 63
    Width = 209
    Height = 25
    Caption = 'TMetodosAnonimosUtil.IterarReverso'
    TabOrder = 4
    OnClick = ButtonIterarReversoClick
  end
  object GroupBox1: TGroupBox
    Left = 208
    Top = 111
    Width = 364
    Height = 51
    Caption = ' TMetodosAnonimosUtil.Iterar (incrementado 5) '
    TabOrder = 5
    object ProgressBar1: TProgressBar
      Left = 12
      Top = 20
      Width = 290
      Height = 17
      TabOrder = 0
    end
    object ButtonIterarIncremento5: TButton
      Left = 311
      Top = 16
      Width = 43
      Height = 25
      Caption = 'Iniciar'
      TabOrder = 1
      OnClick = ButtonIterarIncremento5Click
    end
  end
  object ButtonStringDeLetras: TButton
    Left = 592
    Top = 24
    Width = 241
    Height = 25
    Caption = 'TExpressoesRegularesUtil.StringDeLetras'
    TabOrder = 6
    OnClick = ButtonStringDeLetrasClick
  end
  object ButtonStringDeDigitos: TButton
    Left = 592
    Top = 63
    Width = 241
    Height = 25
    Caption = 'TExpressoesRegularesUtil.StringDeDigitos'
    TabOrder = 7
    OnClick = ButtonStringDeDigitosClick
  end
  object ButtonRemoverDigitos: TButton
    Left = 592
    Top = 101
    Width = 241
    Height = 25
    Caption = 'TExpressoesRegularesUtil.RemoverDigitos'
    TabOrder = 8
    OnClick = ButtonRemoverDigitosClick
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'LockingMode=Normal'
      'DriverID=SQLite')
    Left = 104
    Top = 104
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 160
    Top = 160
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 208
    Top = 224
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 112
    Top = 232
  end
end
