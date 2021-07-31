object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'GCODE Progress Generator'
  ClientHeight = 200
  ClientWidth = 455
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelFileName: TLabel
    Left = 16
    Top = 8
    Width = 57
    Height = 13
    Caption = 'GCODE file'
  end
  object LabelTag: TLabel
    Left = 16
    Top = 64
    Width = 88
    Height = 13
    Caption = 'GCODE Layer Tag'
  end
  object LabelLayerCount: TLabel
    Left = 232
    Top = 64
    Width = 123
    Height = 13
    Caption = 'GCODE Layer Count Tag'
  end
  object EditFile: TEdit
    Left = 16
    Top = 27
    Width = 377
    Height = 21
    TabOrder = 0
    Text = 'EditFile'
  end
  object ButtonFile: TButton
    Left = 399
    Top = 27
    Width = 33
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = ButtonFileClick
  end
  object EditLayerTag: TEdit
    Left = 16
    Top = 83
    Width = 200
    Height = 21
    TabOrder = 2
    Text = ';LAYER:'
  end
  object EditLayerCount: TEdit
    Left = 232
    Top = 83
    Width = 200
    Height = 21
    TabOrder = 3
    Text = ';LAYER_COUNT:'
  end
  object ButtonGenerate: TButton
    Left = 16
    Top = 120
    Width = 129
    Height = 33
    Caption = 'Generate'
    TabOrder = 4
    OnClick = ButtonGenerateClick
  end
  object ProgressBarFileGen: TProgressBar
    Left = 16
    Top = 168
    Width = 416
    Height = 17
    TabOrder = 5
    Visible = False
  end
end
