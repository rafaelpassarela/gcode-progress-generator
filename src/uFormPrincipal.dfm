object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'GCODE Progress Generator'
  ClientHeight = 202
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
  DesignSize = (
    455
    202)
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
    Left = 160
    Top = 64
    Width = 123
    Height = 13
    Caption = 'GCODE Layer Count Tag'
  end
  object LabelStatus: TLabel
    Left = 232
    Top = 131
    Width = 89
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Line Count: x of y'
    Visible = False
    ExplicitTop = 129
  end
  object LabelChangeCount: TLabel
    Left = 303
    Top = 61
    Width = 75
    Height = 13
    Caption = 'Change Count'
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
    Width = 129
    Height = 21
    TabOrder = 2
    Text = ';LAYER:'
  end
  object EditLayerCount: TEdit
    Left = 160
    Top = 83
    Width = 129
    Height = 21
    TabOrder = 3
    Text = ';LAYER_COUNT:'
  end
  object ButtonGenerate: TButton
    Left = 16
    Top = 122
    Width = 129
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'Generate'
    TabOrder = 4
    OnClick = ButtonGenerateClick
    ExplicitTop = 120
  end
  object ProgressBarFileGen: TProgressBar
    Left = 16
    Top = 170
    Width = 416
    Height = 17
    Anchors = [akRight, akBottom]
    TabOrder = 5
    Visible = False
    ExplicitTop = 168
  end
  object SpinEditChangeCount: TSpinEdit
    Left = 303
    Top = 83
    Width = 129
    Height = 22
    Increment = 10
    MaxValue = 2147483647
    MinValue = 50
    TabOrder = 6
    Value = 0
  end
end
