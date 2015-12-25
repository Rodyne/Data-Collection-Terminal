object Mainform: TMainform
  Left = 489
  Top = 116
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Bulls-i Terminal Key Programmer'
  ClientHeight = 177
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblWriteData: TLabel
    Left = 12
    Top = 57
    Width = 30
    Height = 13
    Caption = 'SSID'
  end
  object lblCardUID: TLabel
    Left = 12
    Top = 19
    Width = 54
    Height = 13
    Caption = 'Card UID'
  end
  object Label1: TLabel
    Left = 12
    Top = 83
    Width = 54
    Height = 13
    Caption = 'Password'
  end
  object Label2: TLabel
    Left = 12
    Top = 109
    Width = 55
    Height = 13
    Caption = 'Broker IP'
  end
  object Label3: TLabel
    Left = 240
    Top = 57
    Width = 17
    Height = 13
    Caption = '[1]'
  end
  object Label4: TLabel
    Left = 240
    Top = 84
    Width = 17
    Height = 13
    Caption = '[2]'
  end
  object Label5: TLabel
    Left = 240
    Top = 108
    Width = 17
    Height = 13
    Caption = '[4]'
  end
  object Label6: TLabel
    Left = 239
    Top = 19
    Width = 17
    Height = 13
    Caption = '[0]'
  end
  object WriteCard: TButton
    Left = 92
    Top = 142
    Width = 125
    Height = 20
    Cursor = crHandPoint
    Caption = 'WRITE CARD'
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = WriteCardClick
  end
  object CardUID: TEdit
    Left = 92
    Top = 16
    Width = 141
    Height = 20
    BevelOuter = bvRaised
    Color = clSilver
    Ctl3D = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    Text = ' - WAIT -'
  end
  object ssid: TEdit
    Left = 92
    Top = 54
    Width = 141
    Height = 20
    BevelOuter = bvRaised
    Ctl3D = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    MaxLength = 15
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    Text = 'Indevin'
  end
  object password: TEdit
    Left = 92
    Top = 80
    Width = 141
    Height = 20
    BevelOuter = bvRaised
    Ctl3D = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    MaxLength = 15
    ParentCtl3D = False
    ParentFont = False
    PasswordChar = '#'
    ReadOnly = True
    TabOrder = 3
    Text = 'L3t M3 !N*'
  end
  object broker: TEdit
    Left = 92
    Top = 106
    Width = 141
    Height = 20
    BevelOuter = bvRaised
    Ctl3D = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    MaxLength = 15
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 4
    Text = '192.168.1.68'
  end
  object ShowPWD: TCheckBox
    Left = 268
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Show chars'
    TabOrder = 5
    OnClick = ShowPWDClick
  end
  object Timer: TTimer
    Interval = 500
    OnTimer = TimerTimer
    Left = 280
    Top = 28
  end
end
