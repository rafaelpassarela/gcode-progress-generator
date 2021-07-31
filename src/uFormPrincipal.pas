unit uFormPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ComCtrls, IniFiles;

type
  TFormMain = class(TForm)
    LabelFileName: TLabel;
    EditFile: TEdit;
    ButtonFile: TButton;
    LabelTag: TLabel;
    EditLayerTag: TEdit;
    LabelLayerCount: TLabel;
    EditLayerCount: TEdit;
    ButtonGenerate: TButton;
    ProgressBarFileGen: TProgressBar;
    procedure ButtonFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonGenerateClick(Sender: TObject);
  private const
    C_SECTION = 'config';
    C_IDENT_FILE = 'file.name';
    C_IDENT_TAG  = 'layer.tag';
    C_IDENT_CNT  = 'layer.count';
    C_IDENT_MSG  = 'layer.msg';
  private
    { Private declarations }
    FConfigName : TFileName;
    procedure SaveConfig;
    procedure LoadConfig;
    procedure EnableDisableAll(const Value : Boolean);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.ButtonFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    Title := 'Select the GCODE file';
    DefaultExt := '*.gcode';
    Filter := 'GCODE Files (*.gcode)|*.gcode';
    FilterIndex := 0;
    FileName := EditFile.Text;
    if Execute then
      EditFile.Text := FileName;
  finally
    Free;
  end;
end;

procedure TFormMain.ButtonGenerateClick(Sender: TObject);
var
  lFile: TStringList;
  i: Integer;
  lTotal: Integer;
  lLine: string;
  lCurrent: Integer;
begin
  SaveConfig;
  if FileExists(EditFile.Text) then
  begin
    EnableDisableAll(False);

    try
      lFile := TStringList.Create;
      lFile.LoadFromFile(EditFile.Text);

      ProgressBarFileGen.Max := lFile.Count;

      // get layer count
      lTotal := -1;
      for i := 0 to lFile.Count - 1 do
      begin
        lLine := AnsiUpperCase(lFile[i]);
        if Pos(EditLayerCount.Text, lLine) > 0 then
        begin
          lLine := StringReplace(lLine, EditLayerCount.Text, '', []);
          lTotal := StrToIntDef(lLine, -1);
        end;
        if lTotal > 0 then
          Break;
      end;

      // replace message
      for i := 0 to lFile.Count - 1 do
      begin
        if i mod 3 = 0 then
        begin
          ProgressBarFileGen.Position := i + 1;
          Application.ProcessMessages;
        end;

        lLine := lFile[i];
        if Pos(EditLayerTag.Text, lLine) > 0 then
        begin
          lCurrent := StrToIntDef(StringReplace(lLine, EditLayerTag.Text, '', []), -1);
          if lCurrent > -1 then
          begin
            lLine := Format('%s %4.4d of %4.4d', ['M117 Layer', lCurrent + 1, lTotal]);
            lFile[i] := lLine;
          end;
        end;
      end;
      lFile.SaveToFile(EditFile.Text);
      ShowMessage('GCODE file successful updated!!!');
    finally
      EnableDisableAll(True);
    end;
  end;
end;

procedure TFormMain.EnableDisableAll(const Value: Boolean);
begin
  LabelFileName.Enabled := Value;
  EditFile.Enabled := Value;

  LabelTag.Enabled := Value;
  EditLayerTag.Enabled := Value;

  LabelLayerCount.Enabled := Value;
  EditLayerCount.Enabled := Value;

  ProgressBarFileGen.Visible := not Value;
  ProgressBarFileGen.Position := 0;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FConfigName := StringReplace(Application.ExeName, '.exe', '.ini', [rfIgnoreCase]);
  LoadConfig;
end;

procedure TFormMain.LoadConfig;
begin
  if FileExists(FConfigName) then
  begin
    with TIniFile.Create(FConfigName) do
    try
      EditFile.Text := ReadString(C_SECTION, C_IDENT_FILE, '');
      EditLayerTag.Text := ReadString(C_SECTION, C_IDENT_TAG, '');
      EditLayerCount.Text := ReadString(C_SECTION, C_IDENT_CNT, '');
    finally
      Free;
    end;
  end else
  begin
    EditFile.Clear;
  end;
end;

procedure TFormMain.SaveConfig;
begin
  with TIniFile.Create(FConfigName) do
  try
    WriteString(C_SECTION, C_IDENT_FILE, EditFile.Text);
    WriteString(C_SECTION, C_IDENT_TAG, EditLayerTag.Text);
    WriteString(C_SECTION, C_IDENT_CNT, EditLayerCount.Text);
  finally
    Free;
  end;

end;

end.
