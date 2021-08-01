unit uFormPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ComCtrls, IniFiles,
  System.IOUtils;

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
    LabelStatus: TLabel;
    procedure ButtonFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonGenerateClick(Sender: TObject);
  private const
    C_LINE_COUNT = 'Line Count: ';
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
    procedure UpdateLineCount(const ATotal, ACurrent : Integer);
    procedure GenerateEx;
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
begin
  SaveConfig;
  if FileExists(EditFile.Text) then
  begin
    LabelStatus.Caption := C_LINE_COUNT;
    EnableDisableAll(False);

    try
      GenerateEx;
    finally
      EnableDisableAll(True);
    end;
  end;
end;

procedure TFormMain.EnableDisableAll(const Value: Boolean);
begin
  ButtonFile.Enabled := Value;
  ButtonGenerate.Enabled := Value;

  LabelFileName.Enabled := Value;
  EditFile.Enabled := Value;

  LabelTag.Enabled := Value;
  EditLayerTag.Enabled := Value;

  LabelLayerCount.Enabled := Value;
  EditLayerCount.Enabled := Value;

  ProgressBarFileGen.Visible := not Value;
  ProgressBarFileGen.Position := 0;

  LabelStatus.Visible := not Value;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IFDEF WIN32}
  Self.Caption := Self.Caption + ' (x86)';
  {$ENDIF}

  {$IFDEF WIN64}
  Self.Caption := Self.Caption + ' (x64)';
  {$ENDIF}

  FConfigName := StringReplace(Application.ExeName, '.exe', '.ini', [rfIgnoreCase]);
  LoadConfig;
end;

procedure TFormMain.GenerateEx;
var
  lReaderStream: TFileStream;
  lReader: TStreamReader;  // TTextReader
  lWriterStream: TFileStream;
  lWriter: TStreamWriter;
  lTotalFileLines: Integer;
  lTempFileName: TFileName;
  lFile: TStringList;
  lTotalLayers: Integer;
  i: Integer;
  lLine: string;
  lAux: string;
  lStartTime: TTime;
  lCurrentLayer: Integer;

  procedure FreeStreamPointers;
  begin
    if Assigned(lReader) then
      FreeAndNil(lReader);
    if Assigned(lReaderStream) then
      FreeAndNil(lReaderStream);

    if Assigned(lWriter) then
      FreeAndNil(lWriter);
    if Assigned(lWriterStream) then
      FreeAndNil(lWriterStream);
  end;
begin
  lReader := nil;
  lWriter := nil;
  lReaderStream := nil;
  lWriterStream := nil;
  try
    lStartTime := Now;

    try
      lFile := TStringList.Create;
      lFile.LoadFromFile(EditFile.Text);
      lTotalFileLines := lFile.Count;
    finally
      FreeAndNil(lFile);
    end;

    lTempFileName := TPath.GetTempFileName;

    lReaderStream := TFileStream.Create(EditFile.Text, fmOpenRead);
    lReader := TStreamReader.Create(lReaderStream);

    lWriterStream := TFileStream.Create(lTempFileName, fmOpenWrite);
    lWriter := TStreamWriter.Create(lWriterStream);

    ProgressBarFileGen.Max := lTotalFileLines;
    UpdateLineCount(lTotalFileLines, 0);

    lTotalLayers := -1;
    i := 0;
    while not lReader.EndOfStream do
    begin
      Inc(i);

      lLine := lReader.ReadLine;
      if (lTotalLayers = -1) and (Pos(EditLayerCount.Text, lLine) > 0) then
      begin
        lAux := StringReplace(lLine, EditLayerCount.Text, '', []);
        lTotalLayers := StrToIntDef(lAux, -1);
      end;

      if (lTotalLayers > 0) and (Pos(EditLayerTag.Text, lLine) > 0) then
      begin
        lCurrentLayer := StrToIntDef(StringReplace(lLine, EditLayerTag.Text, '', []), -1);
        if lCurrentLayer > -1 then
          lLine := Format('%s %4.4d of %4.4d', ['M117 Layer', lCurrentLayer + 1, lTotalLayers]);
      end;

      // write the line to the temp file
      lWriter.Write(lLine + sLineBreak);

      // update staus
      if i mod 130 = 0 then
      begin
        ProgressBarFileGen.Position := i + 1;
        UpdateLineCount(ProgressBarFileGen.Max, ProgressBarFileGen.Position);

        Application.ProcessMessages;
      end;
    end;

    FreeStreamPointers;

    CopyFile(PChar(lTempFileName), PChar(EditFile.Text), False);

    ShowMessage('GCODE file successful updated!!!' + sLineBreak + sLineBreak
              + 'Total Layers: ' + IntToStr(lTotalLayers) + sLineBreak
              + 'Process Time: ' + FormatDateTime('hh:mm:ss', Now - lStartTime));
  finally
    if FileExists(lTempFileName) then
      DeleteFile(lTempFileName);

    FreeStreamPointers;

    EnableDisableAll(True);
  end;
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

procedure TFormMain.UpdateLineCount(const ATotal, ACurrent: Integer);
begin
  LabelStatus.Caption := Format('%s %d of %d', [C_LINE_COUNT, ACurrent, ATotal]);
end;

end.
