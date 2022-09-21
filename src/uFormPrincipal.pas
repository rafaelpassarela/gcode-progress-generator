unit uFormPrincipal;

{
Ender 3 Display Length = 21

Mesh with 83 layers
;LAYER:0
;LAYER:82

Layer Count
M117 Layer 0001 of 0083...  | OLD v1.2
M117 LYR 0001 of 0083.....  | NEW v1.3

Cmd Count
M117 Tot. 99.999%.........

}

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ComCtrls,
  IniFiles, System.IOUtils, Vcl.Samples.Spin, Math, StrUtils;

type
  TInfoType = (itLineProgress, itCommandProgress);

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
    LabelChangeCount: TLabel;
    SpinEditChangeCount: TSpinEdit;
    procedure ButtonFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonGenerateClick(Sender: TObject);
  private const
    C_GCODE_TYPE = ';TYPE:';
    C_GCODE_END_INFO = 38;
    C_LINE_COUNT = 'Line Count: ';
    C_SECTION = 'config';
    C_IDENT_FILE = 'file.name';
    C_IDENT_TAG  = 'layer.tag';
    C_IDENT_CNT  = 'layer.count';
    C_IDENT_MSG  = 'layer.msg';
    C_IDENT_CHANGE = 'change.info.count';
  private
    { Private declarations }
    FConfigName : TFileName;
    FLastLayerInfo : string;
    procedure SaveConfig;
    procedure LoadConfig;
    procedure EnableDisableAll(const Value : Boolean);
    procedure UpdateLineCount(const ATotal, ACurrent : Integer);
    procedure GenerateEx;
    function GetEditedFileName : PChar;
    function GetTotalLines: Integer;
    function GetLineCount(const ALine : string; const ATotalLayers : Integer; const ALineType : string): string;
    function GetCommandCount(const ATotalLines, ACurrentLine : Integer; const ALineType: string) : string;
    function GetPrintType(const AExpanded : Boolean; const ALineType : string) : string;
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

  LabelChangeCount.Enabled := Value;
  SpinEditChangeCount.Enabled := Value;

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
  lTotalLayers: Integer;
  i: Integer;
  lLine: string;
  lMsgLayer: string;
  lMsgCommand: string;
  lAux: string;
  lType: string;
  lStartTime: TTime;
  lMode: TInfoType;
  lIntervalCount: Integer;
  lCmdStartLine: Integer;
  lTotalCmd: Integer;
  lPrintCustomLine: Boolean;

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
  lMode := itLineProgress;
  lIntervalCount := 0;
  lCmdStartLine := 0;
  lTotalCmd := 0;

  try
    lStartTime := Now;

    lTotalFileLines := GetTotalLines;

    lTempFileName := TPath.GetTempFileName;

    lReaderStream := TFileStream.Create(EditFile.Text, fmOpenRead);
    lReader := TStreamReader.Create(lReaderStream);

    lWriterStream := TFileStream.Create(lTempFileName, fmOpenWrite);
    lWriter := TStreamWriter.Create(lWriterStream);

    ProgressBarFileGen.Max := lTotalFileLines;
    UpdateLineCount(lTotalFileLines, 0);

    lTotalLayers := -1;
    lType := EmptyStr;
    i := 0;
    while not lReader.EndOfStream do
    begin
      Inc(i);
      lPrintCustomLine := False;

      lLine := lReader.ReadLine;
      // Total Layers
      if (lTotalLayers = -1) and (Pos(EditLayerCount.Text, lLine) > 0) then
      begin
        lAux := StringReplace(lLine, EditLayerCount.Text, '', []);
        lTotalLayers := StrToIntDef(lAux, -1) - 1;
        // Start Interval Count
        lIntervalCount := 1;
        lCmdStartLine := i;
        lTotalCmd := lTotalFileLines - lCmdStartLine - C_GCODE_END_INFO;
      end else
        // Current command type
        if (Pos(C_GCODE_TYPE, lLine) > 0) then
        begin
          lType := StringReplace(lLine, C_GCODE_TYPE, '', []);
        end;

      if (lTotalLayers > 0) then
      begin
        if (Pos(EditLayerTag.Text, lLine) > 0) then
        begin
          lMsgLayer := GetLineCount(lLine, lTotalLayers, lType);
          // Layer change always update screen
          lWriter.Write(lMsgLayer + sLineBreak);
        end;

        if lIntervalCount > 0 then
          lMsgCommand := GetCommandCount(lTotalCmd, i - lCmdStartLine, lType);
      end;

      // change display info
      if i mod SpinEditChangeCount.Value = 0 then
      begin
        lPrintCustomLine := True;
        if lMode = itLineProgress then
          lMode := itCommandProgress
        else
          lMode := itLineProgress;
      end;

      // write the original line to the temp file
      lWriter.Write(lLine + sLineBreak);
      // write the custon line to the temp file
      if lPrintCustomLine then
      begin
        lLine := IfThen(lMode = itLineProgress, lMsgLayer, lMsgCommand);
        lWriter.Write(lLine + sLineBreak);
      end;

      // update staus
      if i mod 130 = 0 then
      begin
        ProgressBarFileGen.Position := i + 1;
        UpdateLineCount(ProgressBarFileGen.Max, ProgressBarFileGen.Position);

        Application.ProcessMessages;
      end;
    end;

    FreeStreamPointers;

    CopyFile(PChar(lTempFileName), GetEditedFileName, False);

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

function TFormMain.GetCommandCount(const ATotalLines,
  ACurrentLine: Integer; const ALineType: string): string;
var
  lPercent: Double;
begin
//M117 Tot. 99.999%.........
  try
    lPercent := (ACurrentLine * 100) / ATotalLines;
  except
    lPercent := 0;
  end;
  Result := Format('M117 Tot. %2.4f%% %s', [lPercent, GetPrintType(True, ALineType)]);
end;

function TFormMain.GetEditedFileName: PChar;
var
  lName : string;
  lDir : string;
begin
  if DebugHook > 0 then
  begin
    lDir := IncludeTrailingPathDelimiter(ExtractFilePath(EditFile.Text));
    lName := ExtractFileName(EditFile.Text);

    lName := lDir + 'debug_' + lName;
  end
  else
    lName := EditFile.Text;

  Result := PChar(lName);
end;

function TFormMain.GetLineCount(const ALine : string; const ATotalLayers : Integer;
  const ALineType : string): string;
var
  lCurrentLayer: Integer;
begin
  lCurrentLayer := StrToIntDef(StringReplace(ALine, EditLayerTag.Text, '', []), -1);
  if lCurrentLayer > -1 then
    FLastLayerInfo := Format('%s %4.4d of %4.4d  %s', ['M117 LYR', lCurrentLayer + 1, ATotalLayers, GetPrintType(False, ALineType)])
//           + Format('%s %4.4d of %4.4d', ['M117 Layer', lCurrentLayer + 1, ATotalLayers])
  else
    FLastLayerInfo := ALine;

  Result := FLastLayerInfo;
end;

function TFormMain.GetPrintType(const AExpanded: Boolean; const ALineType : string): string;
const
  C_KNOW_TYPES: array[0..6,0..2] of string = (
    ('SKIRT',             'BPA', 'Brim'),
    ('WALL-OUTER',        'OUT', 'Wall Out'),
    ('WALL-INNER',        'INN', 'Wall Inn'),
    ('SKIN',              'SKI', 'Skin'),
    ('FILL',              'FIL', 'Infill'),
    ('SUPPORT',           'SUP', 'Support'),
    ('SUPPORT-INTERFACE', 'SIN', 'Sup.Intf')
  );
var
  i: Integer;
begin
  Result := ALineType;

  for i := 0 to 6 do
  begin
    if C_KNOW_TYPES[i][0] = ALineType then
    begin
      Result := C_KNOW_TYPES[i][IfThen(Aexpanded, 2, 1)];
      Break
    end;
  end;
end;

function TFormMain.GetTotalLines: Integer;
var
  lFile: TStringList;
begin
  try
    lFile := TStringList.Create;
    lFile.LoadFromFile(EditFile.Text);
    Result := lFile.Count;
  finally
    FreeAndNil(lFile);
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
      SpinEditChangeCount.Value := ReadInteger(C_SECTION, C_IDENT_CHANGE, 100);
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
    WriteInteger(C_SECTION, C_IDENT_CHANGE, SpinEditChangeCount.Value);
  finally
    Free;
  end;

end;

procedure TFormMain.UpdateLineCount(const ATotal, ACurrent: Integer);
begin
  LabelStatus.Caption := Format('%s %d of %d', [C_LINE_COUNT, ACurrent, ATotal]);
end;

end.
