program GCODEProgGen;

uses
  Vcl.Forms,
  uFormPrincipal in 'src\uFormPrincipal.pas' {FormMain},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'GCODE Progress Generator';
  TStyleManager.TrySetStyle('Glossy');
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
