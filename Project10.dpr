program Project10;



uses
  System.StartUpCopy,
  FMX.Forms,
  Unit12 in 'Unit12.pas' {Form12},
  Unit1 in 'Unit1.pas' {LoginForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape];
  Application.CreateForm(TLoginForm, LoginForm);
  Application.Run;
end.
