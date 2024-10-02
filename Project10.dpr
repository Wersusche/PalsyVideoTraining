program Project10;



uses
  System.StartUpCopy,
  FMX.Forms,
  Unit12 in 'Unit12.pas' {Form12},
  Unit1 in 'Unit1.pas' {LoginForm},
  ClientClassesUnit3 in 'ClientClassesUnit3.pas',
  ClientModuleUnit3 in 'ClientModuleUnit3.pas' {ClientModule3: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape];
  Application.CreateForm(TClientModule3, ClientModule3);
  Application.CreateForm(TLoginForm, LoginForm);
  Application.Run;
end.
