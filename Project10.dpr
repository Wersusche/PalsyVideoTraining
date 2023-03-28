program Project10;



uses
  System.StartUpCopy,
  FMX.Forms,
  Unit12 in 'Unit12.pas' {Form12};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape];
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
