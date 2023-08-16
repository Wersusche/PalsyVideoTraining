unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, System.Hash, System.IniFiles, System.IOUtils;

type
  TLoginForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    Button1: TButton;
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
        Path : string;

      public
    { Public declarations }
         Pusername : string;
     function ValidateCredentials(const Username, Password: string): Boolean;

      end;

  const
  INI_FILE = 'MyApp.ini';
  INI_SECTION = 'LastValues';
var
  LoginForm: TLoginForm;

implementation

{$R *.fmx}

uses Unit12;

procedure TLoginForm.Button1Click(Sender: TObject);
var
    IniFile: TIniFile;
LastValuelogin, LastValuepass: string;
inifilename : string;
Nextform : TForm12;

begin
   inifilename := TPath.Combine(Path, 'MyApp.ini');
   try
   IniFile := TIniFile.Create(inifilename);
   IniFile.WriteString(INI_SECTION, 'MyPassword', PasswordEdit.Text);
   IniFile.WriteString(INI_SECTION, 'MyLogin', UsernameEdit.Text);

  finally
    IniFile.Free;
   end;

  if ValidateCredentials(UsernameEdit.Text, PasswordEdit.Text) then
  begin
    // Credentials are valid
    //Form12 := TForm12.Create(Application); // Assuming the main form class is TMainForm
    Pusername := UsernameEdit.Text;
    Application.CreateForm(TForm12, NextForm);
    NextForm.Show;
    //Form12.Show;
     Self.Destroy;
  end
  else
  begin
    // Invalid credentials
    ShowMessage('Неправильный логин или пароль.');

  end;


 end;



procedure TLoginForm.Button2Click(Sender: TObject);
var
Nextform : TForm12;

begin
   // Application.CreateForm(TForm12, NextForm);
    NextForm.Show;
    end;

procedure TLoginForm.Button3Click(Sender: TObject);
begin
    FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'wersusche';
    FDConnection1.Params.Values['Password'] := 'tyjer1987';
    FDConnection1.Params.Values['Server'] := 'db4free.net';
    FDConnection1.Connected := True;

    FDQuery1.Connection := FDConnection1;
    FDQuery1.SQL.Text := 'SELECT P.idPatients, A.idvideos, A.dlitelnost, V.filename ' +
                  'FROM patients P ' +
                  'INNER JOIN appointments A ON P.idPatients = A.idPatients ' +
                  'INNER JOIN videos V ON A.idvideos = V.idvideos ' +
                  'WHERE P.Username = :UserName';
      FDQuery1.ParamByName('UserName').AsString := 'Boris'; // Replace UserName with the actual user name
      FDQuery1.Open;
  ShowMessage(FDQuery1.FieldByName('filename').AsString);

end;

procedure TLoginForm.FormCreate(Sender: TObject);
var

  IniFile: TIniFile;
  LastValuelogin, LastValuepass: string;
  inifilename : string;
begin
    // Connection settings
    FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'wersusche';
    FDConnection1.Params.Values['Password'] := 'tyjer1987';
    FDConnection1.Params.Values['Server'] := 'db4free.net';
    FDConnection1.Connected := True;



  Case TOSVersion.Platform of
    TOSVersion.TPlatform.pfWindows:
      Path := TPath.Combine('..', '..');
    TOSVersion.TPlatform.pfMacOS:
      Path := TPath.Combine(TPath.GetFullPath('../Resources/StartUp'), 'MyApp.ini');
    TOSVersion.TPlatform.pfiOS, TOSVersion.TPlatform.pfAndroid:
      Path := TPath.Combine(TPath.GetDocumentsPath, 'MyApp.ini');
    TOSVersion.TPlatform.pfWinRT, TOSVersion.TPlatform.pfLinux:
      raise Exception.Create('Unexpected platform');
  end;

   inifilename := TPath.Combine(Path, 'MyApp.ini');

  try
   IniFile := TIniFile.Create(inifilename);
    LastValuepass := IniFile.ReadString(INI_SECTION, 'MyPassword', '');
    LastValuelogin := IniFile.ReadString(INI_SECTION, 'MyLogin', '');
    PasswordEdit.Text := LastValuepass;
    UsernameEdit.Text := LastValuelogin;
  finally
    IniFile.Free;
  end;

  end;


  function TLoginForm.ValidateCredentials(const Username, Password: string): Boolean;
var
     hashedPassword: string;
 // IdMD5: TIdHashMessageDigest5;
begin
  Result := False;
  hashedPassword := THashMD5.GetHashString(Password);

    try
    LoginForm.FDQuery1.Connection := LoginForm.FDConnection1;
    LoginForm.FDQuery1.SQL.Text := 'SELECT COUNT(*) FROM patients WHERE Username = :Username AND Password = :Password';
    LoginForm.FDQuery1.ParamByName('Username').AsString := Username;
    LoginForm.FDQuery1.ParamByName('Password').AsString := hashedPassword;
    LoginForm.FDQuery1.Open;

    // Check result
    if LoginForm.FDQuery1.Fields[0].AsInteger > 0 then
     begin
      Result := true;
      end

  finally
  //FDQuery1.Free;
 // FDConnection1.Free;

 end;
   end;



end.
