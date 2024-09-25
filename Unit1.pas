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
  FireDAC.Comp.Client, System.Hash, System.IniFiles, System.IOUtils,
  Data.SqlExpr, Datasnap.DBClient, Datasnap.DSConnect, Data.DBXCommon, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy,
  Data.DbxHTTPLayer, REST.Types, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, Datasnap.DSClientRest, ClientModuleUnit3;

type
  TLoginForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    Button1: TButton;
    DSRestConnection1: TDSRestConnection;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

function TLoginForm.ValidateCredentials(const Username, Password: string): Boolean;
begin
  try
    // Call the server method
    Result := ClientModule3.ServerMethods1Client.ValidateCredentials(Username, Password);
  except
    on E: Exception do
    begin
      ShowMessage('Error communicating with server: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TLoginForm.Button2Click(Sender: TObject);
var
  Msg: string;
begin
  try
    Msg := ClientModule3.ServerMethods1Client.GetMessage;
    ShowMessage(Msg);
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;

end;


procedure TLoginForm.FormCreate(Sender: TObject);
var

  IniFile: TIniFile;
  LastValuelogin, LastValuepass: string;
  inifilename : string;
begin


Case TOSVersion.Platform of
    TOSVersion.TPlatform.pfWindows:
  Path := ExtractFilePath(ParamStr(0));
 TOSVersion.TPlatform.pfiOS, TOSVersion.TPlatform.pfAndroid:
    Path := TPath.Combine(TPath.GetDocumentsPath, 'MyApp.ini');
  TOSVersion.TPlatform.pfMacOS:
    Path := TPath.Combine(TPath.GetFullPath('../Resources/StartUp'), 'MyApp.ini');
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


//function TLoginForm.ValidateCredentials(const Username, Password: string): Boolean;
//var
// hashedPassword, nothashedPassword: string;
// // IdMD5: TIdHashMessageDigest5;
//begin
//  Result := False;
//  hashedPassword := THashMD5.GetHashString(Password);
//  nothashedPassword:= Password;
//
//    try
//    LoginForm.FDQuery1.Connection := LoginForm.FDConnection1;
//    LoginForm.FDQuery1.SQL.Text := 'SELECT COUNT(*) FROM patients WHERE Username = :Username AND Password = :Password';
//    LoginForm.FDQuery1.ParamByName('Username').AsString := Username;
//    LoginForm.FDQuery1.ParamByName('Password').AsString := nothashedPassword;
//    LoginForm.FDQuery1.Open;
//
//    // Check result
//    if LoginForm.FDQuery1.Fields[0].AsInteger > 0 then
//     begin
//      Result := true;
//      end
//
//  finally
//  //FDQuery1.Free;
// // FDConnection1.Free;
//
// end;
//   end;



end.
