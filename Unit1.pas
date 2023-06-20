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
  FireDAC.Comp.Client, System.Hash;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.fmx}

uses Unit12;

procedure TLoginForm.Button1Click(Sender: TObject);
var

usernam, passwor, hashedPassword: string;

begin

usernam := UsernameEdit.Text;
  passwor := PasswordEdit.Text;
   hashedPassword := THashMD5.GetHashString(passwor);

   try
    FDQuery1.Connection := FDConnection1;
    FDQuery1.SQL.Text := 'SELECT COUNT(*) FROM patients WHERE Username = :usernam AND Password = :passwor';
    FDQuery1.ParamByName('usernam').AsString := usernam;
    FDQuery1.ParamByName('passwor').AsString := hashedPassword;
    FDQuery1.Open;

    // Check result
    if FDQuery1.Fields[0].AsInteger > 0 then
      ShowMessage('Login successful')
    else
      ShowMessage('Login failed');
   finally

    FDQuery1.Free;
    FDConnection1.Free;
   end;

// // Replace 'user' and 'password' with the correct credentials.
//  if (UsernameEdit.Text = 'user') and (PasswordEdit.Text = 'password') then
//  begin
//  Application.CreateForm(TForm12, Form12);
//    //Form12.Create();
//    Form12.Show;
//    Self.Destroy;
//  end
//  else
//  begin
//    ShowMessage('Invalid username or password. Please try again.');
//  end;
end;

procedure TLoginForm.FormCreate(Sender: TObject);

 begin

    // Connection settings
    FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'Wersus';
    FDConnection1.Params.Values['Password'] := '';
    FDConnection1.Params.Values['Server'] := 'localhost';
    FDConnection1.Connected := True;
    // Query

  end;
end.
