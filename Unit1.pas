unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation;

type
  TLoginForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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
begin
 // Replace 'user' and 'password' with the correct credentials.
  if (UsernameEdit.Text = 'user') and (PasswordEdit.Text = 'password') then
  begin
  Application.CreateForm(TForm12, Form12);
    //Form12.Create();
    Form12.Show;
    Self.Destroy;
  end
  else
  begin
    ShowMessage('Invalid username or password. Please try again.');
  end;
end;

end.
