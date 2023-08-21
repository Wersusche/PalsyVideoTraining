unit Unit13;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.TabControl, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FireDAC.Phys.MySQLDef, FireDAC.Phys.MySQL, System.Hash, FMX.DateTimeCtrls, StrUtils, System.Generics.Collections;

type
  TForm13 = class(TForm)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    TabControl1: TTabControl;
    Tab_newpatient: TTabItem;
    Tab_overview: TTabItem;
    TabItem3: TTabItem;
    ListView1: TListView;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    Button1: TButton;
    enterSurname: TEdit;
    enterName: TEdit;
    enterName2: TEdit;
    enterLogin: TEdit;
    EnterPassword: TEdit;
    DateEdit1: TDateEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    FDQuery2: TFDQuery;
    CheckBox1: TCheckBox;
    FDQuery3: TFDQuery;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure enterLoginTyping(Sender: TObject);
    procedure EnterPasswordTyping(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GenerateRandomPassword: string;
    function CheckExistenceFunc(const AUserName: string; AConnection: TFDConnection): Boolean;
    function GenerateUserName(const FirstName, LastName: string; AConnection: TFDConnection): string;
    function FilterNonEnglishCharacters(const AText: string): string;

     end;

var
  Form13: TForm13;

implementation

{$R *.fmx}



procedure TForm13.Button1Click(Sender: TObject);
  var
hashedPassword: string;
Newusername : string;

 begin
  hashedPassword:='';
  Newusername :='';

  if enterSurname.Text.IsEmpty or enterName.Text.IsEmpty then
  begin
  ShowMessage('¬ведите все даннные')
  end
  else
  if Checkbox1.IsChecked then
  begin
  NewUserName := GenerateUserName(enterSurname.Text, enterName.Text, FDConnection1);
  hashedPassword := THashMD5.GetHashString(GenerateRandomPassword);

  enterLogin.Text := NewUserName;
  enterPassword.Text := GenerateRandomPassword;

    try
    FDQuery2.Connection := FDConnection1; // Replace with your TFDConnection component's name

    // Prepare the SQL statement
    FDQuery2.SQL.Text := 'INSERT INTO patients (Name, Surname, Secname, Birthdate, Username, Password) VALUES (:Name, :Surname, :Secname, :DateOfBirth, :AssiUsername, :AssiPassword)';

    //  Assign parameters
    FDQuery2.ParamByName('Name').AsString := enterName.Text;
    FDQuery2.ParamByName('Surname').AsString := enterSurname.Text;
    FDQuery2.ParamByName('Secname').AsString := enterName2.Text;
    FDQuery2.ParamByName('DateOfBirth').AsDate := DateEdit1.Date;
    FDQuery2.ParamByName('AssiUsername').AsString := NewUserName;
    FDQuery2.ParamByName('AssiPassword').AsString := hashedPassword;
    // Execute the SQL statement
    FDQuery2.ExecSQL;
   finally
    FDQuery2.Close;
   end;

  end
  else
  if enterLogin.Text.IsEmpty or enterPassword.Text.IsEmpty then
     begin
  ShowMessage('¬ведите все даннные')
     end
  else
      begin
  hashedPassword := THashMD5.GetHashString(enterPassword.Text);
        try
    FDQuery2.Connection := FDConnection1; // Replace with your TFDConnection component's name

    // Prepare the SQL statement
    FDQuery2.SQL.Text := 'INSERT INTO patients (Name, Surname, Secname, Birthdate, Username, Password) VALUES (:Name, :Surname, :Secname, :DateOfBirth, :AssiUsername, :AssiPassword)';

    //  Assign parameters
    FDQuery2.ParamByName('Name').AsString := enterName.Text;
    FDQuery2.ParamByName('Surname').AsString := enterSurname.Text;
    FDQuery2.ParamByName('Secname').AsString := enterName2.Text;
    FDQuery2.ParamByName('DateOfBirth').AsDate := DateEdit1.Date;
    FDQuery2.ParamByName('AssiUsername').AsString := enterLogin.Text;
    FDQuery2.ParamByName('AssiPassword').AsString := hashedPassword;
    // Execute the SQL statement
    FDQuery2.ExecSQL;
        finally
    FDQuery2.Close;
        end;
      end;
    end;



procedure TForm13.FormCreate(Sender: TObject);
begin
  ListView1.Items.Clear;

 FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'wersusche';
    FDConnection1.Params.Values['Password'] := 'tyjer1987';
    FDConnection1.Params.Values['Server'] := 'db4free.net';
    FDConnection1.Params.Values['CharacterSet'] := 'utf8mb4';
        // Connection settings

FDQuery1.SQL.Text := 'SELECT Name, Surname FROM patients';
FDQuery1.Open;
while not FDQuery1.Eof do
begin
  with ListView1.Items.Add do
  begin
   Text := FDQuery1.FieldByName('Surname').AsString + ' ' +
            FDQuery1.FieldByName('Name').AsString;
   Detail:= FDQuery1.FieldByName('Surname').AsString;

  end;
  FDQuery1.Next;
end;

end;


function TForm13.GenerateRandomPassword: string;
var
  i: Integer;
const
  AllowedChars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
begin
  Randomize; // it's important to initialize the random number generator
  Result := '';
  for i := 1 to 8 do
    Result := Result + AllowedChars[Random(Length(AllowedChars)) + 1];
end;


function TransliterateRussianToEnglish(const Input: string): string;
const
  RusChars: array[1..33] of string = ('а','б','в','г','д','е','Є','ж','з','и',
                                      'й','к','л','м','н','о','п','р','с','т',
                                      'у','ф','х','ц','ч','ш','щ','ъ','ы','ь',
                                      'э','ю','€');
  EngChars: array[1..33] of string = ('a','b','v','g','d','e','yo','zh','z','i',
                                      'y','k','l','m','n','o','p','r','s','t',
                                      'u','f','h','ts','ch','sh','sch','','y','',
                                      'e','yu','ya');
var
  I: Integer;
begin
  Result := Input.ToLower;
  for I := 1 to 33 do
  begin
    Result := StringReplace(Result, RusChars[I], EngChars[I], [rfReplaceAll]);
  end;
end;




function TForm13.CheckExistenceFunc(const AUserName: string; AConnection: TFDConnection): Boolean;
var
  FDQuery: TFDQuery;
begin
  Result := False;

  if not Assigned(AConnection) then
    raise Exception.Create('Connection not assigned.');

  FDQuery := TFDQuery.Create(nil);
  try
    FDQuery.Connection := AConnection;
    FDQuery.SQL.Text := 'SELECT COUNT(*) AS UserCount FROM patients WHERE Username = :username';
    FDQuery.ParamByName('username').AsString := AUserName;
    FDQuery.Open;

    Result := FDQuery.FieldByName('UserCount').AsInteger > 0;
  finally
    FDQuery.free;
  end;
end;



procedure TForm13.enterLoginTyping(Sender: TObject);
begin
 //Edit := Sender as TEdit;
  enterLogin.Text := FilterNonEnglishCharacters(enterLogin.Text);
  enterLogin.CaretPosition := enterLogin.Text.Length;  // move the caret to the end
end;

procedure TForm13.EnterPasswordTyping(Sender: TObject);
begin
  EnterPassword.Text := FilterNonEnglishCharacters(enterPassword.Text);
  EnterPassword.CaretPosition := EnterPassword.Text.Length;  // move the caret to the end
end;

function TForm13.FilterNonEnglishCharacters(const AText: string): string;
var
  C: Char;
begin
  Result := '';
  for C in AText do
    if C in ['a'..'z', 'A'..'Z','0'..'9'] then
      Result := Result + C;
end;

function TForm13.GenerateUserName(const FirstName: string; const LastName: string; AConnection: TFDConnection): string;

var
  BaseUserName: string;
  Counter: Integer;
  TransFirstName, TransLastName: string;
begin
  TransFirstName := TransliterateRussianToEnglish(FirstName);
  TransLastName := TransliterateRussianToEnglish(LastName);
  BaseUserName := LowerCase(TransFirstName + LeftStr(TransLastName, 1));
  Result := BaseUserName;

  Counter := 1;
  while CheckExistenceFunc(Result, AConnection) do
  begin
    Inc(Counter);
    Result := BaseUserName + IntToStr(Counter);
  end;
end;
end.
