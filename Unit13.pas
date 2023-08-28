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
  FireDAC.Phys.MySQLDef, FireDAC.Phys.MySQL, System.Hash, FMX.DateTimeCtrls,
  StrUtils, System.Generics.Collections, System.DateUtils, FMX.Layouts,
  FMX.ListBox, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Objects,
  FMX.MultiView, FMX.TreeView, FMX.ExtCtrls, System.Math;

type
  TForm13 = class(TForm)
    FDConnection1: TFDConnection;
    FDQuery_patientlist: TFDQuery;
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
    FDQuery_newpatient: TFDQuery;
    CheckBox1: TCheckBox;
    FDQuery_patientpersonal: TFDQuery;
    Edit1: TEdit;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    GroupBox5: TGroupBox;
    ListView2: TListView;
    GroupBox6: TGroupBox;
    TabControl2: TTabControl;
    �������: TTabItem;
    �������: TTabItem;
    FDQuery_patappointments: TFDQuery;
    ListView_now: TListView;
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    TreeView1: TTreeView;
    ListBox1: TListBox;
    Button2: TButton;
    FDQuery_disorders: TFDQuery;
    �������: TTabItem;
    ListView_old: TListView;
    ListView_next: TListView;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure enterLoginTyping(Sender: TObject);
    procedure EnterPasswordTyping(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1ChangeCheck(Sender: TObject);
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
  ShowMessage('������� ��� �������')
  end
  else
  if Checkbox1.IsChecked then
  begin
  NewUserName := GenerateUserName(enterSurname.Text, enterName.Text, FDConnection1);
  hashedPassword := THashMD5.GetHashString(GenerateRandomPassword);

  enterLogin.Text := NewUserName;
  enterPassword.Text := GenerateRandomPassword;

    try
    FDQuery_newpatient.Connection := FDConnection1; // Replace with your TFDConnection component's name

    // Prepare the SQL statement
    FDQuery_newpatient.SQL.Text := 'INSERT INTO patients (Name, Surname, Secname, Birthdate, Username, Password) VALUES (:Name, :Surname, :Secname, :DateOfBirth, :AssiUsername, :AssiPassword)';

    //  Assign parameters
    FDQuery_newpatient.ParamByName('Name').AsString := enterName.Text;
    FDQuery_newpatient.ParamByName('Surname').AsString := enterSurname.Text;
    FDQuery_newpatient.ParamByName('Secname').AsString := enterName2.Text;
    FDQuery_newpatient.ParamByName('DateOfBirth').AsDate := DateEdit1.Date;
    FDQuery_newpatient.ParamByName('AssiUsername').AsString := NewUserName;
    FDQuery_newpatient.ParamByName('AssiPassword').AsString := hashedPassword;
    // Execute the SQL statement
    FDQuery_newpatient.ExecSQL;
   finally
    FDQuery_newpatient.Close;
   ShowMessage('����������� ��������� �������� ������� ������!')
   end;

  end
  else
  if enterLogin.Text.IsEmpty or enterPassword.Text.IsEmpty then
     begin
  ShowMessage('������� ��� �������')
     end
  else
      begin
  hashedPassword := THashMD5.GetHashString(enterPassword.Text);
        try
    FDQuery_newpatient.Connection := FDConnection1; // Replace with your TFDConnection component's name

    // Prepare the SQL statement
    FDQuery_newpatient.SQL.Text := 'INSERT INTO patients (Name, Surname, Secname, Birthdate, Username, Password) VALUES (:Name, :Surname, :Secname, :DateOfBirth, :AssiUsername, :AssiPassword)';

    //  Assign parameters
    FDQuery_newpatient.ParamByName('Name').AsString := enterName.Text;
    FDQuery_newpatient.ParamByName('Surname').AsString := enterSurname.Text;
    FDQuery_newpatient.ParamByName('Secname').AsString := enterName2.Text;
    FDQuery_newpatient.ParamByName('DateOfBirth').AsDate := DateEdit1.Date;
    FDQuery_newpatient.ParamByName('AssiUsername').AsString := enterLogin.Text;
    FDQuery_newpatient.ParamByName('AssiPassword').AsString := hashedPassword;
    // Execute the SQL statement
    FDQuery_newpatient.ExecSQL;
        finally
    FDQuery_newpatient.Close;
    ShowMessage('����������� ��������� �������� ������� ������!')
        end;
      end;
    end;



procedure TForm13.FormCreate(Sender: TObject);
begin
 ListView1.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
 ListView2.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
 ListView_now.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
 FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'wersusche';
    FDConnection1.Params.Values['Password'] := 'tyjer1987';
    FDConnection1.Params.Values['Server'] := 'db4free.net';
    FDConnection1.Params.Values['CharacterSet'] := 'utf8mb4';
        // Connection settings

FDQuery_patientlist.SQL.Text := 'SELECT Name, Surname, idPatients FROM patients';
FDQuery_patientlist.Open;

ListView1.Items.BeginUpdate;
ListView1.Items.Clear;
while not FDQuery_patientlist.Eof do
begin
  with ListView1.Items.Add do
  begin
   Text := FDQuery_patientlist.FieldByName('Surname').AsString + ' ' +
            FDQuery_patientlist.FieldByName('Name').AsString;
   Tag := FDQuery_patientlist.FieldByName('idPatients').AsInteger;
  end;
  FDQuery_patientlist.Next;
end;
ListView1.Items.EndUpdate;
FDQuery_patientlist.Close;
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
  RusChars: array[1..33] of string = ('�','�','�','�','�','�','�','�','�','�',
                                      '�','�','�','�','�','�','�','�','�','�',
                                      '�','�','�','�','�','�','�','�','�','�',
                                      '�','�','�');
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




procedure TForm13.Button2Click(Sender: TObject);
var
  CurrentType: string;
  CurrentTypeNode, DisorderNode: TTreeViewItem;
begin
FDQuery_disorders.SQL.Text := 'SELECT d.idDisorders, d.Disorder_name, d.Disorder_type FROM disorders d ORDER BY Disorder_Type';
FDQuery_disorders.Open;
CurrentType := '';
TreeView1.Clear;

while not FDQuery_disorders.Eof do
    begin
 if CurrentType <> FDQuery_disorders.FieldByName('Disorder_type').AsString then
    begin
      CurrentType := FDQuery_disorders.FieldByName('Disorder_type').AsString;
      CurrentTypeNode := TTreeViewItem.Create(TreeView1);
      CurrentTypeNode.Text := CurrentType;
      TreeView1.AddObject(CurrentTypeNode);
    end;

    DisorderNode := TTreeViewItem.Create(CurrentTypeNode);
    DisorderNode.Text := FDQuery_disorders.FieldByName('Disorder_name').AsString;
    CurrentTypeNode.AddObject(DisorderNode);

    FDQuery_disorders.Next;
  end;

  FDQuery_disorders.Close;
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
    if CharInSet(C,['a'..'z', 'A'..'Z','0'..'9']) then
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

procedure TForm13.ListView1DblClick(Sender: TObject);
  var
  SelectedID: Integer;
  YearsDiff: Double;
  ListItem, ListItem2: TListViewItem;
  Row: Integer;
    DailyExerciseCount, TotalExerciseCount: Integer;
  DaysPassed, ProjectedExerciseCount: Integer;
    CompletionRate: Double;
begin
  // Check if an item is selected
  ListView2.Items.Clear;
  ListView_now.Items.Clear;
  if Assigned(ListView1.Selected) then
  begin
    // Get the ID stored in the selected item's Tag property
    SelectedID := ListView1.Selected.Tag;

    // Fetch the details based on the selected ID
    FDQuery_patientpersonal.Close;
    FDQuery_patientpersonal.SQL.Text := 'SELECT p.Name, p.Surname, p.Secname, p.Birthdate, d.Disorder_name, d.Disorder_type ' +
                                         'FROM patients p ' +
                                         'LEFT JOIN patient_disorders pd ON p.idPatients = pd.patient_id ' +
                                         'LEFT JOIN disorders d ON pd.disorder_id = d.idDisorders ' +
                                         'WHERE p.idPatients = :ID';

    FDQuery_patientpersonal.ParamByName('ID').AsInteger := SelectedID;
    FDQuery_patientpersonal.Open;
        try
      if not FDQuery_patientpersonal.Eof then
      begin
        YearsDiff := YearSpan(Now(),FDQuery_patientpersonal.FieldByName('Birthdate').AsDateTime);
        // Assuming column_name_1 should go to Edit1 and column_name_2 to Edit2
        Label1.Text := Format('%s %s %s %s �.�. (%d ��� %d ���.)', [FDQuery_patientpersonal.FieldByName('Surname').AsString,
         FDQuery_patientpersonal.FieldByName('Name').AsString,
        FDQuery_patientpersonal.FieldByName('Secname').AsString,
        FDQuery_patientpersonal.FieldByName('Birthdate').AsString, Trunc(YearsDiff),
        Round(Frac(YearsDiff) * 12)]);

    while not FDQuery_patientpersonal.Eof do
    begin
  ListItem := ListView2.Items.Add;
  ListItem.Text := FDQuery_patientpersonal.FieldByName('Disorder_name').AsString;
  ListItem.Detail := FDQuery_patientpersonal.FieldByName('Disorder_type').AsString;
    FDQuery_patientpersonal.Next;
    end;
    end;
      finally
      FDQuery_patientpersonal.Close;
    end;

    FDQuery_patappointments.Close;
    FDQuery_patappointments.SQL.Text := 'SELECT a.Starttime, a.Endtime, a.kolvden, a.sdelanovsego, v.video_name ' +
                                         'FROM appointments a ' +
                                         'LEFT JOIN videos v ON a.idvideos = v.idvideos ' +
                                         'WHERE a.idPatients = :ID';
      FDQuery_patappointments.ParamByName('ID').AsInteger := SelectedID;
       FDQuery_patappointments.Open;
      ListView_now.BeginUpdate;
  try
    // Assume FDQuery1 has been prepared and opened to select your fields
    while not FDQuery_patappointments.EOF do
    begin
    if InRange(Now(), FDQuery_patappointments.FieldByName('Starttime').AsDateTime, FDQuery_patappointments.FieldByName('Endtime').AsDateTime) then
     begin
      ListItem2 := ListView_now.Items.Add;
      ListItem2.Text := FDQuery_patappointments.FieldByName('video_name').AsString;
      ListItem2.Detail := 'c '+ FDQuery_patappointments.FieldByName('Starttime').AsString + ' �� ' +
                             FDQuery_patappointments.FieldByName('Endtime').AsString  + ', ���������� ��� � ����: ' +
                             FDQuery_patappointments.FieldByName('kolvden').AsString + ', ������� �����: ' +
                   FDQuery_patappointments.FieldByName('sdelanovsego').AsString ;



        // Calculate the number of days from the start date to the current date
    DaysPassed := DaysBetween(Now(), FDQuery_patappointments.FieldByName('Starttime').AsDateTime);

    // Calculate how many exercises should have been done by now
    ProjectedExerciseCount := DaysPassed * DailyExerciseCount;

    // Calculate the completion rate
    if ProjectedExerciseCount > 0 then
      CompletionRate := (TotalExerciseCount / ProjectedExerciseCount) * 100
    else
      CompletionRate := 0;
     var  Background: TListItemSimpleControl;
      // Show a warning MessageBox
      Background := ListItem2.Objects.FindObjectT<TListItemSimpleControl>('background');

    if CompletionRate < 70 then
    begin
        ListItem2.Objects.TextObject.TextColor:= TAlphaColors.Red;
      ListItem2.Objects.DetailObject.TextColor := TAlphaColors.Red;
    end;
     FDQuery_patappointments.Next;
     end
     else
      FDQuery_patappointments.Next;
    end;
      finally
    ListView_now.EndUpdate;
    FDQuery_patappointments.Close;
  end;
  end;
end;



procedure TForm13.TreeView1ChangeCheck(Sender: TObject);
begin
if TTreeViewItem(Sender).Count > 0 then
begin
TTreeViewItem(Sender).IsChecked := False;
end;
end;

procedure TForm13.TreeView1DblClick(Sender: TObject);
begin
var
  SelectedItem: TTreeViewItem;
begin
  SelectedItem := TreeView1.Selected;
  if Assigned(SelectedItem) then
  begin
    SelectedItem.IsExpanded := not SelectedItem.IsExpanded;
  end;
end;
end;

end.


