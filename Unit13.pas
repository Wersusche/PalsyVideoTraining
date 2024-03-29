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
  FMX.MultiView, FMX.TreeView, FMX.ExtCtrls, System.Math, FMX.Calendar;

type
  TForm13 = class(TForm)
    FDConnection1: TFDConnection;
    FDQuery_patientlist: TFDQuery;
    TabControl1: TTabControl;
    Tab_newpatient: TTabItem;
    Tab_overview: TTabItem;
    Tab_cure: TTabItem;
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
    GroupBox6: TGroupBox;
    TabControl2: TTabControl;
    �������: TTabItem;
    �������: TTabItem;
    FDQuery_patappointments: TFDQuery;
    TreeView1: TTreeView;
    FDQuery_disorders: TFDQuery;
    �������: TTabItem;
    FDQuery_totalpatients: TFDQuery;
    FDQuery_openactusers: TFDQuery;
    GroupBox7: TGroupBox;
    Label_allusers: TLabel;
    Label_activeusers: TLabel;
    Label_badusers: TLabel;
    FDQuery_baduserslist: TFDQuery;
    ListView3: TListView;
    Button3: TButton;
    ListView_cure: TListView;
    TreeView2: TTreeView;
    TreeView_now: TTreeView;
    TreeView_old: TTreeView;
    TreeView_next: TTreeView;
    Label2: TLabel;
    Button5: TButton;
    Button_alter_appoint: TButton;
    Button_alter_disorders: TButton;
    GroupBox8: TGroupBox;
    Tabcontrol3: TTabControl;
    Patspecials: TTabItem;
    Lechenie: TTabItem;
    Button2: TButton;
    FDQuery_sortdisorders: TFDQuery;
    TreeView_delcure: TTreeView;
    FDQuery_loadapp: TFDQuery;
    Button_delapp: TButton;
    CheckBox_sorttreat: TCheckBox;
    TreeView_exercises: TTreeView;
    DateEdit_startofex: TDateEdit;
    GroupBox9: TGroupBox;
    GroupBox10: TGroupBox;
    FDQuery_loadex: TFDQuery;
    Label3: TLabel;
    Label4: TLabel;
    DateEdit_endofex: TDateEdit;
    Label5: TLabel;
    Edit2: TEdit;
    Label6: TLabel;
    Edit_sec: TEdit;
    Button_optimize: TButton;
    Edit_min: TEdit;
    label_min: TLabel;
    Label_sec: TLabel;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    GroupBox11: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure enterLoginTyping(Sender: TObject);
    procedure EnterPasswordTyping(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TreeView1ChangeCheck(Sender: TObject);
    procedure Populateuserdata(Listview: TListView);
    procedure Button3Click(Sender: TObject);
    procedure ListView3DblClick(Sender: TObject);
    procedure TreeView2ChangeCheck(Sender: TObject);
    procedure Populateinitiallist(const ListView: TListView);
    procedure Button5Click(Sender: TObject);
    procedure Populatedisorderslist(const TreeView: TTreeView);
    procedure ListView_cureChange(Sender: TObject);
    procedure ListView_cureClick(Sender: TObject);
    procedure CheckTreeViewItemsForPatient(TreeItem: TTreeViewItem; DisorderIDs: TList<Integer>);
    procedure UncheckAllTreeViewItems(TreeItem: TTreeViewItem);
    procedure Button_alter_disordersClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure TreeView_delcureChangeCheck(Sender: TObject);
    procedure Button_delappClick(Sender: TObject);
    procedure Loadexercises(const TreeView: TTreeView);
    procedure CheckBox_sorttreatChange(Sender: TObject);
    procedure Edit2Typing(Sender: TObject);
    procedure Edit_secTyping(Sender: TObject);
    procedure DateEdit_startofexChange(Sender: TObject);
    procedure DateEdit_endofexChange(Sender: TObject);
    procedure SaveExercisesToDB;
    procedure Button_alter_appointClick(Sender: TObject);
    procedure TreeView_exercisesChangeCheck(Sender: TObject);
    procedure MergeTreeViewNodes(TreeView: TTreeView);
    procedure Button_optimizeClick(Sender: TObject);
    procedure CloneTreeItem(SourceItem, TargetParent: TTreeViewItem);
    procedure Edit_minTyping(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Deleteuser(ListView: TListView);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    function GenerateRandomPassword: string;
    function CheckExistenceFunc(const AUserName: string; AConnection: TFDConnection): Boolean;
    function GenerateUserName(const FirstName, LastName: string; AConnection: TFDConnection): string;
    function FilterNonEnglishCharacters(const AText: string): string;
    function  AddAppointmentToListView(Treeview: TTreeview; FDQuery_patappointments: TFDQuery): TTreeViewItem;
    function FilterNonDecimalCharacters(const AText: string): string;
    function IsAnyTreeViewItemChecked(ATreeView: TTreeView): Boolean;
     end;

var
  Form13: TForm13;

implementation

{$R *.fmx}



procedure TForm13.Button1Click(Sender: TObject);
  var
hashedPassword,nothashedPassword: string;
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
  enterLogin.Text := NewUserName;
  enterPassword.Text := GenerateRandomPassword;
  hashedPassword := THashMD5.GetHashString(enterPassword.Text);
  nothashedPassword := enterPassword.Text;
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
    FDQuery_newpatient.ParamByName('AssiPassword').AsString := nothashedPassword;
    // Execute the SQL statement
    FDQuery_newpatient.ExecSQL;
   finally
    FDQuery_newpatient.Close;
   ShowMessage('����������� ��������� �������� ������� ������!');
   Populateinitiallist(Listview_cure);
     Populateinitiallist(Listview1);
   end;

  end
  else
  if enterLogin.Text.IsEmpty or enterPassword.Text.IsEmpty then
     begin
  ShowMessage('������� ��� �������')
     end
  else
  begin
    If CheckExistenceFunc(enterLogin.Text,FDConnection1) then
    begin
     ShowMessage('��� ������������ ��� ����������')
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
    ShowMessage('����������� ��������� �������� ������� ������!');
    Populateinitiallist(Listview_cure);
    Populateinitiallist(Listview1);
    enterName.Text := '';
        enterSurname.Text := '';
            enterName2.Text := '';
        end;
    end;
      end;
    end;

 procedure TForm13.Populateinitiallist(const ListView: TListView);
begin
FDQuery_patientlist.SQL.Text := 'SELECT Name, Surname, idPatients FROM patients';
FDQuery_patientlist.Open;
ListView.Items.Clear;
ListView.Items.BeginUpdate;
while not FDQuery_patientlist.Eof do
begin
  with ListView.Items.Add do
  begin
   Text := FDQuery_patientlist.FieldByName('Surname').AsString + ' ' +
            FDQuery_patientlist.FieldByName('Name').AsString;
   Tag := FDQuery_patientlist.FieldByName('idPatients').AsInteger;
  end;
    FDQuery_patientlist.Next;
end;
  ListView.Items.EndUpdate;
  FDQuery_patientlist.Close;
  end;

  procedure TForm13.FormCreate(Sender: TObject);
begin
 groupbox4.Visible:=false;
 DateEdit_startofex.Date := Now;
 DateEdit_endofex.Date := Now;
 ListView1.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
 ListView3.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
 ListView_cure.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
 FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'wersusche';
    FDConnection1.Params.Values['Password'] := 'tyjer1987';
    FDConnection1.Params.Values['Server'] := 'db4free.net';
    FDConnection1.Params.Values['CharacterSet'] := 'utf8mb4';
        // Connection settings


Populateinitiallist(Listview1);
Populateinitiallist(Listview_cure);
Populatedisorderslist(Treeview1);

FDQuery_totalpatients.SQL.Text := 'SELECT (SELECT COUNT(idPatients) FROM patients) AS total_patients, ' +
                                  '(SELECT COUNT(DISTINCT a.idPatients) FROM appointments a WHERE NOW() ' +
                                   'BETWEEN a.Starttime AND a.Endtime) AS active_patients, ' +
                                   '(SELECT COUNT(DISTINCT a.idPatients) FROM appointments a ' +
                                   'WHERE (NOW() BETWEEN a.Starttime AND a.Endtime) AND done_percent < 70) as bad_patients';
FDQuery_totalpatients.Open;
Label_allusers.Text:= Format('����� ��������� � ����: %d', [FDQuery_totalpatients.FieldByName('total_patients').AsInteger]);
Label_activeusers.Text:= Format('��������� � ������������ ������������:  %d', [FDQuery_totalpatients.FieldByName('active_patients').AsInteger]);
Label_badusers.Text:= Format('��������� � ����������� ���������� ����� 70 %%:  %d', [FDQuery_totalpatients.FieldByName('bad_patients').AsInteger]);



FDQuery_openactusers.SQL.Text :=   '(SELECT DISTINCT a.idPatients, p.Name, p.Surname FROM appointments a JOIN patients p ON a.idPatients = p.idPatients WHERE NOW() ' +
                                   'BETWEEN a.Starttime AND a.Endtime) ';
FDQuery_openactusers.Open;
 FDQuery_openactusers.First;
while not FDQuery_openactusers.Eof do
begin

 with ListView3.Items.Add do
  begin
   Text := FDQuery_openactusers.FieldByName('Surname').AsString + ' ' + FDQuery_openactusers.FieldByName('Name').AsString;
   Tag := FDQuery_openactusers.FieldByName('idPatients').AsInteger;
  end;

  FDQuery_openactusers.Next;
end;


FDQuery_baduserslist.SQL.Text :=   'SELECT DISTINCT subquery.idPatients, subquery.Name, subquery.Surname ' +
                                   'FROM ( SELECT a.idAppointments,  a.idPatients, a.sdelanovsego, p.Name, p.Surname, ' +
                                   'SUM(DATEDIFF(NOW(), a.starttime) * a.kolvden) AS ideal_appointments FROM appointments a ' +
    								               'JOIN patients p ON a.idPatients = p.idPatients ' +
                                   'WHERE NOW() BETWEEN a.starttime AND a.endtime GROUP BY a.idPatients, a.idAppointments, a.sdelanovsego ' +
                                   'HAVING  a.sdelanovsego <= (0.7*ideal_appointments) ) as subquery ';
FDQuery_baduserslist.Open;
 FDQuery_baduserslist.First;
while not FDQuery_baduserslist.Eof do
begin
 // ComboBox3.Items.Add(FDQuery_baduserslist.FieldByName('Surname').AsString+ ' ' + FDQuery_baduserslist.FieldByName('Name').AsString);
  FDQuery_baduserslist.Next;
end;
// Close the query
FDQuery_openactusers.Close;
FDQuery_totalpatients.Close;
end;


function TForm13.GenerateRandomPassword: string;
var
  i: Integer;
  DigitsAtStart: Boolean;
const
  Letters = 'abcdefghijkmnopqrstuvwxyz';
  Digits = '0123456789';
begin
  Randomize;
  Result := '';

  // Randomly decide if digits should be at the start
  DigitsAtStart := Random(2) = 0; // This will be true approximately 50% of the time

  if DigitsAtStart then
    // Generate 2 digits
    for i := 1 to 2 do
      Result := Result + Digits[Random(10) + 1];

  // Generate 4 letters
  for i := 1 to 4 do
    Result := Result + Letters[Random(Length(Letters)) + 1];

  if not DigitsAtStart then
    // Generate 2 digits
    for i := 1 to 2 do
      Result := Result + Digits[Random(10) + 1];
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

begin
Populatedisorderslist(Treeview1);
  end;


procedure TForm13.Button3Click(Sender: TObject);
begin
Groupbox4.Visible:=False;
Groupbox7.Visible := True;
Button3.Visible := False;
end;



procedure TForm13.Button4Click(Sender: TObject);
begin
Populateinitiallist(Listview1);
end;

procedure TForm13.Button_optimizeClick(Sender: TObject);
begin
MergeTreeViewNodes(TreeView_delcure);
end;

procedure TForm13.Button5Click(Sender: TObject);
begin
Populateinitiallist(Listview_cure);
TabControl3.Enabled := ListView_cure.ItemIndex <> -1;
end;

procedure TForm13.Button6Click(Sender: TObject);
begin
if Listview_cure.Selected.Index <> -1 then
begin
Deleteuser(ListView_cure);
Populateinitiallist(Listview_cure);
Populateinitiallist(Listview1);
TabControl3.Enabled := ListView_cure.ItemIndex <> -1;
end
else
  ShowMessage('������� �� ������');
end;

procedure TForm13.Button7Click(Sender: TObject);
begin
if Listview1.ItemIndex <> -1 then
begin
Deleteuser(ListView1);
Populateinitiallist(Listview1);
TabControl3.Enabled := ListView_cure.ItemIndex <> -1;
end
else
  ShowMessage('������� �� ������');
end;

procedure TForm13.Button_alter_appointClick(Sender: TObject);
begin
if IsAnyTreeViewItemChecked(TreeView_exercises) then
begin
SaveExercisesToDB;
Loadexercises(TreeView_exercises);
     FDQuery_loadapp.Close;
    FDQuery_loadapp.SQL.Text := 'SELECT a.Starttime, a.Endtime, a.kolvden, a.sdelanovsego, a.done_percent, v.video_name, a.idAppointments ' +
                                         'FROM appointments a ' +
                                         'LEFT JOIN videos v ON a.idvideos = v.idvideos ' +
                                         'WHERE a.idPatients = :ID';

    FDQuery_loadapp.ParamByName('ID').AsInteger := ListView_cure.Selected.Tag;
    FDQuery_loadapp.Open;

     try
     TreeView_delcure.Clear;
    // Assume FDQuery1 has been prepared and opened to select your fields
    while not FDQuery_loadapp.EOF do
    begin
    if (InRange(Now(), FDQuery_loadapp.FieldByName('Starttime').AsDateTime, FDQuery_loadapp.FieldByName('Endtime').AsDateTime)) or
         (Now() < FDQuery_loadapp.FieldByName('Starttime').AsDateTime) then
        begin
     AddAppointmentToListView(TreeView_delcure, FDQuery_loadapp);
     FDQuery_loadapp.Next;
      end
      else
      FDQuery_loadapp.Next;
    end;
     finally
     FDQuery_loadapp.Close;
  end;
end
else
  ShowMessage('�� ������� ����������!');
end;

procedure TForm13.Button_alter_disordersClick(Sender: TObject);
var
  i, j: Integer;
  TreeItem, ChildItem: TTreeViewItem;
  ListViewTag, TreeViewTag: Integer;
  FDQuery: TFDQuery; // Assuming you have this object set up for your database
begin
  if ListView_cure.Selected <> nil then
  begin
    ListViewTag := ListView_cure.Selected.Tag; // patient_id
    FDQuery := TFDQuery.Create(nil); // Create a new query object
    try
      FDQuery.Connection := FDConnection1; // Assign the FireDAC connection

      for i := 0 to TreeView1.Count - 1 do
      begin
        TreeItem := TreeView1.Items[i];
        for j := 0 to TreeItem.Count - 1 do
        begin
          ChildItem := TreeItem.Items[j];
          TreeViewTag := ChildItem.Tag;
          if ChildItem.IsChecked then
          begin
             // Prepare the SQL query for insertion
            FDQuery.SQL.Text := 'INSERT IGNORE INTO patient_disorders (patient_id, disorder_id) VALUES (:pid, :did)';
          end
          else
          begin
            // Prepare the SQL query for deletion
            FDQuery.SQL.Text := 'DELETE FROM patient_disorders WHERE patient_id = :pid AND disorder_id = :did';
          end;
            // Prepare the SQL query
            FDQuery.ParamByName('pid').AsInteger := ListViewTag;
            FDQuery.ParamByName('did').AsInteger := TreeViewTag;

            // Execute the SQL command
            FDQuery.ExecSQL;
          end;
        end;
      finally
      FDQuery.Free; // Free the query object
    end;
    Loadexercises(TreeView_exercises);
    ShowMessage('��������� ������� ���������!');
  end
  else
  Showmessage('������� �� ������!');
  end;

procedure TForm13.Button_delappClick(Sender: TObject);
var
  i, j: Integer;
  TreeItem, ChildItem: TTreeViewItem;
  TreeViewTag: Integer;
  FDQuery: TFDQuery; // Assuming you have this object set up for your database
begin
  if ListView_cure.Selected <> nil then
  begin

    FDQuery := TFDQuery.Create(nil); // Create a new query object
    try
      FDQuery.Connection := FDConnection1; // Assign the FireDAC connection

 for i := 0 to TreeView_delcure.Count - 1 do
  begin
    TreeItem := TreeView_delcure.Items[i];
    TreeViewTag := TreeItem.Tag;
    if TreeItem.IsChecked then
    begin
      // Prepare and execute SQL query for parent item
      FDQuery.SQL.Text := 'DELETE FROM appointments WHERE idAppointments = :did';
      FDQuery.ParamByName('did').AsInteger := TreeViewTag;
      FDQuery.ExecSQL;
    end;

    // Loop through child items
    for j := 0 to TreeItem.Count - 1 do
    begin
      ChildItem := TreeItem.Items[j];
      TreeViewTag := ChildItem.Tag;
      if ChildItem.IsChecked then
      begin
        // Prepare and execute SQL query for child item
        FDQuery.SQL.Text := 'DELETE FROM appointments WHERE idAppointments = :did';
        FDQuery.ParamByName('did').AsInteger := TreeViewTag;
        FDQuery.ExecSQL;
      end;
    end;
  end;

      finally
      FDQuery.Free; // Free the query object

      end;
  end
  else
  begin
  Showmessage('������� �� ������!');
  end;

  TreeView_delcure.Clear;
  FDQuery_loadapp.Close;
  FDQuery_loadapp.SQL.Text := 'SELECT a.Starttime, a.Endtime, a.kolvden, a.sdelanovsego, a.done_percent, v.video_name, a.idAppointments ' +
                                         'FROM appointments a ' +
                                         'LEFT JOIN videos v ON a.idvideos = v.idvideos ' +
                                         'WHERE a.idPatients = :ID';

    FDQuery_loadapp.ParamByName('ID').AsInteger := ListView_cure.Selected.Tag;
    FDQuery_loadapp.Open;

     try
    // Assume FDQuery1 has been prepared and opened to select your fields
    while not FDQuery_loadapp.EOF do
    begin
    if (InRange(Now(), FDQuery_loadapp.FieldByName('Starttime').AsDateTime, FDQuery_loadapp.FieldByName('Endtime').AsDateTime)) or
         (Now() < FDQuery_loadapp.FieldByName('Starttime').AsDateTime) then
        begin
     AddAppointmentToListView(TreeView_delcure, FDQuery_loadapp);
     FDQuery_loadapp.Next;
      end
      else
      FDQuery_loadapp.Next;
    end;
     finally

    FDQuery_loadapp.Close;

   end;
end;

procedure TForm13.CheckBox_sorttreatChange(Sender: TObject);
begin
Loadexercises(TreeView_exercises);
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
begin
Populateuserdata(Listview1)
end;

procedure TForm13.ListView3DblClick(Sender: TObject);
begin
Populateuserdata(Listview3);
end;

procedure TForm13.ListView_cureChange(Sender: TObject);
var
  i: Integer;
  DisorderIDs: TList<Integer>;
  TreeItem: TTreeViewItem;
begin
TabControl3.Enabled := ListView_cure.ItemIndex <> -1;
TreeView_delcure.Clear;
  for i := 0 to TreeView1.Count - 1 do
  begin
    UncheckAllTreeViewItems(TreeView1.Items[i]);
  end;

DisorderIDs := TList<Integer>.Create;
FDQuery_sortdisorders.SQL.Text := 'SELECT disorder_id FROM patient_disorders where patient_id = :ID';
FDQuery_sortdisorders.ParamByName('ID').AsInteger := ListView_cure.Selected.Tag;
FDQuery_sortdisorders.Open;

      while not FDQuery_sortdisorders.Eof do
      begin
        DisorderIDs.Add(FDQuery_sortdisorders.FieldByName('disorder_id').AsInteger);
        FDQuery_sortdisorders.Next;
      end;

      FDQuery_sortdisorders.Close;

for i := 0 to TreeView1.Count - 1 do
  begin
    CheckTreeViewItemsForPatient(TreeView1.Items[i], DisorderIDs);
  end;

    FDQuery_loadapp.Close;
     FDQuery_loadapp.SQL.Text := 'SELECT a.Starttime, a.Endtime, a.kolvden, a.sdelanovsego, a.done_percent, ' +
                            'v.video_name, a.idAppointments, p.Username, p.Password ' + // Added Username from patients table
                            'FROM appointments a ' +
                            'LEFT JOIN videos v ON a.idvideos = v.idvideos ' +
                            'LEFT JOIN patients p ON a.idPatients = p.idPatients ' + // Join with patients table
                            'WHERE a.idPatients = :ID';

    FDQuery_loadapp.ParamByName('ID').AsInteger := ListView_cure.Selected.Tag;
    FDQuery_loadapp.Open;

    Label7.Text := FDQuery_loadapp.FieldByName('Username').AsString;
     Label8.Text := FDQuery_loadapp.FieldByName('Password').AsString;
     try
    // Assume FDQuery1 has been prepared and opened to select your fields
    while not FDQuery_loadapp.EOF do
    begin
    if (InRange(Now(), FDQuery_loadapp.FieldByName('Starttime').AsDateTime, FDQuery_loadapp.FieldByName('Endtime').AsDateTime)) or
         (Now() < FDQuery_loadapp.FieldByName('Starttime').AsDateTime) then
        begin
     AddAppointmentToListView(TreeView_delcure, FDQuery_loadapp);
     FDQuery_loadapp.Next;
      end
      else
      FDQuery_loadapp.Next;
    end;
     finally
     FDQuery_loadapp.Close;

   Loadexercises(TreeView_exercises);

  end;






end;

procedure TForm13.ListView_cureClick(Sender: TObject);

begin
TabControl3.Enabled := ListView_cure.ItemIndex <> -1;
end;

procedure TForm13.TreeView1ChangeCheck(Sender: TObject);
begin
if TTreeViewItem(Sender).Count > 0 then
begin
TTreeViewItem(Sender).IsChecked := False;
end;
end;

procedure TForm13.TreeView2ChangeCheck(Sender: TObject);
begin
if TTreeViewItem(Sender).Count = 0 then
begin
TTreeViewItem(Sender).IsChecked := False;
end;
end;

procedure TForm13.TreeView_delcureChangeCheck(Sender: TObject);
begin

if TTreeViewItem(Sender).Count = 0 then
begin
TTreeViewItem(Sender).IsChecked := False;
end;

  for var i := 0 to TreeView_delcure.Count - 1 do
  begin
    if TTreeViewItem(Sender).IsChecked then
    begin
      Button_delapp.enabled := True;
      Exit;
    end
    else
    Button_delapp.enabled := False;
  end;
end;

procedure TForm13.TreeView_exercisesChangeCheck(Sender: TObject);
  var
  i: Integer;
  ParentItem, ChildItem: TTreeViewItem;
begin
ParentItem := TTreeViewItem(Sender);
if ParentItem.Count > 0 then
begin
for  i := 0 to ParentItem.Count - 1 do
    begin
      ChildItem := ParentItem.Items[i];
      ChildItem.IsChecked := True;
    end;
//ParentItem.IsChecked := False;
end;
end;

function TForm13.AddAppointmentToListView(Treeview: TTreeView; FDQuery_patappointments: TFDQuery): TTreeViewItem;
var
  ParentNode, ChildNode, ChildNode2: TTreeViewItem;
begin
   TreeView.BeginUpdate;

    ParentNode := TTreeViewItem.Create(TreeView);
    ParentNode.Parent := TreeView;
    ParentNode.Text := '���������� � ' + FDQuery_patappointments.FieldByName('Starttime').AsString +
                       ' �� ' + FDQuery_patappointments.FieldByName('Endtime').AsString;
    ParentNode.Tag := FDQuery_patappointments.FieldByName('idAppointments').AsInteger;
  if (InRange(Now(), FDQuery_patappointments.FieldByName('Starttime').AsDateTime, FDQuery_patappointments.FieldByName('Endtime').AsDateTime) and
   (FDQuery_patappointments.FieldByName('done_percent').AsInteger < 70)) or
  ((Now() > FDQuery_patappointments.FieldByName('Endtime').AsDateTime) and (FDQuery_patappointments.FieldByName('done_percent').AsInteger < 70)) then

  begin
   ParentNode.StyledSettings := ParentNode.StyledSettings -  [TStyledSetting.FontColor];
   ParentNode.TextSettings.FontColor := TAlphaColorRec.Red;
   end;

    // Create child nodes for the details
    ChildNode := TTreeViewItem.Create(ParentNode);
    ChildNode.Parent := ParentNode;
    ChildNode.Text := '����������: ' + FDQuery_patappointments.FieldByName('video_name').AsString;
    ChildNode.Tag := FDQuery_patappointments.FieldByName('idAppointments').AsInteger;

    ChildNode2 := TTreeViewItem.Create(ChildNode);
    ChildNode2.Parent := ChildNode;
    ChildNode2.Text := '���������� � ����: ' + FDQuery_patappointments.FieldByName('kolvden').AsString;

    ChildNode2 := TTreeViewItem.Create(ChildNode);
    ChildNode2.Parent := ChildNode;
    ChildNode2.Text := '������� �����: ' + FDQuery_patappointments.FieldByName('sdelanovsego').AsString;

    ChildNode2 := TTreeViewItem.Create(ChildNode);
    ChildNode2.Parent := ChildNode;
    ChildNode2.Text := '������� ����������: ' + FDQuery_patappointments.FieldByName('done_percent').AsString + '%';

    TreeView.EndUpdate;

  end;




  procedure TForm13.Populateuserdata(Listview: TListView);
  var
  SelectedID: Integer;
  YearsDiff: Double;
  ListItem, ListItem2, listnow, listnext, listold: TTreeViewItem;
  Row: Integer;
    DailyExerciseCount, TotalExerciseCount: Integer;
  DaysPassed, ProjectedExerciseCount: Integer;
    CompletionRate: Double;
      ParentNode, ChildNode: TTreeViewItem;
  TypeField, NameField: TField;
      TypeDict: TDictionary<string, TTreeViewItem>;
        DisorderType, DisorderName: string;
        NoDisorders: Boolean;
begin
  // Check if an item is selected
  TreeView2.Clear;
  TreeView_now.Clear;
  TreeView_old.Clear;
  TreeView_next.Clear;
  if Assigned(ListView.Selected) then
  begin
     GroupBox7.Visible :=false;
     GroupBox4.Visible :=true;
     Button3.Visible := True;
    // Get the ID stored in the selected item's Tag property
    SelectedID := ListView.Selected.Tag;


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
      end;

   TreeView2.BeginUpdate;
  TypeDict := TDictionary<string, TTreeViewItem>.Create;
  NoDisorders := True;
  try
    TreeView2.Clear;
     while not FDQuery_patientpersonal.Eof do
         begin
     TypeField := FDQuery_patientpersonal.FindField('Disorder_type');
      NameField := FDQuery_patientpersonal.FindField('Disorder_name');

    if Assigned(TypeField) and not TypeField.IsNull and
    Assigned(NameField) and not NameField.IsNull then
      // Check if we've moved on to a new Disorder_type
       begin
       NoDisorders := False;
        DisorderType := TypeField.AsString;
        DisorderName := NameField.AsString;

        // Look for existing parent TreeView item for this Disorder_type
        if not TypeDict.TryGetValue(DisorderType, ParentNode) then
        begin
          // Create a new parent TreeView item for this Disorder_type
          ParentNode := TTreeViewItem.Create(TreeView2);
          ParentNode.Parent := TreeView2;
          ParentNode.Text := DisorderType;
          ParentNode.Expand;  // Expand the parent item to show children
          // Add to dictionary for future lookup
          TypeDict.Add(DisorderType, ParentNode);
        end;

        // Create child item for Disorder_name
        ChildNode := TTreeViewItem.Create(ParentNode);
        ChildNode.Parent := ParentNode;
        ChildNode.Text := DisorderName;
      end;

      FDQuery_patientpersonal.Next;
    end;
    if NoDisorders then
    begin
      ParentNode := TTreeViewItem.Create(TreeView2);
      ParentNode.Parent := TreeView2;
      ParentNode.Text := '� �������� �� ������� ���������!';
    end;

  finally
    TreeView2.EndUpdate;
    TypeDict.Free;
  end;

        finally
  FDQuery_patientpersonal.Close;
        end;

    FDQuery_patappointments.Close;
    FDQuery_patappointments.SQL.Text := 'SELECT a.Starttime, a.Endtime, a.kolvden, a.sdelanovsego, a.done_percent, v.video_name, a.idAppointments ' +
                                         'FROM appointments a ' +
                                         'LEFT JOIN videos v ON a.idvideos = v.idvideos ' +
                                         'WHERE a.idPatients = :ID';
      FDQuery_patappointments.ParamByName('ID').AsInteger := SelectedID;
       FDQuery_patappointments.Open;

  try
    // Assume FDQuery1 has been prepared and opened to select your fields
    while not FDQuery_patappointments.EOF do
    begin
    if InRange(Now(), FDQuery_patappointments.FieldByName('Starttime').AsDateTime, FDQuery_patappointments.FieldByName('Endtime').AsDateTime) then
        begin
     listnow:= AddAppointmentToListView(TreeView_now, FDQuery_patappointments);

//    if CompletionRate < 70 then
//    begin
//     listnow.Objects.TextObject.TextColor:= TAlphaColors.Red;
//     listnow.Objects.DetailObject.TextColor := TAlphaColors.Red;
//    end;
     FDQuery_patappointments.Next;
       end
     else if Now() > FDQuery_patappointments.FieldByName('Endtime').AsDateTime then
      begin
      listold:= AddAppointmentToListView(TreeView_old, FDQuery_patappointments);
      FDQuery_patappointments.Next;
      end
      else if Now() < FDQuery_patappointments.FieldByName('Starttime').AsDateTime then
      begin
      listnext:= AddAppointmentToListView(TreeView_next, FDQuery_patappointments);
      FDQuery_patappointments.Next;
      end;

    end;
     finally

    FDQuery_patappointments.Close;
  end;
  end;
end;

 procedure TForm13.Populatedisorderslist(const TreeView: TTreeView);
var
  CurrentType: string;
  CurrentTypeNode, DisorderNode: TTreeViewItem;
begin
FDQuery_disorders.SQL.Text := 'SELECT d.idDisorders, d.Disorder_name, d.Disorder_type FROM disorders d ORDER BY Disorder_Type';
FDQuery_disorders.Open;
CurrentType := '';
TreeView.Clear;

while not FDQuery_disorders.Eof do
    begin
 if CurrentType <> FDQuery_disorders.FieldByName('Disorder_type').AsString then
    begin
      CurrentType := FDQuery_disorders.FieldByName('Disorder_type').AsString;
      CurrentTypeNode := TTreeViewItem.Create(TreeView1);
      CurrentTypeNode.Text := CurrentType;
      TreeView.AddObject(CurrentTypeNode);
    end;

    DisorderNode := TTreeViewItem.Create(CurrentTypeNode);
    DisorderNode.Text := FDQuery_disorders.FieldByName('Disorder_name').AsString;
    DisorderNode.Tag :=  FDQuery_disorders.FieldByName('idDisorders').AsInteger;
    CurrentTypeNode.AddObject(DisorderNode);

    FDQuery_disorders.Next;
  end;

  FDQuery_disorders.Close;
 end;


 procedure TForm13.CheckTreeViewItemsForPatient(TreeItem: TTreeViewItem; DisorderIDs: TList<Integer>);
 var
  i: Integer;
  ChildItem: TTreeViewItem;
 begin
   // Check if this item should be checked
  if DisorderIDs.Contains(TreeItem.Tag) then
  begin
    TreeItem.IsChecked := True;
    TreeItem.ParentItem.IsExpanded := True;
  end
  else
  begin
   TreeItem.IsExpanded := False;
  end;

  // Recursively call this procedure for all child items
  for i := 0 to TreeItem.Count - 1 do
  begin
    ChildItem := TreeItem.Items[i];
    CheckTreeViewItemsForPatient(ChildItem, DisorderIDs);
  end;
 end;

 procedure TForm13.DateEdit_endofexChange(Sender: TObject);
begin
  if DateEdit_endofex.DateTime < DateEdit_startofex.DateTime then
    DateEdit_startofex.DateTime := DateEdit_endofex.DateTime;
end;

procedure TForm13.DateEdit_startofexChange(Sender: TObject);
begin
   if DateEdit_startofex.DateTime < Now then
   begin
    DateEdit_startofex.DateTime := Now;
   end;

   if DateEdit_startofex.DateTime > DateEdit_endofex.DateTime then
   begin
    DateEdit_endofex.DateTime := DateEdit_startofex.DateTime
   end;
end;

procedure TForm13.Edit2Typing(Sender: TObject);
begin
  Edit2.Text := FilterNonDecimalCharacters(Edit2.Text);
  Edit2.CaretPosition := Edit2.Text.Length;  // move the caret to the end
end;

procedure TForm13.Edit_minTyping(Sender: TObject);
var
Value : Integer;
begin
  Edit_sec.Text := FilterNonDecimalCharacters(Edit_sec.Text);
  Edit_sec.CaretPosition := Edit_sec.Text.Length;  // move the caret to the end
  if TryStrToInt(Edit_sec.Text, Value) then
  begin
    if Value > 60 then
    begin
      ShowMessage('������� ����� ����� 0 � 60.');
      Edit_sec.Text := Edit_sec.Text.Substring(0, Edit_sec.Text.Length - 1);
      Edit_sec.CaretPosition := Edit_sec.Text.Length;  // move the caret to the end
    end;
  end
  else
  begin
    // Handle cases where Edit_sec.Text is not a valid integer (e.g., it's empty)
    // Here you can handle this case or just do nothing.
  end;
end;

procedure TForm13.Edit_secTyping(Sender: TObject);
var
Value : Integer;
begin
  Edit_sec.Text := FilterNonDecimalCharacters(Edit_sec.Text);
  Edit_sec.CaretPosition := Edit_sec.Text.Length;  // move the caret to the end
  if TryStrToInt(Edit_sec.Text, Value) then
  begin
    if Value > 60 then
    begin
      ShowMessage('������� ����� ����� 0 � 60.');
      Edit_sec.Text := Edit_sec.Text.Substring(0, Edit_sec.Text.Length - 1);
      Edit_sec.CaretPosition := Edit_sec.Text.Length;  // move the caret to the end
    end;
  end
  else
  begin
    // Handle cases where Edit_sec.Text is not a valid integer (e.g., it's empty)
    // Here you can handle this case or just do nothing.
  end;
end;

procedure TForm13.UncheckAllTreeViewItems(TreeItem: TTreeViewItem);
var
  i: Integer;
  ChildItem: TTreeViewItem;
begin
  // Uncheck this item
  TreeItem.IsChecked := False;

  // Recursively call this procedure for all child items
  for i := 0 to TreeItem.Count - 1 do
  begin
    ChildItem := TreeItem.Items[i];
    UncheckAllTreeViewItems(ChildItem);
  end;
end;

 procedure TForm13.Loadexercises(const TreeView: TTreeView);
var
  FDQuery: TFDQuery;
  TreeItem: TTreeViewItem;
  CurrentType: string;
  CurrentTypeNode, ExerciseNode: TTreeViewItem;
begin
  FDQuery := TFDQuery.Create(nil);
    try
    FDQuery.Connection := FDConnection1;
    if CheckBox_sorttreat.IsChecked then
    begin
    CurrentType := '';
    FDQuery.SQL.Text := 'SELECT v.video_name, v.idvideos, v.ex_type ' +
                  'FROM videos v ' +
                  'WHERE v.idvideos NOT IN (SELECT dv.videos_id FROM disorder_videos dv ' +
                  'INNER JOIN patient_disorders pd ON dv.disorder_id = pd.disorder_id ' +
                  'WHERE pd.patient_id = :PatientID) ORDER BY v.ex_type';


//    FDQuery.SQL.Text := 'SELECT v.video_name ' +
//                  'FROM videos v ' +
//                  'INNER JOIN disorder_videos dv ON v.idvideos = dv.videos_id  ' +
//                  'INNER JOIN patient_disorders pd ON dv.disorder_id = pd.disorder_id ' +
//                  'WHERE pd.patient_id = :PatientID';

    FDQuery.ParamByName('PatientID').AsInteger := ListView_cure.Selected.Tag; ;
    FDQuery.Open;
    TreeView.Clear;
    while not FDQuery.Eof do
    begin
 if CurrentType <> FDQuery.FieldByName('ex_type').AsString then
    begin
      CurrentType := FDQuery.FieldByName('ex_type').AsString;
      CurrentTypeNode := TTreeViewItem.Create(TreeView1);
      CurrentTypeNode.Text := CurrentType;
      TreeView.AddObject(CurrentTypeNode);
      //CurrentTypeNode.IsExpanded := True;
    end;

    ExerciseNode := TTreeViewItem.Create(CurrentTypeNode);
    ExerciseNode.Text := FDQuery.FieldByName('video_name').AsString;
    ExerciseNode.Tag :=  FDQuery.FieldByName('idvideos').AsInteger;
    CurrentTypeNode.AddObject(ExerciseNode);
    FDQuery.Next;
  end;

    end
    else if not CheckBox_sorttreat.IsChecked then
    begin
    FDQuery.SQL.Text := 'SELECT v.video_name, v.idvideos, v.ex_type ' +
                  'FROM videos v ORDER BY v.ex_type ';


//    FDQuery.SQL.Text := 'SELECT v.video_name ' +
//                  'FROM videos v ' +
//                  'INNER JOIN disorder_videos dv ON v.idvideos = dv.videos_id  ' +
//                  'INNER JOIN patient_disorders pd ON dv.disorder_id = pd.disorder_id ' +
//                  'WHERE pd.patient_id = :PatientID';

    FDQuery.Open;
    CurrentType := '';
    TreeView.Clear;
    while not FDQuery.Eof do
    begin
 if CurrentType <> FDQuery.FieldByName('ex_type').AsString then
    begin
      CurrentType := FDQuery.FieldByName('ex_type').AsString;
      CurrentTypeNode := TTreeViewItem.Create(TreeView1);
      CurrentTypeNode.Text := CurrentType;
      TreeView.AddObject(CurrentTypeNode);
    end;

    ExerciseNode := TTreeViewItem.Create(CurrentTypeNode);
    ExerciseNode.Text := FDQuery.FieldByName('video_name').AsString;
    ExerciseNode.Tag :=  FDQuery.FieldByName('idvideos').AsInteger;
    CurrentTypeNode.AddObject(ExerciseNode);
    FDQuery.Next;
  end;
    end;

  finally
    FDQuery.Free;
  end;
end;

function TForm13.FilterNonDecimalCharacters(const AText: string): string;
var
  C: Char;
begin
  Result := '';
  for C in AText do
    if CharInSet(C,['0'..'9']) then
      Result := Result + C;
end;



  procedure TForm13.SaveExercisesToDB;
var
  i: Integer;
  TreeItem, ChildItem: TTreeViewItem;
  ExerciseID,PatientID, ExerciseAmount: Integer;
  ExerciseLongevity: String;
  StartDate, EndDate: TDateTime;
  FDQuery: TFDQuery;
  IntersectCount: Integer;
  UserChoice: Integer;
  label
  insertentry;
begin
  // Collect data from other controls
  ExerciseAmount := StrToIntDef(Edit2.Text, 1);
  ExerciseLongevity := '00:'+Edit_min.Text+':'+Edit_sec.Text;
  StartDate := DateEdit_startofex.Date;
  EndDate := DateEdit_endofex.Date;
  PatientID :=   ListView_cure.Selected.Tag;
  // Create and prepare a FireDAC query
  FDQuery := TFDQuery.Create(nil);
  try
    FDQuery.Connection := FDConnection1;  // Your FireDAC Connection component
    // Loop through all TreeView items
    for i := 0 to TreeView_exercises.Count - 1 do
    begin
      TreeItem := TreeView_exercises.Items[i];
      for var j := 0 to TreeItem.Count - 1 do
    begin
      ChildItem := TreeItem.Items[j];

      if ChildItem.IsChecked then
      begin

        ExerciseID := ChildItem.Tag;


        FDQuery.SQL.Text := 'SELECT COUNT(*) FROM appointments WHERE idPatients = :patient_id ' +
                            'AND idvideos = :exercise_id AND ((:start_date BETWEEN Starttime AND Endtime) ' +
                            'OR (:end_date BETWEEN Starttime AND Endtime) OR (Starttime BETWEEN :start_date AND :end_date) ' +
                            'OR (Endtime BETWEEN :start_date AND :end_date))';

        FDQuery.ParamByName('exercise_id').AsInteger := ExerciseID;
        FDQuery.ParamByName('start_date').AsDate := StartDate;
        FDQuery.ParamByName('end_date').AsDate := EndDate;
        FDQuery.ParamByName('patient_id').AsInteger := PatientID;
         FDQuery.Open;
         IntersectCount := FDQuery.Fields[0].AsInteger;
         if IntersectCount = 0 then
         begin
         insertentry:
         FDQuery.Close;
         FDQuery.SQL.Text := 'INSERT INTO appointments (Starttime, Endtime, idPatients, idvideos, kolvden, dlitelnost) VALUES (:start_date, :end_date, :patient_id, :exercise_id, :amount, :longevity)';
          // Execute the query
        FDQuery.ParamByName('exercise_id').AsInteger := ExerciseID;
        FDQuery.ParamByName('amount').AsInteger := ExerciseAmount;
        FDQuery.ParamByName('longevity').AsString := ExerciseLongevity;
        FDQuery.ParamByName('start_date').AsDate := StartDate;
        FDQuery.ParamByName('end_date').AsDate := EndDate;
        FDQuery.ParamByName('patient_id').AsInteger := PatientID;
        FDQuery.ExecSQL;
         end
         else
         begin
         UserChoice := MessageDlg('������� ��������� ����������� � ������������� �������� � ������� � ���������� "' +
         TreeItem.Text + '" � ������ � ' + datetostr(StartDate) + ' �� ' + datetostr(EndDate) + '. �� �������, ��� ������ ������ ������?',
         TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0);
         if UserChoice = mrYes then
         goto insertentry;
         end;
      end;
      end;
    end;

  finally
    FDQuery.Free;
  end;

  ShowMessage('���������� ������� �������!');
end;

procedure TForm13.MergeTreeViewNodes(TreeView: TTreeView);
var
  ParentDict: TDictionary<String, TTreeViewItem>;
  OriginalParentNode, NewParentNode, ChildNode, ClonedChildNode: TTreeViewItem;
  i, j: Integer;
  ParentName: String;
begin
  ParentDict := TDictionary<String, TTreeViewItem>.Create;
  try
    // Loop through the TreeView to collect all nodes and find unique parent node names
    for i := TreeView.Count - 1 downto 0 do
    begin
      OriginalParentNode := TreeView.Items[i];
      ParentName := OriginalParentNode.Text;

      if not ParentDict.ContainsKey(ParentName) then
      begin
        // This is a new unique parent node, add it to the dictionary
        ParentDict.Add(ParentName, OriginalParentNode);
      end
      else
      begin
        // This parent node already exists, let's merge
        NewParentNode := ParentDict[ParentName];
        NewParentNode.Tag := 0;
        // Clone all children to the new parent node
        for j := 0 to OriginalParentNode.Count - 1 do
        begin
          ChildNode := OriginalParentNode.Items[j];
          CloneTreeItem(ChildNode, NewParentNode);
          // Copy other properties as needed
        end;

        // Now we can remove the original parent node
        OriginalParentNode.Free;
      end;
    end;
  finally
    ParentDict.Free;
  end;
end;


procedure TForm13.CloneTreeItem(SourceItem, TargetParent: TTreeViewItem);
var
  NewItem, ChildItem: TTreeViewItem;
  i: Integer;
begin
  NewItem := TTreeViewItem.Create(TargetParent);
  NewItem.Parent := TargetParent;
  NewItem.Text := SourceItem.Text;
  NewItem.Tag := SourceItem.Tag;
  // Copy other properties as needed

  for i := 0 to SourceItem.Count - 1 do
  begin
    ChildItem := SourceItem.Items[i];
    CloneTreeItem(ChildItem, NewItem);
  end;
end;


  procedure TForm13.Deleteuser(ListView: TListView);
 var
  FDQuery: TFDQuery;
  PatientID: Integer;
begin
  FDQuery := TFDQuery.Create(nil);
  FDQuery.Connection := FDConnection1;
  PatientID := Listview.Selected.Tag;
        FDQuery.Close;
        FDQuery.SQL.Text := 'START TRANSACTION; DELETE FROM appointments WHERE idPatients = :patient_id; ' +
                            'DELETE FROM patient_disorders WHERE patient_id = :patient_id; ' +
                            'DELETE FROM patients WHERE idPatients = :patient_id; ' +
                            'COMMIT; ';
        FDQuery.ParamByName('patient_id').AsInteger := PatientID;
        FDQuery.ExecSQL;
end;

function TForm13.IsAnyTreeViewItemChecked(ATreeView: TTreeView): Boolean;
var
  i: Integer;
  Item: TTreeViewItem;
begin
  Result := False;
  for i := 0 to ATreeView.Count - 1 do
  begin
    Item := ATreeView.Items[i];
    if Item.IsChecked then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.


