unit Unit12;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.DateUtils,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Media, FMX.Objects,FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait,
  Data.DB, FireDAC.Comp.Client, FMX.Edit, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, System.IOUtils,
  FireDAC.Comp.UI, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, System.Diagnostics, System.IniFiles,
   FMX.TextLayout, Datasnap.DSClientRest, ClientModuleUnit1, Datasnap.DBClient,
  Datasnap.DSConnect;

   type
  TPlaylistItem = record
    VideoID: string;
    PlaybackTime: TTime; // In seconds
    Videoname: string;
    appointmentsID : integer;
    CumulativeTime: Double; // In seconds
  end;

type
  TForm12 = class(TForm)
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDQuery1: TFDQuery;
    MediaPlayer1: TMediaPlayer;
    MediaplayerControl: TMediaPlayerControl;
    Timer1: TTimer;
    tbVolume: TTrackBar;
    bPlayClick: TButton;
    bStopClick: TButton;
    ListBox1: TListBox;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Label2: TLabel;
    FDQuery2: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    Timer4: TTimer;
    Layoutvideo: TLayout;
    Layoutitems: TLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    FDQuery3: TFDQuery;
    Edit1: TEdit;
    Button3: TButton;
    MediaPlayer2: TMediaPlayer;
    Timer_startfading: TTimer;
    Timer_for: TTimer;
    Timer_startrising: TTimer;
    Button1: TButton;
    Button2: TButton;
    StyleBook1: TStyleBook;
    bSkipClick: TButton;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    tbVideo: TTrackBar;
    Button4: TButton;
    DSRestConnection1: TDSRestConnection;
    DSProviderConnection1: TDSProviderConnection;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure bPlayClickClick(Sender: TObject);
    procedure bStopClickClick(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure FDConnection1BeforeConnect(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Timer4Timer(Sender: TObject);
    procedure Timer5Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Timer_startfadingTimer(Sender: TObject);
    procedure Timer_startrisingTimer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure bSkipClickClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);

  private
    { Private declarations }
    FUpdatingTrackBar: Boolean;
    Path: string;
    FWatchedVideosCount: Integer;
    CurrentVolume: Extended;
    FTotalPlaybackTime: Double; // In seconds
    Pusername : string;
    Fullexercisetime: TTime;
    Hour1, Min1, Sec1, MSec1: Word;
    TargetMediaPlayer: TMediaPlayer;
    Stopwatch: TStopwatch;
    IsMP3Loaded: Boolean;
    FirstLoop: Boolean;
    function CalculateTextHeight(const Text: string; const Font: TFont; const Width: Single): Single;
    procedure ListTxtFiles;
    procedure Nextvideoload;
    procedure InitializeVideoTrackBar;
    function TimeInSecondsOf(ATime: TTime): Double;
    procedure UpdateExerciseList;
    procedure SaveSettingsToIniFile;
 procedure UpdateCumulativeTimeInDatabase(const Item: TPlaylistItem);
procedure CompleteExercise(var Item: TPlaylistItem);
 procedure RemoveExerciseFromPlaylist(const Item: TPlaylistItem);


  public
    { Public declarations }
    function GenerateRandomPassword: string;
    function GetVideoFilePath(VideoID: string): string;
    function LoadRandomMP3 : string;
    procedure StartFading(MediaPlayer: TMediaPlayer);
    procedure StartRising(MediaPlayer: TMediaPlayer);
  end;



  const
  INI_FILE = 'MyApp.ini';
  INI_SECTION = 'LastValues';

var
  Form12: TForm12;
  Playlist: TArray<TPlaylistItem>;
  CurrentItemIndex: Integer;


implementation

{$R *.fmx}

uses Unit1;
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TForm12.ListTxtFiles;
var
    Files: TStringDynArray;
  FileName: string;
begin
  case TOSVersion.Platform of
    TOSVersion.TPlatform.pfWindows:
      Path := '..\..\Videos\';
    TOSVersion.TPlatform.pfMacOS:
      Path := TPath.GetFullPath('../Resources/StartUp');
    TOSVersion.TPlatform.pfiOS, TOSVersion.TPlatform.pfAndroid:
      Path := TPath.GetDocumentsPath;
    TOSVersion.TPlatform.pfWinRT, TOSVersion.TPlatform.pfLinux:
      raise Exception.Create('Unexpected platform');
  end;
  //Path := TPath.GetDocumentsPath + PathDelim; // Deployment folder
 Files := TDirectory.GetFiles(Path, '*.mp4', TSearchOption.soAllDirectories);

  ListBox1.BeginUpdate;
  try
    ListBox1.Clear;
    for FileName in Files do
    begin
      ListBox1.Items.Add(TPath.GetFileName(FileName));
    end;
  finally
    ListBox1.EndUpdate;
  end;
end;

procedure TForm12.RemoveExerciseFromPlaylist(const Item: TPlaylistItem);
var
  I, J: Integer;
begin
  for I := 0 to High(Playlist) do
  begin
    if Playlist[I].appointmentsID = Item.appointmentsID then
    begin
      // Remove from the playlist
      for J := I to High(Playlist) - 1 do
        Playlist[J] := Playlist[J + 1];
      SetLength(Playlist, Length(Playlist) - 1);
      Break;
    end;
  end;
end;

procedure TForm12.UpdateExerciseList;
var
  I: Integer;
  Hour, Min, Sec, MSec: Word;
  TextHeight: Single;
  ExerciseDuration, PercentageCompleted: Double;
begin
  ListBox1.BeginUpdate;
  try
    // Remove all items except the group header (assuming it's at index 0)
    while ListBox1.Count > 1 do
      ListBox1.Items.Delete(1);

    for I := 0 to High(Playlist) do
    begin
      // Get exercise duration in seconds
      ExerciseDuration := TimeInSecondsOf(Playlist[I].PlaybackTime);

      // Calculate percentage completed
      PercentageCompleted := (Playlist[I].CumulativeTime / ExerciseDuration) * 100;
      if PercentageCompleted > 100 then
        PercentageCompleted := 100;

      DecodeTime(Playlist[I].PlaybackTime, Hour, Min, Sec, MSec);
      ListBox1.Items.Add(Format('����������: %s, �����: %d ��� %d ���, ���������: %d%%',
        [Playlist[I].Videoname, Min, Sec, Round(PercentageCompleted)]));
      ListBox1.ListItems[ListBox1.Items.Count - 1].Tag := Playlist[I].appointmentsID;
      ListBox1.ListItems[ListBox1.Items.Count - 1].WordWrap := True;
      ListBox1.ListItems[ListBox1.Items.Count - 1].TextSettings.WordWrap := True;
      ListBox1.ListItems[ListBox1.Items.Count - 1].StyleLookup := 'ListBoxItem1Style1';

      // Adjust height based on text length
      TextHeight := CalculateTextHeight(ListBox1.ListItems[ListBox1.Items.Count - 1].Text,
        ListBox1.ListItems[ListBox1.Items.Count - 1].Font, ListBox1.Width);
      ListBox1.ListItems[ListBox1.Items.Count - 1].Height := TextHeight + 10; // Add padding
    end;
  finally
    ListBox1.EndUpdate;
  end;
end;

procedure TForm12.UpdateCumulativeTimeInDatabase(const Item: TPlaylistItem);
begin
  FDQuery3.Connection := FDConnection1;
  FDQuery3.SQL.Text := 'UPDATE appointments A ' +
                       'SET A.CumulativeTimeSpent = :CumulativeTime ' +
                       'WHERE A.idAppointments = :apptag';
  FDQuery3.ParamByName('CumulativeTime').AsFloat := Item.CumulativeTime;
  FDQuery3.ParamByName('apptag').AsInteger := Item.appointmentsID;
  FDQuery3.ExecSQL;
end;

procedure TForm12.InitializeVideoTrackBar;
var
  ExerciseDuration, RemainingTime: Double;
begin
  // Get total exercise duration in seconds
  ExerciseDuration := TimeInSecondsOf(Playlist[CurrentItemIndex].PlaybackTime);

  // Calculate remaining time for the exercise
  RemainingTime := ExerciseDuration - Playlist[CurrentItemIndex].CumulativeTime;
  if RemainingTime < 0 then
    RemainingTime := 0;

  tbVideo.Min := 0;
  tbVideo.Max := ExerciseDuration;
  tbVideo.Value := Playlist[CurrentItemIndex].CumulativeTime;
  tbVideo.Enabled := False; // Disable seeking

  // Update the label with remaining time
  Label2.Text := Format('���������� ����� ����������: %d ���', [Round(RemainingTime)]);
end;

procedure TForm12.SaveSettingsToIniFile;
var
  IniFile: TIniFile;
  inifilename: string;
begin
  inifilename := TPath.Combine(Path, 'MyApp.ini');
  try
    IniFile := TIniFile.Create(inifilename);
    IniFile.WriteString(INI_SECTION, 'MyVolume', FloatToStr(tbVolume.Value));
  finally
    IniFile.Free;
  end;
end;

procedure TForm12.bPlayClickClick(Sender: TObject);
begin
  if bPlayClick.Text = '������ ����������' then
  begin
   // Start the first video
  CurrentItemIndex := 0;
  try
  MediaPlayer1.FileName := GetVideoFilePath(Playlist[CurrentItemIndex].VideoID);
  //MediaPlayer1.Open;
  MediaPlayer1.Play;
  //startrising(MediaPlayer1);
      // Initialize tbVideo
      InitializeVideoTrackBar;

  FirstLoop := True;
  except
  on E: Exception do
  begin
    ShowMessage('���������� ��������� �����. �������� ��� ��������� ��������� �������������� �������.');
    Application.Terminate;
  end;
  end;
  Stopwatch.Start;
  Timer1.Interval := 100; // Convert seconds to milliseconds
  Timer1.Enabled := True;
  Timer4.Interval := 1000;
  Timer4.Enabled := true;
FWatchedVideosCount := 0;
FTotalPlaybackTime := 0;
  MediaPlayer1.Volume:= tbVolume.Value ;
 bPlayClick.Text := '���������� ����������'
 end
 else if bPlayClick.Text = '���������� ����������' then
 begin
  MediaPlayer1.Play;
  Stopwatch.Start;
  Timer4.Enabled := true;
  if not FirstLoop then
  begin
  MediaPlayer2.Play;
  end;
  end

end;

procedure TForm12.bSkipClickClick(Sender: TObject);
var
  SkippedItem: TPlaylistItem;
  I: Integer;
  Hour, Min, Sec, MSec: Word;
  TextHeight: Single;
begin
  // Before rearranging the playlist, add elapsed time to cumulative time
  Stopwatch.Stop;
  Playlist[CurrentItemIndex].CumulativeTime := Playlist[CurrentItemIndex].CumulativeTime + Stopwatch.Elapsed.TotalSeconds;
  UpdateCumulativeTimeInDatabase(Playlist[CurrentItemIndex]);
  Stopwatch.Reset;


  // Stop the current media playback
  Timer1.Enabled := False;  // Temporarily disable the Timer to prevent interference
  MediaPlayer1.Stop;
  MediaPlayer2.Stop;

  // Move the current video (Playlist[CurrentItemIndex]) to the end of the playlist
  SkippedItem := Playlist[CurrentItemIndex];  // Store the current item
  for I := CurrentItemIndex to High(Playlist) - 1 do
    Playlist[I] := Playlist[I + 1];  // Shift all remaining items left
  Playlist[High(Playlist)] := SkippedItem;   // Place the skipped item at the end

  // If the current item is the last one, reset to the first item
  if CurrentItemIndex >= Length(Playlist) - 1 then
    CurrentItemIndex := 0;
  //else
    //Inc(CurrentItemIndex);  // Increment only once manually

  // Refresh the ListBox1 to reflect the updated playlist
  ListBox1.BeginUpdate;
  try
    // Remove all items except the group header
    while ListBox1.Count > 1 do
      ListBox1.Items.Delete(1);

    for I := 0 to High(Playlist) do
       begin
      with TListBoxItem.Create(ListBox1) do
      begin
      DecodeTime(Playlist[I].PlaybackTime, Hour, Min, Sec, MSec);
        Parent := ListBox1;
        Text := Format('����������: %s, �����: %d ��� %d ���', [Playlist[I].Videoname, Min, Sec]);
        Tag := Playlist[I].appointmentsID;
        WordWrap := True;
        TextSettings.WordWrap := True;
        StyleLookup := 'ListBoxItem1Style1';

        // Adjust height based on text length
        TextHeight := CalculateTextHeight(Text, Font, ListBox1.Width);
        Height := TextHeight + 10;  // Add padding
      end;
    end;
  finally
    ListBox1.EndUpdate;
  end;

  // Load and play the next video
  MediaPlayer1.FileName := GetVideoFilePath(Playlist[CurrentItemIndex].VideoID);
  MediaPlayer1.Play;
  MediaPlayer1.Volume := tbVolume.Value;

  // Reset the stopwatch for the new video
  Stopwatch.Reset;
  Stopwatch.Start;

  // Re-enable the Timer
  Timer1.Enabled := True;
end;


procedure TForm12.bStopClickClick(Sender: TObject);
begin
  // Stop the stopwatch
  Stopwatch.Stop;

  // Add elapsed time to cumulative time
  Playlist[CurrentItemIndex].CumulativeTime := Playlist[CurrentItemIndex].CumulativeTime + Stopwatch.Elapsed.TotalSeconds;

  // Update cumulative time in the database
  UpdateCumulativeTimeInDatabase(Playlist[CurrentItemIndex]);

  // Reset the stopwatch
  Stopwatch.Reset;

  // Stop media playback
  Timer4.Enabled := False;
  MediaPlayer1.Stop;
  MediaPlayer2.Stop;
  bPlayClick.Text := '���������� ����������';
end;

procedure TForm12.Button1Click(Sender: TObject);
begin
StartFading(Mediaplayer1);
end;

procedure TForm12.Button2Click(Sender: TObject);
begin
startrising(MediaPlayer1);
end;

procedure TForm12.Button3Click(Sender: TObject);
begin
Edit1.Text:= GenerateRandomPassword;
end;

procedure TForm12.Button4Click(Sender: TObject);
var
  Msg: string;
begin
  try
    Msg := ClientModule1.ServerMethods1Client.GetMessage;
    ShowMessage(Msg);
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TForm12.FormClose(Sender: TObject; var Action: TCloseAction);
begin
timer1.Enabled := False;

timer4.Enabled := False;
Mediaplayer1.Stop;
Application.Terminate;
end;

procedure TForm12.CompleteExercise(var Item: TPlaylistItem);
begin
  FDQuery3.Connection := FDConnection1;
  FDQuery3.SQL.Text := 'UPDATE appointments A ' +
                       'SET A.sdelanovden = A.sdelanovden + 1, ' +
                       'A.sdelanovsego = A.sdelanovsego + 1, ' +
                       'A.Lastsession = UTC_TIMESTAMP(), ' +
                       'A.CumulativeTimeSpent = 0 ' +
                       'WHERE A.idAppointments = :apptag';
  FDQuery3.ParamByName('apptag').AsInteger := Item.appointmentsID;
  FDQuery3.ExecSQL;

  // Reset cumulative time
  Item.CumulativeTime := 0;
end;


procedure TForm12.FormCreate(Sender: TObject);
var
  Item: TPlaylistItem;
  DateQuery: TFDQuery;
  DateTimesessionFromDB, NowfromDB: TDateTime;
  HoursDifference : Integer;
  Hour, Min, Sec, MSec: Word;
   IniFile: TIniFile;
  LastVolume: extended;
  TextHeight: Single;
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
    try
    LastVolume := strtofloat(IniFile.ReadString(INI_SECTION, 'MyVolume', ''));
    tbVolume.value := LastVolume;
    except
        // If conversion fails, set to default value 1
        tbVolume.Value := 1;
    end;

   finally
    IniFile.Free;
  end;


IsMP3Loaded := False;
CurrentVolume := tbVolume.Value;
Pusername :=LoginForm.Pusername;
Stopwatch := TStopwatch.Create;
Fullexercisetime := 0;
    // Connection settings
    FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'wersusche';
    FDConnection1.Params.Values['Password'] := 'tyjer1987';
    FDConnection1.Params.Values['Server'] := 'db4free.net';
    FDConnection1.Params.Values['CharacterSet'] := 'utf8mb4'; // or 'utf8mb4';
    FDConnection1.Connected := True;
    FDQuery1.Connection := FDConnection1;

FDQuery1.SQL.Text := 'SELECT P.idPatients, A.idAppointments, A.idvideos, A.dlitelnost, A.Lastsession, V.filename, V.video_name, NOW() AS CurrentTime, ' +
                     'TIMESTAMPDIFF(HOUR, A.Lastsession, NOW()) AS HoursDifference, A.CumulativeTimeSpent ' +
                     'FROM patients P INNER JOIN ' +
                     'appointments A ON P.idPatients = A.idPatients INNER JOIN videos V ON A.idvideos = V.idvideos ' +
                     'WHERE P.Username = :UserName AND CURDATE() BETWEEN A.Starttime AND A.Endtime AND A.sdelanovden < A.kolvden';
                          //'HAVING HoursDifference > 2';

      FDQuery1.ParamByName('UserName').AsString := Pusername; // Replace UserName with the actual user name
      FDQuery1.Open;
       // Build the playlist

if FDQuery1.RecordCount > 0 then
begin
  SetLength(Playlist, 0);  // Initialize the playlist size

  while not FDQuery1.Eof do
  begin
    HoursDifference := FDQuery1.FieldByName('HoursDifference').AsInteger;

    if HoursDifference >= 0 then
    begin
      SetLength(Playlist, Length(Playlist) + 1);
      Playlist[High(Playlist)].VideoID := FDQuery1.FieldByName('filename').AsString;
      Playlist[High(Playlist)].PlaybackTime := FDQuery1.FieldByName('dlitelnost').AsDateTime;
      Playlist[High(Playlist)].Videoname := FDQuery1.FieldByName('video_name').AsString;
      Playlist[High(Playlist)].appointmentsID := FDQuery1.FieldByName('idAppointments').AsInteger;
      Playlist[High(Playlist)].CumulativeTime := FDQuery1.FieldByName('CumulativeTimeSpent').AsFloat;
      end;

    FDQuery1.Next;
  end;

  if Length(Playlist) > 0 then
  begin
    for var I := 0 to High(Playlist) do
    begin
      Item := Playlist[I];
      Fullexercisetime := Fullexercisetime + (TimeInSecondsOf(Item.PlaybackTime) - Item.CumulativeTime);
      DecodeTime(Item.PlaybackTime, Hour, Min, Sec, MSec);
      ListBox1.Items.Add(Format('����������: %s, �����: %d ��� %d ���', [Item.Videoname, Min, Sec]));
      ListBox1.ListItems[ListBox1.Items.Count-1].Tag := Item.appointmentsID;
      //ListBox1.ListItems[ListBox1.Items.Count-1].Height := 113;
      ListBox1.ListItems[ListBox1.Items.Count-1].WordWrap:= true;
      ListBox1.ListItems[ListBox1.Items.Count-1].TextSettings.WordWrap:= true;
      ListBox1.ListItems[ListBox1.Items.Count-1].StyleLookup := 'ListBoxItem1Style1';
      TextHeight := CalculateTextHeight(ListBox1.ListItems[ListBox1.Items.Count-1].Text, ListBox1.ListItems[ListBox1.Items.Count-1].Font, ListBox1.Width);
      ListBox1.ListItems[ListBox1.Items.Count-1].Height := TextHeight + 10; // Add some padding
    end;
    FDQuery1.Free;
    bplayclick.Enabled:=true;
    bstopclick.Enabled:=true;
    DecodeTime(Fullexercisetime, Hour1, Min1, Sec1, MSec1);
    label2.Text := Format('����� ���������� ����� �������: %d ��� %d ���', [Min1, Sec1]);
  end
  else
  begin
    ShowMessage('������� ������ ������ 2 ����� ����� ���������! ��� ���� ���� �����!');
    Application.Terminate;
  end;
    end
    else
   begin
  DateQuery := TFDQuery.Create(nil);
    DateQuery.Connection := FDConnection1;
       DateQuery.SQL.Text := 'SELECT P.idPatients, A.idvideos, A.dlitelnost, V.filename, V.video_name ' +
                  'FROM patients P ' +
                  'INNER JOIN appointments A ON P.idPatients = A.idPatients ' +
                  'INNER JOIN videos V ON A.idvideos = V.idvideos ' +
                  'WHERE P.Username = :UserName AND CURDATE() BETWEEN A.Starttime AND A.Endtime';
      DateQuery.ParamByName('UserName').AsString := Pusername; // Replace UserName with the actual user name
      DateQuery.Open;
            
      if DateQuery.IsEmpty then
      begin
   ShowMessage('������� � ���� ������� �� ������������� ����������! ���� ��������, ��� ����� ������������ - �������� ��������� ������ �����!');
   Application.Terminate;
      end
     else
     begin
   ShowMessage('������� �� ������� ������ ��� ������ ����������! ����������� ������!');
   Application.Terminate;
     end;
    end;
end;


procedure TForm12.FormDestroy(Sender: TObject);
begin
mediaplayer1.Stop;
Application.Terminate;
end;

function TForm12.GetVideoFilePath(VideoID: string): string;
const
  VideoFolder = '..\Videos'; // Replace with the actual path
  VideoExtension = '.mp4'; // Replace with the actual extension if different

begin
  case TOSVersion.Platform of
    TOSVersion.TPlatform.pfWindows:
     Path := ExtractFilePath(ParamStr(0));
    TOSVersion.TPlatform.pfMacOS:
      Path := TPath.GetFullPath('../Resources/StartUp');
    TOSVersion.TPlatform.pfiOS, TOSVersion.TPlatform.pfAndroid:
      Path := TPath.GetDocumentsPath;
    TOSVersion.TPlatform.pfWinRT, TOSVersion.TPlatform.pfLinux:
      raise Exception.Create('Unexpected platform');
  end;
  Result := TPath.Combine(TPath.Combine(Path, 'Videos'), VideoID + VideoExtension);
end;

procedure TForm12.Timer1Timer(Sender: TObject);
var
  ExerciseDuration, TotalTimeSpent, RemainingTime: Double;
begin
  // Calculate total time spent on current exercise
  TotalTimeSpent := Playlist[CurrentItemIndex].CumulativeTime + Stopwatch.Elapsed.TotalSeconds;

  // Get exercise duration
  ExerciseDuration := TimeInSecondsOf(Playlist[CurrentItemIndex].PlaybackTime);

  // Update tbVideo
  tbVideo.Value := TotalTimeSpent;

  // Calculate remaining time for the current exercise
  RemainingTime := ExerciseDuration - TotalTimeSpent;
  if RemainingTime < 0 then
    RemainingTime := 0;

  // Update label for remaining time
  Label2.Text := Format('���������� ����� ����������: %d ���', [Round(RemainingTime)]);

  // Check if exercise is completed
  if TotalTimeSpent >= ExerciseDuration then
  begin
    Nextvideoload;
    Exit; // Exit the procedure to avoid further processing
  end;

  // Handle video looping and background music
  if MediaPlayer1.CurrentTime >= MediaPlayer1.Duration then
  begin
    MediaPlayer1.CurrentTime := 0;
    MediaPlayer1.Play;

    // If it's not the first loop, play the background music
    if not FirstLoop then
    begin
      MediaPlayer1.Volume := 0;  // Mute the main video player

      // Load and play the MP3 only if it hasn't been loaded yet
      if not IsMP3Loaded then
      begin
        MediaPlayer2.FileName := LoadRandomMP3;
        MediaPlayer2.Play;
        IsMP3Loaded := True;  // MP3 is now loaded
      end
      else
      begin
        // MP3 is already loaded, just continue playing it
        MediaPlayer2.Play;

        // Loop the MP3 if it has ended
        if MediaPlayer2.CurrentTime >= MediaPlayer2.Duration then
        begin
          MediaPlayer2.FileName := LoadRandomMP3;
          MediaPlayer2.Play;
        end;
      end;

      MediaPlayer2.Volume := tbVolume.Value;  // Set volume for MediaPlayer2
    end
    else
    begin
      FirstLoop := False;  // Not the first loop anymore
    end;
  end;
end;

procedure TForm12.Timer4Timer(Sender: TObject);
var
  TotalRemainingTime: Double;
  I: Integer;
  Min1, Sec1: Integer;
begin
  // Calculate total remaining time across all exercises
  TotalRemainingTime := 0;
  for I := 0 to High(Playlist) do
  begin
    TotalRemainingTime := TotalRemainingTime + (TimeInSecondsOf(Playlist[I].PlaybackTime) - Playlist[I].CumulativeTime);
  end;

  // Update the label
  Min1 := Trunc(TotalRemainingTime) div 60;
  Sec1 := Trunc(TotalRemainingTime) mod 60;
  Label2.Text := Format('����� ���������� ����� �������: %d ��� %d ���', [Min1, Sec1]);

  // Stop the timer if no time remains
  if TotalRemainingTime <= 0 then
    Timer4.Enabled := False;
end;

procedure TForm12.Timer5Timer(Sender: TObject);
begin
//if MediaPlayer1.CurrentTime >= MediaPlayer1.Duration then
//MediaPlayer1.CurrentTime:=0;
//MediaPlayer1.Play;
end;

procedure TForm12.Timer_startfadingTimer(Sender: TObject);
begin
  CurrentVolume := CurrentVolume - 0.2;
  TargetMediaPlayer.Volume := CurrentVolume;
  if CurrentVolume <= 0 then
  begin
    Timer_startfading.Enabled := False;
    TargetMediaPlayer.Volume := 0; // Ensure it's zero
    TargetMediaPlayer := nil;
  end;
end;

procedure TForm12.Timer_startrisingTimer(Sender: TObject);
begin
  CurrentVolume := CurrentVolume + 0.2;
  TargetMediaPlayer.Volume := CurrentVolume;

  if CurrentVolume >= tbVolume.Value then
  begin
    Timer_startrising.Enabled := False;
    TargetMediaPlayer.Volume := tbVolume.Value; // Ensure it's zero
    TargetMediaPlayer := nil;
  end;
end;

procedure TForm12.FDConnection1BeforeConnect(Sender: TObject);
begin
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  FDConnection1.Params.Values['Database'] :=
      TPath.Combine(TPath.GetDocumentsPath, 'pushcount.db');
  {$ENDIF}
end;

procedure TForm12.tbVolumeChange(Sender: TObject);

begin
  if mediaplayer1.state = TMediaState.Playing then
  MediaPlayer1.Volume := tbVolume.value
  else
   MediaPlayer2.Volume := tbVolume.value;

end;




function TForm12.GenerateRandomPassword: string;
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

  function TForm12.LoadRandomMP3;
var
  Files: TStringDynArray;
  RandomFile: string;
  RandomIndex: Integer;
begin
  Result :='';
  // Retrieve all MP3 files from the folder
  Path := ExtractFilePath(ParamStr(0));
  Files := TDirectory.GetFiles(TPath.Combine(Path, 'Videos\Background_music'), '*.mp3'); // Change the path accordingly

  if Length(Files) = 0 then
  begin
    ShowMessage('No MP3 files found in the folder.');
    Exit;
  end;

  // Generate a random index
  Randomize;
  RandomIndex := Random(Length(Files));

  // Pick a random file
  Result := Files[RandomIndex];

  // Load the random MP3 file into MediaPlayer2

end;

 procedure TForm12.StartFading(MediaPlayer: TMediaPlayer);
begin
  CurrentVolume := tbVolume.Value;
  TargetMediaPlayer := MediaPlayer;
  Timer_startfading.Enabled := True;
end;


procedure TForm12.StartRising(MediaPlayer: TMediaPlayer);
begin
  CurrentVolume := 0;
  TargetMediaPlayer := MediaPlayer;
  Timer_startrising.Enabled := True;
end;

function TForm12.TimeInSecondsOf(ATime: TTime): Double;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(ATime, Hour, Min, Sec, MSec);
  Result := Hour * 3600 + Min * 60 + Sec + MSec / 1000.0;
end;


procedure TForm12.Timer2Timer(Sender: TObject);
begin

      CurrentVolume := CurrentVolume - 0.1;
      if CurrentVolume < 0 then CurrentVolume := 0;
      mediaplayer1.Volume := CurrentVolume;
      mediaplayer2.Volume := CurrentVolume;

    end;

  procedure TForm12.Nextvideoload;
var
  apptag: Integer;
  ExerciseDuration: Double;
begin
  // Stop the stopwatch
  Stopwatch.Stop;

  // Add elapsed time to cumulative time
  Playlist[CurrentItemIndex].CumulativeTime := Playlist[CurrentItemIndex].CumulativeTime + Stopwatch.Elapsed.TotalSeconds;

  // Get the appointmentsID for database updates
  apptag := Playlist[CurrentItemIndex].appointmentsID;

  // Get the exercise duration in seconds
  ExerciseDuration := TimeInSecondsOf(Playlist[CurrentItemIndex].PlaybackTime);

  // Check if the exercise is completed
  if Playlist[CurrentItemIndex].CumulativeTime >= ExerciseDuration then
  begin
    // Mark the exercise as completed
    CompleteExercise(Playlist[CurrentItemIndex]);

    // Remove the exercise from the playlist
    RemoveExerciseFromPlaylist(Playlist[CurrentItemIndex]);

    // Update the exercise list display
    UpdateExerciseList;
  end
  else
  begin
    // Update cumulative time in the database
    UpdateCumulativeTimeInDatabase(Playlist[CurrentItemIndex]);
  end;

  // Reset the stopwatch
  Stopwatch.Reset;

  // Proceed to the next exercise
  if CurrentItemIndex >= Length(Playlist) - 1 then
    CurrentItemIndex := 0
  else
    Inc(CurrentItemIndex);

  // Check if there are any exercises left
  if Length(Playlist) = 0 then
  begin
    // All exercises are completed
    MediaPlayer1.Stop;
    MediaPlayer2.Stop;
    Timer1.Enabled := False;
    Stopwatch.Stop;

    // Save settings to INI file (e.g., volume)
    SaveSettingsToIniFile;

    ShowMessage('�� �������! ������� ��������!');
    Timer1.Enabled := False;
    Timer4.Enabled := False;
    bPlayClick.Enabled := False;
    bStopClick.Enabled := False;
    Application.Terminate;
  end
  else
  begin
    // Start the next video

    MediaPlayer2.Stop;
    IsMP3Loaded := False; // Reset for the next exercise
    MediaPlayer1.FileName := GetVideoFilePath(Playlist[CurrentItemIndex].VideoID);
    MediaPlayer1.Play;
    MediaPlayer1.Volume := tbVolume.Value;

    // Reinitialize tbVideo for the new exercise
    InitializeVideoTrackBar;

    // Start the stopwatch for the new exercise
    Stopwatch.Start;
  end;
end;


  function TForm12.CalculateTextHeight(const Text: string; const Font: TFont; const Width: Single): Single;
var
  Layout: TTextLayout;
begin
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    try
      Layout.Font := Font;
      Layout.MaxSize := TPointF.Create(Width, TTextLayout.MaxLayoutSize.Y);
      Layout.Text := Text;
      Layout.WordWrap := True;
    finally
      Layout.EndUpdate;
    end;
    Result := Layout.Height;
  finally
    Layout.Free;
  end;
end;


end.

