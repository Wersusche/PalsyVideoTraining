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
  FireDAC.Comp.UI, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, System.Diagnostics;

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
    Timer3: TTimer;
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
    ListBoxGroupHeader1: TListBoxGroupHeader;
    MediaPlayer2: TMediaPlayer;
    Timer_startfading: TTimer;
    Timer_for: TTimer;
    Timer_startrising: TTimer;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure bPlayClickClick(Sender: TObject);
    procedure bStopClickClick(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure tbVolumeChange(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
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

  private
    { Private declarations }
    FUpdatingTrackBar: Boolean;
    Path: string;
    FWatchedVideosCount: Integer;
    CurrentVolume: Integer;
    FTotalPlaybackTime: Double; // In seconds
    Pusername : string;
    Fullexercisetime: TTime;
    Hour1, Min1, Sec1, MSec1: Word;
    TargetMediaPlayer: TMediaPlayer;
    procedure ListTxtFiles;


  public
    { Public declarations }
    function GenerateRandomPassword: string;
    function GetVideoFilePath(VideoID: string): string;
    function LoadRandomMP3 : string;
    procedure StartFading(MediaPlayer: TMediaPlayer);
    procedure StartRising(MediaPlayer: TMediaPlayer);
  end;

 type
  TPlaylistItem = record
    VideoID: string;
    PlaybackTime: TTime; // In seconds
    Videoname: string;
  end;

var
  Form12: TForm12;
  Playlist: TArray<TPlaylistItem>;
  CurrentItemIndex: Integer;
  Stopwatch: TStopwatch;

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

procedure TForm12.bPlayClickClick(Sender: TObject);
begin
  if bPlayClick.Text = 'Начать упражнение' then
  begin
   // Start the first video
  CurrentItemIndex := 0;
  try
  MediaPlayer1.FileName := GetVideoFilePath(Playlist[CurrentItemIndex].VideoID);
  //MediaPlayer1.Open;
  MediaPlayer1.Play;
  except
  on E: Exception do
  begin
    ShowMessage('Невозможно проиграть видео. Возможно вам требуется установка дополнительных кодеков.');
  end;
  end;
  Stopwatch.Start;
  Timer1.Interval := 100; // Convert seconds to milliseconds
  Timer1.Enabled := True;
  Timer4.Interval := 1000;
  Timer4.Enabled := true;
FWatchedVideosCount := 0;
FTotalPlaybackTime := 0;
  //MediaPlayer1.Volume:=100;
 tbVolume.Value := MediaPlayer1.Volume;
 bPlayClick.Text := 'Продолжить упражнение'
 end
 else if bPlayClick.Text = 'Продолжить упражнение' then
 begin
  MediaPlayer1.Play;
  Stopwatch.Start;
   Timer4.Enabled := true;
  end
 //MediaPlayer1.Volume := ((tbVolume.Max - tbVolume.value) + tbVolume.Min)/100;
 //MediaPlayer1.Play;
end;

procedure TForm12.bStopClickClick(Sender: TObject);
begin
Stopwatch.Stop;
Timer4.Enabled := false;
MediaPlayer1.Stop;
bPlayClick.Text := 'Продолжить упражнение'
end;

procedure TForm12.Button2Click(Sender: TObject);
begin
FDConnection1.Connected := true;
FDQuery2.Open;
  if not FDQuery2.IsEmpty then
    Label2.Text := FDQuery2.FieldByName('count').AsString
  else
    Label2.Text := 'No data found';
  FDQuery2.Close;
  FDConnection1.Connected := False;
end;

procedure TForm12.Button3Click(Sender: TObject);
begin
Edit1.Text:= GenerateRandomPassword;
end;

procedure TForm12.FormClose(Sender: TObject; var Action: TCloseAction);
begin
timer1.Enabled := False;

timer3.Enabled := False;
timer4.Enabled := False;
Mediaplayer1.Stop;
Application.Terminate;
end;

procedure TForm12.FormCreate(Sender: TObject);
var
  Item: TPlaylistItem;
  DateQuery: TFDQuery;
  DateTimesessionFromDB, NowfromDB: TDateTime;
  Hour, Min, Sec, MSec: Word;
  //Fullexercisetime: TTime;
begin
CurrentVolume := 100;
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
    FDQuery1.SQL.Text := 'SELECT P.idPatients, A.idvideos, A.dlitelnost, A.Lastsession, V.filename, V.video_name, NOW() ' +
                  'FROM patients P ' +
                  'INNER JOIN appointments A ON P.idPatients = A.idPatients ' +
                  'INNER JOIN videos V ON A.idvideos = V.idvideos ' +
                  'WHERE P.Username = :UserName AND CURDATE() BETWEEN A.Starttime AND A.Endtime AND A.sdelanovden < A.kolvden';
      FDQuery1.ParamByName('UserName').AsString := Pusername; // Replace UserName with the actual user name
      FDQuery1.Open;
      label2.Text := FDQuery1.FieldByName('Lastsession').AsString;
      DateTimesessionFromDB := FDQuery1.FieldByName('Lastsession').AsDateTime;
      NowfromDB := FDQuery1.FieldByName('NOW()').AsDateTime;
       // Build the playlist

if FDQuery1.RecordCount > 0 then
 begin
 if (HoursBetween(DateTimesessionFromDB, NowfromDB)) >= 1 then
  begin
    SetLength(Playlist, FDQuery1.RecordCount);
    while not FDQuery1.Eof do
    begin
      Playlist[FDQuery1.RecNo-1].VideoID := FDQuery1.FieldByName('filename').AsString;
      Playlist[FDQuery1.RecNo-1].PlaybackTime := FDQuery1.FieldByName('dlitelnost').AsDateTime;
      Playlist[FDQuery1.RecNo-1].Videoname := FDQuery1.FieldByName('video_name').AsString;
      FDQuery1.Next;
    end;

    for var I := 0 to High(Playlist) do
  begin
    Item := Playlist[I];
    Fullexercisetime := Fullexercisetime + Item.PlaybackTime;
    DecodeTime(Item.PlaybackTime, Hour, Min, Sec, MSec);
    ListBox1.Items.Add(Format('Упражнение: %s, Время: %d мин %d сек', [Item.Videoname, Min, Sec]));
  ListBox1.ListItems[ListBox1.Items.Count-1].WordWrap:=true;
  end;
  FDQuery1.Free;
 bplayclick.Enabled:=true;
 bstopclick.Enabled:=true;
  DecodeTime(Fullexercisetime, Hour1, Min1, Sec1, MSec1);
 label2.Text := Format('Общее оставшееся время занятия: %d мин %d сек', [Min1, Sec1]);

  end
  else
  begin
   ShowMessage('Кажется прошло меньше 1 часа между занятиями! Жду тебя чуть позже!');
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
   ShowMessage('Кажется у тебя сегодня не запланировано упражнений! Если считаешь, что нужно позаниматься - попробуй позвонить своему врачу!');
   Application.Terminate;
      end
     else
     begin
   ShowMessage('Кажется ты сегодня сделал все нужные упражнения! Возвращайся завтра!');
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
  VideoFolder = 'C:\path\to\your\videos'; // Replace with the actual path
  VideoExtension = '.mp4'; // Replace with the actual extension if different

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
  Result := TPath.Combine(Path, VideoID + VideoExtension);
end;

procedure TForm12.Timer1Timer(Sender: TObject);
var
  Hour, Min, Sec, MSec: Word;
  TimeInSeconds: double;
  FirstLoop: Boolean;
  label
  nextvd;
      
begin
  DecodeTime(Playlist[CurrentItemIndex].PlaybackTime, Hour, Min, Sec, MSec);
  TimeInSeconds := Min * 60 + Sec;

  // Set FirstLoop to True when you start the entire sequence
    FirstLoop := True;
    if Stopwatch.Elapsed.TotalSeconds >= TimeInSeconds  then
    goto nextvd;
    if (MediaPlayer1.CurrentTime >= MediaPlayer1.Duration)   then
    if Stopwatch.Elapsed.TotalSeconds < TimeInSeconds  then
      begin
      FirstLoop := False;  // Reset for next iterations
      MediaPlayer1.CurrentTime := 0;
      MediaPlayer1.Play;
            // If it's not the first loop, play the external audio track
      if not FirstLoop then
      begin
      MediaPlayer1.Volume := 0; // Mute
        MediaPlayer2.FileName := LoadRandomMP3;
        MediaPlayer2.Play;
        MediaPlayer2.Volume := 100; // Full volume
      end;

    end
    
    else
begin
  // Stop the current video
   nextvd:
  MediaPlayer1.Stop;
  MediaPlayer2.Stop;
   // Move to the next video
  Inc(CurrentItemIndex);
    // Reset the FirstLoop variable if you've moved to a new video
     FirstLoop := True;
  if CurrentItemIndex >= Length(Playlist) then
    begin
    CurrentItemIndex := 0;
    MediaPlayer1.Stop;
    Timer1.Enabled:= False;
    Stopwatch.Stop;
    FDQuery3.Connection := FDConnection1;
    FDQuery3.SQL.Text := 'UPDATE appointments A ' +
                     'INNER JOIN patients P ON P.idPatients = A.idPatients ' +
                     'INNER JOIN videos V ON A.idvideos = V.idvideos ' +
                     'SET A.sdelanovden = A.sdelanovden + 1, A.sdelanovsego = A.sdelanovsego + 1, A.lastsession = UTC_TIMESTAMP()' +
                     'WHERE P.Username = :UserName AND CURDATE() BETWEEN A.Starttime AND A.Endtime AND A.sdelanovden < A.kolvden';
    FDQuery3.ParamByName('UserName').AsString := Pusername; // Replace UserName with the actual user name
    FDQuery3.ExecSQL;
    ShowMessage('Ты молодец! Занятие окончено!');
 timer1.Enabled := false;

   timer3.Enabled := false;
    timer4.Enabled := false;
 bplayclick.Enabled:=false;
 bstopclick.Enabled:=false;
   end
else
begin
   // Start the next video
  MediaPlayer1.FileName := GetVideoFilePath(Playlist[CurrentItemIndex].VideoID);
 // MediaPlayer1.Open;
  MediaPlayer1.Play;
  Stopwatch.reset;
  Stopwatch.Start;
end;

end;

end;


procedure TForm12.Timer3Timer(Sender: TObject);
begin


   if (MediaPlayer1.Media = nil) or ((MediaPlayer1.State = TMediaState.Stopped) and (MediaPlayer1.CurrentTime >= MediaPlayer1.Duration)) then
  begin
  if ListBox1.Items.Count > 0 then
  begin
    if ListBox1.ItemIndex < ListBox1.Items.Count - 1 then
      ListBox1.ItemIndex := ListBox1.ItemIndex + 1
    else
      ListBox1.ItemIndex := 0;
     // Do something with ItemText, e.g., show it in a label or memo
  end;

   MediaPlayer1.FileName :=  TPath.Combine(Path, ListBox1.Items.Strings[ListBox1.ItemIndex]);
    MediaPlayer1.CurrentTime := 0;
   MediaPlayer1.Volume := ((tbVolume.Max - tbVolume.value) + tbVolume.Min)/100;
   //MediaPlayer1.Play;
  end;

end;

procedure TForm12.Timer4Timer(Sender: TObject);
  var
  RemainingTime: Int64;

begin
  // Calculate the remaining time in seconds
 // RemainingTime := Round(Playlist[CurrentItemIndex].PlaybackTime - Stopwatch.Elapsed.TotalSeconds);
  // Update the label
 RemainingTime :=  Min1 * 60 + Sec1;
 Dec(RemainingTime);
 Min1 := RemainingTime div 60;
 Sec1 := RemainingTime mod 60;
 label2.Text := Format('Общее оставшееся время занятия: %d мин %d сек', [Min1, Sec1]);
 if RemainingTime <=0 then
 timer4.Enabled := false;

end;

procedure TForm12.Timer5Timer(Sender: TObject);
begin
//if MediaPlayer1.CurrentTime >= MediaPlayer1.Duration then
//MediaPlayer1.CurrentTime:=0;
//MediaPlayer1.Play;
end;

procedure TForm12.Timer_startfadingTimer(Sender: TObject);
begin
  CurrentVolume := CurrentVolume - 1;
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
  CurrentVolume := CurrentVolume + 1;
  TargetMediaPlayer.Volume := CurrentVolume;

  if CurrentVolume <= 100 then
  begin
    Timer_startfading.Enabled := False;
    TargetMediaPlayer.Volume := 100; // Ensure it's zero
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

procedure TForm12.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  //MediaPlayer1.Stop;
 // MediaPlayer1.FileName := TPath.Combine(Path, Item.Text);

  //tbVolume.Value:= MediaPlayer1.Volume*100;
end;



procedure TForm12.tbVolumeChange(Sender: TObject);
begin
MediaPlayer1.Volume := ((tbVolume.Max - tbVolume.value) + tbVolume.Min)/100;
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
  Files := TDirectory.GetFiles('..\..\Videos\Background_music', '*.mp3'); // Change the path accordingly

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
  CurrentVolume := 100;
  TargetMediaPlayer := MediaPlayer;
  Timer_startfading.Enabled := True;
end;


procedure TForm12.StartRising(MediaPlayer: TMediaPlayer);
begin
  CurrentVolume := 0;
  TargetMediaPlayer := MediaPlayer;
  Timer_startrising.Enabled := True;
end;



procedure TForm12.Timer2Timer(Sender: TObject);
begin

    if Stopwatch.Elapsed.TotalSeconds >= 7 then
    begin

  CurrentVolume := CurrentVolume - 1;
  mediaplayer1.Volume := CurrentVolume;

  if CurrentVolume <= 0 then
  begin
    Timer_startfading.Enabled := False;
    mediaplayer1.Volume := 0; // Ensure it's zero
    TargetMediaPlayer := nil;
  end;
    //StartFading(MediaPlayer1);
    end;
end;

end.

