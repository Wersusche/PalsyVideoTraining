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
  FireDAC.Comp.UI, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, System.Diagnostics, System.IniFiles;

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
    ListBoxGroupHeader1: TListBoxGroupHeader;
    MediaPlayer2: TMediaPlayer;
    Timer_startfading: TTimer;
    Timer_for: TTimer;
    Timer_startrising: TTimer;
    Button1: TButton;
    Button2: TButton;
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
    procedure Button1Click(Sender: TObject);

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
    procedure ListTxtFiles;
    procedure Nextvideoload;


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
  FirstLoop := True;
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

procedure TForm12.FormClose(Sender: TObject; var Action: TCloseAction);
begin
timer1.Enabled := False;

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
   IniFile: TIniFile;
  LastVolume: extended;
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
  Hour, Min, Sec, MSec: Word;
  TimeInSeconds: double;

begin
  DecodeTime(Playlist[CurrentItemIndex].PlaybackTime, Hour, Min, Sec, MSec);
  TimeInSeconds := Min * 60 + Sec;

    if Stopwatch.Elapsed.TotalSeconds >= TimeInSeconds-3  then
    begin
      startfading(Mediaplayer2);
    end;

    if Stopwatch.Elapsed.TotalSeconds >= TimeInSeconds  then
    begin
    Nextvideoload;
    end;

    if (MediaPlayer1.CurrentTime >= MediaPlayer1.Duration)   then
       begin
      MediaPlayer1.CurrentTime := 0;
      MediaPlayer1.Play;

            // If it's not the first loop, play the external audio track
      if not FirstLoop then
      begin
       MediaPlayer1.Volume := 0;  // Mute the main player
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
      end;
     MediaPlayer2.Volume := tbVolume.Value;  // Full volume for MediaPlayer2
    end
      else
    begin
      FirstLoop := False;  // Not the first loop anymore
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

procedure TForm12.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  //MediaPlayer1.Stop;
 // MediaPlayer1.FileName := TPath.Combine(Path, Item.Text);

  //tbVolume.Value:= MediaPlayer1.Volume*100;
end;



procedure TForm12.tbVolumeChange(Sender: TObject);

begin
  MediaPlayer1.Volume := tbVolume.value;
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



procedure TForm12.Timer2Timer(Sender: TObject);
begin

      CurrentVolume := CurrentVolume - 0.1;
      if CurrentVolume < 0 then CurrentVolume := 0;
      mediaplayer1.Volume := CurrentVolume;
      mediaplayer2.Volume := CurrentVolume;

    end;

  procedure TForm12.Nextvideoload;
  var
IniFile: TIniFile;

inifilename : string;
  begin
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
    MediaPlayer2.Stop;
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

  inifilename := TPath.Combine(Path, 'MyApp.ini');
   try
   IniFile := TIniFile.Create(inifilename);
   IniFile.WriteString(INI_SECTION, 'MyVolume', floattostr(tbVolume.Value));

  finally
    IniFile.Free;
   end;

  ShowMessage('Ты молодец! Занятие окончено!');
 timer1.Enabled := false;
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
  Startrising(Mediaplayer1);
 // MediaPlayer1.Volume := tbVolume.Value;
  Stopwatch.reset;
  Stopwatch.Start;
end;
  end;
end.

