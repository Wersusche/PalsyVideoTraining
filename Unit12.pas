unit Unit12;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
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
    Label1: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDQuery1: TFDQuery;
    MediaPlayer1: TMediaPlayer;
    Z: TMediaPlayerControl;
    Timer1: TTimer;
    Timer2: TTimer;
    LabelTotalPlaybackTime: TLabel;
    tbVolume: TTrackBar;
    bPlayClick: TButton;
    bStopClick: TButton;
    ListBox1: TListBox;
    Timer3: TTimer;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Button2: TButton;
    Label2: TLabel;
    FDQuery2: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    Timer4: TTimer;
    Layoutvideo: TLayout;
    Layoutitems: TLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
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

  private
    { Private declarations }
    FUpdatingTrackBar: Boolean;
     Path: string;
    FWatchedVideosCount: Integer;
    FTotalPlaybackTime: Double; // In seconds
    Pusername : string;
    procedure ListTxtFiles;

  public
    { Public declarations }
    function GenerateRandomPassword: string;
    function GetVideoFilePath(VideoID: string): string;
  end;

 type
  TPlaylistItem = record
    VideoID: string;
    PlaybackTime: Integer; // In seconds
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
  MediaPlayer1.FileName := GetVideoFilePath(Playlist[CurrentItemIndex].VideoID);
  //MediaPlayer1.Open;
  MediaPlayer1.Play;
  Stopwatch.Start;
  Timer1.Interval := 100; // Convert seconds to milliseconds
  Timer1.Enabled := True;

FWatchedVideosCount := 0;
FTotalPlaybackTime := 0;
  MediaPlayer1.Volume:=100;
 tbVolume.Value := MediaPlayer1.Volume;
 bPlayClick.Text := 'Продолжить упражнение'
 end
 else if bPlayClick.Text = 'Продолжить упражнение' then
 begin
  MediaPlayer1.Play;
  Stopwatch.Start;
  end
 //MediaPlayer1.Volume := ((tbVolume.Max - tbVolume.value) + tbVolume.Min)/100;
 //MediaPlayer1.Play;
end;

procedure TForm12.bStopClickClick(Sender: TObject);
begin
Stopwatch.Stop;
MediaPlayer1.Stop;
bPlayClick.Text := 'Продолжить упражнение'
end;

procedure TForm12.Button1Click(Sender: TObject);
begin
FDConnection1.Open;
FDQuery1.SQL.Text := 'UPDATE push_count SET count = count + 1 WHERE id = 1;';
FDQuery1.ExecSQL;
FDQuery1.SQL.Text := 'SELECT count FROM push_count WHERE id = 1;';
FDQuery1.Open;
Edit1.Text := FDQuery1.FieldByName('count').AsString;
FDQuery1.Close;
FDConnection1.Close;
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
timer2.Enabled := False;
timer3.Enabled := False;
timer4.Enabled := False;
Mediaplayer1.Stop;
Application.Terminate;
end;

procedure TForm12.FormCreate(Sender: TObject);
var
  Item: TPlaylistItem;

begin
Pusername :=LoginForm.Pusername;
Stopwatch := TStopwatch.Create;
    // Connection settings
    FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'wersusche';
    FDConnection1.Params.Values['Password'] := 'tyjer1987';
    FDConnection1.Params.Values['Server'] := 'db4free.net';
    FDConnection1.Params.Values['CharacterSet'] := 'utf8mb4'; // or 'utf8mb4';
    FDConnection1.Connected := True;

    FDQuery1.Connection := FDConnection1;
    FDQuery1.SQL.Text := 'SELECT P.idPatients, A.idvideos, A.dlitelnost, V.filename, V.video_name ' +
                  'FROM patients P ' +
                  'INNER JOIN appointments A ON P.idPatients = A.idPatients ' +
                  'INNER JOIN videos V ON A.idvideos = V.idvideos ' +
                  'WHERE P.Username = :UserName';
      FDQuery1.ParamByName('UserName').AsString := Pusername; // Replace UserName with the actual user name
      FDQuery1.Open;
      label2.Text := FDQuery1.FieldByName('video_name').AsString;
    // Build the playlist
    SetLength(Playlist, FDQuery1.RecordCount);
    while not FDQuery1.Eof do
    begin
      Playlist[FDQuery1.RecNo-1].VideoID := FDQuery1.FieldByName('filename').AsString;
      Playlist[FDQuery1.RecNo-1].PlaybackTime := FDQuery1.FieldByName('dlitelnost').AsInteger;
      Playlist[FDQuery1.RecNo-1].Videoname := FDQuery1.FieldByName('video_name').AsString;
      FDQuery1.Next;
    end;

    for var I := 0 to High(Playlist) do
  begin
    Item := Playlist[I];
    ListBox1.Items.Add(Format('Упражнение: %s, Время: %d сек', [Item.Videoname, Item.PlaybackTime]));
  end;
 //----------------------------------------------------------------------------------------------------------

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
  label
  nextvd;

begin
    if Stopwatch.Elapsed.TotalSeconds >= Playlist[CurrentItemIndex].PlaybackTime  then
    goto nextvd;
    if (MediaPlayer1.CurrentTime >= MediaPlayer1.Duration)   then
    if Stopwatch.Elapsed.TotalSeconds < Playlist[CurrentItemIndex].PlaybackTime then
      begin
      MediaPlayer1.CurrentTime := 0;
      MediaPlayer1.Play;
    end

    else
begin
  // Stop the current video
   nextvd:
  MediaPlayer1.Stop;
   // Move to the next video
  Inc(CurrentItemIndex);
  if CurrentItemIndex >= Length(Playlist) then
    begin
    CurrentItemIndex := 0;
    MediaPlayer1.Stop;
    Timer1.Enabled:= False;
    Stopwatch.Stop;
    ShowMessage('Ты молодец! На сегодня всё!');
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
  //Timer1.Interval := Playlist[CurrentItemIndex].PlaybackTime * 1000; // Convert seconds to milliseconds
end;


//begin
//  // Check if the video playback has ended
//  if MediaPlayer1.CurrentTime >= MediaPlayer1.Duration then
//  begin
//    // Increment the counter
//    Inc(FWatchedVideosCount);
//
//    // Update the TLabel
//    Label1.Text := Format('Watched Videos: %d', [FWatchedVideosCount]);
//
//    // Disable the timer to prevent counting the same video multiple times
//    //Timer1.Enabled := False;
//  end;
end;

procedure TForm12.Timer2Timer(Sender: TObject);
begin
// Update the total playback time only if the video is playing
  if MediaPlayer1.State = TMediaState.Playing then
  begin
    FTotalPlaybackTime := FTotalPlaybackTime + (Timer1.Interval / 500);
  end;
  LabelTotalPlaybackTime.Text := Format('Total Playback Time: %.1f s', [FTotalPlaybackTime]);

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
  RemainingTime: Integer;
begin
  // Calculate the remaining time in seconds
  RemainingTime := Round(Playlist[CurrentItemIndex].PlaybackTime - Stopwatch.Elapsed.TotalSeconds);
  // Update the label
  Label1.Text := Format('Remaining time: %d sec', [RemainingTime]);
end;

procedure TForm12.Timer5Timer(Sender: TObject);
begin
//if MediaPlayer1.CurrentTime >= MediaPlayer1.Duration then
//MediaPlayer1.CurrentTime:=0;
//MediaPlayer1.Play;
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
MediaPlayer1.Volume := ((tbVolume.Max - tbVolume.value) + tbVolume.Min)/50;
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


end.




//
//uses
//  ..., System.Diagnostics;
//
//var
//  Form1: TForm1;
//  Stopwatch: TStopwatch;
//
//implementation
//
//procedure TForm1.FormCreate(Sender: TObject);
//begin
//  Stopwatch := TStopwatch.StartNew; // Start the stopwatch
//  MediaPlayer1.FileName := 'YourVideoFile.mp4'; // Set your video file
//  MediaPlayer1.Open;
//  MediaPlayer1.Play;
//  Timer1.Enabled := True;
//end;
//
//procedure TForm1.Timer1Timer(Sender: TObject);
//begin
//  if Stopwatch.Elapsed.TotalSeconds >= 60 then // Check if 60 seconds have passed
//  begin
//    Timer1.Enabled := False; // Disable the timer
//    MediaPlayer1.Stop;
//  end
//  else
//  begin
//    MediaPlayer1.Position := 0; // Rewind the video
//    MediaPlayer1.Play;
//  end;
//end;
