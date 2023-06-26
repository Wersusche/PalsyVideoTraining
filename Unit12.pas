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
    MediaPlayerControl1: TMediaPlayerControl;
    Timer1: TTimer;
    Timer2: TTimer;
    LabelTotalPlaybackTime: TLabel;
    tbVolume: TTrackBar;
    tbProcess: TTrackBar;
    bPlayClick: TButton;
    bStopClick: TButton;
    ListBox1: TListBox;
    Timer3: TTimer;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Button2: TButton;
    Label2: TLabel;
    FDQuery2: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    TPlaylistItem = record;
    VideoID: string;
    PlaybackTime: Integer; // In seconds


    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure bPlayClickClick(Sender: TObject);
    procedure bStopClickClick(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure tbProcessChange(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure FDConnection1BeforeConnect(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);

  private
    { Private declarations }
    FUpdatingTrackBar: Boolean;
     Path: string;
    FWatchedVideosCount: Integer;
    FTotalPlaybackTime: Double; // In seconds

    procedure ListTxtFiles;

  public
    { Public declarations }
    function GenerateRandomPassword: string;
  end;

var
  Form12: TForm12;
  Playlist: TArray<TPlaylistItem>;
  CurrentItemIndex: Integer;
  Stopwatch: TStopwatch;

implementation

{$R *.fmx}
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
 MediaPlayer1.Volume := ((tbVolume.Max - tbVolume.value) + tbVolume.Min)/100;
 MediaPlayer1.Play;
end;

procedure TForm12.bStopClickClick(Sender: TObject);
begin
MediaPlayer1.Stop;
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

procedure TForm12.FormCreate(Sender: TObject);

begin
 Stopwatch := TStopwatch.Create;
    // Connection settings
    FDConnection1.DriverName := 'MySQL';
    FDConnection1.Params.Values['Database'] := 'palsy_db';
    FDConnection1.Params.Values['User_Name'] := 'Wersus';
    FDConnection1.Params.Values['Password'] := '';
    FDConnection1.Params.Values['Server'] := 'localhost';
    FDConnection1.Connected := True;

    FDQuery1.Connection := FDConnection1;
    FDQuery1.SQL.Text := 'SELECT VideoID, PlaybackTime FROM YourTable';
    FDQuery1.Open;

    // Build the playlist
    SetLength(Playlist, FDQuery1.RecordCount);
    while not FDQuery1.Eof do
    begin
      Playlist[Query.RecNo-1].VideoID := Query.FieldByName('VideoID').AsString;
      Playlist[Query.RecNo-1].PlaybackTime := Query.FieldByName('PlaybackTime').AsInteger;
      Query.Next;
    end;

  // Start the first video
  CurrentItemIndex := 0;
  MediaPlayer1.FileName := GetVideoFilePath(Playlist[CurrentItemIndex].VideoID);
  MediaPlayer1.Open;
  MediaPlayer1.Play;
  Stopwatch.Start;
  Timer1.Interval := Playlist[CurrentItemIndex].PlaybackTime * 1000; // Convert seconds to milliseconds
  Timer1.Enabled := True;
end;

FWatchedVideosCount := 0;
FTotalPlaybackTime := 0;
//FDConnection1.Open;
//FDQuery1.SQL.Text := 'CREATE TABLE IF NOT EXISTS push_count (id INTEGER PRIMARY KEY, count INTEGER);';
//FDQuery1.ExecSQL;
//FDQuery1.SQL.Text := 'INSERT OR IGNORE INTO push_count (id, count) VALUES (1, 0);';
//FDQuery1.ExecSQL;
//FDConnection1.Close;
 //FillFilesList;



//------------------------ListTxtFiles;



 //(trackbar.Max - trackBar.Position) + trackBar.Min;
 // MediaPlayer1.Volume.MaxValue := tbVolume.Max;
 //MediaPlayer1.Volume.MinValue :=  tbVolume.Min;
  MediaPlayer1.Volume:=100;
 tbVolume.Value := MediaPlayer1.Volume;
end;


function TForm12.GetVideoFilePath(VideoID: string): string;
begin
  // Return the full path to the video file corresponding to the given ID
  // This depends on how your video files are organized
end;



procedure TForm12.Timer1Timer(Sender: TObject);

begin
  // Stop the current video
  MediaPlayer1.Stop;

  // Move to the next video
  Inc(CurrentItemIndex);
  if CurrentItemIndex >= Length(Playlist) then
    CurrentItemIndex := 0;

  // Start the next video
  MediaPlayer1.FileName := GetVideoFilePath(Playlist[CurrentItemIndex].VideoID);
  MediaPlayer1.Open;
  MediaPlayer1.Play;
  Stopwatch.Restart;
  Timer1.Interval := Playlist[CurrentItemIndex].PlaybackTime * 1000; // Convert seconds to milliseconds
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

//     if tbProcess.Max <> MediaPlayer1.Duration then
//    tbProcess.Max := MediaPlayer1.Duration;
//  if tbProcess.Value <> MediaPlayer1.CurrentTime then
//    tbProcess.Value := MediaPlayer1.CurrentTime;

 if Assigned(MediaPlayer1.Media) and (MediaPlayer1.State = TMediaState.Playing) then
  begin
  if tbProcess.Max <> MediaPlayer1.Duration then
  tbProcess.Max := MediaPlayer1.Duration;
   FUpdatingTrackBar := True;
    try
    tbProcess.Value := MediaPlayer1.CurrentTime;
   finally
    FUpdatingTrackBar := False;
  end;
  end;

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
   tbProcess.Max := MediaPlayer1.Duration;
   MediaPlayer1.CurrentTime := 0;
   MediaPlayer1.Volume := ((tbVolume.Max - tbVolume.value) + tbVolume.Min)/100;
   //MediaPlayer1.Play;
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
  MediaPlayer1.FileName := TPath.Combine(Path, Item.Text);

  //tbVolume.Value:= MediaPlayer1.Volume*100;
end;

procedure TForm12.tbProcessChange(Sender: TObject);
begin
if not FUpdatingTrackBar then
begin
  //MediaPlayer1.Stop;
  MediaPlayer1.CurrentTime :=  Round(tbProcess.Value);
 end;
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
