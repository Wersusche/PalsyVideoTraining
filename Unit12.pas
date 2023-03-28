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
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, System.IOUtils;

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
  private
    { Private declarations }
    FLibraryPath: string;
    Path: string;
    FWatchedVideosCount: Integer;
    FTotalPlaybackTime: Double; // In seconds
    procedure FillFilesList;
    function GetPathWithVideo: string;
    procedure ListTxtFiles;

  public
    { Public declarations }
  end;

var
  Form12: TForm12;

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

procedure TForm12.FormCreate(Sender: TObject);

begin
FWatchedVideosCount := 0;
FTotalPlaybackTime := 0;
FDConnection1.Open;
FDQuery1.SQL.Text := 'CREATE TABLE IF NOT EXISTS push_count (id INTEGER PRIMARY KEY, count INTEGER);';
FDQuery1.ExecSQL;
FDQuery1.SQL.Text := 'INSERT OR IGNORE INTO push_count (id, count) VALUES (1, 0);';
FDQuery1.ExecSQL;
FDConnection1.Close;
 FLibraryPath := GetPathWithVideo;
 //FillFilesList;
 ListTxtFiles;
 tbVolume.Value := MediaPlayer1.Volume;
end;

procedure TForm12.Timer1Timer(Sender: TObject);
begin
  // Check if the video playback has ended
  if (MediaPlayer1.State = TMediaState.Stopped) and (MediaPlayer1.CurrentTime >= MediaPlayer1.Duration) then
  begin
    // Increment the counter
    Inc(FWatchedVideosCount);

    // Update the TLabel
    Label1.Text := Format('Watched Videos: %d', [FWatchedVideosCount]);

    // Disable the timer to prevent counting the same video multiple times
    Timer1.Enabled := False;
  end;
end;

procedure TForm12.Timer2Timer(Sender: TObject);
begin
// Update the total playback time only if the video is playing
  if MediaPlayer1.State = TMediaState.Playing then
  begin
    FTotalPlaybackTime := FTotalPlaybackTime + (Timer1.Interval / 1000);
  end;
  LabelTotalPlaybackTime.Text := Format('Total Playback Time: %.1f s', [FTotalPlaybackTime]);

end;

procedure TForm12.Timer3Timer(Sender: TObject);
begin
  if tbProcess.Max <> MediaPlayer1.Duration then
    tbProcess.Max := MediaPlayer1.Duration;
  if tbProcess.Value <> MediaPlayer1.CurrentTime then
    tbProcess.Value := MediaPlayer1.CurrentTime;

  if (MediaPlayer1.Media = nil) or ((MediaPlayer1.State = TMediaState.Stopped) and (MediaPlayer1.CurrentTime >= MediaPlayer1.Duration)) then
   Listbox1.ItemIndex:= Listbox1.ItemIndex + 1;
 //MediaPlayer1.FileName := TPath.Combine(Path, Listbox1.ItemIndex, Item.Text);
 ListBox1.Items.Strings[Listbox1.ItemIndex];
end;

procedure TForm12.FDConnection1BeforeConnect(Sender: TObject);
begin
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  FDConnection1.Params.Values['Database'] :=
      TPath.Combine(TPath.GetDocumentsPath, 'pushcount.db');
  {$ENDIF}
end;

procedure TForm12.FillFilesList;
var
  F: TSearchRec;
  Path: string;
  Attr: Integer;
begin
  Path := TPath.Combine(FLibraryPath, '*.mp4');
{$IFDEF MSWINDOWS}
//Attr := faReadOnly + faArchive;
{$ELSE}
  Attr := 0;
{$ENDIF}
  FindFirst(Path, Attr, F);
  if F.name <> '' then
  begin
    ListBox1.Items.Add(F.name);
    while FindNext(F) = 0 do
      ListBox1.Items.Add(F.name);
  end;
  FindClose(F);
end;

function TForm12.GetPathWithVideo: string;
begin
  case TOSVersion.Platform of
    TOSVersion.TPlatform.pfWindows:
      Result := '..\..\Videos\';
    TOSVersion.TPlatform.pfMacOS:
      Result := TPath.GetFullPath('../Resources/StartUp');
    TOSVersion.TPlatform.pfiOS, TOSVersion.TPlatform.pfAndroid:
      Result := TPath.GetDocumentsPath;
    TOSVersion.TPlatform.pfWinRT, TOSVersion.TPlatform.pfLinux:
      raise Exception.Create('Unexpected platform');
  end;
end;

procedure TForm12.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  MediaPlayer1.Stop;
  MediaPlayer1.FileName := TPath.Combine(Path, Item.Text);
end;

procedure TForm12.tbProcessChange(Sender: TObject);
begin
 //MediaPlayer1.CurrentTime := tbProcess.Value;
end;

procedure TForm12.tbVolumeChange(Sender: TObject);
begin
MediaPlayer1.Volume := tbVolume.Value;
end;

end.


//uses
//  System.SysUtils, System.Types, System.UITypes, System.Classes,
//  System.Variants,
//  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
//  FMX.Media, FMX.Objects,
//  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox;
//
//type
//  TfrmMain = class(TForm)
//    Rectangle1: TRectangle;
//    MediaPlayerControl1: TMediaPlayerControl;
//    MediaPlayer1: TMediaPlayer;
//    Timer1: TTimer;
//    tbVolume: TTrackBar;
//    ScrollBox1: TScrollBox;
//    ListBox1: TListBox;
//    ListBoxGroupHeader1: TListBoxGroupHeader;
//    bToTheStart: TButton;
//    Panel1: TPanel;
//    bPlay: TButton;
//    bStop: TButton;
//    bToTheEnd: TButton;
//    b10SecBackward: TButton;
//    b10SecForward: TButton;
//    Panel2: TPanel;
//    Panel3: TPanel;
//    bMovingParentUp: TButton;
//    bMovingParentDown: TButton;
//    bChangeParentsVisible: TButton;
//    tbProcess: TTrackBar;
//    procedure bPlayClick(Sender: TObject);
//    procedure FormCreate(Sender: TObject);
//    procedure ListBox1ItemClick(const Sender: TCustomListBox;
//      const Item: TListBoxItem);
//    procedure bStopClick(Sender: TObject);
//    procedure bToTheStartClick(Sender: TObject);
//    procedure tbVolumeChange(Sender: TObject);
//    procedure Timer1Timer(Sender: TObject);
//    procedure bToTheEndClick(Sender: TObject);
//    procedure b10SecBackwardClick(Sender: TObject);
//    procedure b10SecForwardClick(Sender: TObject);
//    procedure Panel1Click(Sender: TObject);
//    procedure bMovingParentDownClick(Sender: TObject);
//    procedure bMovingParentUpClick(Sender: TObject);
//    procedure bChangeParentsVisibleClick(Sender: TObject);
//  private
//    { Private declarations }
//    FLibraryPath: string;
//    function GetPathWithVideo: string;
//    procedure FillFilesList;
//  public
//    { Public declarations }
//  end;
//
//var
//  frmMain: TfrmMain;
//
//implementation
//
//uses
//  System.IOUtils;
//
//const
//  TenSecond = 100000000;
//  StepForShifting = 10;
//
//{$R *.fmx}
//
////procedure TfrmMain.b10SecBackwardClick(Sender: TObject);
//var
//  LState: TMediaState;
//begin
//  LState := MediaPlayer1.State;
//  if MediaPlayer1.CurrentTime > TenSecond then
//  begin
//    if LState = TMediaState.Playing then
//      MediaPlayer1.Stop;
//    MediaPlayer1.CurrentTime := MediaPlayer1.CurrentTime - TenSecond;
//    if LState = TMediaState.Playing then
//      MediaPlayer1.Play;
//  end
//  else
//    MediaPlayer1.CurrentTime := 0;
//end;

//procedure TfrmMain.b10SecForwardClick(Sender: TObject);
//var
//  LState: TMediaState;
//begin
//  LState := MediaPlayer1.State;
//  if MediaPlayer1.CurrentTime < (MediaPlayer1.Duration - TenSecond) then
//  begin
//    if LState = TMediaState.Playing then
//      MediaPlayer1.Stop;
//    MediaPlayer1.CurrentTime := MediaPlayer1.CurrentTime + TenSecond;
//    if LState = TMediaState.Playing then
//      MediaPlayer1.Play;
//  end
//  else
//  begin
//    MediaPlayer1.CurrentTime := MediaPlayer1.Duration;
//  end;
//end;

//procedure TfrmMain.bChangeParentsVisibleClick(Sender: TObject);
//begin
//  Rectangle1.Visible := not Rectangle1.Visible;
//end;
//
//procedure TfrmMain.bMovingParentDownClick(Sender: TObject);
//begin
//  Rectangle1.Position.Y := Rectangle1.Position.Y + StepForShifting;
//end;
//
//procedure TfrmMain.bMovingParentUpClick(Sender: TObject);
//begin
//  Rectangle1.Position.Y := Rectangle1.Position.Y - StepForShifting;
//end;



procedure TfrmMain.bToTheEndClick(Sender: TObject);
begin
  MediaPlayer1.CurrentTime := MediaPlayer1.Duration;
  MediaPlayer1.Stop;
end;

procedure TfrmMain.bToTheStartClick(Sender: TObject);
begin
  MediaPlayer1.CurrentTime := 0;
end;





procedure TfrmMain.Panel1Click(Sender: TObject);
begin
  FLibraryPath := GetPathWithVideo;
  FillFilesList;
  tbVolume.Value := MediaPlayer1.Volume;
end;




