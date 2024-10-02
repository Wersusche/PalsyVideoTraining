unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth,
  FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.Stan.Pool, FireDAC.Phys.MySQL,
  FireDAC.DApt, FireDAC.Stan.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Option,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, Data.DB, System.JSON;

type
{$METHODINFO ON}
  TServerMethods1 = class(TComponent)
  private
    { Private declarations }
  public
    { Public declarations }
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function GetMessage: string;
    function ValidateCredentials(const Username, Password: string): Boolean;
   function GetPlaylist(const Username: string): TJSONArray;
    function UpdateCumulativeTime(const AppointmentsID: Integer; const CumulativeTime: Double): Boolean;
    function CompleteExercise(const AppointmentsID: Integer): Boolean;
    function CheckForScheduledExercises(const Username: string): Boolean;

    end;
{$METHODINFO OFF}

implementation


uses System.StrUtils;

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethods1.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;


function TServerMethods1.UpdateCumulativeTime(const AppointmentsID: Integer; const CumulativeTime: Double): Boolean;
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
begin
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  try
  try
    // Set up the connection
    FDConnection.DriverName := 'MySQL';
    FDConnection.Params.Values['Database'] := 'palsy_db';
    FDConnection.Params.Values['User_Name'] := 'wersusche';
    FDConnection.Params.Values['Password'] := 'tyjer1987';
    FDConnection.Params.Values['Server'] := 'db4free.net';
    FDConnection.Params.Values['CharacterSet'] := 'utf8mb4';
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

    // Prepare the query
    FDQuery.Connection := FDConnection;
    FDQuery.SQL.Text :=
      'UPDATE appointments SET CumulativeTimeSpent = :CumulativeTime WHERE idAppointments = :AppointmentsID';
    FDQuery.ParamByName('CumulativeTime').AsFloat := CumulativeTime;
    FDQuery.ParamByName('AppointmentsID').AsInteger := AppointmentsID;
    FDQuery.ExecSQL;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      raise Exception.Create('Error in UpdateCumulativeTime: ' + E.Message);
    end;
end;
  finally
    FDQuery.Free;
    FDConnection.Free;
  end;
end;

function TServerMethods1.CompleteExercise(const AppointmentsID: Integer): Boolean;
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
begin
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  try
  try
    // Set up the connection
    FDConnection.DriverName := 'MySQL';
    FDConnection.Params.Values['Database'] := 'palsy_db';
    FDConnection.Params.Values['User_Name'] := 'wersusche';
    FDConnection.Params.Values['Password'] := 'tyjer1987';
    FDConnection.Params.Values['Server'] := 'db4free.net';
    FDConnection.Params.Values['CharacterSet'] := 'utf8mb4';
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

    // Prepare the query
    FDQuery.Connection := FDConnection;
    FDQuery.SQL.Text :=
      'UPDATE appointments SET sdelanovden = sdelanovden + 1, ' +
      'sdelanovsego = sdelanovsego + 1, Lastsession = UTC_TIMESTAMP(), ' +
      'CumulativeTimeSpent = 0 WHERE idAppointments = :AppointmentsID';
    FDQuery.ParamByName('AppointmentsID').AsInteger := AppointmentsID;
    FDQuery.ExecSQL;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      raise Exception.Create('Error in CompleteExercise: ' + E.Message);
    end;
  end;
  finally
    FDQuery.Free;
    FDConnection.Free;
  end;
end;

 function TServerMethods1.CheckForScheduledExercises(const Username: string): Boolean;
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
begin
  Result := False;
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  try
  try
    // Set up the connection (as before)
        FDConnection.DriverName := 'MySQL';
    FDConnection.Params.Values['Database'] := 'palsy_db';
    FDConnection.Params.Values['User_Name'] := 'wersusche';
    FDConnection.Params.Values['Password'] := 'tyjer1987';
    FDConnection.Params.Values['Server'] := 'db4free.net';
    FDConnection.Params.Values['CharacterSet'] := 'utf8mb4';
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

    // Prepare the query
    FDQuery.Connection := FDConnection;
    FDQuery.SQL.Text :=
      'SELECT COUNT(*) AS ExerciseCount FROM patients P INNER JOIN ' +
      'appointments A ON P.idPatients = A.idPatients ' +
      'WHERE P.Username = :UserName AND CURDATE() BETWEEN A.Starttime AND A.Endtime';
    FDQuery.ParamByName('UserName').AsString := Username;
    FDQuery.Open;

    if FDQuery.FieldByName('ExerciseCount').AsInteger > 0 then
      Result := True;
  except
    on E: Exception do
    begin
      raise Exception.Create('Error in CheckForScheduledExercises: ' + E.Message);
    end;
  end;
  finally
    FDQuery.Free;
    FDConnection.Free;
  end;
end;

function TServerMethods1.GetPlaylist(const Username: string): TJSONArray;
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
    OriginalDecimalSeparator: Char;
begin
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  JSONArray := TJSONArray.Create;
    OriginalDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.'; // Set decimal separator to do
  try
  try
    // Set up the connection
    FDConnection.DriverName := 'MySQL';
    FDConnection.Params.Values['Database'] := 'palsy_db';
    FDConnection.Params.Values['User_Name'] := 'wersusche';
    FDConnection.Params.Values['Password'] := 'tyjer1987';
    FDConnection.Params.Values['Server'] := 'db4free.net';
    FDConnection.Params.Values['CharacterSet'] := 'utf8mb4';
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

// Prepare the query
    FDQuery.Connection := FDConnection;
    FDQuery.SQL.Text :=
      'SELECT P.idPatients, A.idAppointments, A.idvideos, A.dlitelnost, A.Lastsession, ' +
      'V.filename, V.video_name, NOW() AS CurrentTime, ' +
      'TIMESTAMPDIFF(HOUR, A.Lastsession, NOW()) AS HoursDifference, A.CumulativeTimeSpent ' +
      'FROM patients P INNER JOIN ' +
      'appointments A ON P.idPatients = A.idPatients INNER JOIN videos V ON A.idvideos = V.idvideos ' +
      'WHERE P.Username = :UserName AND CURDATE() BETWEEN A.Starttime AND A.Endtime AND A.sdelanovden < A.kolvden';

    FDQuery.ParamByName('UserName').AsString := Username;

    FDQuery.Open;

    while not FDQuery.Eof do
    begin
      JSONObject := TJSONObject.Create;
      JSONObject.AddPair('idPatients', FDQuery.FieldByName('idPatients').AsString);
      JSONObject.AddPair('idAppointments', FDQuery.FieldByName('idAppointments').AsString);
      JSONObject.AddPair('idvideos', FDQuery.FieldByName('idvideos').AsString);
      JSONObject.AddPair('dlitelnost', FDQuery.FieldByName('dlitelnost').AsString);
      JSONObject.AddPair('Lastsession', FDQuery.FieldByName('Lastsession').AsString);
      JSONObject.AddPair('filename', FDQuery.FieldByName('filename').AsString);
      JSONObject.AddPair('video_name', FDQuery.FieldByName('video_name').AsString);
      JSONObject.AddPair('CurrentTime', FDQuery.FieldByName('CurrentTime').AsString);
      JSONObject.AddPair('HoursDifference', FDQuery.FieldByName('HoursDifference').AsString);
      JSONObject.AddPair('CumulativeTimeSpent', FDQuery.FieldByName('CumulativeTimeSpent').AsString);

      JSONArray.AddElement(JSONObject);
      FDQuery.Next;
    end;

    Result := JSONArray;
  except
    on E: Exception do
    begin
    FormatSettings.DecimalSeparator := OriginalDecimalSeparator; // Restore original decimal separator
      JSONArray.Free;
      raise Exception.Create('Error in GetPlaylist: ' + E.Message);
    end;
  end;
    finally
    FDQuery.Free;
    FDConnection.Free;
  end;
end;

function TServerMethods1.GetMessage: string;
begin
  Result := 'Hello from Wersus!';
end;

function TServerMethods1.ValidateCredentials(const Username, Password: string): Boolean;
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
  hashedPassword: string;
begin
  Result := False;

  // Create connection
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  try
    // Connection settings
    FDConnection.DriverName := 'MySQL';
    FDConnection.Params.Values['Database'] := 'palsy_db';
    FDConnection.Params.Values['User_Name'] := 'wersusche';
    FDConnection.Params.Values['Password'] := 'tyjer1987';
    FDConnection.Params.Values['Server'] := 'db4free.net';
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

    // Prepare query
    FDQuery.Connection := FDConnection;
    FDQuery.SQL.Text := 'SELECT COUNT(*) FROM patients WHERE Username = :Username AND Password = :Password';
    FDQuery.ParamByName('Username').AsString := Username;
    FDQuery.ParamByName('Password').AsString := Password; // Or hashedPassword if you store hashed passwords
    FDQuery.Open;

    // Check result
    if FDQuery.Fields[0].AsInteger > 0 then
      Result := True;

  finally
    FDQuery.Free;
    FDConnection.Free;
  end;
end;
end.

