// 
// Created by the DataSnap proxy generator.
// 25.09.2024 17:12:23
// 

unit ClientClassesUnit3;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.DBXJSONReflect;

type
  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FEchoStringCommand: TDSRestCommand;
    FReverseStringCommand: TDSRestCommand;
    FGetMessageCommand: TDSRestCommand;
    FValidateCredentialsCommand: TDSRestCommand;
    FGetPlaylistCommand: TDSRestCommand;
    FGetPlaylistCommand_Cache: TDSRestCommand;
    FUpdateCumulativeTimeCommand: TDSRestCommand;
    FCompleteExerciseCommand: TDSRestCommand;
    FCheckForScheduledExercisesCommand: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function EchoString(Value: string; const ARequestFilter: string = ''): string;
    function ReverseString(Value: string; const ARequestFilter: string = ''): string;
    function GetMessage(const ARequestFilter: string = ''): string;
    function ValidateCredentials(Username: string; Password: string; const ARequestFilter: string = ''): Boolean;
    function GetPlaylist(Username: string; const ARequestFilter: string = ''): TJSONArray;
    function GetPlaylist_Cache(Username: string; const ARequestFilter: string = ''): IDSRestCachedJSONArray;
    function UpdateCumulativeTime(AppointmentsID: Integer; CumulativeTime: Double; const ARequestFilter: string = ''): Boolean;
    function CompleteExercise(AppointmentsID: Integer; const ARequestFilter: string = ''): Boolean;
    function CheckForScheduledExercises(Username: string; const ARequestFilter: string = ''): Boolean;
  end;

const
  TServerMethods1_EchoString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_ReverseString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_GetMessage: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_ValidateCredentials: array [0..2] of TDSRestParameterMetaData =
  (
    (Name: 'Username'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'Password'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 4; TypeName: 'Boolean')
  );

  TServerMethods1_GetPlaylist: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Username'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TJSONArray')
  );

  TServerMethods1_GetPlaylist_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Username'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_UpdateCumulativeTime: array [0..2] of TDSRestParameterMetaData =
  (
    (Name: 'AppointmentsID'; Direction: 1; DBXType: 6; TypeName: 'Integer'),
    (Name: 'CumulativeTime'; Direction: 1; DBXType: 7; TypeName: 'Double'),
    (Name: ''; Direction: 4; DBXType: 4; TypeName: 'Boolean')
  );

  TServerMethods1_CompleteExercise: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AppointmentsID'; Direction: 1; DBXType: 6; TypeName: 'Integer'),
    (Name: ''; Direction: 4; DBXType: 4; TypeName: 'Boolean')
  );

  TServerMethods1_CheckForScheduledExercises: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Username'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 4; TypeName: 'Boolean')
  );

implementation

function TServerMethods1Client.EchoString(Value: string; const ARequestFilter: string): string;
begin
  if FEchoStringCommand = nil then
  begin
    FEchoStringCommand := FConnection.CreateCommand;
    FEchoStringCommand.RequestType := 'GET';
    FEchoStringCommand.Text := 'TServerMethods1.EchoString';
    FEchoStringCommand.Prepare(TServerMethods1_EchoString);
  end;
  FEchoStringCommand.Parameters[0].Value.SetWideString(Value);
  FEchoStringCommand.Execute(ARequestFilter);
  Result := FEchoStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.ReverseString(Value: string; const ARequestFilter: string): string;
begin
  if FReverseStringCommand = nil then
  begin
    FReverseStringCommand := FConnection.CreateCommand;
    FReverseStringCommand.RequestType := 'GET';
    FReverseStringCommand.Text := 'TServerMethods1.ReverseString';
    FReverseStringCommand.Prepare(TServerMethods1_ReverseString);
  end;
  FReverseStringCommand.Parameters[0].Value.SetWideString(Value);
  FReverseStringCommand.Execute(ARequestFilter);
  Result := FReverseStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.GetMessage(const ARequestFilter: string): string;
begin
  if FGetMessageCommand = nil then
  begin
    FGetMessageCommand := FConnection.CreateCommand;
    FGetMessageCommand.RequestType := 'GET';
    FGetMessageCommand.Text := 'TServerMethods1.GetMessage';
    FGetMessageCommand.Prepare(TServerMethods1_GetMessage);
  end;
  FGetMessageCommand.Execute(ARequestFilter);
  Result := FGetMessageCommand.Parameters[0].Value.GetWideString;
end;

function TServerMethods1Client.ValidateCredentials(Username: string; Password: string; const ARequestFilter: string): Boolean;
begin
  if FValidateCredentialsCommand = nil then
  begin
    FValidateCredentialsCommand := FConnection.CreateCommand;
    FValidateCredentialsCommand.RequestType := 'GET';
    FValidateCredentialsCommand.Text := 'TServerMethods1.ValidateCredentials';
    FValidateCredentialsCommand.Prepare(TServerMethods1_ValidateCredentials);
  end;
  FValidateCredentialsCommand.Parameters[0].Value.SetWideString(Username);
  FValidateCredentialsCommand.Parameters[1].Value.SetWideString(Password);
  FValidateCredentialsCommand.Execute(ARequestFilter);
  Result := FValidateCredentialsCommand.Parameters[2].Value.GetBoolean;
end;

function TServerMethods1Client.GetPlaylist(Username: string; const ARequestFilter: string): TJSONArray;
begin
  if FGetPlaylistCommand = nil then
  begin
    FGetPlaylistCommand := FConnection.CreateCommand;
    FGetPlaylistCommand.RequestType := 'GET';
    FGetPlaylistCommand.Text := 'TServerMethods1.GetPlaylist';
    FGetPlaylistCommand.Prepare(TServerMethods1_GetPlaylist);
  end;
  FGetPlaylistCommand.Parameters[0].Value.SetWideString(Username);
  FGetPlaylistCommand.Execute(ARequestFilter);
  Result := TJSONArray(FGetPlaylistCommand.Parameters[1].Value.GetJSONValue(FInstanceOwner));
end;

function TServerMethods1Client.GetPlaylist_Cache(Username: string; const ARequestFilter: string): IDSRestCachedJSONArray;
begin
  if FGetPlaylistCommand_Cache = nil then
  begin
    FGetPlaylistCommand_Cache := FConnection.CreateCommand;
    FGetPlaylistCommand_Cache.RequestType := 'GET';
    FGetPlaylistCommand_Cache.Text := 'TServerMethods1.GetPlaylist';
    FGetPlaylistCommand_Cache.Prepare(TServerMethods1_GetPlaylist_Cache);
  end;
  FGetPlaylistCommand_Cache.Parameters[0].Value.SetWideString(Username);
  FGetPlaylistCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedJSONArray.Create(FGetPlaylistCommand_Cache.Parameters[1].Value.GetString);
end;

function TServerMethods1Client.UpdateCumulativeTime(AppointmentsID: Integer; CumulativeTime: Double; const ARequestFilter: string): Boolean;
begin
  if FUpdateCumulativeTimeCommand = nil then
  begin
    FUpdateCumulativeTimeCommand := FConnection.CreateCommand;
    FUpdateCumulativeTimeCommand.RequestType := 'GET';
    FUpdateCumulativeTimeCommand.Text := 'TServerMethods1.UpdateCumulativeTime';
    FUpdateCumulativeTimeCommand.Prepare(TServerMethods1_UpdateCumulativeTime);
  end;
  FUpdateCumulativeTimeCommand.Parameters[0].Value.SetInt32(AppointmentsID);
  FUpdateCumulativeTimeCommand.Parameters[1].Value.SetDouble(CumulativeTime);
  FUpdateCumulativeTimeCommand.Execute(ARequestFilter);
  Result := FUpdateCumulativeTimeCommand.Parameters[2].Value.GetBoolean;
end;

function TServerMethods1Client.CompleteExercise(AppointmentsID: Integer; const ARequestFilter: string): Boolean;
begin
  if FCompleteExerciseCommand = nil then
  begin
    FCompleteExerciseCommand := FConnection.CreateCommand;
    FCompleteExerciseCommand.RequestType := 'GET';
    FCompleteExerciseCommand.Text := 'TServerMethods1.CompleteExercise';
    FCompleteExerciseCommand.Prepare(TServerMethods1_CompleteExercise);
  end;
  FCompleteExerciseCommand.Parameters[0].Value.SetInt32(AppointmentsID);
  FCompleteExerciseCommand.Execute(ARequestFilter);
  Result := FCompleteExerciseCommand.Parameters[1].Value.GetBoolean;
end;

function TServerMethods1Client.CheckForScheduledExercises(Username: string; const ARequestFilter: string): Boolean;
begin
  if FCheckForScheduledExercisesCommand = nil then
  begin
    FCheckForScheduledExercisesCommand := FConnection.CreateCommand;
    FCheckForScheduledExercisesCommand.RequestType := 'GET';
    FCheckForScheduledExercisesCommand.Text := 'TServerMethods1.CheckForScheduledExercises';
    FCheckForScheduledExercisesCommand.Prepare(TServerMethods1_CheckForScheduledExercises);
  end;
  FCheckForScheduledExercisesCommand.Parameters[0].Value.SetWideString(Username);
  FCheckForScheduledExercisesCommand.Execute(ARequestFilter);
  Result := FCheckForScheduledExercisesCommand.Parameters[1].Value.GetBoolean;
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection);
begin
  inherited Create(ARestConnection);
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ARestConnection, AInstanceOwner);
end;

destructor TServerMethods1Client.Destroy;
begin
  FEchoStringCommand.DisposeOf;
  FReverseStringCommand.DisposeOf;
  FGetMessageCommand.DisposeOf;
  FValidateCredentialsCommand.DisposeOf;
  FGetPlaylistCommand.DisposeOf;
  FGetPlaylistCommand_Cache.DisposeOf;
  FUpdateCumulativeTimeCommand.DisposeOf;
  FCompleteExerciseCommand.DisposeOf;
  FCheckForScheduledExercisesCommand.DisposeOf;
  inherited;
end;

end.

