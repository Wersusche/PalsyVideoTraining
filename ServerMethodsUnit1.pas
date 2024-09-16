unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth,
  FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.Stan.Pool, FireDAC.Phys.MySQL,
  FireDAC.DApt, FireDAC.Stan.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Option,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait;

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

