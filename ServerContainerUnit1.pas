unit ServerContainerUnit1;

interface

uses System.SysUtils, System.Classes,
  Datasnap.DSHTTPCommon, Datasnap.DSHTTP,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  IPPeerServer, IPPeerAPI, Datasnap.DSAuth, IndyPeerImpl,
  IdHTTPWebBrokerBridge, IdSSLOpenSSL, IdContext, IdCustomHTTPServer,
  Datasnap.DSHTTPWebBroker, IdBaseComponent, IdComponent, IdServerIOHandler,
  IdSSL, IdHTTPServer, IdCustomTCPServer;

type
  TServerContainer1 = class(TDataModule)
    DSServer1: TDSServer;
    DSHTTPService1: TDSHTTPService;
    DSServerClass1: TDSServerClass;
    IdServerIOHandlerSSLOpenSSL1: TIdServerIOHandlerSSLOpenSSL;
    IdHTTPServer1: TIdHTTPServer;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
  private
    { Private declarations }
  public
  end;

var
  ServerContainer1: TServerContainer1;

implementation


{$R *.dfm}

uses
  ServerMethodsUnit1;

procedure TServerContainer1.DSServerClass1GetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := ServerMethodsUnit1.TServerMethods1;
end;

end.

