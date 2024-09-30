object ServerContainer1: TServerContainer1
  Height = 271
  Width = 415
  object DSServer1: TDSServer
    Left = 96
    Top = 11
  end
  object DSHTTPService1: TDSHTTPService
    HttpPort = 8080
    Server = DSServer1
    DSHostname = '91.207.183.63'
    Filters = <>
    Left = 96
    Top = 135
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    Left = 200
    Top = 11
  end
  object IdServerIOHandlerSSLOpenSSL1: TIdServerIOHandlerSSLOpenSSL
    SSLOptions.CertFile = 'C:\Users\Cheptsov VV\Downloads\chiminc.crt'
    SSLOptions.KeyFile = 'C:\Users\Cheptsov VV\Downloads\chiminck.key'
    SSLOptions.Method = sslvSSLv23
    SSLOptions.SSLVersions = [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2]
    SSLOptions.Mode = sslmServer
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 256
    Top = 144
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 443
    IOHandler = IdServerIOHandlerSSLOpenSSL1
    Left = 304
    Top = 72
  end
end
