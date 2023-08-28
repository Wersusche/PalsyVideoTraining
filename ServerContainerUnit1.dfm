object ServerContainer1: TServerContainer1
  Height = 271
  Width = 415
  object DSServer1: TDSServer
    Left = 96
    Top = 11
  end
  object DSTCPServerTransport1: TDSTCPServerTransport
    Server = DSServer1
    Filters = <>
    Left = 96
    Top = 73
  end
  object DSHTTPService1: TDSHTTPService
    HttpPort = 8080
    Server = DSServer1
    Filters = <>
    Left = 96
    Top = 135
  end
  object DSCertFiles1: TDSCertFiles
    OnGetPEMFileSBPasskey = DSCertFiles1GetPEMFileSBPasskey
    Left = 200
    Top = 197
  end
  object DSHTTPService2: TDSHTTPService
    HttpPort = 8081
    CertFiles = DSCertFiles1
    Server = DSServer1
    Filters = <>
    Left = 200
    Top = 135
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    Left = 200
    Top = 11
  end
end
