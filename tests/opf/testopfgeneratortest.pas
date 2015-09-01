unit TestOPFGeneratorTest;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFOIDGeneratorTest }

  TTestOPFOIDGeneratorTest = class(TTestOPFInvoiceManualMappingTestCase)
  published
    procedure SequenceGenerator;
    procedure GUIDGenerator;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreOPFMetadata,
  JCoreOPFADM,
  JCoreOPFOID,
  JCoreOPFDriver,
  JCoreOPFConfig,
  JCoreOPFSession,
  TestOPFModelInvoice;

{ TTestOPFOIDGeneratorTest }

procedure TTestOPFOIDGeneratorTest.SequenceGenerator;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VProduct: TProduct;
  VID: string;
begin
  VConfiguration := TJCoreOPFConfiguration.Create(TJCoreOPFModel.Create);
  VConfiguration.DriverClass := TTestSQLDriver;
  VConfiguration.AddMappingClass([TTestEmptyMapping]);
  VConfiguration.Model.AddADMClass([TJCoreOPFADMAnsiStringNativeCtl]);
  VConfiguration.Model.OIDClass := TJCoreOPFOIDInt64;
  VConfiguration.Model.OIDGenerator := TJCoreOPFOIDGeneratorSequence.Create('GEN_APP');
  VSession := VConfiguration.CreateSession;
  VProduct := TProduct.Create;
  try
    TTestSQLDriver.Data.Add('28');
    TTestSQLDriver.ExpectedResultsets.Add(1);
    VSession.Store(VProduct);
    VID := VProduct._proxy.OID.AsString;
    AssertEquals('id', '28', VID);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFOIDGeneratorTest.GUIDGenerator;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VProduct: TProduct;
  VID: string;
  I: Integer;
begin
  VConfiguration := TJCoreOPFConfiguration.Create(TJCoreOPFModel.Create);
  VConfiguration.DriverClass := TTestEmptyDriver;
  VConfiguration.AddMappingClass([TTestEmptyMapping]);
  VConfiguration.Model.AddADMClass([TJCoreOPFADMAnsiStringNativeCtl]);
  VConfiguration.Model.OIDGenerator := TJCoreOPFOIDGeneratorGUID.Create;
  VConfiguration.Model.OIDClass := TJCoreOPFOIDString;
  VSession := VConfiguration.CreateSession;
  VProduct := TProduct.Create;
  try
    VSession.Store(VProduct);
    VID := VProduct._proxy.OID.AsString;
    AssertEquals('id size', 32, Length(VID));
    for I := 1 to Length(VID) do
      AssertTrue('id[' + IntToStr(I) + '] invalid (' + VID + ')', VID[I] in ['0'..'9','A'..'F']);
  finally
    FreeAndNil(VProduct);
  end;
end;

initialization
  RegisterTest('jcore.opf.oidgenerator', TTestOPFOIDGeneratorTest);

end.

