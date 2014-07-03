unit TestOPFGeneratorTest;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFGeneratorTest }

  TTestOPFGeneratorTest = class(TTestOPFProxyInvoiceManualMappingTestCase)
  published
    procedure GUIDTest;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreOPFGenerator,
  JCoreOPFOID,
  JCoreOPFConfig,
  JCoreOPFSession,
  TestOPFModelRegistry,
  TestOPFModelInvoice;

{ TTestOPFGeneratorTest }

procedure TTestOPFGeneratorTest.GUIDTest;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VProduct: TProduct;
  VID: string;
  I: Integer;
begin
  VConfiguration := TJCoreOPFConfiguration.Create(TTestOPFModelProxyInvoice.Create);
  VConfiguration.AddDriverClass(TTestEmptyDriver);
  VConfiguration.DriverName := TTestEmptyDriver.DriverName;
  VConfiguration.AddMappingClass([TTestEmptyMapping]);
  VConfiguration.Model.GeneratorStrategy := jgsGUID;
  VConfiguration.Model.OIDClass := TJCoreOPFStringOID;
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
  RegisterTest('jcore.opf.generator', TTestOPFGeneratorTest);

end.

