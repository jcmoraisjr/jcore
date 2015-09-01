unit TestOPFGeneratorTest;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFOIDGeneratorTest }

  TTestOPFOIDGeneratorTest = class(TTestOPFInvoiceManualMappingTestCase)
  published
    procedure Int64Generator;
    procedure GUIDGenerator;
    procedure RetrieveEmptyOIDList;
    procedure RepopulateOIDList;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreClasses,
  JCoreOPFMetadata,
  JCoreOPFADM,
  JCoreOPFOIDGen,
  JCoreOPFOID,
  JCoreOPFDriver,
  JCoreOPFConfig,
  JCoreOPFSession,
  TestOPFModelInvoice;

type

  { TTestOPFGenerator }

  TTestOPFGenerator = class(TJCoreOPFOIDGeneratorSQLDriver)
  private
    FLast: Integer;
  protected
    procedure InternalGenerateOIDs(const AOIDCount: Integer); override;
    property Last: Integer read FLast;
  public
    function OIDCount: Integer;
  end;

procedure TTestOPFGenerator.InternalGenerateOIDs(const AOIDCount: Integer);
var
  I: Integer;
begin
  for I := 0 to Pred(AOIDCount) do
  begin
    Inc(FLast);
    OIDList.Add(Last);
  end;
end;

function TTestOPFGenerator.OIDCount: Integer;
begin
  Result := OIDList.Count;
end;

{ TTestOPFOIDGeneratorTest }

procedure TTestOPFOIDGeneratorTest.Int64Generator;
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
  VConfiguration.Model.SequenceName := 'GEN_APP';
  VSession := VConfiguration.CreateSession;
  VProduct := TProduct.Create;
  try
    VSession.Store(VProduct);
    VID := VProduct._proxy.OID.AsString;
    AssertEquals('id', '1', VID);
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
  VConfiguration.Model.SequenceName := '';
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

procedure TTestOPFOIDGeneratorTest.RetrieveEmptyOIDList;
var
  VGenerator: IJCoreOPFOIDGenerator;
begin
  VGenerator := TTestOPFGenerator.Create(nil, '');
  VGenerator.GenerateOIDs(2);
  VGenerator.ReadInt64;
  VGenerator.ReadInt64;
  try
    VGenerator.ReadInt64;
    Fail('EJCoreOPF(2124) expected');
  except
    on E: EJCoreOPF do
      if E.Code <> 2124 then
        raise;
  end;
end;

procedure TTestOPFOIDGeneratorTest.RepopulateOIDList;
var
  VGenerator: TTestOPFGenerator;
  VOID: Int64;
begin
  VGenerator := TTestOPFGenerator.Create(nil, '');
  try
    VGenerator.GenerateOIDs(1);
    AssertEquals('oidlist count 1', 1, VGenerator.OIDCount);
    VOID := VGenerator.ReadInt64;
    AssertEquals('oid value 1', 1, VOID);
    VGenerator.GenerateOIDs(3);
    AssertEquals('oidlist count 3', 3, VGenerator.OIDCount);
    VOID := VGenerator.ReadInt64;
    AssertEquals('oid value 2', 2, VOID);
    VOID := VGenerator.ReadInt64;
    AssertEquals('oid value 3', 3, VOID);
    VGenerator.GenerateOIDs(3);
    AssertEquals('oidlist count 4', 4, VGenerator.OIDCount);
    VOID := VGenerator.ReadInt64;
    AssertEquals('oid value 4', 4, VOID);
    VOID := VGenerator.ReadInt64;
    AssertEquals('oid value 5', 5, VOID);
    VOID := VGenerator.ReadInt64;
    AssertEquals('oid value 6', 6, VOID);
    AssertEquals('oidlist count 5', 4, VGenerator.OIDCount);
  finally
    FreeAndNil(VGenerator);
  end;
end;

initialization
  RegisterTest('jcore.opf.oidgenerator', TTestOPFOIDGeneratorTest);

end.

