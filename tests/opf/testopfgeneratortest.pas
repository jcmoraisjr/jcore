unit TestOPFGeneratorTest;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFOIDGeneratorTest }

  TTestOPFOIDGeneratorTest = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure SequenceGeneratorSimple;
    procedure SequenceGeneratorInheritance;
    procedure AutoincGeneratorSimple;
    procedure AutoincGeneratorInheritance;
    procedure GUIDGenerator;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreOPFOID,
  JCoreOPFDriver,
  TestOPFModelInvoice;

{ TTestOPFOIDGeneratorTest }

procedure TTestOPFOIDGeneratorTest.SequenceGeneratorSimple;
var
  VProduct: TProduct;
  VID: string;
begin
  Config.Model.OIDGenerator := TJCoreOPFOIDGeneratorSequence.Create('GEN_APP');
  VProduct := TProduct.Create;
  try
    TTestSQLDriver.Data.Add('28');
    TTestSQLDriver.ExpectedResultsets.Add(1);
    Session.Store(VProduct);
    VID := VProduct._proxy.OID.AsString;
    AssertEquals('id', '28', VID);
    AssertSQLDriverCommands([
     'ExecSQL SELECT next(''GEN_APP'')',
     'WriteInt64 28',
     'WriteString ',
     'ExecSQL INSERT INTO PRODUCT (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFOIDGeneratorTest.SequenceGeneratorInheritance;
var
  VCompany: TCompany;
  VID: string;
begin
  Config.Model.OIDGenerator := TJCoreOPFOIDGeneratorSequence.Create('GEN_APP');
  VCompany := TCompany.Create;
  try
    TTestSQLDriver.Data.Add('11');
    TTestSQLDriver.ExpectedResultsets.Add(1);
    Session.Store(VCompany);
    VID := VCompany._proxy.OID.AsString;
    AssertEquals('id', '11', VID);
    AssertSQLDriverCommands([
     'ExecSQL SELECT next(''GEN_APP'')',
     'WriteInt64 11',
     'WriteString ',
     'WriteNull',
     'ExecSQL INSERT INTO CLIENT (ID,NAME,ADDRESS) VALUES (?,?,?)',
     'WriteInt64 11',
     'WriteString ',
     'ExecSQL INSERT INTO COMPANY (ID,CONTACTNAME) VALUES (?,?)']);
  finally
    FreeAndNil(VCompany);
  end;
end;

procedure TTestOPFOIDGeneratorTest.AutoincGeneratorSimple;
var
  VProduct: TProduct;
  VID: string;
begin
  Config.Model.OIDGenerator := TJCoreOPFOIDGeneratorAutoinc.Create;
  VProduct := TProduct.Create;
  try
    TTestSQLDriver.Data.Add('13');
    TTestSQLDriver.ExpectedResultsets.Add(1); // insert
    TTestSQLDriver.ExpectedResultsets.Add(1); // select
    Session.Store(VProduct);
    VID := VProduct._proxy.OID.AsString;
    AssertEquals('id', '13', VID);
    AssertSQLDriverCommands([
     'WriteString ',
     'ExecSQL INSERT INTO PRODUCT (NAME) VALUES (?)',
     'ExecSQL SELECT lastId()']);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFOIDGeneratorTest.AutoincGeneratorInheritance;
var
  VCompany: TCompany;
  VID: string;
begin
  Config.Model.OIDGenerator := TJCoreOPFOIDGeneratorAutoinc.Create;
  VCompany := TCompany.Create;
  try
    TTestSQLDriver.Data.Add('18');
    TTestSQLDriver.ExpectedResultsets.Add(1); // first insert
    TTestSQLDriver.ExpectedResultsets.Add(1); // select
    Session.Store(VCompany);
    VID := VCompany._proxy.OID.AsString;
    AssertEquals('id', '18', VID);
    AssertSQLDriverCommands([
     'WriteString ',
     'WriteNull',
     'ExecSQL INSERT INTO CLIENT (NAME,ADDRESS) VALUES (?,?)',
     'ExecSQL SELECT lastId()',
     'WriteInt64 18',
     'WriteString ',
     'ExecSQL INSERT INTO COMPANY (ID,CONTACTNAME) VALUES (?,?)']);
  finally
    FreeAndNil(VCompany);
  end;
end;

procedure TTestOPFOIDGeneratorTest.GUIDGenerator;
var
  VProduct: TProduct;
  VID: string;
  I: Integer;
begin
  Config.Model.OIDGenerator := TJCoreOPFOIDGeneratorGUID.Create;
  Config.Model.OIDClass := TJCoreOPFOIDString;
  VProduct := TProduct.Create;
  try
    Session.Store(VProduct);
    VID := VProduct._proxy.OID.AsString;
    AssertEquals('id size', 32, Length(VID));
    for I := 1 to Length(VID) do
      AssertTrue('id[' + IntToStr(I) + '] invalid (' + VID + ')', VID[I] in ['0'..'9','A'..'F']);
  finally
    FreeAndNil(VProduct);
  end;
end;

initialization
  RegisterTest('jcore.opf.generator', TTestOPFOIDGeneratorTest);

end.

