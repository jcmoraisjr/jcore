unit TestOPFMappingAutoTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFInsertAutoMappingTests }

  TTestOPFInsertAutoMappingTests = class(TTestOPFProxyInvoiceAutoMappingTestCase)
  published
    procedure Single;
  end;

  { TTestOPFSelectAutoMappingTests }

  TTestOPFSelectAutoMappingTests = class(TTestOPFProxyInvoiceAutoMappingTestCase)
  published
    procedure Single;
  end;

  { TTestOPFUpdateAutoMappingTests }

  TTestOPFUpdateAutoMappingTests = class(TTestOPFProxyInvoiceAutoMappingTestCase)
  published
    procedure Single;
  end;

  { TTestOPFDeleteAutoMappingTests }

  TTestOPFDeleteAutoMappingTests = class(TTestOPFProxyInvoiceAutoMappingTestCase)
  published
    procedure SingleFromEntity;
    procedure SingleFromClass;
  end;

implementation

uses
  testregistry,
  sysutils,
  TestOPFModelInvoice;

{ TTestOPFInsertAutoMappingTests }

procedure TTestOPFInsertAutoMappingTests.Single;
var
  VProduct: TProduct;
begin
  VProduct := TProduct.Create;
  try
    VProduct.Name := 'aprod';
    Session.Store(VProduct);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteString aprod',
     'ExecSQL INSERT INTO PRODUCT (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VProduct);
  end;
end;

{ TTestOPFSelectAutoMappingTests }

procedure TTestOPFSelectAutoMappingTests.Single;
var
  VProduct: TProduct;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('6');
  TTestSQLDriver.Data.Add('theprod');
  VProduct := Session.Retrieve(TProduct, '6') as TProduct;
  try
    AssertNotNull('prod nil', VProduct);
    AssertEquals('prod id', '6', VProduct._proxy.OID.AsString);
    AssertEquals('prod name', 'theprod', VProduct.Name);
    AssertSQLDriverCommands([
     'WriteInt64 6',
     {1}'ExecSQL SELECT ID,NAME FROM PRODUCT WHERE ID=?']);
    AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  finally
    FreeAndNil(VProduct);
  end;
end;

{ TTestOPFUpdateAutoMappingTests }

procedure TTestOPFUpdateAutoMappingTests.Single;
var
  VProduct: TProduct;
begin
  VProduct := TProduct.Create;
  try
    VProduct.Name := 'aprod';
    Session.Store(VProduct);
    TTestSQLDriver.Commands.Clear;
    VProduct.Name := 'newprod';
    Session.Store(VProduct);
    AssertSQLDriverCommands([
     'WriteString newprod',
     'WriteInt64 1',
     'ExecSQL UPDATE PRODUCT SET NAME=? WHERE ID=?']);
  finally
    FreeAndNil(VProduct);
  end;
end;

{ TTestOPFDeleteAutoMappingTests }

procedure TTestOPFDeleteAutoMappingTests.SingleFromEntity;
var
  VProduct: TProduct;
begin
  VProduct := TProduct.Create;
  try
    VProduct.Name := 'aprod';
    Session.Store(VProduct);
    TTestSQLDriver.Commands.Clear;
    Session.Dispose(VProduct);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'ExecSQL DELETE FROM PRODUCT WHERE ID=?']);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFDeleteAutoMappingTests.SingleFromClass;
begin
  Session.Dispose(TProduct, ['7']);
  AssertSQLDriverCommands([
   'WriteInt64 7',
   'ExecSQL DELETE FROM PRODUCT WHERE ID=?']);
end;

initialization
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFInsertAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFSelectAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFUpdateAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFDeleteAutoMappingTests);

end.

