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
    procedure Inheritance;
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
    procedure InheritanceUpdateParentClass;
    procedure InheritanceUpdateSubClass;
    procedure Inheritance;
  end;

  { TTestOPFDeleteAutoMappingTests }

  TTestOPFDeleteAutoMappingTests = class(TTestOPFProxyInvoiceAutoMappingTestCase)
  published
    procedure SingleFromEntity;
    procedure SingleFromClass;
    procedure InheritanceFromEntity;
    procedure InheritanceFromClass;
    procedure InheritanceFromClassWithOIDArray;
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

procedure TTestOPFInsertAutoMappingTests.Inheritance;
var
  VCompany: TCompany;
begin
  VCompany := TCompany.Create;
  try
    VCompany.Name := 'corp';
    VCompany.ContactName := 'james';
    Session.Store(VCompany);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteString corp',
     'ExecSQL INSERT INTO CLIENT (ID,NAME) VALUES (?,?)',
     'WriteInt64 1',
     'WriteString james',
     'ExecSQL INSERT INTO COMPANY (ID,CONTACTNAME) VALUES (?,?)']);
  finally
    FreeAndNil(VCompany);
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

procedure TTestOPFUpdateAutoMappingTests.InheritanceUpdateParentClass;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'james';
    VPerson.Nick := 'j';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Name := 'j.james';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString j.james',
     'WriteInt64 1',
     'ExecSQL UPDATE CLIENT SET NAME=? WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.InheritanceUpdateSubClass;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'j.james';
    VPerson.Nick := 'j';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Nick := 'jj';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString jj',
     'WriteInt64 1',
     'ExecSQL UPDATE PERSON SET NICK=? WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.Inheritance;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'james';
    VPerson.Nick := 'j';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Name := 'j.james';
    VPerson.Nick := 'jj';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString j.james',
     'WriteInt64 1',
     'ExecSQL UPDATE CLIENT SET NAME=? WHERE ID=?',
     'WriteString jj',
     'WriteInt64 1',
     'ExecSQL UPDATE PERSON SET NICK=? WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
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

procedure TTestOPFDeleteAutoMappingTests.InheritanceFromEntity;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'joe';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    Session.Dispose(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'ExecSQL DELETE FROM CLIENT WHERE ID=?',
     'WriteInt64 1',
     'ExecSQL DELETE FROM PERSON WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFDeleteAutoMappingTests.InheritanceFromClass;
begin
  Session.Dispose(TPerson, ['5']);
  AssertSQLDriverCommands([
   'WriteInt64 5',
   'ExecSQL DELETE FROM CLIENT WHERE ID=?',
   'WriteInt64 5',
   'ExecSQL DELETE FROM PERSON WHERE ID=?']);
end;

procedure TTestOPFDeleteAutoMappingTests.InheritanceFromClassWithOIDArray;
begin
  Session.Dispose(TPerson, ['7', '9', '11']);
  AssertSQLDriverCommands([
   'WriteInt64 7',
   'WriteInt64 9',
   'WriteInt64 11',
   'ExecSQL DELETE FROM CLIENT WHERE ID IN (?,?,?)',
   'WriteInt64 7',
   'WriteInt64 9',
   'WriteInt64 11',
   'ExecSQL DELETE FROM PERSON WHERE ID IN (?,?,?)']);
end;

initialization
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFInsertAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFSelectAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFUpdateAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFDeleteAutoMappingTests);

end.

