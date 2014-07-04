unit TestOPFSessionTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFSessionTests }

  TTestOPFSessionTests = class(TTestOPFProxyInvoiceAutoMappingTestCase)
  published
    procedure DriverNotFound;
    procedure MappingNotFound;
    procedure WithoutModelWithAggregation;
    procedure WithoutModelWithoutInheritance;
    procedure WithoutModelWithInheritance;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreOPFException,
  JCoreOPFGenerator,
  JCoreOPFConfig,
  JCoreOPFSession,
  JCoreOPFMappingSQL,
  TestOPFModelContact,
  TestOPFModelInvoice,
  TestOPFModelRegistry;

{ TTestOPFSessionTests }

procedure TTestOPFSessionTests.DriverNotFound;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.AddMappingClass([TTestEmptyMapping]);
  try
    VSession := VConfiguration.CreateSession;
    Fail(EJCoreOPFUndefinedDriver.ClassName + ' expected');
  except
    on E: EJCoreOPFUndefinedDriver do;
  end;
  VConfiguration.AddDriverClass([TTestEmptyDriver]);
  try
    VConfiguration.DriverName := TTestEmptyDriver.DriverName + ' invalid';
    Fail(EJCoreOPFDriverNotFound.ClassName + ' expected');
  except
    on E: EJCoreOPFDriverNotFound do;
  end;
  VConfiguration.DriverName := TTestEmptyDriver.DriverName;
  VSession := VConfiguration.CreateSession;
  AssertNotNull(VSession);
end;

procedure TTestOPFSessionTests.MappingNotFound;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VProduct: TProduct;
begin
  VConfiguration := TJCoreOPFConfiguration.Create(TTestOPFModelIPIDContact.Create);
  VConfiguration.DriverClass := TTestEmptyDriver;
  VConfiguration.Model.GeneratorStrategy := jgsCustom;
  VSession := VConfiguration.CreateSession;
  AssertNotNull(VSession);
  VProduct := TProduct.Create;
  try
    AssertExceptionStore(VSession, VProduct, EJCoreOPFMappingNotFound);
    VConfiguration.AddMappingClass([TTestEmptyMapping]);
    VSession.Store(VProduct);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFSessionTests.WithoutModelWithAggregation;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TTestProxyPerson;
  VInvoice: TInvoice;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.DriverClass := TTestSQLDriver;
  VConfiguration.AddMappingClass([TTestEmptyMapping]);
  VSession := VConfiguration.CreateSession;
  VPerson := TTestProxyPerson.Create;
  try
    try
      VSession.Store(VPerson);
      Fail(EJCoreOPFUnsupportedAttributeType.ClassName + ' expected');
    except
      on E: EJCoreOPFUnsupportedAttributeType do;
    end;
    VConfiguration.Model.AddClass([TTestProxyPhone, TTestProxyCity]);
    VSession.Store(VPerson);
  finally
    FreeAndNil(VPerson);
  end;
  VInvoice := TInvoice.Create;
  try
    try
      VSession.Store(VInvoice);
      Fail(EJCoreOPFUnsupportedAttributeType.ClassName + ' expected');
    except
      on E: EJCoreOPFUnsupportedAttributeType do;
    end;
    VConfiguration.Model.AddClass([TClient, TInvoiceItem]);
    VSession.Store(VInvoice);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFSessionTests.WithoutModelWithoutInheritance;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VProduct: TProduct;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.DriverClass := TTestSQLDriver;
  VConfiguration.AddMappingClass([TJCoreOPFSQLAutoMapping]);
  VConfiguration.Model.AddClass([TProduct]);
  VSession := VConfiguration.CreateSession;
  VProduct := TProduct.Create;
  try
    VProduct.Name := 'prod';
    VSession.Store(VProduct);
    AssertSQLDriverCommands([
     'WriteString ' + VProduct._proxy.OID.AsString,
     'WriteString prod',
     'ExecSQL INSERT INTO PRODUCT (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFSessionTests.WithoutModelWithInheritance;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VCompany: TCompany;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.DriverClass := TTestSQLDriver;
  VConfiguration.AddMappingClass([TJCoreOPFSQLAutoMapping]);
  VConfiguration.Model.AddClass([TProduct]);
  VSession := VConfiguration.CreateSession;
  VCompany := TCompany.Create;
  try
    VCompany.Name := 'comp';
    VCompany.ContactName := 'contact';
    VSession.Store(VCompany);
    AssertSQLDriverCommands([
     'WriteString ' + VCompany._proxy.OID.AsString,
     'WriteString comp',
     'ExecSQL INSERT INTO CLIENT (ID,NAME) VALUES (?,?)',
     'WriteString ' + VCompany._proxy.OID.AsString,
     'WriteString contact',
     'ExecSQL INSERT INTO COMPANY (ID,CONTACTNAME) VALUES (?,?)']);
  finally
    FreeAndNil(VCompany);
  end;
end;

initialization
  RegisterTest('jcore.opf.session', TTestOPFSessionTests);

end.

