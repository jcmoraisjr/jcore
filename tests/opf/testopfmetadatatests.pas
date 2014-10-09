unit TestOPFMetadataTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFMetadataTest }

  TTestOPFMetadataTest = class(TTestOPFInvoiceManualMappingTestCase)
  published
    procedure CreatePIDInheritance;
    procedure CircularReference;
  end;

  { TTestOPFIPIDMetadataTests }

  TTestOPFIPIDMetadataTests = class(TTestOPFIPIDContactTestCase)
  published
    procedure CreateIPID;
    procedure AttributeList;
    procedure InheritedAttributeList;
    procedure NonPidAttributeList;
  end;

  { TTestOPFProxyMetadataTests }

  TTestOPFProxyMetadataTests = class(TTestOPFProxyContactTestCase)
  published
    procedure CreateProxy;
    procedure LazyEntity;
    procedure LazyCollection;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreEntity,
  JCoreOPFConfig,
  JCoreOPFSession,
  JCoreOPFMappingSQL,
  JCoreOPFMetadata,
  TestOPFModelContact,
  TestOPFModelInvoice,
  TestOPFModelCircular;

type
  TTestPIDFriend = class(TJCoreOPFPID);

{ TTestOPFMetadataTest }

procedure TTestOPFMetadataTest.CreatePIDInheritance;
var
  VCompany: TCompany;
  VPID: TJCoreOPFPID;
  VADMMap: TJCoreOPFADMMap;
begin
  VCompany := TCompany.Create;
  try
    Session.Store(VCompany);
    AssertNotNull('company proxy', VCompany._proxy);
    VPID := VCompany._proxy.PID as TJCoreOPFPID;
    AssertNotNull('company pid', VPID);
    VADMMap := TTestPIDFriend(VPID).ADMMap;
    AssertEquals('pid cnt adm', 3, VADMMap.Count);
    AssertEquals('pid.adm0', 'Name', VADMMap.Data[0].Metadata.Name);
    AssertEquals('pid.adm1', 'Address', VADMMap.Data[1].Metadata.Name);
    AssertEquals('pid.adm2', 'ContactName', VADMMap.Data[2].Metadata.Name);
  finally
    FreeAndNil(VCompany);
  end;
end;

procedure TTestOPFMetadataTest.CircularReference;
var
  VConfig: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TCircularPerson;
begin
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.DriverClass := TTestSQLDriver;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.AddClass([TCircularPerson]);
  VSession := VConfig.CreateSession;
  VPerson := TCircularPerson.Create;
  try
    VSession.Store(VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFIPIDMetadataTests }

procedure TTestOPFIPIDMetadataTests.CreateIPID;
var
  VPerson: TTestIPIDPerson;
  VPID: IJCorePID;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    AssertNull(VPerson._PID);
    Session.Store(VPerson);
    VPID := VPerson._PID;
    AssertNotNull(VPID);
    AssertSame(VPID.Entity, VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFIPIDMetadataTests.AttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := Session.AcquireMetadata(TTestIPIDPerson);
  AssertEquals('meta.cnt', 6, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Name', VMetadata[0].Name);
  AssertEquals('meta1.name', 'Age', VMetadata[1].Name);
  AssertEquals('meta2.name', 'Phones', VMetadata[2].Name);
  AssertEquals('meta3.name', 'Address', VMetadata[3].Name);
  AssertEquals('meta4.name', 'City', VMetadata[4].Name);
  AssertEquals('meta5.name', 'Languages', VMetadata[5].Name);
end;

procedure TTestOPFIPIDMetadataTests.InheritedAttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := Session.AcquireMetadata(TTestIPIDEmployee);
  AssertEquals('meta.cnt', 1, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Salary', VMetadata[0].Name);
end;

procedure TTestOPFIPIDMetadataTests.NonPidAttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := Session.AcquireMetadata(TTestIPIDSimple);
  AssertEquals('meta.cnt', 1, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Field1', VMetadata[0].Name);
end;

{ TTestOPFProxyMetadataTests }

procedure TTestOPFProxyMetadataTests.CreateProxy;
var
  VCity: TTestProxyCity;
begin
  VCity := TTestProxyCity.Create;
  try
    AssertNull('city.proxy', VCity._Proxy);
    AssertTrue('city.isdirty', VCity._Proxy.IsDirty);
    AssertFalse('city.ispersistent', VCity._Proxy.IsPersistent);
    AssertNull('city.oid null', VCity._Proxy.OID);
    AssertNull('city.owner null', VCity._Proxy.Owner);
    AssertNull('city.pid null', VCity._Proxy.PID);
    Session.Store(VCity);
    AssertNotNull('city.pid', VCity._Proxy.PID);
    AssertSame('city.entity', VCity._Proxy.PID.Entity, VCity);
  finally
    FreeAndNil(VCity);
  end;
end;

procedure TTestOPFProxyMetadataTests.LazyEntity;
var
  VPerson: TTestProxyPerson;
  VCity: TTestProxyCity;
begin
  TTestSQLDriver.Data.Add('2');
  TTestSQLDriver.Data.Add('somename');
  TTestSQLDriver.Data.Add('20');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('4');
  TTestSQLDriver.ExpectedResultsets.Add(1);
  VPerson := Session.Retrieve(TTestProxyPerson, '2') as TTestProxyPerson;
  try
    AssertEquals('person cmd', 0, TTestSQLDriver.Data.Count);
    AssertEquals('person exprs', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertNotNull('person nil', VPerson);
    AssertNotNull('person oid nil', VPerson._Proxy.OID);
    AssertEquals('person oid', '2', VPerson._Proxy.OID.AsString);
    AssertEquals('person name', 'somename', VPerson.Name);
    AssertSQLDriverCommands([
     'WriteInt64 2',
     'ExecSQL SELECT ID,NAME,AGE,ADDRESS,CITY FROM PERSON WHERE ID=?']);

    TTestSQLDriver.Data.Add('4');
    TTestSQLDriver.Data.Add('acity');
    TTestSQLDriver.ExpectedResultsets.Add(1);
    TTestSQLDriver.Commands.Clear;
    VCity := VPerson.City;
    AssertEquals('city cmd', 0, TTestSQLDriver.Data.Count);
    AssertEquals('city exprs', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertNotNull('city nil', VCity);
    AssertNotNull('city oid nil', VCity._Proxy.OID);
    AssertEquals('city oid', '4', VCity._Proxy.OID.AsString);
    AssertEquals('city name', 'acity', VCity.Name);
    AssertSQLDriverCommands([
     'WriteInt64 4',
     'ExecSQL SELECT ID,NAME FROM CITY WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFProxyMetadataTests.LazyCollection;
var
  VPerson: TTestProxyPerson;
  VPhones: TTestProxyPhoneList;
begin
  TTestSQLDriver.Data.Add('21');
  TTestSQLDriver.Data.Add('name');
  TTestSQLDriver.Data.Add('38');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.ExpectedResultsets.Add(1);
  VPerson := Session.Retrieve(TTestProxyPerson, '21') as TTestProxyPerson;
  try
    AssertEquals('person cmd', 0, TTestSQLDriver.Data.Count);
    AssertEquals('person exprs', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertNotNull('person nil', VPerson);
    AssertNotNull('person oid nil', VPerson._Proxy.OID);
    AssertEquals('person oid', '21', VPerson._Proxy.OID.AsString);
    AssertEquals('person name', 'name', VPerson.Name);
    AssertSQLDriverCommands([
     'WriteInt64 21',
     'ExecSQL SELECT ID,NAME,AGE,ADDRESS,CITY FROM PERSON WHERE ID=?']);

    TTestSQLDriver.Data.Add('18');
    TTestSQLDriver.Data.Add('1213-9876');
    TTestSQLDriver.Data.Add('19');
    TTestSQLDriver.Data.Add('2231-6621');
    TTestSQLDriver.Data.Add('20');
    TTestSQLDriver.Data.Add('9989-3399');
    TTestSQLDriver.ExpectedResultsets.Add(3);
    TTestSQLDriver.Commands.Clear;
    VPhones := VPerson.Phones;
    AssertEquals('phones cmd', 0, TTestSQLDriver.Data.Count);
    AssertEquals('phones exprs', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertNotNull('phones nil', VPhones);
    AssertEquals('phones count', 3, VPhones.Count);
    AssertNotNull('phone0 oid nil', VPhones[0]._Proxy.OID);
    AssertEquals('phone0 oid', '18', VPhones[0]._Proxy.OID.AsString);
    AssertEquals('phone0 number', '1213-9876', VPhones[0].Number);
    AssertNotNull('phone1 oid nil', VPhones[1]._Proxy.OID);
    AssertEquals('phone1 oid', '19', VPhones[1]._Proxy.OID.AsString);
    AssertEquals('phone1 number', '2231-6621', VPhones[1].Number);
    AssertNotNull('phone2 oid nil', VPhones[2]._Proxy.OID);
    AssertEquals('phone2 oid', '20', VPhones[2]._Proxy.OID.AsString);
    AssertEquals('phone2 number', '9989-3399', VPhones[2].Number);
    AssertSQLDriverCommands([
     'WriteInt64 21',
     'ExecSQL SELECT ID,NUMBER FROM PHONE WHERE PERSON=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

initialization
  RegisterTest('jcore.opf.metadata.common', TTestOPFMetadataTest);
  RegisterTest('jcore.opf.metadata.ipid', TTestOPFIPIDMetadataTests);
  RegisterTest('jcore.opf.metadata.proxy', TTestOPFProxyMetadataTests);

end.

