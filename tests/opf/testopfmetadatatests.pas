unit TestOPFMetadataTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFIPIDMetadataTests }

  TTestOPFIPIDMetadataTests = class(TTestOPFIPIDTestCase)
  published
    procedure CreateIPID;
    procedure AttributeList;
    procedure InheritedAttributeList;
    procedure NonPidAttributeList;
  end;

  { TTestOPFProxyMetadataTests }

  TTestOPFProxyMetadataTests = class(TTestOPFProxyTestCase)
  published
    procedure CreateProxy;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreEntity,
  JCoreOPFMetadata,
  TestOPFModelIPID,
  TestOPFModelProxy;

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

initialization
  RegisterTest('jcore.opf.metadata.ipid', TTestOPFIPIDMetadataTests);
  RegisterTest('jcore.opf.metadata.proxy', TTestOPFProxyMetadataTests);

end.

