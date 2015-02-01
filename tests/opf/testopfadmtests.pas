unit TestOPFADMTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFMetadataAttributeTest }

  TTestOPFMetadataAttributeTest = class(TTestOPFSimpleTestCase)
  published
    procedure InsertIntegerClassAttr;
    procedure InsertIntegerIntfAttr;
    procedure SelectIntegerValueIntfAttr;
    procedure SelectIntegerNilIntfAttr;
    procedure UpdateIntegerValueIntfAttr;
    procedure UpdateIntegerNilIntfAttr;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreEntity,
  JCoreOPFADM,
  TestOPFModelInvoice;

type

  TTestIntegerAttr = class(TCustomAttrEntity)
  private
    FAgeClass: TJCoreOPFIntegerType;
    FAgeIntf: IJCoreIntegerType;
    FAgeNative: Integer;
  published
    property AgeClass: TJCoreOPFIntegerType read FAgeClass write FAgeClass;
    property AgeIntf: IJCoreIntegerType read FAgeIntf write FAgeIntf;
    property AgeNative: Integer read FAgeNative write FAgeNative;
  end;

{ TTestOPFMetadataAttributeTest }

procedure TTestOPFMetadataAttributeTest.InsertIntegerClassAttr;
var
  VIntegerAttr: TTestIntegerAttr;
begin
  Config.Model.AddClass([TTestIntegerAttr]);
  VIntegerAttr := TTestIntegerAttr.Create;
  try
    VIntegerAttr.AgeClass.Value := 10;
    Session.Store(VIntegerAttr);
    AssertSQLDriverCommands([
     'WriteString ' + VIntegerAttr._proxy.OID.AsString,
     'WriteInt32 10',
     'WriteInt32 0',
     'WriteInt32 0',
     'ExecSQL INSERT INTO TESTINTEGERATTR (ID,AGECLASS,AGEINTF,AGENATIVE) VALUES (?,?,?,?)']);
  finally
    FreeAndNil(VIntegerAttr);
  end;
end;

procedure TTestOPFMetadataAttributeTest.InsertIntegerIntfAttr;
var
  VIntegerAttr: TTestIntegerAttr;
begin
  Config.Model.AddClass([TTestIntegerAttr]);
  VIntegerAttr := TTestIntegerAttr.Create;
  try
    VIntegerAttr.AgeIntf.Value := 10;
    Session.Store(VIntegerAttr);
    AssertSQLDriverCommands([
     'WriteString ' + VIntegerAttr._proxy.OID.AsString,
     'WriteInt32 0',
     'WriteInt32 10',
     'WriteInt32 0',
     'ExecSQL INSERT INTO TESTINTEGERATTR (ID,AGECLASS,AGEINTF,AGENATIVE) VALUES (?,?,?,?)']);
  finally
    FreeAndNil(VIntegerAttr);
  end;
end;

procedure TTestOPFMetadataAttributeTest.SelectIntegerValueIntfAttr;
var
  VIntegerAttr: TTestIntegerAttr;
begin
  Config.Model.AddClass([TTestIntegerAttr]);
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('18');
  TTestSQLDriver.Data.Add('0');
  TTestSQLDriver.Data.Add('15');
  TTestSQLDriver.Data.Add('0');
  VIntegerAttr := Session.Retrieve(TTestIntegerAttr, '18') as TTestIntegerAttr;
  try
    AssertSQLDriverCommands([
     'WriteString 18',
     'ExecSQL SELECT ID,AGECLASS,AGEINTF,AGENATIVE FROM TESTINTEGERATTR WHERE ID=?']);
    AssertEquals('ageintf', 15, VIntegerAttr.AgeIntf.Value);
  finally
    FreeAndNil(VIntegerAttr);
  end;
end;

procedure TTestOPFMetadataAttributeTest.SelectIntegerNilIntfAttr;
var
  VIntegerAttr: TTestIntegerAttr;
begin
  Config.Model.AddClass([TTestIntegerAttr]);
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('28');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('0');
  VIntegerAttr := Session.Retrieve(TTestIntegerAttr, '28') as TTestIntegerAttr;
  try
    AssertSQLDriverCommands([
     'WriteString 28',
     'ExecSQL SELECT ID,AGECLASS,AGEINTF,AGENATIVE FROM TESTINTEGERATTR WHERE ID=?']);
    AssertTrue('ageintf', VIntegerAttr.AgeIntf.IsNull);
  finally
    FreeAndNil(VIntegerAttr);
  end;
end;

procedure TTestOPFMetadataAttributeTest.UpdateIntegerValueIntfAttr;
var
  VIntegerAttr: TTestIntegerAttr;
begin
  Config.Model.AddClass([TTestIntegerAttr]);
  VIntegerAttr := TTestIntegerAttr.Create;
  try
    VIntegerAttr.AgeIntf.Value := 10;
    Session.Store(VIntegerAttr);
    TTestSQLDriver.Commands.Clear;
    VIntegerAttr.AgeIntf.Value := 20;
    Session.Store(VIntegerAttr);
    AssertSQLDriverCommands([
     'WriteInt32 20',
     'WriteString ' + VIntegerAttr._proxy.OID.AsString,
     'ExecSQL UPDATE TESTINTEGERATTR SET AGEINTF=? WHERE ID=?']);
  finally
    FreeAndNil(VIntegerAttr);
  end;
end;

procedure TTestOPFMetadataAttributeTest.UpdateIntegerNilIntfAttr;
var
  VIntegerAttr: TTestIntegerAttr;
begin
  Config.Model.AddClass([TTestIntegerAttr]);
  VIntegerAttr := TTestIntegerAttr.Create;
  try
    VIntegerAttr.AgeIntf.Value := 15;
    Session.Store(VIntegerAttr);
    TTestSQLDriver.Commands.Clear;
    VIntegerAttr.AgeIntf.Clear;
    Session.Store(VIntegerAttr);
    AssertSQLDriverCommands([
     'WriteNull',
     'WriteString ' + VIntegerAttr._proxy.OID.AsString,
     'ExecSQL UPDATE TESTINTEGERATTR SET AGEINTF=? WHERE ID=?']);
  finally
    FreeAndNil(VIntegerAttr);
  end;
end;

initialization
  RegisterTest('jcore.opf.metadata.attr', TTestOPFMetadataAttributeTest);

end.

