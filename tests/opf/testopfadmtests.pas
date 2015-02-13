unit TestOPFADMTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFMetadataADMControllerTest }

  TTestOPFMetadataADMControllerTest = class(TTestOPFSimpleTestCase)
  published
    procedure InsertIntegerIntfAttr;
    procedure SelectIntegerValueIntfAttr;
    procedure SelectIntegerNilIntfAttr;
    procedure UpdateIntegerFromValueToValueIntfAttr;
    procedure UpdateIntegerFromValueToNullIntfAttr;
    procedure UpdateIntegerFromNullToValueIntfAttr;
  end;

  { TTestOPFMetadataADMValueTypeTest }

  TTestOPFMetadataADMValueTypeTest = class(TTestOPFSimpleTestCase)
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
  JCoreTypes,
  JCoreEntity,
  JCoreOPFADM,
  TestOPFModelInvoice;

type

  TTestIntegerValueTypeIntf = class(TCustomAttrEntity)
  private
    FAgeNative: Integer;
    FAge: IJCoreIntegerType;
  published
    property AgeNative: Integer read FAgeNative write FAgeNative;
    property Age: IJCoreIntegerType read FAge write FAge;
  end;

  TTestIntegerValueTypeClass = class(TCustomAttrEntity)
  private
    FAgeNative: Integer;
    FAge: TJCoreOPFIntegerType;
  published
    property AgeNative: Integer read FAgeNative write FAgeNative;
    property Age: TJCoreOPFIntegerType read FAge write FAge;
  end;

  TTestIntegerIntfCtl = class(TCustomAttrEntity)
  private
    FAgeNative: Integer;
    FAge: IJCoreInteger;
  published
    property AgeNative: Integer read FAgeNative write FAgeNative;
    property Age: IJCoreInteger read FAge write FAge;
  end;

{ TTestOPFMetadataADMControllerTest }

procedure TTestOPFMetadataADMControllerTest.InsertIntegerIntfAttr;
var
  VInteger: TTestIntegerIntfCtl;
begin
  Config.Model.AddClass([TTestIntegerIntfCtl]);
  VInteger := TTestIntegerIntfCtl.Create;
  try
    VInteger.Age := TJCoreInteger.ValueOf(16);
    Session.Store(VInteger);
    AssertSQLDriverCommands([
     'WriteString ' + VInteger._proxy.OID.AsString,
     'WriteInt32 0',
     'WriteInt32 16',
     'ExecSQL INSERT INTO TESTINTEGERINTFCTL (ID,AGENATIVE,AGE) VALUES (?,?,?)']);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMControllerTest.SelectIntegerValueIntfAttr;
var
  VInteger: TTestIntegerIntfCtl;
begin
  Config.Model.AddClass([TTestIntegerIntfCtl]);
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('3');
  TTestSQLDriver.Data.Add('0');
  TTestSQLDriver.Data.Add('16');
  VInteger := Session.Retrieve(TTestIntegerIntfCtl, '3') as TTestIntegerIntfCtl;
  try
    AssertSQLDriverCommands([
     'WriteString 3',
     'ExecSQL SELECT ID,AGENATIVE,AGE FROM TESTINTEGERINTFCTL WHERE ID=?']);
    AssertEquals('age', 16, VInteger.Age.AsInteger);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMControllerTest.SelectIntegerNilIntfAttr;
var
  VInteger: TTestIntegerIntfCtl;
begin
  Config.Model.AddClass([TTestIntegerIntfCtl]);
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('3');
  TTestSQLDriver.Data.Add('0');
  TTestSQLDriver.Data.Add('null');
  VInteger := Session.Retrieve(TTestIntegerIntfCtl, '3') as TTestIntegerIntfCtl;
  try
    AssertSQLDriverCommands([
     'WriteString 3',
     'ExecSQL SELECT ID,AGENATIVE,AGE FROM TESTINTEGERINTFCTL WHERE ID=?']);
    AssertNull('age', VInteger.Age);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMControllerTest.UpdateIntegerFromValueToValueIntfAttr;
var
  VInteger: TTestIntegerIntfCtl;
begin
  Config.Model.AddClass([TTestIntegerIntfCtl]);
  VInteger := TTestIntegerIntfCtl.Create;
  try
    VInteger.Age := TJCoreInteger.ValueOf(14);
    Session.Store(VInteger);
    VInteger.Age := TJCoreInteger.ValueOf(18);
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInteger);
    AssertSQLDriverCommands([
     'WriteInt32 18',
     'WriteString ' + VInteger._proxy.OID.AsString,
     'ExecSQL UPDATE TESTINTEGERINTFCTL SET AGE=? WHERE ID=?']);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMControllerTest.UpdateIntegerFromValueToNullIntfAttr;
var
  VInteger: TTestIntegerIntfCtl;
begin
  Config.Model.AddClass([TTestIntegerIntfCtl]);
  VInteger := TTestIntegerIntfCtl.Create;
  try
    VInteger.Age := TJCoreInteger.ValueOf(14);
    Session.Store(VInteger);
    VInteger.Age := nil;
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInteger);
    AssertSQLDriverCommands([
     'WriteNull',
     'WriteString ' + VInteger._proxy.OID.AsString,
     'ExecSQL UPDATE TESTINTEGERINTFCTL SET AGE=? WHERE ID=?']);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMControllerTest.UpdateIntegerFromNullToValueIntfAttr;
var
  VInteger: TTestIntegerIntfCtl;
begin
  Config.Model.AddClass([TTestIntegerIntfCtl]);
  VInteger := TTestIntegerIntfCtl.Create;
  try
    VInteger.Age := nil;
    Session.Store(VInteger);
    VInteger.Age := TJCoreInteger.ValueOf(21);
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInteger);
    AssertSQLDriverCommands([
     'WriteInt32 21',
     'WriteString ' + VInteger._proxy.OID.AsString,
     'ExecSQL UPDATE TESTINTEGERINTFCTL SET AGE=? WHERE ID=?']);
  finally
    FreeAndNil(VInteger);
  end;
end;

{ TTestOPFMetadataADMValueTypeTest }

procedure TTestOPFMetadataADMValueTypeTest.InsertIntegerClassAttr;
var
  VInteger: TTestIntegerValueTypeClass;
begin
  Config.Model.AddClass([TTestIntegerValueTypeClass]);
  VInteger := TTestIntegerValueTypeClass.Create;
  try
    VInteger.Age.Value := 10;
    Session.Store(VInteger);
    AssertSQLDriverCommands([
     'WriteString ' + VInteger._proxy.OID.AsString,
     'WriteInt32 0',
     'WriteInt32 10',
     'ExecSQL INSERT INTO TESTINTEGERVALUETYPECLASS (ID,AGENATIVE,AGE) VALUES (?,?,?)']);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMValueTypeTest.InsertIntegerIntfAttr;
var
  VInteger: TTestIntegerValueTypeIntf;
begin
  Config.Model.AddClass([TTestIntegerValueTypeIntf]);
  VInteger := TTestIntegerValueTypeIntf.Create;
  try
    VInteger.Age.Value := 10;
    Session.Store(VInteger);
    AssertSQLDriverCommands([
     'WriteString ' + VInteger._proxy.OID.AsString,
     'WriteInt32 0',
     'WriteInt32 10',
     'ExecSQL INSERT INTO TESTINTEGERVALUETYPEINTF (ID,AGENATIVE,AGE) VALUES (?,?,?)']);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMValueTypeTest.SelectIntegerValueIntfAttr;
var
  VInteger: TTestIntegerValueTypeIntf;
begin
  Config.Model.AddClass([TTestIntegerValueTypeIntf]);
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('18');
  TTestSQLDriver.Data.Add('0');
  TTestSQLDriver.Data.Add('15');
  VInteger := Session.Retrieve(TTestIntegerValueTypeIntf, '18') as TTestIntegerValueTypeIntf;
  try
    AssertSQLDriverCommands([
     'WriteString 18',
     'ExecSQL SELECT ID,AGENATIVE,AGE FROM TESTINTEGERVALUETYPEINTF WHERE ID=?']);
    AssertEquals('age', 15, VInteger.Age.Value);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMValueTypeTest.SelectIntegerNilIntfAttr;
var
  VInteger: TTestIntegerValueTypeIntf;
begin
  Config.Model.AddClass([TTestIntegerValueTypeIntf]);
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('28');
  TTestSQLDriver.Data.Add('0');
  TTestSQLDriver.Data.Add('null');
  VInteger := Session.Retrieve(TTestIntegerValueTypeIntf, '28') as TTestIntegerValueTypeIntf;
  try
    AssertSQLDriverCommands([
     'WriteString 28',
     'ExecSQL SELECT ID,AGENATIVE,AGE FROM TESTINTEGERVALUETYPEINTF WHERE ID=?']);
    AssertTrue('age', VInteger.Age.IsNull);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMValueTypeTest.UpdateIntegerValueIntfAttr;
var
  VInteger: TTestIntegerValueTypeIntf;
begin
  Config.Model.AddClass([TTestIntegerValueTypeIntf]);
  VInteger := TTestIntegerValueTypeIntf.Create;
  try
    VInteger.Age.Value := 10;
    Session.Store(VInteger);
    TTestSQLDriver.Commands.Clear;
    VInteger.Age.Value := 20;
    Session.Store(VInteger);
    AssertSQLDriverCommands([
     'WriteInt32 20',
     'WriteString ' + VInteger._proxy.OID.AsString,
     'ExecSQL UPDATE TESTINTEGERVALUETYPEINTF SET AGE=? WHERE ID=?']);
  finally
    FreeAndNil(VInteger);
  end;
end;

procedure TTestOPFMetadataADMValueTypeTest.UpdateIntegerNilIntfAttr;
var
  VInteger: TTestIntegerValueTypeIntf;
begin
  Config.Model.AddClass([TTestIntegerValueTypeIntf]);
  VInteger := TTestIntegerValueTypeIntf.Create;
  try
    VInteger.Age.Value := 15;
    Session.Store(VInteger);
    TTestSQLDriver.Commands.Clear;
    VInteger.Age.Clear;
    Session.Store(VInteger);
    AssertSQLDriverCommands([
     'WriteNull',
     'WriteString ' + VInteger._proxy.OID.AsString,
     'ExecSQL UPDATE TESTINTEGERVALUETYPEINTF SET AGE=? WHERE ID=?']);
  finally
    FreeAndNil(VInteger);
  end;
end;

initialization
  RegisterTest('jcore.opf.metadata.adm.object', TTestOPFMetadataADMControllerTest);
  RegisterTest('jcore.opf.metadata.adm.valuetype', TTestOPFMetadataADMValueTypeTest);

end.

