unit TestOPFMappingTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFMappingTests }

  TTestOPFMappingTests = class(TTestOPFIPIDTestCase)
  published
    procedure TransactionPIDList;
    procedure StoreOwnerDontUpdateAggregations;
    procedure FailOwnOwnedComposition;
  end;

  { TTestOPFCleanDirtyAttributeTests }

  TTestOPFCleanDirtyAttributeTests = class(TTestOPFIPIDTestCase)
  published
    procedure CacheNotUpdated;
    procedure IntegerClear;
    procedure StringClean;
    procedure CompositionSimpleChanged;
    procedure CompositionChanged;
    procedure CompositionAdded;
    procedure CompositionRemoved;
    procedure CompositionRemovedAdded;
    procedure CompositionChangedOrder;
    procedure AggregationSimpleChanged;
    procedure AggregationChanged;
    procedure AggregationAdded;
    procedure AggregationRemoved;
    procedure AggregationRemovedAdded;
    procedure AggregationChangedOrder;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreOPFMetadata,
  TestOPFModel;

{ TTestOPFMappingTests }

procedure TTestOPFMappingTests.TransactionPIDList;
var
  VPerson: TTestIPIDPerson;
  VCity: TTestIPIDCity;
  VPhone1: TTestIPIDPhone;
  VPhone2: TTestIPIDPhone;
  VLang1: TTestIPIDLanguage;
  VLang2: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VCity := TTestIPIDCity.Create;
    VPerson.City := VCity;
    VPhone1 := TTestIPIDPhone.Create;
    VPerson.Phones.Add(VPhone1);
    VPhone2 := TTestIPIDPhone.Create;
    VPerson.Phones.Add(VPhone2);
    VLang1 := TTestIPIDLanguage.Create('1');
    VPerson.Languages.Add(VLang1);
    VLang2 := TTestIPIDLanguage.Create('2');
    VPerson.Languages.Add(VLang2);
    VPerson.Languages.Add(VLang2);
    VLang2.AddRef;
    TTestOPFSession.CommitCount := 0;
    Session.Store(VPerson);
    AssertEquals('commit1 cnt', 1, TTestOPFSession.CommitCount);
    AssertEquals('pid1 cnt', 6, TTestOPFSession.LastCommitPIDList.Count);
    AssertNotNull('vperson1 pid', VPerson._PID);
    AssertTrue('vperson.pid in list', TTestOPFSession.ExistPIDInCommitPIDList(VPerson._PID));
    AssertNotNull('vcity pid', VCity._PID);
    AssertTrue('vcity.pid in list', TTestOPFSession.ExistPIDInCommitPIDList(VCity._PID));
    AssertNotNull('vphone1 pid', VPhone1._PID);
    AssertTrue('vphone1.pid in list', TTestOPFSession.ExistPIDInCommitPIDList(VPhone1._PID));
    AssertNotNull('vphone2 pid', VPhone2._PID);
    AssertTrue('vphone2.pid in list', TTestOPFSession.ExistPIDInCommitPIDList(VPhone2._PID));
    AssertNotNull('vlang1 pid', VLang1._PID);
    AssertTrue('vlang1.pid in list', TTestOPFSession.ExistPIDInCommitPIDList(VLang1._PID));
    AssertNotNull('vlang2 pid', VLang2._PID);
    AssertTrue('vlang2.pid in list', TTestOPFSession.ExistPIDInCommitPIDList(VLang2._PID));
    VPerson.City := nil;
    VCity := nil;
    TTestOPFSession.CommitCount := 0;
    Session.Store(VPerson);
    AssertEquals('commit2 cnt', 1, TTestOPFSession.CommitCount);
    AssertEquals('pid2 cnt', 5, TTestOPFSession.LastCommitPIDList.Count);
    AssertSame('vperson2 pid', TTestOPFSession.LastCommitPIDList[0], VPerson._PID as TJCoreOPFPID);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFMappingTests.StoreOwnerDontUpdateAggregations;
var
  VPerson: TTestIPIDPerson;
  VLang: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang := TTestIPIDLanguage.Create('english');
    VPerson.Languages.Add(VLang);
    Session.Store(VPerson);
    AssertNotNull('lang pid', VLang._PID);
    AssertTrue('lang clean', not VLang._PID.IsDirty);
    VPerson.Name := 'SomeName';
    VLang.Name := 'spanish';
    Session.Store(VPerson);
    AssertTrue('lang dirty', VLang._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFMappingTests.FailOwnOwnedComposition;
var
  VPerson: TTestIPIDPerson;
  VLang: TTestIPIDLanguage;
  VPhone: TTestIPIDPhone;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang := TTestIPIDLanguage.Create('english');
    VPerson.Languages.Add(VLang);
    VPerson.Languages.Add(VLang);
    VLang.AddRef;
    Session.Store(VPerson);
    VPhone := TTestIPIDPhone.Create;
    VPerson.Phones.Add(VPhone);
    VPerson.Phones.Add(VPhone);
    VPhone.AddRef;
    { TODO : Implement duplication check in the same admcollection }
    // AssertExceptionStore(FSession, VPerson, EJCoreOPFObjectAlreadyOwned);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFCleanDirtyAttributeTests }

procedure TTestOPFCleanDirtyAttributeTests.CacheNotUpdated;
var
  VPerson: TTestIPIDPerson;
  VPID: TJCoreOPFPID;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Age := 0;
    VPID := Session.AcquireMapping(VPerson.ClassType).AcquirePID(VPerson);
    AssertTrue('person dirty1', VPID.IsDirty);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertSame('same pid instance', VPID, VPerson._PID as TJCoreOPFPID);
    AssertFalse('person dirty2', VPerson._PID.IsDirty);
    VPerson.Age := 10;
    AssertTrue('person dirty3', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.IntegerClear;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Age := 30;
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Age := 33;
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
    VPerson.Age := 30;
    AssertFalse('person dirty3', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.StringClean;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'Some name';
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Name := 'Other name';
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
    VPerson.Name := 'Some name';
    AssertFalse('person dirty3', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.CompositionSimpleChanged;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Address := TTestIPIDAddress.Create;
    VPerson.Address.Street := 'freeway s/n';
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Address.Street := 'freeway 150';
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
    VPerson.Address.Street := 'freeway s/n';
    AssertFalse('person dirty3', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.CompositionChanged;
var
  VPerson: TTestIPIDPerson;
  VPhone: TTestIPIDPhone;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPhone := TTestIPIDPhone.Create;
    VPhone.Number := '123';
    VPerson.Phones.Add(VPhone);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Phones[0].Number := '123-123';
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
    VPerson.Phones[0].Number := '123';
    AssertFalse('person dirty3', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.CompositionAdded;
var
  VPerson: TTestIPIDPerson;
  VPhone: TTestIPIDPhone;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPhone := TTestIPIDPhone.Create;
    VPhone.Number := '321';
    VPerson.Phones.Add(VPhone);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPhone := TTestIPIDPhone.Create;
    VPhone.Number := '321-321';
    VPerson.Phones.Add(VPhone);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
    VPerson.Phones.Delete(1);
    AssertFalse('person dirty3', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.CompositionRemoved;
var
  VPerson: TTestIPIDPerson;
  VPhone: TTestIPIDPhone;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPhone := TTestIPIDPhone.Create;
    VPhone.Number := '456';
    VPerson.Phones.Add(VPhone);
    VPhone := TTestIPIDPhone.Create;
    VPhone.Number := '456-789';
    VPerson.Phones.Add(VPhone);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Phones.Delete(0);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.CompositionRemovedAdded;
var
  VPerson: TTestIPIDPerson;
  VPhone: TTestIPIDPhone;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPhone := TTestIPIDPhone.Create;
    VPhone.Number := '456';
    VPerson.Phones.Add(VPhone);
    VPhone := TTestIPIDPhone.Create;
    VPhone.Number := '456-789';
    VPerson.Phones.Add(VPhone);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Phones.Delete(1);
    VPhone := TTestIPIDPhone.Create;
    VPhone.Number := '456-7890';
    VPerson.Phones.Add(VPhone);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.CompositionChangedOrder;
var
  VPerson: TTestIPIDPerson;
  VPhone1, VPhone2: TTestIPIDPhone;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPhone1 := TTestIPIDPhone.Create;
    VPhone1.Number := '456';
    VPhone2 := TTestIPIDPhone.Create;
    VPhone2.Number := '789';
    VPerson.Phones.Add(VPhone1);
    VPerson.Phones.Add(VPhone2);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPhone1.AddRef;
    VPhone2.AddRef;
    VPerson.Phones.Clear;
    VPerson.Phones.Add(VPhone2);
    VPerson.Phones.Add(VPhone1);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.AggregationSimpleChanged;
var
  VPerson: TTestIPIDPerson;
  VCity: TTestIPIDCity;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VCity := TTestIPIDCity.Create;
    VCity.Name := 'sampa';
    VPerson.City := VCity;
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.City.Name := 'ny';
    AssertFalse('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.AggregationChanged;
var
  VPerson: TTestIPIDPerson;
  VLang: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang := TTestIPIDLanguage.Create('english');
    VPerson.Languages.Add(VLang);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Languages[0].Name := 'spanish';
    AssertFalse('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.AggregationAdded;
var
  VPerson: TTestIPIDPerson;
  VLang1, VLang2: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang1 := TTestIPIDLanguage.Create('english');
    VPerson.Languages.Add(VLang1);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VLang2 := TTestIPIDLanguage.Create('german');
    VPerson.Languages.Add(VLang2);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.AggregationRemoved;
var
  VPerson: TTestIPIDPerson;
  VLang1, VLang2: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang1 := TTestIPIDLanguage.Create('spanish');
    VLang2 := TTestIPIDLanguage.Create('italian');
    VPerson.Languages.Add(VLang1);
    VPerson.Languages.Add(VLang2);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Languages.Delete(1);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.AggregationRemovedAdded;
var
  VPerson: TTestIPIDPerson;
  VLang1, VLang2, VLang3: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang1 := TTestIPIDLanguage.Create('italian');
    VLang2 := TTestIPIDLanguage.Create('german');
    VPerson.Languages.Add(VLang1);
    VPerson.Languages.Add(VLang2);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Languages.Delete(1);
    VLang3 := TTestIPIDLanguage.Create('portuguese');
    VPerson.Languages.Add(VLang3);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.AggregationChangedOrder;
var
  VPerson: TTestIPIDPerson;
  VLang1, VLang2: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang1 := TTestIPIDLanguage.Create('english');
    VLang2 := TTestIPIDLanguage.Create('german');
    VPerson.Languages.Add(VLang1);
    VPerson.Languages.Add(VLang2);
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VLang1.AddRef;
    VPerson.Languages.Delete(0);
    VLang2.AddRef;
    VPerson.Languages.Delete(0);
    VPerson.Languages.Add(VLang2);
    VPerson.Languages.Add(VLang1);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

initialization
  RegisterTest('jcore.opf.mapping.core', TTestOPFMappingTests);
  RegisterTest('jcore.opf.mapping.cleandirty', TTestOPFCleanDirtyAttributeTests);

end.

