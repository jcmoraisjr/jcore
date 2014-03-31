unit TestOPFMappingManualTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFInsertManualMappingTests }

  TTestOPFInsertManualMappingTests = class(TTestOPF)
  published
    procedure Person;
    procedure PersonAddress;
    procedure PersonCity;
    procedure PersonPhones;
    procedure PersonLanguages;
    procedure PersonAggregationChange;
  end;

  { TTestOPFUpdateManualMappingTests }

  TTestOPFUpdateManualMappingTests = class(TTestOPF)
  published
    procedure City;
    procedure PersonCity;
    procedure PersonAddPhones;
    procedure PersonRemovePhones;
    procedure PersonAddLanguages;
    procedure PersonRemoveLanguages;
    procedure PersonRemoveAddLanguages;
    procedure PersonNoLanguagesChange;
    procedure PersonAggregationChange;
  end;

  { TTestOPFSelectManualMappingTests }

  TTestOPFSelectManualMappingTests = class(TTestOPF)
  published
    procedure City;
    procedure PersonAddressCity;
    procedure PersonCity;
    procedure PersonNullAddressCity;
    procedure PersonPhones;
    procedure PersonLanguages;
  end;

  { TTestOPFDeleteOneManualMappingTests }

  TTestOPFDeleteOneManualMappingTests = class(TTestOPF)
  published
    procedure City;
    procedure PersonAddress;
    procedure PersonCity;
    procedure PersonOnePhone;
    procedure PersonOneLanguage;
    procedure PersonPhones;
    procedure PersonLanguages;
  end;

  { TTestOPFDeleteArrayManualMappingTests }

  TTestOPFDeleteArrayManualMappingTests = class(TTestOPF)
  published
    procedure City;
    procedure PersonAddress;
    procedure PersonCity;
    procedure PersonPhones;
    procedure PersonLanguages;
  end;

implementation

uses
  testregistry,
  sysutils,
  TestOPFModel,
  TestOPFMappingManual;

{ TTestOPFInsertManualMappingTests }

procedure TTestOPFInsertManualMappingTests.Person;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'TheName';
    VPerson.Age := 15;
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteString TheName',
     'WriteInteger 15',
     'WriteNull',
     'WriteNull',
     'ExecSQL ' + CSQLINSERTPERSON]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualMappingTests.PersonAddress;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'name';
    VPerson.Age := 25;
    VPerson.Address := TTestAddress.Create;
    VPerson.Address.Street := 'route 66';
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 1, VPerson._PID.OID.AsInteger);
    AssertNotNull('address pid', VPerson.Address._PID);
    AssertEquals('address oid', 2, VPerson.Address._PID.OID.AsInteger);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteString name',
     'WriteInteger 25',
     'WriteInteger 2',
     'WriteString route 66',
     'WriteString ',
     'ExecSQL ' + CSQLINSERTADDRESS,
     'WriteInteger 2',
     'WriteNull',
     'ExecSQL ' + CSQLINSERTPERSON]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualMappingTests.PersonCity;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'SomeName';
    VPerson.Age := 25;
    VPerson.City := TTestCity.Create;
    VPerson.City.Name := 'CityName';
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 1, VPerson._PID.OID.AsInteger);
    AssertNotNull('city pid', VPerson.City._PID);
    AssertEquals('city oid', 2, VPerson.City._PID.OID.AsInteger);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteString SomeName',
     'WriteInteger 25',
     'WriteNull',
     'WriteInteger 2',
     'WriteString CityName',
     'ExecSQL ' + CSQLINSERTCITY,
     'WriteInteger 2',
     'ExecSQL ' + CSQLINSERTPERSON]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualMappingTests.PersonPhones;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'thename';
    VPerson.Age := 10;
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones[0].Number := '636-3626';
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones[1].Number := '212-4321';
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 1, VPerson._PID.OID.AsInteger);
    AssertNotNull('phone0 pid', VPerson.Phones[0]._PID);
    AssertEquals('phone0 oid', 2, VPerson.Phones[0]._PID.OID.AsInteger);
    AssertNotNull('phone1 pid', VPerson.Phones[1]._PID);
    AssertEquals('phone1 oid', 3, VPerson.Phones[1]._PID.OID.AsInteger);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteString thename',
     'WriteInteger 10',
     'WriteNull',
     'WriteNull',
     'ExecSQL ' + CSQLINSERTPERSON,
     'WriteInteger 2',
     'WriteInteger 1',
     'WriteString 636-3626',
     'ExecSQL ' + CSQLINSERTPHONE,
     'WriteInteger 3',
     'WriteInteger 1',
     'WriteString 212-4321',
     'ExecSQL ' + CSQLINSERTPHONE]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualMappingTests.PersonLanguages;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'SomeName';
    VPerson.Languages.Add(TTestLanguage.Create('English'));
    VPerson.Languages.Add(TTestLanguage.Create('Spanish'));
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 1, VPerson._PID.OID.AsInteger);
    AssertNotNull('lang0 pid', VPerson.Languages[0]._PID);
    AssertEquals('lang0 oid', 2, VPerson.Languages[0]._PID.OID.AsInteger);
    AssertNotNull('lang1 pid', VPerson.Languages[1]._PID);
    AssertEquals('lang1 oid', 3, VPerson.Languages[1]._PID.OID.AsInteger);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteString SomeName',
     'WriteInteger 0',
     'WriteNull',
     'WriteNull',
     'ExecSQL ' + CSQLINSERTPERSON,
     'WriteInteger 2',
     'WriteString English',
     'ExecSQL ' + CSQLINSERTLANG,
     'WriteInteger 3',
     'WriteString Spanish',
     'ExecSQL ' + CSQLINSERTLANG,
     'WriteInteger 1',
     'WriteInteger 2',
     'ExecSQL ' + CSQLINSERTPERSON_LANG,
     'WriteInteger 1',
     'WriteInteger 3',
     'ExecSQL ' + CSQLINSERTPERSON_LANG]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualMappingTests.PersonAggregationChange;
var
  VPerson: TTestPerson;
  VLang: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VLang := TTestLanguage.Create('portuguese');
    Session.Store(VLang);
    AssertNotNull('lang pid', VLang._PID);
    AssertEquals('lang oid', 1, VLang._PID.OID.AsInteger);
    VPerson.Name := 'name';
    VPerson.Languages.Add(VLang);
    VLang.Name := 'german';
    TTestSQLDriver.Commands.Clear;
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 2, VPerson._PID.OID.AsInteger);
    AssertSQLDriverCommands([
     'WriteInteger 2',
     'WriteString name',
     'WriteInteger 0',
     'WriteNull',
     'WriteNull',
     'ExecSQL ' + CSQLINSERTPERSON,
     'WriteInteger 2',
     'WriteInteger 1',
     'ExecSQL ' + CSQLINSERTPERSON_LANG]);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFUpdateManualMappingTests }

procedure TTestOPFUpdateManualMappingTests.City;
var
  VCity: TTestCity;
begin
  VCity := TTestCity.Create;
  try
    VCity.Name := 'TheName';
    Session.Store(VCity);
    TTestSQLDriver.Commands.Clear;
    VCity.Name := 'OtherName';
    Session.Store(VCity);
    AssertSQLDriverCommands([
     'WriteString OtherName',
     'WriteInteger 1',
     'ExecSQL ' + CSQLUPDATECITY]);
  finally
    FreeAndNil(VCity);
  end;
end;

procedure TTestOPFUpdateManualMappingTests.PersonCity;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'TheName';
    VPerson.Age := 15;
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Age := 18;
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString TheName',
     'WriteInteger 18',
     'WriteNull',
     'WriteNull',
     'WriteInteger 1',
     'ExecSQL ' + CSQLUPDATEPERSON]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateManualMappingTests.PersonAddPhones;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'somename';
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones[0].Number := '123';
    VPerson.Phones[1].Number := '456';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Phones[1].Number := '987';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteString 987',
     'WriteInteger 3',
     'ExecSQL ' + CSQLUPDATEPHONE]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateManualMappingTests.PersonRemovePhones;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'name';
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones[0].Number := '123';
    VPerson.Phones[1].Number := '456';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Phones.Delete(1);
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInteger 3',
     'ExecSQL ' + CSQLDELETEPHONE + '=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateManualMappingTests.PersonAddLanguages;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'somename';
    VPerson.Languages.Add(TTestLanguage.Create('german'));
    Session.Store(VPerson);
    VPerson.Languages.Add(TTestLanguage.Create('spanish'));
    TTestSQLDriver.Commands.Clear;
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInteger 3',
     'WriteString spanish',
     'ExecSQL ' + CSQLINSERTLANG,
     'WriteInteger 1',
     'WriteInteger 3',
     'ExecSQL ' + CSQLINSERTPERSON_LANG]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateManualMappingTests.PersonRemoveLanguages;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'thename';
    VPerson.Languages.Add(TTestLanguage.Create('italian'));
    VPerson.Languages.Add(TTestLanguage.Create('portuguese'));
    Session.Store(VPerson);
    VPerson.Languages.Delete(0);
    TTestSQLDriver.Commands.Clear;
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteInteger 2',
     'ExecSQL ' + CSQLDELETEPERSON_LANG_IDs + '=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateManualMappingTests.PersonRemoveAddLanguages;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'aname';
    VPerson.Languages.Add(TTestLanguage.Create('English'));
    VPerson.Languages.Add(TTestLanguage.Create('Brazilian portuguese'));
    Session.Store(VPerson);
    VPerson.Languages.Delete(0);
    VPerson.Languages.Add(TTestLanguage.Create('Spanish'));
    TTestSQLDriver.Commands.Clear;
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteInteger 2',
     'ExecSQL ' + CSQLDELETEPERSON_LANG_IDs + '=?',
     'WriteInteger 4',
     'WriteString Spanish',
     'ExecSQL ' + CSQLINSERTLANG,
     'WriteInteger 1',
     'WriteInteger 4',
     'ExecSQL ' + CSQLINSERTPERSON_LANG]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateManualMappingTests.PersonNoLanguagesChange;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'SomeName';
    VPerson.Languages.Add(TTestLanguage.Create('English'));
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Name := 'anothername';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString anothername',
     'WriteInteger 0',
     'WriteNull',
     'WriteNull',
     'WriteInteger 1',
     'ExecSQL ' + CSQLUPDATEPERSON]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateManualMappingTests.PersonAggregationChange;
var
  VPerson: TTestPerson;
  VLang: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'name';
    VLang := TTestLanguage.Create('english');
    VPerson.Languages.Add(VLang);
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VLang.Name := 'italian';
    Session.Store(VPerson);
    AssertSQLDriverCommands([]);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFSelectManualMappingTests }

procedure TTestOPFSelectManualMappingTests.City;
var
  VCity: TTestCity;
begin
  // city
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('thecityname');

  VCity := Session.Retrieve(TTestCity, '15') as TTestCity;
  try
    AssertEquals(0, TTestSQLDriver.Data.Count);
    AssertEquals(0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertSQLDriverCommands([
     'WriteInteger 15',
     'ExecSQL ' + CSQLSELECTCITY]);
    AssertNotNull(VCity);
    AssertNotNull(VCity._PID);
    AssertEquals(15, VCity._PID.OID.AsInteger);
    AssertEquals('thecityname', VCity.Name);
  finally
    FreeAndNil(VCity);
  end;
end;

procedure TTestOPFSelectManualMappingTests.PersonAddressCity;
var
  VPerson: TTestPerson;
  VAddress: TTestAddress;
  VCity: TTestCity;
begin
  // person
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('name');
  TTestSQLDriver.Data.Add('3');

  TTestSQLDriver.Data.Add('18');
  // address
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('thestreet');
  TTestSQLDriver.Data.Add('01000-001');

  TTestSQLDriver.Data.Add('11');
  // city
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('thecity');

  VPerson := Session.Retrieve(TTestPerson, '2') as TTestPerson;
  try
    AssertEquals('data count', 0, TTestSQLDriver.Data.Count);
    AssertEquals('exprs count', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertSQLDriverCommands([
     'WriteInteger 2',
     'ExecSQL ' + CSQLSELECTPERSON,
     'WriteInteger 18',
     'ExecSQL ' + CSQLSELECTADDRESS,
     'WriteInteger 11',
     'ExecSQL ' + CSQLSELECTCITY,
     'WriteInteger 2',
     'ExecSQL ' + CSQLSELECTPERSON_PHONES,
     'WriteInteger 2',
     'ExecSQL ' + CSQLSELECTPERSON_LANG]);
    AssertNotNull('person', VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 2, VPerson._PID.OID.AsInteger);
    AssertEquals('person name', 'name', VPerson.Name);
    AssertEquals('person age', 3, VPerson.Age);
    VAddress := VPerson.Address;
    AssertNotNull('address', VAddress);
    AssertNotNull('address pid', VAddress._PID);
    AssertEquals('address oid', 18, VAddress._PID.OID.AsInteger);
    AssertEquals('address street', 'thestreet', VAddress.Street);
    AssertEquals('address zipcode', '01000-001', VAddress.ZipCode);
    VCity := VPerson.City;
    AssertNotNull('city', VCity);
    AssertNotNull('city pid', VCity._PID);
    AssertEquals('city oid', 11, VCity._PID.OID.AsInteger);
    AssertEquals('city name', 'thecity', VCity.Name);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectManualMappingTests.PersonCity;
var
  VPerson: TTestPerson;
  VCity: TTestCity;
begin
  // person
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('thepersonname');
  TTestSQLDriver.Data.Add('30');
  TTestSQLDriver.Data.Add('null');

  TTestSQLDriver.Data.Add('5');
  // city
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('nameofcity');

  VPerson := Session.Retrieve(TTestPerson, '8') as TTestPerson;
  try
    AssertEquals('data count', 0, TTestSQLDriver.Data.Count);
    AssertEquals('exprs count', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertSQLDriverCommands([
     'WriteInteger 8',
     'ExecSQL ' + CSQLSELECTPERSON,
     'WriteInteger 5',
     'ExecSQL ' + CSQLSELECTCITY,
     'WriteInteger 8',
     'ExecSQL ' + CSQLSELECTPERSON_PHONES,
     'WriteInteger 8',
     'ExecSQL ' + CSQLSELECTPERSON_LANG]);
    AssertNotNull('person', VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 8, VPerson._PID.OID.AsInteger);
    AssertEquals('person name', 'thepersonname', VPerson.Name);
    AssertEquals('person age', 30, VPerson.Age);
    VCity := VPerson.City;
    AssertNotNull('city', VCity);
    AssertNotNull('city pid', VCity._PID);
    AssertEquals('city oid', 5, VCity._PID.OID.AsInteger);
    AssertEquals('city name', 'nameofcity', VCity.Name);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectManualMappingTests.PersonNullAddressCity;
var
  VPerson: TTestPerson;
begin
  // person
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('personname');
  TTestSQLDriver.Data.Add('22');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');

  VPerson := Session.Retrieve(TTestPerson, '18') as TTestPerson;
  try
    AssertEquals(0, TTestSQLDriver.Data.Count);
    AssertEquals(0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertSQLDriverCommands([
     'WriteInteger 18',
     'ExecSQL ' + CSQLSELECTPERSON,
     'WriteInteger 18',
     'ExecSQL ' + CSQLSELECTPERSON_PHONES,
     'WriteInteger 18',
     'ExecSQL ' + CSQLSELECTPERSON_LANG]);
    AssertNotNull(VPerson);
    AssertNotNull(VPerson._PID);
    AssertEquals(18, VPerson._PID.OID.AsInteger);
    AssertEquals('personname', VPerson.Name);
    AssertEquals(22, VPerson.Age);
    AssertNull(VPerson.City);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectManualMappingTests.PersonPhones;
var
  VPerson: TTestPerson;
begin
  // person
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('aname');
  TTestSQLDriver.Data.Add('5');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');

  // two phone objects
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('11');
  TTestSQLDriver.Data.Add('212');
  TTestSQLDriver.Data.Add('12');
  TTestSQLDriver.Data.Add('555');

  VPerson := Session.Retrieve(TTestPerson, '9') as TTestPerson;
  try
    AssertEquals('data count', 0, TTestSQLDriver.Data.Count);
    AssertEquals('exprs count', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertSQLDriverCommands([
     'WriteInteger 9',
     'ExecSQL ' + CSQLSELECTPERSON,
     'WriteInteger 9',
     'ExecSQL ' + CSQLSELECTPERSON_PHONES,
     'WriteInteger 9',
     'ExecSQL ' + CSQLSELECTPERSON_LANG]);
    AssertNotNull('person', VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 9, VPerson._PID.OID.AsInteger);
    AssertEquals('person name', 'aname', VPerson.Name);
    AssertEquals('person age', 5, VPerson.Age);
    AssertNull('city', VPerson.City);
    AssertEquals('phone cnt', 2, VPerson.Phones.Count);
    AssertNotNull('phone0 pid', VPerson.Phones[0]._PID);
    AssertEquals('phone0 oid', 11, VPerson.Phones[0]._PID.OID.AsInteger);
    AssertEquals('phone0 number', '212', VPerson.Phones[0].Number);
    AssertNotNull('phone1 pid', VPerson.Phones[1]._PID);
    AssertEquals('phone1 oid', 12, VPerson.Phones[1]._PID.OID.AsInteger);
    AssertEquals('phone1 number', '555', VPerson.Phones[1].Number);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectManualMappingTests.PersonLanguages;
var
  VPerson: TTestPerson;
begin
  // person
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('personname');
  TTestSQLDriver.Data.Add('0');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');

  // phones
  TTestSQLDriver.ExpectedResultsets.Add(0);

  // language
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('3');
  TTestSQLDriver.Data.Add('spanish');
  TTestSQLDriver.Data.Add('8');
  TTestSQLDriver.Data.Add('german');

  // let's go
  VPerson := Session.Retrieve(TTestPerson, '5') as TTestPerson;
  try
    AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
    AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertSQLDriverCommands([
     'WriteInteger 5',
     'ExecSQL ' + CSQLSELECTPERSON,
     'WriteInteger 5',
     'ExecSQL ' + CSQLSELECTPERSON_PHONES,
     'WriteInteger 5',
     'ExecSQL ' + CSQLSELECTPERSON_LANG]);
    AssertNotNull('person', VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 5, VPerson._PID.OID.AsInteger);
    AssertEquals('person name', 'personname', VPerson.Name);
    AssertEquals('person age', 0, VPerson.Age);
    AssertNull('city', VPerson.City);
    AssertEquals('phone cnt', 0, VPerson.Phones.Count);
    AssertEquals('lang cnt', 2, VPerson.Languages.Count);
    AssertNotNull('lang0 pid', VPerson.Languages[0]);
    AssertEquals('lang0 oid', 3, VPerson.Languages[0]._PID.OID.AsInteger);
    AssertEquals('lang0 name', 'spanish', VPerson.Languages[0].Name);
    AssertNotNull('lang1 pid', VPerson.Languages[1]);
    AssertEquals('lang1 oid', 8, VPerson.Languages[1]._PID.OID.AsInteger);
    AssertEquals('lang1 name', 'german', VPerson.Languages[1].Name);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFDeleteOneManualMappingTests }

procedure TTestOPFDeleteOneManualMappingTests.City;
begin
  Session.Dispose(TTestCity, ['5']);
  AssertSQLDriverCommands([
   'WriteInteger 5',
   'ExecSQL ' + CSQLDELETECITY + '=?']);
end;

procedure TTestOPFDeleteOneManualMappingTests.PersonAddress;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('4');

  Session.Dispose(TTestPerson, ['12']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 12',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + '=?',
   'WriteInteger 12',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + '=?',
   'WriteInteger 12',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + '=?',
   'WriteInteger 4',
   'ExecSQL ' + CSQLDELETEADDRESS + '=?',
   'WriteInteger 12',
   'ExecSQL ' + CSQLDELETEPERSON + '=?']);
end;

procedure TTestOPFDeleteOneManualMappingTests.PersonCity;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['3']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 3',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + '=?',
   'WriteInteger 3',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + '=?',
   'WriteInteger 3',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + '=?',
   'WriteInteger 3',
   'ExecSQL ' + CSQLDELETEPERSON + '=?']);
end;

procedure TTestOPFDeleteOneManualMappingTests.PersonOnePhone;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('15');
  // Delete Phones
  TTestSQLDriver.ExpectedResultsets.Add(1);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['7']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 7',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + '=?',
   'WriteInteger 15',
   'ExecSQL ' + CSQLDELETEPHONE + '=?',
   'WriteInteger 7',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + '=?',
   'WriteInteger 7',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + '=?',
   'WriteInteger 7',
   'ExecSQL ' + CSQLDELETEPERSON + '=?']);
end;

procedure TTestOPFDeleteOneManualMappingTests.PersonOneLanguage;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(1);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['6']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 6',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + '=?',
   'WriteInteger 6',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + '=?',
   'WriteInteger 6',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + '=?',
   'WriteInteger 6',
   'ExecSQL ' + CSQLDELETEPERSON + '=?']);
end;

procedure TTestOPFDeleteOneManualMappingTests.PersonPhones;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('17');
  TTestSQLDriver.Data.Add('18');
  // Delete Phones
  TTestSQLDriver.ExpectedResultsets.Add(2);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['2']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 2',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + '=?',
   'WriteInteger 17',
   'WriteInteger 18',
   'ExecSQL ' + CSQLDELETEPHONE + ' IN (?,?)',
   'WriteInteger 2',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + '=?',
   'WriteInteger 2',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + '=?',
   'WriteInteger 2',
   'ExecSQL ' + CSQLDELETEPERSON + '=?']);
end;

procedure TTestOPFDeleteOneManualMappingTests.PersonLanguages;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(2);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['5']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 5',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + '=?',
   'WriteInteger 5',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + '=?',
   'WriteInteger 5',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + '=?',
   'WriteInteger 5',
   'ExecSQL ' + CSQLDELETEPERSON + '=?']);
end;

{ TTestOPFDeleteArrayManualMappingTests }

procedure TTestOPFDeleteArrayManualMappingTests.City;
begin
  Session.Dispose(TTestCity, ['13', '22']);
  AssertSQLDriverCommands([
   'WriteInteger 13',
   'WriteInteger 22',
   'ExecSQL ' + CSQLDELETECITY + ' IN (?,?)']);
end;

procedure TTestOPFDeleteArrayManualMappingTests.PersonAddress;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(3);
  TTestSQLDriver.Data.Add('10');
  TTestSQLDriver.Data.Add('13');
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['9', '11', '16']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 9',
   'WriteInteger 11',
   'WriteInteger 16',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + ' IN (?,?,?)',
   'WriteInteger 9',
   'WriteInteger 11',
   'WriteInteger 16',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + ' IN (?,?,?)',
   'WriteInteger 9',
   'WriteInteger 11',
   'WriteInteger 16',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + ' IN (?,?,?)',
   'WriteInteger 10',
   'WriteInteger 13',
   'ExecSQL ' + CSQLDELETEADDRESS + ' IN (?,?)',
   'WriteInteger 9',
   'WriteInteger 11',
   'WriteInteger 16',
   'ExecSQL ' + CSQLDELETEPERSON + ' IN (?,?,?)']);
end;

procedure TTestOPFDeleteArrayManualMappingTests.PersonCity;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(3);
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['21', '22', '23']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 21',
   'WriteInteger 22',
   'WriteInteger 23',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + ' IN (?,?,?)',
   'WriteInteger 21',
   'WriteInteger 22',
   'WriteInteger 23',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + ' IN (?,?,?)',
   'WriteInteger 21',
   'WriteInteger 22',
   'WriteInteger 23',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + ' IN (?,?,?)',
   'WriteInteger 21',
   'WriteInteger 22',
   'WriteInteger 23',
   'ExecSQL ' + CSQLDELETEPERSON + ' IN (?,?,?)']);
end;

procedure TTestOPFDeleteArrayManualMappingTests.PersonPhones;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('11');
  TTestSQLDriver.Data.Add('13');
  // Delete Phones
  TTestSQLDriver.ExpectedResultsets.Add(7);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(3);
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['10', '12', '14']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 10',
   'WriteInteger 12',
   'WriteInteger 14',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + ' IN (?,?,?)',
   'WriteInteger 11',
   'WriteInteger 13',
   'ExecSQL ' + CSQLDELETEPHONE + ' IN (?,?)',
   'WriteInteger 10',
   'WriteInteger 12',
   'WriteInteger 14',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + ' IN (?,?,?)',
   'WriteInteger 10',
   'WriteInteger 12',
   'WriteInteger 14',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + ' IN (?,?,?)',
   'WriteInteger 10',
   'WriteInteger 12',
   'WriteInteger 14',
   'ExecSQL ' + CSQLDELETEPERSON + ' IN (?,?,?)']);
end;

procedure TTestOPFDeleteArrayManualMappingTests.PersonLanguages;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(2);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestPerson, ['15', '18']);

  AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
  AssertSQLDriverCommands([
   'WriteInteger 15',
   'WriteInteger 18',
   'ExecSQL ' + CSQLSELECTPERSON_PHONES_FOR_DELETE + ' IN (?,?)',
   'WriteInteger 15',
   'WriteInteger 18',
   'ExecSQL ' + CSQLDELETEPERSON_LANG + ' IN (?,?)',
   'WriteInteger 15',
   'WriteInteger 18',
   'ExecSQL ' + CSQLSELECTPERSON_FOR_DELETE + ' IN (?,?)',
   'WriteInteger 15',
   'WriteInteger 18',
   'ExecSQL ' + CSQLDELETEPERSON + ' IN (?,?)']);
end;

initialization
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFInsertManualMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFUpdateManualMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFSelectManualMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFDeleteOneManualMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFDeleteArrayManualMappingTests);

end.

