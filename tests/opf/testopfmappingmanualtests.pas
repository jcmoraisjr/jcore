unit TestOPFMappingManualTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFInsertManualIPIDMappingTests }

  TTestOPFInsertManualIPIDMappingTests = class(TTestOPFIPIDContactTestCase)
  published
    procedure Person;
    procedure PersonAddress;
    procedure PersonCity;
    procedure PersonPhones;
    procedure PersonLanguages;
    procedure PersonAggregationChange;
  end;

  { TTestOPFUpdateManualIPIDMappingTests }

  TTestOPFUpdateManualIPIDMappingTests = class(TTestOPFIPIDContactTestCase)
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

  { TTestOPFSelectManualIPIDMappingTests }

  TTestOPFSelectManualIPIDMappingTests = class(TTestOPFIPIDContactTestCase)
  published
    procedure City;
    procedure PersonAddressCity;
    procedure PersonCity;
    procedure PersonNullAddressCity;
    procedure PersonPhones;
    procedure PersonLanguages;
  end;

  { TTestOPFDeleteOneManualIPIDMappingTests }

  TTestOPFDeleteOneManualIPIDMappingTests = class(TTestOPFIPIDContactTestCase)
  published
    procedure City;
    procedure PersonAddress;
    procedure PersonCity;
    procedure PersonOnePhone;
    procedure PersonOneLanguage;
    procedure PersonPhones;
    procedure PersonLanguages;
  end;

  { TTestOPFDeleteArrayManualIPIDMappingTests }

  TTestOPFDeleteArrayManualIPIDMappingTests = class(TTestOPFIPIDContactTestCase)
  published
    procedure City;
    procedure PersonAddress;
    procedure PersonCity;
    procedure PersonPhones;
    procedure PersonLanguages;
  end;

  { TTestOPFInsertManualProxyMappingTests }

  TTestOPFInsertManualProxyMappingTests = class(TTestOPFProxyInvoiceTestCase)
  published
    procedure Person;
    procedure InvoiceClient;
    procedure InvoiceItem;
  end;

  { TTestOPFUpdateManualProxyMappingTests }

  TTestOPFUpdateManualProxyMappingTests = class(TTestOPFProxyInvoiceTestCase)
  published
    procedure Company;
  end;

implementation

uses
  testregistry,
  sysutils,
  TestOPFModelContact,
  TestOPFModelInvoice,
  TestOPFMappingContact,
  TestOPFMappingInvoice;

{ TTestOPFInsertManualIPIDMappingTests }

procedure TTestOPFInsertManualIPIDMappingTests.Person;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
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

procedure TTestOPFInsertManualIPIDMappingTests.PersonAddress;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'name';
    VPerson.Age := 25;
    VPerson.Address := TTestIPIDAddress.Create;
    VPerson.Address.Street := 'route 66';
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', '1', VPerson._PID.OID.AsString);
    AssertNotNull('address pid', VPerson.Address._PID);
    AssertEquals('address oid', '2', VPerson.Address._PID.OID.AsString);
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

procedure TTestOPFInsertManualIPIDMappingTests.PersonCity;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'SomeName';
    VPerson.Age := 25;
    VPerson.City := TTestIPIDCity.Create;
    VPerson.City.Name := 'CityName';
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', '1', VPerson._PID.OID.AsString);
    AssertNotNull('city pid', VPerson.City._PID);
    AssertEquals('city oid', '2', VPerson.City._PID.OID.AsString);
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

procedure TTestOPFInsertManualIPIDMappingTests.PersonPhones;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'thename';
    VPerson.Age := 10;
    VPerson.Phones.Add(TTestIPIDPhone.Create);
    VPerson.Phones[0].Number := '636-3626';
    VPerson.Phones.Add(TTestIPIDPhone.Create);
    VPerson.Phones[1].Number := '212-4321';
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', '1', VPerson._PID.OID.AsString);
    AssertNotNull('phone0 pid', VPerson.Phones[0]._PID);
    AssertEquals('phone0 oid', '2', VPerson.Phones[0]._PID.OID.AsString);
    AssertNotNull('phone1 pid', VPerson.Phones[1]._PID);
    AssertEquals('phone1 oid', '3', VPerson.Phones[1]._PID.OID.AsString);
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

procedure TTestOPFInsertManualIPIDMappingTests.PersonLanguages;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'SomeName';
    VPerson.Languages.Add(TTestIPIDLanguage.Create('English'));
    VPerson.Languages.Add(TTestIPIDLanguage.Create('Spanish'));
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', '1', VPerson._PID.OID.AsString);
    AssertNotNull('lang0 pid', VPerson.Languages[0]._PID);
    AssertEquals('lang0 oid', '2', VPerson.Languages[0]._PID.OID.AsString);
    AssertNotNull('lang1 pid', VPerson.Languages[1]._PID);
    AssertEquals('lang1 oid', '3', VPerson.Languages[1]._PID.OID.AsString);
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

procedure TTestOPFInsertManualIPIDMappingTests.PersonAggregationChange;
var
  VPerson: TTestIPIDPerson;
  VLang: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang := TTestIPIDLanguage.Create('portuguese');
    Session.Store(VLang);
    AssertNotNull('lang pid', VLang._PID);
    AssertEquals('lang oid', '1', VLang._PID.OID.AsString);
    VPerson.Name := 'name';
    VPerson.Languages.Add(VLang);
    VLang.Name := 'german';
    TTestSQLDriver.Commands.Clear;
    Session.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', '2', VPerson._PID.OID.AsString);
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

{ TTestOPFUpdateManualIPIDMappingTests }

procedure TTestOPFUpdateManualIPIDMappingTests.City;
var
  VCity: TTestIPIDCity;
begin
  VCity := TTestIPIDCity.Create;
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

procedure TTestOPFUpdateManualIPIDMappingTests.PersonCity;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
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

procedure TTestOPFUpdateManualIPIDMappingTests.PersonAddPhones;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'somename';
    VPerson.Phones.Add(TTestIPIDPhone.Create);
    VPerson.Phones.Add(TTestIPIDPhone.Create);
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

procedure TTestOPFUpdateManualIPIDMappingTests.PersonRemovePhones;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'name';
    VPerson.Phones.Add(TTestIPIDPhone.Create);
    VPerson.Phones.Add(TTestIPIDPhone.Create);
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

procedure TTestOPFUpdateManualIPIDMappingTests.PersonAddLanguages;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'somename';
    VPerson.Languages.Add(TTestIPIDLanguage.Create('german'));
    Session.Store(VPerson);
    VPerson.Languages.Add(TTestIPIDLanguage.Create('spanish'));
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

procedure TTestOPFUpdateManualIPIDMappingTests.PersonRemoveLanguages;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'thename';
    VPerson.Languages.Add(TTestIPIDLanguage.Create('italian'));
    VPerson.Languages.Add(TTestIPIDLanguage.Create('portuguese'));
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

procedure TTestOPFUpdateManualIPIDMappingTests.PersonRemoveAddLanguages;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'aname';
    VPerson.Languages.Add(TTestIPIDLanguage.Create('English'));
    VPerson.Languages.Add(TTestIPIDLanguage.Create('Brazilian portuguese'));
    Session.Store(VPerson);
    VPerson.Languages.Delete(0);
    VPerson.Languages.Add(TTestIPIDLanguage.Create('Spanish'));
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

procedure TTestOPFUpdateManualIPIDMappingTests.PersonNoLanguagesChange;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'SomeName';
    VPerson.Languages.Add(TTestIPIDLanguage.Create('English'));
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

procedure TTestOPFUpdateManualIPIDMappingTests.PersonAggregationChange;
var
  VPerson: TTestIPIDPerson;
  VLang: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Name := 'name';
    VLang := TTestIPIDLanguage.Create('english');
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

{ TTestOPFSelectManualIPIDMappingTests }

procedure TTestOPFSelectManualIPIDMappingTests.City;
var
  VCity: TTestIPIDCity;
begin
  // city
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('thecityname');

  VCity := Session.Retrieve(TTestIPIDCity, '15') as TTestIPIDCity;
  try
    AssertEquals(0, TTestSQLDriver.Data.Count);
    AssertEquals(0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertSQLDriverCommands([
     'WriteInteger 15',
     'ExecSQL ' + CSQLSELECTCITY]);
    AssertNotNull(VCity);
    AssertNotNull(VCity._PID);
    AssertEquals('15', VCity._PID.OID.AsString);
    AssertEquals('thecityname', VCity.Name);
  finally
    FreeAndNil(VCity);
  end;
end;

procedure TTestOPFSelectManualIPIDMappingTests.PersonAddressCity;
var
  VPerson: TTestIPIDPerson;
  VAddress: TTestIPIDAddress;
  VCity: TTestIPIDCity;
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

  VPerson := Session.Retrieve(TTestIPIDPerson, '2') as TTestIPIDPerson;
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
    AssertEquals('person oid', '2', VPerson._PID.OID.AsString);
    AssertEquals('person name', 'name', VPerson.Name);
    AssertEquals('person age', 3, VPerson.Age);
    VAddress := VPerson.Address;
    AssertNotNull('address', VAddress);
    AssertNotNull('address pid', VAddress._PID);
    AssertEquals('address oid', '18', VAddress._PID.OID.AsString);
    AssertEquals('address street', 'thestreet', VAddress.Street);
    AssertEquals('address zipcode', '01000-001', VAddress.ZipCode);
    VCity := VPerson.City;
    AssertNotNull('city', VCity);
    AssertNotNull('city pid', VCity._PID);
    AssertEquals('city oid', '11', VCity._PID.OID.AsString);
    AssertEquals('city name', 'thecity', VCity.Name);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectManualIPIDMappingTests.PersonCity;
var
  VPerson: TTestIPIDPerson;
  VCity: TTestIPIDCity;
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

  VPerson := Session.Retrieve(TTestIPIDPerson, '8') as TTestIPIDPerson;
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
    AssertEquals('person oid', '8', VPerson._PID.OID.AsString);
    AssertEquals('person name', 'thepersonname', VPerson.Name);
    AssertEquals('person age', 30, VPerson.Age);
    VCity := VPerson.City;
    AssertNotNull('city', VCity);
    AssertNotNull('city pid', VCity._PID);
    AssertEquals('city oid', '5', VCity._PID.OID.AsString);
    AssertEquals('city name', 'nameofcity', VCity.Name);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectManualIPIDMappingTests.PersonNullAddressCity;
var
  VPerson: TTestIPIDPerson;
begin
  // person
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('personname');
  TTestSQLDriver.Data.Add('22');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');

  VPerson := Session.Retrieve(TTestIPIDPerson, '18') as TTestIPIDPerson;
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
    AssertEquals('18', VPerson._PID.OID.AsString);
    AssertEquals('personname', VPerson.Name);
    AssertEquals(22, VPerson.Age);
    AssertNull(VPerson.City);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectManualIPIDMappingTests.PersonPhones;
var
  VPerson: TTestIPIDPerson;
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

  VPerson := Session.Retrieve(TTestIPIDPerson, '9') as TTestIPIDPerson;
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
    AssertEquals('person oid', '9', VPerson._PID.OID.AsString);
    AssertEquals('person name', 'aname', VPerson.Name);
    AssertEquals('person age', 5, VPerson.Age);
    AssertNull('city', VPerson.City);
    AssertEquals('phone cnt', 2, VPerson.Phones.Count);
    AssertNotNull('phone0 pid', VPerson.Phones[0]._PID);
    AssertEquals('phone0 oid', '11', VPerson.Phones[0]._PID.OID.AsString);
    AssertEquals('phone0 number', '212', VPerson.Phones[0].Number);
    AssertNotNull('phone1 pid', VPerson.Phones[1]._PID);
    AssertEquals('phone1 oid', '12', VPerson.Phones[1]._PID.OID.AsString);
    AssertEquals('phone1 number', '555', VPerson.Phones[1].Number);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectManualIPIDMappingTests.PersonLanguages;
var
  VPerson: TTestIPIDPerson;
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
  VPerson := Session.Retrieve(TTestIPIDPerson, '5') as TTestIPIDPerson;
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
    AssertEquals('person oid', '5', VPerson._PID.OID.AsString);
    AssertEquals('person name', 'personname', VPerson.Name);
    AssertEquals('person age', 0, VPerson.Age);
    AssertNull('city', VPerson.City);
    AssertEquals('phone cnt', 0, VPerson.Phones.Count);
    AssertEquals('lang cnt', 2, VPerson.Languages.Count);
    AssertNotNull('lang0 pid', VPerson.Languages[0]);
    AssertEquals('lang0 oid', '3', VPerson.Languages[0]._PID.OID.AsString);
    AssertEquals('lang0 name', 'spanish', VPerson.Languages[0].Name);
    AssertNotNull('lang1 pid', VPerson.Languages[1]);
    AssertEquals('lang1 oid', '8', VPerson.Languages[1]._PID.OID.AsString);
    AssertEquals('lang1 name', 'german', VPerson.Languages[1].Name);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFDeleteOneManualIPIDMappingTests }

procedure TTestOPFDeleteOneManualIPIDMappingTests.City;
begin
  Session.Dispose(TTestIPIDCity, ['5']);
  AssertSQLDriverCommands([
   'WriteInteger 5',
   'ExecSQL ' + CSQLDELETECITY + '=?']);
end;

procedure TTestOPFDeleteOneManualIPIDMappingTests.PersonAddress;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('4');

  Session.Dispose(TTestIPIDPerson, ['12']);

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

procedure TTestOPFDeleteOneManualIPIDMappingTests.PersonCity;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestIPIDPerson, ['3']);

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

procedure TTestOPFDeleteOneManualIPIDMappingTests.PersonOnePhone;
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

  Session.Dispose(TTestIPIDPerson, ['7']);

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

procedure TTestOPFDeleteOneManualIPIDMappingTests.PersonOneLanguage;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(1);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestIPIDPerson, ['6']);

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

procedure TTestOPFDeleteOneManualIPIDMappingTests.PersonPhones;
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

  Session.Dispose(TTestIPIDPerson, ['2']);

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

procedure TTestOPFDeleteOneManualIPIDMappingTests.PersonLanguages;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(2);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestIPIDPerson, ['5']);

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

{ TTestOPFDeleteArrayManualIPIDMappingTests }

procedure TTestOPFDeleteArrayManualIPIDMappingTests.City;
begin
  Session.Dispose(TTestIPIDCity, ['13', '22']);
  AssertSQLDriverCommands([
   'WriteInteger 13',
   'WriteInteger 22',
   'ExecSQL ' + CSQLDELETECITY + ' IN (?,?)']);
end;

procedure TTestOPFDeleteArrayManualIPIDMappingTests.PersonAddress;
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

  Session.Dispose(TTestIPIDPerson, ['9', '11', '16']);

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

procedure TTestOPFDeleteArrayManualIPIDMappingTests.PersonCity;
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

  Session.Dispose(TTestIPIDPerson, ['21', '22', '23']);

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

procedure TTestOPFDeleteArrayManualIPIDMappingTests.PersonPhones;
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

  Session.Dispose(TTestIPIDPerson, ['10', '12', '14']);

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

procedure TTestOPFDeleteArrayManualIPIDMappingTests.PersonLanguages;
begin
  // Phones IDs
  TTestSQLDriver.ExpectedResultsets.Add(0);
  // Languages IDs
  TTestSQLDriver.ExpectedResultsets.Add(2);
  // Address ID
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');

  Session.Dispose(TTestIPIDPerson, ['15', '18']);

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

{ TTestOPFInsertManualProxyMappingTests }

procedure TTestOPFInsertManualProxyMappingTests.Person;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'Jack';
    VPerson.Nick := 'J';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteString Jack',
     'ExecSQL ' + CSQLINSERTINVOICECLIENT,
     'WriteInteger 1',
     'WriteString J',
     'ExecSQL ' + CSQLINSERTINVOICEPERSON]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualProxyMappingTests.InvoiceClient;
var
  VInvoice: TInvoice;
begin
  VInvoice := TInvoice.Create;
  try
    VInvoice.Client := TClient.Create;
    VInvoice.Client.Name := 'aclient';
    VInvoice.Date := '01/01';
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInteger 1',
     'WriteInteger 2',
     'WriteString aclient',
     'ExecSQL ' + CSQLINSERTINVOICECLIENT,
     'WriteInteger 2',
     'WriteString 01/01',
     'ExecSQL ' + CSQLINSERTINVOICEINVOICE]);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFInsertManualProxyMappingTests.InvoiceItem;
var
  VProduct: TProduct;
  VInvoice: TInvoice;
  VItemProduct: TInvoiceItemProduct;
  VItemService: TInvoiceItemService;
begin
  VInvoice := TInvoice.Create;
  try
    VProduct := TProduct.Create;
    try
      VProduct.Name := 'prod';
      Session.Store(VProduct);  // ID 1
      VItemProduct := TInvoiceItemProduct.Create;
      VInvoice.Items.Add(VItemProduct);  // ID 3
      VItemProduct.Qty := 15;
      VItemProduct.Product := VProduct;
      VItemProduct.Total := 28;
      VProduct.AddRef;

      VItemService := TInvoiceItemService.Create;
      VInvoice.Items.Add(VItemService);  // ID 4
      VItemService.Description := 'srv description';
      VItemService.Total := 90;

      VInvoice.Date := '05/01';

      TTestSQLDriver.Commands.Clear;
      Session.Store(VInvoice);  // ID 2
      AssertSQLDriverCommands([
       'WriteInteger 2',
       'WriteNull',
       'WriteString 05/01',
       'ExecSQL INSERT INTO INVOICE (ID,CLIENT,DATE) VALUES (?,?,?)',
       'WriteInteger 3',
       'WriteInteger 28',
       'ExecSQL INSERT INTO INVOICEITEM (ID,TOTAL) VALUES (?,?)',
       'WriteInteger 3',
       'WriteInteger 15',
       'WriteInteger 1',
       'ExecSQL INSERT INTO INVOICEITEMPRODUCT (ID,QTY,PRODUCT) VALUES (?,?,?)',
       'WriteInteger 4',
       'WriteInteger 90',
       'ExecSQL INSERT INTO INVOICEITEM (ID,TOTAL) VALUES (?,?)',
       'WriteInteger 4',
       'WriteString srv description',
       'ExecSQL INSERT INTO INVOICEITEMSERVICE (ID,DESCRIPTION) VALUES (?,?)']);
    finally
      FreeAndNil(VProduct);
    end;
  finally
    FreeAndNil(VInvoice);
  end;
end;

{ TTestOPFUpdateManualProxyMappingTests }

procedure TTestOPFUpdateManualProxyMappingTests.Company;
var
  VCompany: TCompany;
begin
  VCompany := TCompany.Create;
  try
    VCompany.Name := 'company co';
    VCompany.ContactName := 'joe';
    Session.Store(VCompany);
    TTestSQLDriver.Commands.Clear;
    VCompany.Name := 'comp corp';
    Session.Store(VCompany);
    AssertSQLDriverCommands([
     'WriteString comp corp',
     'WriteInteger 1',
     'ExecSQL UPDATE CLIENT SET NAME=? WHERE ID=?']);
    TTestSQLDriver.Commands.Clear;
    VCompany.ContactName := 'wil';
    Session.Store(VCompany);
    AssertSQLDriverCommands([
     'WriteString wil',
     'WriteInteger 1',
     'ExecSQL UPDATE COMPANY SET CONTACTNAME=? WHERE ID=?']);
  finally
    FreeAndNil(VCompany);
  end;
end;

initialization
  RegisterTest('jcore.opf.mapping.manualmapping.simple', TTestOPFInsertManualIPIDMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping.simple', TTestOPFUpdateManualIPIDMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping.simple', TTestOPFSelectManualIPIDMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping.simple', TTestOPFDeleteOneManualIPIDMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping.simple', TTestOPFDeleteArrayManualIPIDMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping.inheritance', TTestOPFInsertManualProxyMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping.inheritance', TTestOPFUpdateManualProxyMappingTests);

end.

