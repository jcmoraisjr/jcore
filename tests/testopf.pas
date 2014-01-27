unit TestOPF;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fgl,
  fpcunit,
  JCoreLogger,
  JCoreOPFID,
  JCoreOPFDriver,
  JCoreOPFMapping,
  JCoreOPFSession,
  JCoreOPFConfig;

type

  { TTestOPF }

  TTestOPF = class(TTestCase)
  private
    FConfiguration: IJCoreOPFConfiguration;
    FSession: IJCoreOPFSession;
    class var FLOG: IJCoreLogger;
  protected
    function CreateConfiguration(const ADriverClassArray: array of TJCoreOPFDriverClass; const AMappingClassArray: array of TJCoreOPFMappingClass): IJCoreOPFConfiguration;
    procedure SetUp; override;
    procedure TearDown; override;
    class property LOG: IJCoreLogger read FLOG;
  end;

  { TTestOPFCore }

  TTestOPFCore = class(TTestOPF)
  published
    procedure CreatePID;
    procedure DriverNotFound;
    procedure MappingNotFound;
  end;

  { TTestOPFInsertManualMapping }

  TTestOPFInsertManualMapping = class(TTestOPF)
  published
    procedure StoreInsertPersonManualMapping;
    procedure StoreInsertPersonCityManualMapping;
    procedure StoreInsertPersonPhonesManualMapping;
    procedure StoreInsertPersonLanguagesManualMapping;
  end;

  { TTestOPFUpdateManualMapping }

  TTestOPFUpdateManualMapping = class(TTestOPF)
  published
    procedure StoreUpdateCityManualMapping;
    procedure StoreUpdatePersonCityManualMapping;
    procedure StoreUpdatePersonPhonesManualMapping;
  end;

  { TTestOPFSelectManualMapping }

  TTestOPFSelectManualMapping = class(TTestOPF)
  published
    procedure SelectCityManualMapping;
    procedure SelectPersonCityManualMapping;
    procedure SelectPersonNullCityManualMapping;
    procedure SelectPersonPhonesManualMapping;
  end;

  { TTestEmptyDriver }

  TTestEmptyDriver = class(TJCoreOPFSQLDriver)
  public
    class function DriverName: string; override;
  end;

  { TTestEmptyMapping }

  TTestEmptyMapping = class(TJCoreOPFSQLMapping)
  public
    class function Apply(const AClass: TClass): Boolean; override;
    procedure InternalStore(const APID: IJCoreOPFPID); override;
  end;

  { TTestBase }

  TTestBase = class(TObject)
  private
    FPID: IJCoreOPFPID;
  published
    property _PID: IJCoreOPFPID read FPID write FPID;
  end;

  { TTestCity }

  TTestCity = class(TTestBase)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TTestPhone }

  TTestPhone = class(TTestBase)
  private
    FNumber: string;
  published
    property Number: string read FNumber write FNumber;
  end;

  TTestPhoneList = specialize TFPGObjectList<TTestPhone>;

  { TTestLanguage }

  TTestLanguage = class(TTestBase)
  private
    FName: string;
  public
    constructor Create(const AName: string);
  published
    property Name: string read FName write FName;
  end;

  TTestLanguageList = specialize TFPGObjectList<TTestLanguage>;

  { TTestPerson }

  TTestPerson = class(TTestBase)
  private
    FName: string;
    FAge: Integer;
    FPhones: TTestPhoneList;
    FCity: TTestCity;
    FLanguages: TTestLanguageList;
  public
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Phones: TTestPhoneList read FPhones write FPhones;
    property City: TTestCity read FCity write FCity;
    property Languages: TTestLanguageList read FLanguages write FLanguages;
  end;

  TTestIntegerList = specialize TFPGList<Integer>;

  { TTestSQLDriver }

  TTestSQLDriver = class(TJCoreOPFSQLDriver)
  private
    function PopData(const APopFromQueue: Boolean = True): string;
    class var FCommands: TStringList;
    class var FData:  TStringList;
    class var FExpectedResultsets: TTestIntegerList;
  protected
    function InternalExecSQL(const ASQL: string): Integer; override;
  public
    class constructor Create;
    class destructor Destroy;
    class function DriverName: string; override;
    function ReadInteger: Integer; override;
    function ReadNull: Boolean; override;
    function ReadString: string; override;
    procedure WriteInteger(const AValue: Integer); override;
    procedure WriteString(const AValue: string); override;
    procedure WriteNull; override;
    class property Commands: TStringList read FCommands;
    class property Data: TStringList read FData;
    class property ExpectedResultsets: TTestIntegerList read FExpectedResultsets write FExpectedResultsets;
  end;

  { TTestAbstractSQLMapping }

  TTestAbstractSQLMapping = class(TJCoreOPFSQLMapping)
  private
    class var FCurrentOID: Integer;
  protected
    function CreateOID(const AOID: string): TJCoreOPFOID; override;
    function GenerateOID: Integer;
  public
    class procedure ClearOID;
  end;

  { TTestPersonSQLMapping }

  TTestPersonSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: IJCoreOPFPID); override;
    procedure WriteExternalsToDriver(const APID: IJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: IJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestCitySQLMapping }

  TTestCitySQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: IJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: IJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestPhoneSQLMapping }

  TTestPhoneSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; override;
    function GenerateSelectOwnedListStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: IJCoreOPFPID); override;
    function ReadOwnedOIDFromDriver: TJCoreOPFOID; override;
    procedure WriteInternalsToDriver(const APID: IJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestLanguageSQLMapping }

  TTestLanguageSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertSharedItemStatement(const AOwnerPID, ASharedPID: IJCoreOPFPID): string; override;
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; override;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: IJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: IJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreOPFException,
  JCoreOPFOID;

const
  CSQLINSERTCITY = 'INSERT INTO CITY (ID,NAME) VALUES (?,?)';
  CSQLINSERTPERSON = 'INSERT INTO PERSON (ID,NAME,AGE,CITY) VALUES (?,?,?,?)';
  CSQLINSERTPHONE = 'INSERT INTO PHONE (ID,PERSON,NUMBER) VALUES (?,?,?)';
  CSQLINSERTLANG = 'INSERT INTO LANG (ID,NAME) VALUES (?,?)';
  CSQLINSERTPERSON_LANG = 'INSERT INTO PERSON_LANG (ID_PERSON,ID_LANG) VALUES (?,?)';
  CSQLSELECTCITY = 'SELECT NAME FROM CITY WHERE ID=?';
  CSQLSELECTPERSON = 'SELECT NAME,AGE,CITY FROM PERSON WHERE ID=?';
  CSQLSELECTOWNEDPHONES = 'SELECT ID,NUMBER FROM PHONE WHERE PERSON=?';
  CSQLUPDATECITY = 'UPDATE CITY SET NAME=? WHERE ID=?';
  CSQLUPDATEPERSON = 'UPDATE PERSON SET NAME=?, AGE=? WHERE ID=?';
  CSQLUPDATEPHONE = 'UPDATE PHONE SET PERSON=?, NUMBER=? WHERE ID=?';
  CSQLUPDATELANG = 'UPDATE LANG SET NAME=? WHERE ID=?';

{ TTestOPF }

function TTestOPF.CreateConfiguration(const ADriverClassArray: array of TJCoreOPFDriverClass;
  const AMappingClassArray: array of TJCoreOPFMappingClass): IJCoreOPFConfiguration;
var
  VDriverClass: TJCoreOPFDriverClass;
  VMappingClass: TJCoreOPFMappingClass;
begin
  Result := TJCoreOPFConfiguration.Create;
  try
    for VDriverClass in ADriverClassArray do
      Result.AddDriverClass(VDriverClass);
    for VMappingClass in AMappingClassArray do
      Result.AddMappingClass(VMappingClass);
    Result.DriverName := ADriverClassArray[0].DriverName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TTestOPF.SetUp;
begin
  inherited SetUp;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.teste.opf');
  AssertEquals(0, TTestSQLDriver.Commands.Count);
  AssertEquals(0, TTestSQLDriver.Data.Count);
  FConfiguration := CreateConfiguration([TTestSQLDriver], [
   TTestPersonSQLMapping, TTestCitySQLMapping, TTestPhoneSQLMapping,
   TTestLanguageSQLMapping]);
  FSession := FConfiguration.CreateSession;
end;

procedure TTestOPF.TearDown;
begin
  inherited TearDown;
  FSession := nil;
  TTestAbstractSQLMapping.ClearOID;
  TTestSQLDriver.Commands.Clear;
  TTestSQLDriver.Data.Clear;
end;

{ TTestOPFCore }

procedure TTestOPFCore.CreatePID;
var
  VPerson: TTestPerson;
  VPID: IJCoreOPFPID;
begin
  VPerson := TTestPerson.Create;
  try
    AssertNull(VPerson._PID);
    FSession.Store(VPerson);
    VPID := VPerson._PID;
    AssertNotNull(VPID);
    AssertSame(VPID.GetEntity, VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCore.DriverNotFound;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.AddMappingClass(TTestEmptyMapping);

  try
    VSession := VConfiguration.CreateSession;
    Fail('EJCoreOPFUndefinedDriver expected');
  except
    on E: EAssertionFailedError do
      raise;
    on E: Exception do
    begin
      LOG.Debug('', E);
      AssertEquals(E.ClassType, EJCoreOPFUndefinedDriver.ClassType);
    end;
  end;

  VConfiguration.AddDriverClass(TTestEmptyDriver);
  try
    VConfiguration.DriverName := TTestEmptyDriver.DriverName + ' invalid';
    Fail('EJCoreOPFDriverNotFound expected');
  except
    on E: EAssertionFailedError do
      raise;
    on E: Exception do
    begin
      LOG.Debug('', E);
      AssertEquals(E.ClassType, EJCoreOPFDriverNotFound.ClassType);
    end;
  end;

  VConfiguration.DriverName := TTestEmptyDriver.DriverName;
  VSession := VConfiguration.CreateSession;
  AssertNotNull(VSession);
end;

procedure TTestOPFCore.MappingNotFound;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TTestPerson;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.AddDriverClass(TTestEmptyDriver);
  VConfiguration.DriverName := TTestEmptyDriver.DriverName;
  VSession := VConfiguration.CreateSession;
  AssertNotNull(VSession);
  VPerson := TTestPerson.Create;
  try
    try
      VSession.Store(VPerson);
      Fail('EJCoreOPFMappingNotFound expected');
    except
      on E: EAssertionFailedError do
        raise;
      on E: Exception do
      begin
        LOG.Debug('', E);
        AssertEquals(EJCoreOPFMappingNotFound.ClassType, E.ClassType);
      end;
    end;
    VConfiguration.AddMappingClass(TTestEmptyMapping);
    VSession.Store(VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFInsertManualMapping }

procedure TTestOPFInsertManualMapping.StoreInsertPersonManualMapping;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'TheName';
    VPerson.Age := 15;
    FSession.Store(VPerson);
    AssertEquals(5, TTestSQLDriver.Commands.Count);
    AssertEquals('WriteInteger 1', TTestSQLDriver.Commands[0]);
    AssertEquals('WriteString TheName', TTestSQLDriver.Commands[1]);
    AssertEquals('WriteInteger 15', TTestSQLDriver.Commands[2]);
    AssertEquals('WriteNull', TTestSQLDriver.Commands[3]);
    AssertEquals('ExecSQL ' + CSQLINSERTPERSON, TTestSQLDriver.Commands[4]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualMapping.StoreInsertPersonCityManualMapping;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'SomeName';
    VPerson.Age := 25;
    VPerson.City := TTestCity.Create;
    VPerson.City.Name := 'CityName';
    FSession.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 1, VPerson._PID.OID.AsInteger);
    AssertNotNull('city pid', VPerson.City._PID);
    AssertEquals('city oid', 2, VPerson.City._PID.OID.AsInteger);
    AssertEquals('cmd count', 8, TTestSQLDriver.Commands.Count);
    AssertEquals('cmd0', 'WriteInteger 1', TTestSQLDriver.Commands[0]);
    AssertEquals('cmd1', 'WriteString SomeName', TTestSQLDriver.Commands[1]);
    AssertEquals('cmd2', 'WriteInteger 25', TTestSQLDriver.Commands[2]);
    AssertEquals('cmd3', 'WriteInteger 2', TTestSQLDriver.Commands[3]);
    AssertEquals('cmd4', 'WriteString CityName', TTestSQLDriver.Commands[4]);
    AssertEquals('cmd5', 'ExecSQL ' + CSQLINSERTCITY, TTestSQLDriver.Commands[5]);
    AssertEquals('cmd6', 'WriteInteger 2', TTestSQLDriver.Commands[6]);
    AssertEquals('cmd7', 'ExecSQL ' + CSQLINSERTPERSON, TTestSQLDriver.Commands[7]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualMapping.StoreInsertPersonPhonesManualMapping;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'thename';
    VPerson.Age := 10;
    VPerson.Phones := TTestPhoneList.Create(True);
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones[0].Number := '636-3626';
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones[1].Number := '212-4321';
    FSession.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 1, VPerson._PID.OID.AsInteger);
    AssertNotNull('phone0 pid', VPerson.Phones[0]._PID);
    AssertEquals('phone0 oid', 2, VPerson.Phones[0]._PID.OID.AsInteger);
    AssertNotNull('phone1 pid', VPerson.Phones[1]._PID);
    AssertEquals('phone1 oid', 3, VPerson.Phones[1]._PID.OID.AsInteger);
    AssertEquals('cmd count', 13, TTestSQLDriver.Commands.Count);
    AssertEquals('cmd0', 'WriteInteger 1', TTestSQLDriver.Commands[0]);
    AssertEquals('cmd1', 'WriteString thename', TTestSQLDriver.Commands[1]);
    AssertEquals('cmd2', 'WriteInteger 10', TTestSQLDriver.Commands[2]);
    AssertEquals('cmd3', 'WriteNull', TTestSQLDriver.Commands[3]);
    AssertEquals('cmd4', 'ExecSQL ' + CSQLINSERTPERSON, TTestSQLDriver.Commands[4]);
    AssertEquals('cmd5', 'WriteInteger 2', TTestSQLDriver.Commands[5]);
    AssertEquals('cmd6', 'WriteInteger 1', TTestSQLDriver.Commands[6]);
    AssertEquals('cmd7', 'WriteString 636-3626', TTestSQLDriver.Commands[7]);
    AssertEquals('cmd8', 'ExecSQL ' + CSQLINSERTPHONE, TTestSQLDriver.Commands[8]);
    AssertEquals('cmd9', 'WriteInteger 3', TTestSQLDriver.Commands[9]);
    AssertEquals('cmd10', 'WriteInteger 1', TTestSQLDriver.Commands[10]);
    AssertEquals('cmd11', 'WriteString 212-4321', TTestSQLDriver.Commands[11]);
    AssertEquals('cmd12', 'ExecSQL ' + CSQLINSERTPHONE, TTestSQLDriver.Commands[12]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertManualMapping.StoreInsertPersonLanguagesManualMapping;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'SomeName';
    VPerson.Languages := TTestLanguageList.Create(True);
    VPerson.Languages.Add(TTestLanguage.Create('English'));
    VPerson.Languages.Add(TTestLanguage.Create('Spanish'));
    FSession.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertEquals('person oid', 1, VPerson._PID.OID.AsInteger);
    AssertNotNull('lang0 pid', VPerson.Languages[0]._PID);
    AssertEquals('lang0 oid', 2, VPerson.Languages[0]._PID.OID.AsInteger);
    AssertNotNull('lang1 pid', VPerson.Languages[1]._PID);
    AssertEquals('lang1 oid', 3, VPerson.Languages[1]._PID.OID.AsInteger);
    AssertEquals('cmd count', 17, TTestSQLDriver.Commands.Count);
    AssertEquals('cmd0', 'WriteInteger 1', TTestSQLDriver.Commands[0]);
    AssertEquals('cmd1', 'WriteString SomeName', TTestSQLDriver.Commands[1]);
    AssertEquals('cmd2', 'WriteInteger 0', TTestSQLDriver.Commands[2]);
    AssertEquals('cmd3', 'WriteNull', TTestSQLDriver.Commands[3]);
    AssertEquals('cmd4', 'ExecSQL ' + CSQLINSERTPERSON, TTestSQLDriver.Commands[4]);
    AssertEquals('cmd5', 'WriteInteger 2', TTestSQLDriver.Commands[5]);
    AssertEquals('cmd6', 'WriteString English', TTestSQLDriver.Commands[6]);
    AssertEquals('cmd7', 'ExecSQL ' + CSQLINSERTLANG, TTestSQLDriver.Commands[7]);
    AssertEquals('cmd8', 'WriteInteger 3', TTestSQLDriver.Commands[8]);
    AssertEquals('cmd9', 'WriteString Spanish', TTestSQLDriver.Commands[9]);
    AssertEquals('cmd10', 'ExecSQL ' + CSQLINSERTLANG, TTestSQLDriver.Commands[10]);
    AssertEquals('cmd11', 'WriteInteger 1', TTestSQLDriver.Commands[11]);
    AssertEquals('cmd12', 'WriteInteger 2', TTestSQLDriver.Commands[12]);
    AssertEquals('cmd13', 'ExecSQL ' + CSQLINSERTPERSON_LANG, TTestSQLDriver.Commands[13]);
    AssertEquals('cmd14', 'WriteInteger 1', TTestSQLDriver.Commands[14]);
    AssertEquals('cmd15', 'WriteInteger 3', TTestSQLDriver.Commands[15]);
    AssertEquals('cmd16', 'ExecSQL ' + CSQLINSERTPERSON_LANG, TTestSQLDriver.Commands[16]);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFUpdateManualMapping }

procedure TTestOPFUpdateManualMapping.StoreUpdateCityManualMapping;
var
  VCity: TTestCity;
begin
  VCity := TTestCity.Create;
  try
    VCity.Name := 'TheName';
    FSession.Store(VCity);
    TTestSQLDriver.Commands.Clear;
    VCity.Name := 'OtherName';
    FSession.Store(VCity);
    AssertEquals(3, TTestSQLDriver.Commands.Count);
    AssertEquals('WriteString OtherName', TTestSQLDriver.Commands[0]);
    AssertEquals('WriteInteger 1', TTestSQLDriver.Commands[1]);
    AssertEquals('ExecSQL ' + CSQLUPDATECITY, TTestSQLDriver.Commands[2]);
  finally
    FreeAndNil(VCity);
  end;
end;

procedure TTestOPFUpdateManualMapping.StoreUpdatePersonCityManualMapping;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'TheName';
    VPerson.Age := 15;
    FSession.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Age := 18;
    FSession.Store(VPerson);
    AssertEquals(5, TTestSQLDriver.Commands.Count);
    AssertEquals('WriteString TheName', TTestSQLDriver.Commands[0]);
    AssertEquals('WriteInteger 18', TTestSQLDriver.Commands[1]);
    AssertEquals('WriteNull', TTestSQLDriver.Commands[2]);
    AssertEquals('WriteInteger 1', TTestSQLDriver.Commands[3]);
    AssertEquals('ExecSQL ' + CSQLUPDATEPERSON, TTestSQLDriver.Commands[4]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateManualMapping.StoreUpdatePersonPhonesManualMapping;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'somename';
    VPerson.Phones := TTestPhoneList.Create(True);
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones.Add(TTestPhone.Create);
    VPerson.Phones[0].Number := '123';
    VPerson.Phones[1].Number := '456';
    FSession.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Phones[1].Number := '987';
    FSession.Store(VPerson);
    AssertEquals('cmd count', 13, TTestSQLDriver.Commands.Count);
    AssertEquals('cmd0', 'WriteString somename', TTestSQLDriver.Commands[0]);
    AssertEquals('cmd1', 'WriteInteger 0', TTestSQLDriver.Commands[1]);
    AssertEquals('cmd2', 'WriteNull', TTestSQLDriver.Commands[2]);
    AssertEquals('cmd3', 'WriteInteger 1', TTestSQLDriver.Commands[3]);
    AssertEquals('cmd4', 'ExecSQL ' + CSQLUPDATEPERSON, TTestSQLDriver.Commands[4]);
    AssertEquals('cmd5', 'WriteInteger 1', TTestSQLDriver.Commands[5]);
    AssertEquals('cmd6', 'WriteString 123', TTestSQLDriver.Commands[6]);
    AssertEquals('cmd7', 'WriteInteger 2', TTestSQLDriver.Commands[7]);
    AssertEquals('cmd8', 'ExecSQL ' + CSQLUPDATEPHONE, TTestSQLDriver.Commands[8]);
    AssertEquals('cmd9', 'WriteInteger 1', TTestSQLDriver.Commands[9]);
    AssertEquals('cmd10', 'WriteString 987', TTestSQLDriver.Commands[10]);
    AssertEquals('cmd11', 'WriteInteger 3', TTestSQLDriver.Commands[11]);
    AssertEquals('cmd12', 'ExecSQL ' + CSQLUPDATEPHONE, TTestSQLDriver.Commands[12]);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFSelectManualMapping }

procedure TTestOPFSelectManualMapping.SelectCityManualMapping;
var
  VCity: TTestCity;
begin
  TTestSQLDriver.Data.Add('thecityname');
  TTestSQLDriver.ExpectedResultsets.Add(1);
  VCity := FSession.Retrieve(TTestCity, '15') as TTestCity;
  try
    AssertEquals(0, TTestSQLDriver.Data.Count);
    AssertEquals(0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertEquals(2, TTestSQLDriver.Commands.Count);
    AssertEquals('WriteInteger 15', TTestSQLDriver.Commands[0]);
    AssertEquals('ExecSQL ' + CSQLSELECTCITY, TTestSQLDriver.Commands[1]);
    AssertNotNull(VCity);
    AssertNotNull(VCity._PID);
    AssertEquals(15, VCity._PID.OID.AsInteger);
    AssertEquals('thecityname', VCity.Name);
  finally
    FreeAndNil(VCity);
  end;
end;

procedure TTestOPFSelectManualMapping.SelectPersonCityManualMapping;
var
  VPerson: TTestPerson;
  VCity: TTestCity;
begin
  TTestSQLDriver.Data.Add('thepersonname');
  TTestSQLDriver.Data.Add('30');
  TTestSQLDriver.Data.Add('5');
  TTestSQLDriver.Data.Add('nameofcity');
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.ExpectedResultsets.Add(1);
  VPerson := FSession.Retrieve(TTestPerson, '8') as TTestPerson;
  try
    AssertEquals('data count', 0, TTestSQLDriver.Data.Count);
    AssertEquals('exprs count', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertEquals('cmd count', 6, TTestSQLDriver.Commands.Count);
    AssertEquals('cmd0', 'WriteInteger 8', TTestSQLDriver.Commands[0]);
    AssertEquals('cmd1', 'ExecSQL ' + CSQLSELECTPERSON, TTestSQLDriver.Commands[1]);
    AssertEquals('cmd2', 'WriteInteger 5', TTestSQLDriver.Commands[2]);
    AssertEquals('cmd3', 'ExecSQL ' + CSQLSELECTCITY, TTestSQLDriver.Commands[3]);
    AssertEquals('cmd4', 'WriteInteger 8', TTestSQLDriver.Commands[4]);
    AssertEquals('cmd5', 'ExecSQL ' + CSQLSELECTOWNEDPHONES, TTestSQLDriver.Commands[5]);
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

procedure TTestOPFSelectManualMapping.SelectPersonNullCityManualMapping;
var
  VPerson: TTestPerson;
begin
  TTestSQLDriver.Data.Add('personname');
  TTestSQLDriver.Data.Add('22');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.ExpectedResultsets.Add(1);
  VPerson := FSession.Retrieve(TTestPerson, '18') as TTestPerson;
  try
    AssertEquals(0, TTestSQLDriver.Data.Count);
    AssertEquals(0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertEquals(4, TTestSQLDriver.Commands.Count);
    AssertEquals('WriteInteger 18', TTestSQLDriver.Commands[0]);
    AssertEquals('ExecSQL ' + CSQLSELECTPERSON, TTestSQLDriver.Commands[1]);
    AssertEquals('WriteInteger 18', TTestSQLDriver.Commands[2]);
    AssertEquals('ExecSQL ' + CSQLSELECTOWNEDPHONES, TTestSQLDriver.Commands[3]);
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

procedure TTestOPFSelectManualMapping.SelectPersonPhonesManualMapping;
var
  VPerson: TTestPerson;
begin
  TTestSQLDriver.Data.Add('aname');
  TTestSQLDriver.Data.Add('5');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('11');
  TTestSQLDriver.Data.Add('212');
  TTestSQLDriver.Data.Add('12');
  TTestSQLDriver.Data.Add('555');
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.ExpectedResultsets.Add(2);
  VPerson := FSession.Retrieve(TTestPerson, '9') as TTestPerson;
  try
    AssertEquals('data count', 0, TTestSQLDriver.Data.Count);
    AssertEquals('exprs count', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertEquals('cmd count', 4, TTestSQLDriver.Commands.Count);
    AssertEquals('cmd0', 'WriteInteger 9', TTestSQLDriver.Commands[0]);
    AssertEquals('cmd1', 'ExecSQL ' + CSQLSELECTPERSON, TTestSQLDriver.Commands[1]);
    AssertEquals('cmd2', 'WriteInteger 9', TTestSQLDriver.Commands[2]);
    AssertEquals('cmd3', 'ExecSQL ' + CSQLSELECTOWNEDPHONES, TTestSQLDriver.Commands[3]);
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
    AssertNotNull('phone1 pid', VPerson.Phones[0]._PID);
    AssertEquals('phone1 oid', 12, VPerson.Phones[1]._PID.OID.AsInteger);
    AssertEquals('phone1 number', '555', VPerson.Phones[1].Number);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestLanguage }

constructor TTestLanguage.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TTestPerson }

destructor TTestPerson.Destroy;
begin
  FreeAndNil(FPhones);
  FreeAndNil(FCity);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

{ TTestEmptyDriver }

class function TTestEmptyDriver.DriverName: string;
begin
  Result := 'TestEmptyDriver';
end;

{ TTestEmptyMapping }

class function TTestEmptyMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := True;
end;

procedure TTestEmptyMapping.InternalStore(const APID: IJCoreOPFPID);
begin
end;

{ TTestSQLDriver }

function TTestSQLDriver.PopData(const APopFromQueue: Boolean): string;
begin
  if Data.Count = 0 then
    raise Exception.Create('Trying to read an empty data queue');
  Result := Data[0];
  if APopFromQueue then
    Data.Delete(0);
end;

function TTestSQLDriver.InternalExecSQL(const ASQL: string): Integer;
begin
  FCommands.Add('ExecSQL ' + ASQL);
  if ExpectedResultsets.Count > 0 then
  begin
    Result := ExpectedResultsets[0];
    ExpectedResultsets.Delete(0);
  end else
    Result := 0;
end;

class constructor TTestSQLDriver.Create;
begin
  FCommands := TStringList.Create;
  FData := TStringList.Create;
  FExpectedResultsets := TTestIntegerList.Create;
end;

class destructor TTestSQLDriver.Destroy;
begin
  FreeAndNil(FCommands);
  FreeAndNil(FData);
  FreeAndNil(FExpectedResultsets);
end;

class function TTestSQLDriver.DriverName: string;
begin
  Result := 'TestSQLDriver';
end;

function TTestSQLDriver.ReadInteger: Integer;
begin
  Result := StrToInt(PopData);
end;

function TTestSQLDriver.ReadNull: Boolean;
begin
  Result := PopData(False) = 'null';
  if Result then
    PopData;
end;

function TTestSQLDriver.ReadString: string;
begin
  Result := PopData;
end;

procedure TTestSQLDriver.WriteInteger(const AValue: Integer);
begin
  FCommands.Add('WriteInteger ' + IntToStr(AValue));
end;

procedure TTestSQLDriver.WriteString(const AValue: string);
begin
  FCommands.Add('WriteString ' + AValue);
end;

procedure TTestSQLDriver.WriteNull;
begin
  FCommands.Add('WriteNull');
end;

{ TTestAbstractSQLMapping }

function TTestAbstractSQLMapping.CreateOID(const AOID: string): TJCoreOPFOID;
var
  VOID: Integer;
begin
  if AOID = '' then
    VOID := GenerateOID
  else
    VOID := StrToInt(AOID);
  Result := TJCoreOPFIntegerOID.Create(VOID);
end;

function TTestAbstractSQLMapping.GenerateOID: Integer;
begin
  Inc(FCurrentOID);
  Result := FCurrentOID;
end;

class procedure TTestAbstractSQLMapping.ClearOID;
begin
  FCurrentOID := 0;
end;

{ TTestPersonSQLMapping }

function TTestPersonSQLMapping.GenerateInsertStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLINSERTPERSON;
end;

function TTestPersonSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTPERSON;
end;

function TTestPersonSQLMapping.GenerateUpdateStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLUPDATEPERSON;
end;

procedure TTestPersonSQLMapping.ReadFromDriver(const APID: IJCoreOPFPID);
var
  VPerson: TTestPerson;
begin
  VPerson := APID.Entity as TTestPerson;
  VPerson.Name := Driver.ReadString;
  VPerson.Age := Driver.ReadInteger;
  if not Driver.ReadNull then
    VPerson.City := Mapper.Retrieve(TTestCity, IntToStr(Driver.ReadInteger)) as TTestCity;
  VPerson.Phones := TTestPhoneList(Mapper.RetrieveOwnedListPID(TTestPhone, VPerson._PID));
end;

procedure TTestPersonSQLMapping.WriteExternalsToDriver(const APID: IJCoreOPFPID);
var
  VPerson: TTestPerson;
begin
  VPerson := APID.Entity as TTestPerson;
  StoreOwnedObjectList(VPerson._PID, VPerson.Phones);
  Mapper.StoreSharedListPID(TTestLanguage, VPerson._PID, CreatePIDArray(VPerson.Languages));
end;

procedure TTestPersonSQLMapping.WriteInternalsToDriver(const APID: IJCoreOPFPID);
var
  VPerson: TTestPerson;
begin
  VPerson := APID.Entity as TTestPerson;
  Driver.WriteString(VPerson.Name);
  Driver.WriteInteger(VPerson.Age);
  if Assigned(VPerson.City) then
  begin
    Mapper.Store(VPerson.City);
    VPerson.City._PID.OID.WriteToDriver(Driver);
  end else
  begin
    { TODO : Need as much calls as the size of the FK }
    Driver.WriteNull;
  end;
end;

class function TTestPersonSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestPerson;
end;

{ TTestCitySQLMapping }

function TTestCitySQLMapping.GenerateInsertStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLINSERTCITY;
end;

function TTestCitySQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTCITY;
end;

function TTestCitySQLMapping.GenerateUpdateStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLUPDATECITY;
end;

procedure TTestCitySQLMapping.ReadFromDriver(const APID: IJCoreOPFPID);
var
  VCity: TTestCity;
begin
  VCity := APID.Entity as TTestCity;
  VCity.Name := Driver.ReadString;
end;

procedure TTestCitySQLMapping.WriteInternalsToDriver(const APID: IJCoreOPFPID);
var
  VCity: TTestCity;
begin
  VCity := APID.Entity as TTestCity;
  Driver.WriteString(VCity.Name);
end;

class function TTestCitySQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestCity;
end;

{ TTestPhoneSQLMapping }

function TTestPhoneSQLMapping.GenerateInsertStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLINSERTPHONE;
end;

function TTestPhoneSQLMapping.GenerateSelectOwnedListStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTOWNEDPHONES;
end;

function TTestPhoneSQLMapping.GenerateUpdateStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLUPDATEPHONE;
end;

procedure TTestPhoneSQLMapping.ReadFromDriver(const APID: IJCoreOPFPID);
var
  VPhone: TTestPhone;
begin
  VPhone := APID.Entity as TTestPhone;
  VPhone.Number := Driver.ReadString;
end;

function TTestPhoneSQLMapping.ReadOwnedOIDFromDriver: TJCoreOPFOID;
begin
  Result := TJCoreOPFIntegerOID.Create(Driver.ReadInteger);
end;

procedure TTestPhoneSQLMapping.WriteInternalsToDriver(const APID: IJCoreOPFPID);
var
  VPhone: TTestPhone;
begin
  VPhone := APID.Entity as TTestPhone;
  APID.Owner.OID.WriteToDriver(Driver);
  Driver.WriteString(VPhone.Number);
end;

class function TTestPhoneSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestPhone;
end;

{ TTestLanguageSQLMapping }

function TTestLanguageSQLMapping.GenerateInsertSharedItemStatement(
  const AOwnerPID, ASharedPID: IJCoreOPFPID): string;
begin
  if AOwnerPID.Entity.ClassType = TTestPerson then
    Result := CSQLINSERTPERSON_LANG
  else
    Result := inherited GenerateInsertSharedItemStatement(AOwnerPID, ASharedPID);
end;

function TTestLanguageSQLMapping.GenerateInsertStatement(
  const APID: IJCoreOPFPID): string;
begin
  Result := CSQLINSERTLANG;
end;

function TTestLanguageSQLMapping.GenerateUpdateStatement(
  const APID: IJCoreOPFPID): string;
begin
  Result := CSQLUPDATELANG;
end;

procedure TTestLanguageSQLMapping.ReadFromDriver(const APID: IJCoreOPFPID);
var
  VLang: TTestLanguage;
begin
  VLang := APID.Entity as TTestLanguage;
  VLang.Name := Driver.ReadString;
end;

procedure TTestLanguageSQLMapping.WriteInternalsToDriver(
  const APID: IJCoreOPFPID);
var
  VLang: TTestLanguage;
begin
  VLang := APID.Entity as TTestLanguage;
  Driver.WriteString(VLang.Name);
end;

class function TTestLanguageSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestLanguage;
end;

initialization
  RegisterTest('jcore.opf.core', TTestOPFCore);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFInsertManualMapping);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFUpdateManualMapping);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFSelectManualMapping);

end.

