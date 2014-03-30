unit TestOPF;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  Classes,
  fgl,
  fpcunit,
  JCoreClasses,
  JCoreLogger,
  JCoreEntity,
  JCoreMetadata,
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMetadata,
  JCoreOPFMapping,
  JCoreOPFSession,
  JCoreOPFConfig;

type

  { TTestOPFConfig }

  TTestOPFConfig = class(TJCoreOPFConfiguration)
  protected
    function InternalCreateSession(const ADriver: TJCoreOPFDriver): IJCoreOPFSession; override;
  end;

  ITestOPFSession = interface(IJCoreOPFSession)
  ['{641946FC-586B-F7CA-5B84-25C3DC812F08}']
    function AcquireMapping(const AClass: TClass): TJCoreOPFMapping;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
  end;

  { TTestOPFSession }

  TTestOPFSession = class(TJCoreOPFSession, ITestOPFSession)
  private
    class var FCommitCount: Integer;
    class var FLastCommitPIDList: TJCoreOPFPIDList;
  protected
    procedure Commit; override;
  public
    class constructor Create;
    class destructor Destroy;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
    class function ExistPIDInCommitPIDList(const APID: IJCorePID): Boolean;
    class property CommitCount: Integer read FCommitCount write FCommitCount;
    class property LastCommitPIDList: TJCoreOPFPIDList read FLastCommitPIDList;
  end;

  { TTestOPFModel }

  TTestOPFModel = class(TJCoreOPFModel)
  protected
    function BuildMetadata(const AClass: TClass): TJCoreClassMetadata; override;
    procedure InitRegistry; override;
  end;

  { TTestOPF }

  TTestOPF = class(TTestCase)
  private
    FConfiguration: IJCoreOPFConfiguration;
    FSession: ITestOPFSession;
    class var FLOG: IJCoreLogger;
  protected
    procedure AssertExceptionStore(const ASession: IJCoreOPFSession; const AEntity: TObject; const AException: ExceptClass);
    procedure AssertSQLDriverCommands(const ACommands: array of string);
    function CreateConfiguration(const ADriverClassArray: array of TJCoreOPFDriverClass; const AMappingClassArray: array of TJCoreOPFMappingClass): IJCoreOPFConfiguration;
    procedure SetUp; override;
    procedure TearDown; override;
    class property LOG: IJCoreLogger read FLOG;
  end;

  { TTestOPFSessionTests }

  TTestOPFSessionTests = class(TTestOPF)
  published
    procedure DriverNotFound;
    procedure MappingNotFound;
  end;

  { TTestOPFMetadataTests }

  TTestOPFMetadataTests = class(TTestOPF)
  published
    procedure CreatePID;
    procedure AttributeList;
    procedure InheritedAttributeList;
    procedure NonPidAttributeList;
  end;

  { TTestOPFMappingTests }

  TTestOPFMappingTests = class(TTestOPF)
  published
    procedure TransactionPIDList;
    procedure StoreOwnerDontUpdateAggregations;
    procedure FailOwnOwnedComposition;
  end;

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

  { TTestOPFCleanDirtyAttributeTests }

  TTestOPFCleanDirtyAttributeTests = class(TTestOPF)
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

  { TTestEmptyDriver }

  TTestEmptyDriver = class(TJCoreOPFSQLDriver)
  public
    class function DriverName: string; override;
  end;

  { TTestEmptyMapping }

  TTestEmptyMapping = class(TJCoreOPFSQLMapping)
  public
    class function Apply(const AClass: TClass): Boolean; override;
    procedure InternalStore(const APID: TJCoreOPFPID); override;
  end;

  { TTestBase }

  TTestBase = class(TJCoreManagedObject)
  private
    FPID: IJCorePID;
  published
    property _PID: IJCorePID read FPID write FPID;
  end;

  { TTestSimple }

  TTestSimple = class(TObject)
  private
    FPID: IJCorePID;
    FField1: Integer;
  published
    property _PID: IJCorePID read FPID write FPID;
    property Field1: Integer read FField1 write FField1;
  end;

  { TTestAddress }

  TTestAddress = class(TTestBase)
  private
    FStreet: string;
    FZipCode: string;
  published
    property Street: string read FStreet write FStreet;
    property ZipCode: string read FZipCode write FZipCode;
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
    FAddress: TTestAddress;
    FCity: TTestCity;
    FLanguages: TTestLanguageList;
    function GetLanguages: TTestLanguageList;
    function GetPhones: TTestPhoneList;
    procedure SetAddress(AValue: TTestAddress);
    procedure SetCity(AValue: TTestCity);
    procedure SetLanguages(AValue: TTestLanguageList);
    procedure SetPhones(AValue: TTestPhoneList);
  protected
    procedure Finit; override;
  published
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Phones: TTestPhoneList read GetPhones write SetPhones;
    property Address: TTestAddress read FAddress write SetAddress;
    property City: TTestCity read FCity write SetCity;
    property Languages: TTestLanguageList read GetLanguages write SetLanguages;
  end;

  TTestEmployee = class(TTestPerson)
  private
    FSalary: Currency;
  published
    property Salary: Currency read FSalary;
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
    function CreateOIDFromDriver(const ADriver: TJCoreOPFDriver): TJCoreOPFOID; override;
    function CreateOIDFromString(const AOID: string): TJCoreOPFOID; override;
    function GenerateOID: Integer;
  public
    class procedure ClearOID;
  end;

  { TTestSimpleSQLMapping }

  TTestSimpleSQLMapping = class(TTestAbstractSQLMapping)
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestPersonSQLMapping }

  TTestPersonSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string; override;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string; override;
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectCompositionsForDeleteStatement(const AClass: TClass; const ASize: Integer): string; override;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteExternalsToDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestEmployeeSQLMapping }

  TTestEmployeeSQLMapping = class(TTestPersonSQLMapping)
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestAddressSQLMapping }

  TTestAddressSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestCitySQLMapping }

  TTestCitySQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestPhoneSQLMapping }

  TTestPhoneSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TClass; const AOwnerAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestLanguageSQLMapping }

  TTestLanguageSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TClass; const AOwnerAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

implementation

uses
  testregistry,
  JCoreOPFException;

const
  CSQLINSERTADDRESS = 'INSERT INTO ADDRESS (ID,STREET,ZIPCODE) VALUES (?,?,?)';
  CSQLINSERTCITY = 'INSERT INTO CITY (ID,NAME) VALUES (?,?)';
  CSQLINSERTPERSON = 'INSERT INTO PERSON (ID,NAME,AGE,ADDRESS,CITY) VALUES (?,?,?,?,?)';
  CSQLINSERTPHONE = 'INSERT INTO PHONE (ID,PERSON,NUMBER) VALUES (?,?,?)';
  CSQLINSERTLANG = 'INSERT INTO LANG (ID,NAME) VALUES (?,?)';
  CSQLINSERTPERSON_LANG = 'INSERT INTO PERSON_LANG (ID_PERSON,ID_LANG) VALUES (?,?)';
  CSQLSELECTADDRESS = 'SELECT STREET,ZIPCODE FROM ADDRESS WHERE ID=?';
  CSQLSELECTCITY = 'SELECT NAME FROM CITY WHERE ID=?';
  CSQLSELECTPERSON = 'SELECT NAME,AGE,ADDRESS,CITY FROM PERSON WHERE ID=?';
  CSQLSELECTPERSON_PHONES = 'SELECT ID,NUMBER FROM PHONE WHERE PERSON=?';
  CSQLSELECTPERSON_LANG = 'SELECT L.ID,L.NAME FROM LANG L INNER JOIN PERSON_LANG PL ON PL.ID_LANG=L.ID WHERE PL.ID_PERSON=?';
  CSQLSELECTPERSON_FOR_DELETE = 'SELECT ADDRESS FROM PERSON WHERE ID';
  CSQLSELECTPERSON_PHONES_FOR_DELETE = 'SELECT ID FROM PHONE WHERE PERSON';
  CSQLSELECTPERSON_LANG_FOR_DELETE = 'SELECT L.ID FROM LANG L INNER JOIN PERSON_LANG PL ON PL.ID_LANG=L.ID WHERE PL.ID_PERSON';
  CSQLUPDATEADDRESS = 'UPDATE ADDRESS SET STREET=?, ZIPCODE=? WHERE ID=?';
  CSQLUPDATECITY = 'UPDATE CITY SET NAME=? WHERE ID=?';
  CSQLUPDATEPERSON = 'UPDATE PERSON SET NAME=?, AGE=?, ADDRESS=?, CITY=? WHERE ID=?';
  CSQLUPDATEPHONE = 'UPDATE PHONE SET PERSON=?, NUMBER=? WHERE ID=?';
  CSQLUPDATELANG = 'UPDATE LANG SET NAME=? WHERE ID=?';
  CSQLDELETEADDRESS = 'DELETE FROM ADDRESS WHERE ID';
  CSQLDELETECITY = 'DELETE FROM CITY WHERE ID';
  CSQLDELETEPHONE = 'DELETE FROM PHONE WHERE ID';
  CSQLDELETEPERSON = 'DELETE FROM PERSON WHERE ID';
  CSQLDELETEPERSON_LANG = 'DELETE FROM PERSON_LANG WHERE ID_PERSON';
  CSQLDELETEPERSON_LANG_IDs = 'DELETE FROM PERSON_LANG WHERE ID_PERSON=? AND ID_LANG';

{ TTestOPFConfig }

function TTestOPFConfig.InternalCreateSession(const ADriver: TJCoreOPFDriver): IJCoreOPFSession;
begin
  Result := TTestOPFSession.Create(Self, Model, ADriver);
end;

{ TTestOPFSession }

procedure TTestOPFSession.Commit;
var
  I: Integer;
begin
  Inc(FCommitCount);
  LastCommitPIDList.Clear;
  for I := 0 to Pred(InTransactionPIDList.Count) do
    LastCommitPIDList.Add(InTransactionPIDList[I]);
  inherited Commit;
end;

class constructor TTestOPFSession.Create;
begin
  FLastCommitPIDList := TJCoreOPFPIDList.Create(False);
end;

class destructor TTestOPFSession.Destroy;
begin
  FreeAndNil(FLastCommitPIDList);
end;

function TTestOPFSession.AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
begin
  Result := AcquireMapping(AClass).AcquireMetadata(AClass);
end;

class function TTestOPFSession.ExistPIDInCommitPIDList(const APID: IJCorePID): Boolean;
begin
  Result := LastCommitPIDList.IndexOf(APID as TJCoreOPFPID) >= 0;
end;

{ TTestOPFModel }

function TTestOPFModel.BuildMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := inherited BuildMetadata(AClass) as TJCoreOPFClassMetadata;
  try
    if AClass = TTestPerson then
    begin
      VMetadata.AttributeByName('Languages').CompositionType := jctAggregation;
      VMetadata.AttributeByName('Languages').CompositionLinkType := jcltExternal;
      VMetadata.AttributeByName('City').CompositionType := jctAggregation;
    end;
  except
    FreeAndNil(VMetadata);
    raise;
  end;
  Result := VMetadata;
end;

procedure TTestOPFModel.InitRegistry;
begin
  inherited InitRegistry;
  AddClass(TTestPerson);
  AddClass(TTestPhone);
  AddClass(TTestLanguage);
  AddClass(TTestAddress);
  AddClass(TTestCity);
end;

{ TTestOPF }

procedure TTestOPF.AssertExceptionStore(const ASession: IJCoreOPFSession;
  const AEntity: TObject; const AException: ExceptClass);
begin
  try
    ASession.Store(AEntity);
    Fail(AException.ClassName + ' expected');
  except
    on E: EAssertionFailedError do
      raise;
    on E: Exception do
    begin
      LOG.Debug('', E);
      AssertEquals(AException.ClassType, E.ClassType);
    end;
  end;
end;

procedure TTestOPF.AssertSQLDriverCommands(const ACommands: array of string);
var
  I: Integer;
begin
  try
    AssertEquals('cmd count', Length(ACommands), TTestSQLDriver.Commands.Count);
    for I := Low(ACommands) to High(ACommands) do
      AssertEquals('cmd' + IntToStr(I), ACommands[I], TTestSQLDriver.Commands[I]);
  except
    LOG.Warn('Executed commands:');
    for I := 0 to Pred(TTestSQLDriver.Commands.Count) do
      LOG.Warn('  ' + IntToStr(I) + ': ' + TTestSQLDriver.Commands[I]);
    raise;
  end;
  TTestSQLDriver.Commands.Clear;
end;

function TTestOPF.CreateConfiguration(const ADriverClassArray: array of TJCoreOPFDriverClass;
  const AMappingClassArray: array of TJCoreOPFMappingClass): IJCoreOPFConfiguration;
var
  VDriverClass: TJCoreOPFDriverClass;
  VMappingClass: TJCoreOPFMappingClass;
begin
  Result := TTestOPFConfig.Create(TTestOPFModel.Create);
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
   TTestSimpleSQLMapping, TTestPersonSQLMapping, TTestEmployeeSQLMapping,
   TTestAddressSQLMapping, TTestCitySQLMapping, TTestPhoneSQLMapping,
   TTestLanguageSQLMapping]);
  FSession := FConfiguration.CreateSession as ITestOPFSession;
end;

procedure TTestOPF.TearDown;
begin
  inherited TearDown;
  FSession := nil;
  FConfiguration := nil;
  TTestAbstractSQLMapping.ClearOID;
  TTestSQLDriver.Commands.Clear;
  TTestSQLDriver.Data.Clear;
end;

{ TTestOPFSessionTests }

procedure TTestOPFSessionTests.DriverNotFound;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
begin
  VConfiguration := TJCoreOPFConfiguration.Create;
  VConfiguration.AddMappingClass(TTestEmptyMapping);

  try
    VSession := VConfiguration.CreateSession;
    Fail(EJCoreOPFUndefinedDriver.ClassName + ' expected');
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
    Fail(EJCoreOPFDriverNotFound.ClassName + ' expected');
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

procedure TTestOPFSessionTests.MappingNotFound;
var
  VConfiguration: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TTestPerson;
begin
  VConfiguration := TJCoreOPFConfiguration.Create(TTestOPFModel.Create);
  VConfiguration.AddDriverClass(TTestEmptyDriver);
  VConfiguration.DriverName := TTestEmptyDriver.DriverName;
  VSession := VConfiguration.CreateSession;
  AssertNotNull(VSession);
  VPerson := TTestPerson.Create;
  try
    AssertExceptionStore(VSession, VPerson, EJCoreOPFMappingNotFound);
    VConfiguration.AddMappingClass(TTestEmptyMapping);
    VSession.Store(VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFMetadataTests }

procedure TTestOPFMetadataTests.CreatePID;
var
  VPerson: TTestPerson;
  VPID: IJCorePID;
begin
  VPerson := TTestPerson.Create;
  try
    AssertNull(VPerson._PID);
    FSession.Store(VPerson);
    VPID := VPerson._PID;
    AssertNotNull(VPID);
    AssertSame(VPID.Entity, VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFMetadataTests.AttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := FSession.AcquireMetadata(TTestPerson);
  AssertEquals('meta.cnt', 6, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Name', VMetadata[0].Name);
  AssertEquals('meta1.name', 'Age', VMetadata[1].Name);
  AssertEquals('meta2.name', 'Phones', VMetadata[2].Name);
  AssertEquals('meta3.name', 'Address', VMetadata[3].Name);
  AssertEquals('meta4.name', 'City', VMetadata[4].Name);
  AssertEquals('meta5.name', 'Languages', VMetadata[5].Name);
end;

procedure TTestOPFMetadataTests.InheritedAttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := FSession.AcquireMetadata(TTestEmployee);
  AssertEquals('meta.cnt', 1, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Salary', VMetadata[0].Name);
end;

procedure TTestOPFMetadataTests.NonPidAttributeList;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := FSession.AcquireMetadata(TTestSimple);
  AssertEquals('meta.cnt', 1, VMetadata.AttributeCount);
  AssertEquals('meta0.name', 'Field1', VMetadata[0].Name);
end;

{ TTestOPFMappingTests }

procedure TTestOPFMappingTests.TransactionPIDList;
var
  VPerson: TTestPerson;
  VCity: TTestCity;
  VPhone1: TTestPhone;
  VPhone2: TTestPhone;
  VLang1: TTestLanguage;
  VLang2: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VCity := TTestCity.Create;
    VPerson.City := VCity;
    VPhone1 := TTestPhone.Create;
    VPerson.Phones.Add(VPhone1);
    VPhone2 := TTestPhone.Create;
    VPerson.Phones.Add(VPhone2);
    VLang1 := TTestLanguage.Create('1');
    VPerson.Languages.Add(VLang1);
    VLang2 := TTestLanguage.Create('2');
    VPerson.Languages.Add(VLang2);
    VPerson.Languages.Add(VLang2);
    VLang2.AddRef;
    TTestOPFSession.CommitCount := 0;
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
    AssertEquals('commit2 cnt', 1, TTestOPFSession.CommitCount);
    AssertEquals('pid2 cnt', 5, TTestOPFSession.LastCommitPIDList.Count);
    AssertSame('vperson2 pid', TTestOPFSession.LastCommitPIDList[0], VPerson._PID as TJCoreOPFPID);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFMappingTests.StoreOwnerDontUpdateAggregations;
var
  VPerson: TTestPerson;
  VLang: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VLang := TTestLanguage.Create('english');
    VPerson.Languages.Add(VLang);
    FSession.Store(VPerson);
    AssertNotNull('lang pid', VLang._PID);
    AssertTrue('lang clean', not VLang._PID.IsDirty);
    VPerson.Name := 'SomeName';
    VLang.Name := 'spanish';
    FSession.Store(VPerson);
    AssertTrue('lang dirty', VLang._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFMappingTests.FailOwnOwnedComposition;
var
  VPerson: TTestPerson;
  VLang: TTestLanguage;
  VPhone: TTestPhone;
begin
  VPerson := TTestPerson.Create;
  try
    VLang := TTestLanguage.Create('english');
    VPerson.Languages.Add(VLang);
    VPerson.Languages.Add(VLang);
    VLang.AddRef;
    FSession.Store(VPerson);
    VPhone := TTestPhone.Create;
    VPerson.Phones.Add(VPhone);
    VPerson.Phones.Add(VPhone);
    VPhone.AddRef;
    { TODO : Implement duplication check in the same admcollection }
    // AssertExceptionStore(FSession, VPerson, EJCoreOPFObjectAlreadyOwned);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFInsertManualMappingTests }

procedure TTestOPFInsertManualMappingTests.Person;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'TheName';
    VPerson.Age := 15;
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
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
    FSession.Store(VLang);
    AssertNotNull('lang pid', VLang._PID);
    AssertEquals('lang oid', 1, VLang._PID.OID.AsInteger);
    VPerson.Name := 'name';
    VPerson.Languages.Add(VLang);
    VLang.Name := 'german';
    TTestSQLDriver.Commands.Clear;
    FSession.Store(VPerson);
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
    FSession.Store(VCity);
    TTestSQLDriver.Commands.Clear;
    VCity.Name := 'OtherName';
    FSession.Store(VCity);
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
    FSession.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Age := 18;
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Phones[1].Number := '987';
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Phones.Delete(1);
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
    VPerson.Languages.Add(TTestLanguage.Create('spanish'));
    TTestSQLDriver.Commands.Clear;
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
    VPerson.Languages.Delete(0);
    TTestSQLDriver.Commands.Clear;
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
    VPerson.Languages.Delete(0);
    VPerson.Languages.Add(TTestLanguage.Create('Spanish'));
    TTestSQLDriver.Commands.Clear;
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Name := 'anothername';
    FSession.Store(VPerson);
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
    FSession.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VLang.Name := 'italian';
    FSession.Store(VPerson);
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

  VCity := FSession.Retrieve(TTestCity, '15') as TTestCity;
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

  VPerson := FSession.Retrieve(TTestPerson, '2') as TTestPerson;
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

  VPerson := FSession.Retrieve(TTestPerson, '8') as TTestPerson;
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

  VPerson := FSession.Retrieve(TTestPerson, '18') as TTestPerson;
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

  VPerson := FSession.Retrieve(TTestPerson, '9') as TTestPerson;
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
  VPerson := FSession.Retrieve(TTestPerson, '5') as TTestPerson;
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
  FSession.Dispose(TTestCity, ['5']);
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

  FSession.Dispose(TTestPerson, ['12']);

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

  FSession.Dispose(TTestPerson, ['3']);

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

  FSession.Dispose(TTestPerson, ['7']);

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

  FSession.Dispose(TTestPerson, ['6']);

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

  FSession.Dispose(TTestPerson, ['2']);

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

  FSession.Dispose(TTestPerson, ['5']);

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
  FSession.Dispose(TTestCity, ['13', '22']);
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

  FSession.Dispose(TTestPerson, ['9', '11', '16']);

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

  FSession.Dispose(TTestPerson, ['21', '22', '23']);

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

  FSession.Dispose(TTestPerson, ['10', '12', '14']);

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

  FSession.Dispose(TTestPerson, ['15', '18']);

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

{ TTestOPFCleanDirtyAttributeTests }

procedure TTestOPFCleanDirtyAttributeTests.CacheNotUpdated;
var
  VPerson: TTestPerson;
  VPID: TJCoreOPFPID;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Age := 0;
    VPID := FSession.AcquireMapping(VPerson.ClassType).AcquirePID(VPerson);
    AssertTrue('person dirty1', VPID.IsDirty);
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Age := 30;
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := 'Some name';
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Address := TTestAddress.Create;
    VPerson.Address.Street := 'freeway s/n';
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
  VPhone: TTestPhone;
begin
  VPerson := TTestPerson.Create;
  try
    VPhone := TTestPhone.Create;
    VPhone.Number := '123';
    VPerson.Phones.Add(VPhone);
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
  VPhone: TTestPhone;
begin
  VPerson := TTestPerson.Create;
  try
    VPhone := TTestPhone.Create;
    VPhone.Number := '321';
    VPerson.Phones.Add(VPhone);
    FSession.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPhone := TTestPhone.Create;
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
  VPerson: TTestPerson;
  VPhone: TTestPhone;
begin
  VPerson := TTestPerson.Create;
  try
    VPhone := TTestPhone.Create;
    VPhone.Number := '456';
    VPerson.Phones.Add(VPhone);
    VPhone := TTestPhone.Create;
    VPhone.Number := '456-789';
    VPerson.Phones.Add(VPhone);
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
  VPhone: TTestPhone;
begin
  VPerson := TTestPerson.Create;
  try
    VPhone := TTestPhone.Create;
    VPhone.Number := '456';
    VPerson.Phones.Add(VPhone);
    VPhone := TTestPhone.Create;
    VPhone.Number := '456-789';
    VPerson.Phones.Add(VPhone);
    FSession.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Phones.Delete(1);
    VPhone := TTestPhone.Create;
    VPhone.Number := '456-7890';
    VPerson.Phones.Add(VPhone);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.CompositionChangedOrder;
var
  VPerson: TTestPerson;
  VPhone1, VPhone2: TTestPhone;
begin
  VPerson := TTestPerson.Create;
  try
    VPhone1 := TTestPhone.Create;
    VPhone1.Number := '456';
    VPhone2 := TTestPhone.Create;
    VPhone2.Number := '789';
    VPerson.Phones.Add(VPhone1);
    VPerson.Phones.Add(VPhone2);
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
  VCity: TTestCity;
begin
  VPerson := TTestPerson.Create;
  try
    VCity := TTestCity.Create;
    VCity.Name := 'sampa';
    VPerson.City := VCity;
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
  VLang: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VLang := TTestLanguage.Create('english');
    VPerson.Languages.Add(VLang);
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
  VLang1, VLang2: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VLang1 := TTestLanguage.Create('english');
    VPerson.Languages.Add(VLang1);
    FSession.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VLang2 := TTestLanguage.Create('german');
    VPerson.Languages.Add(VLang2);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.AggregationRemoved;
var
  VPerson: TTestPerson;
  VLang1, VLang2: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VLang1 := TTestLanguage.Create('spanish');
    VLang2 := TTestLanguage.Create('italian');
    VPerson.Languages.Add(VLang1);
    VPerson.Languages.Add(VLang2);
    FSession.Store(VPerson);
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
  VPerson: TTestPerson;
  VLang1, VLang2, VLang3: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VLang1 := TTestLanguage.Create('italian');
    VLang2 := TTestLanguage.Create('german');
    VPerson.Languages.Add(VLang1);
    VPerson.Languages.Add(VLang2);
    FSession.Store(VPerson);
    AssertNotNull('person pid', VPerson._PID);
    AssertFalse('person dirty1', VPerson._PID.IsDirty);
    VPerson.Languages.Delete(1);
    VLang3 := TTestLanguage.Create('portuguese');
    VPerson.Languages.Add(VLang3);
    AssertTrue('person dirty2', VPerson._PID.IsDirty);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFCleanDirtyAttributeTests.AggregationChangedOrder;
var
  VPerson: TTestPerson;
  VLang1, VLang2: TTestLanguage;
begin
  VPerson := TTestPerson.Create;
  try
    VLang1 := TTestLanguage.Create('english');
    VLang2 := TTestLanguage.Create('german');
    VPerson.Languages.Add(VLang1);
    VPerson.Languages.Add(VLang2);
    FSession.Store(VPerson);
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

{ TTestLanguage }

constructor TTestLanguage.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TTestPerson }

function TTestPerson.GetLanguages: TTestLanguageList;
begin
  if not Assigned(FLanguages) then
    FLanguages := TTestLanguageList.Create;
  Result := FLanguages;
end;

function TTestPerson.GetPhones: TTestPhoneList;
begin
  if not Assigned(FPhones) then
    FPhones := TTestPhoneList.Create;
  Result := FPhones;
end;

procedure TTestPerson.SetAddress(AValue: TTestAddress);
begin
  FreeAndNil(FAddress);
  FAddress := AValue;
end;

procedure TTestPerson.SetCity(AValue: TTestCity);
begin
  if FCity <> AValue then
  begin
    FreeAndNil(FCity);
    FCity := AValue;
  end;
end;

procedure TTestPerson.SetLanguages(AValue: TTestLanguageList);
begin
  FreeAndNil(FLanguages);
  FLanguages := AValue;
end;

procedure TTestPerson.SetPhones(AValue: TTestPhoneList);
begin
  FreeAndNil(FPhones);
  FPhones := AValue;
end;

procedure TTestPerson.Finit;
begin
  FreeAndNil(FPhones);
  FreeAndNil(FAddress);
  FreeAndNil(FCity);
  FreeAndNil(FLanguages);
  inherited Finit;
end;

{ TTestEmployeeSQLMapping }

class function TTestEmployeeSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestEmployee;
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

procedure TTestEmptyMapping.InternalStore(const APID: TJCoreOPFPID);
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

function TTestAbstractSQLMapping.CreateOIDFromDriver(
  const ADriver: TJCoreOPFDriver): TJCoreOPFOID;
begin
  if not ADriver.ReadNull then
    Result := TJCoreOPFIntegerOID.Create(ADriver.ReadInteger)
  else
    Result := nil;
end;

function TTestAbstractSQLMapping.CreateOIDFromString(const AOID: string): TJCoreOPFOID;
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

{ TTestSimpleSQLMapping }

class function TTestSimpleSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestSimple;
end;

{ TTestPersonSQLMapping }

function TTestPersonSQLMapping.GenerateDeleteExternalLinkIDsStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestLanguage then
    Result := CSQLDELETEPERSON_LANG_IDs + BuildParams(ASize)
  else
    Result := inherited GenerateDeleteExternalLinkIDsStatement(AAttrMetadata, ASize);
end;

function TTestPersonSQLMapping.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestLanguage then
    Result := CSQLDELETEPERSON_LANG + BuildParams(ASize)
  else
    Result := inherited GenerateDeleteExternalLinksStatement(AAttrMetadata, ASize);
end;

function TTestPersonSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEPERSON + BuildParams(ASize);
end;

function TTestPersonSQLMapping.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if AAttrMetadata.CompositionClass = TTestLanguage then
    Result := CSQLINSERTPERSON_LANG
  else
    Result := inherited GenerateInsertExternalLinksStatement(AAttrMetadata);
end;

function TTestPersonSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTPERSON;
end;

function TTestPersonSQLMapping.GenerateSelectCompositionsForDeleteStatement(
  const AClass: TClass; const ASize: Integer): string;
begin
  Result := CSQLSELECTPERSON_FOR_DELETE + BuildParams(ASize);
end;

function TTestPersonSQLMapping.GenerateSelectForDeleteStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestPhone then
    Result := CSQLSELECTPERSON_PHONES_FOR_DELETE + BuildParams(ASize)
  else if AAttrMetadata.CompositionClass = TTestLanguage then
    Result := CSQLSELECTPERSON_LANG_FOR_DELETE + BuildParams(ASize)
  else
    Result := inherited GenerateSelectForDeleteStatement(AAttrMetadata, ASize);
end;

function TTestPersonSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTPERSON;
end;

function TTestPersonSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATEPERSON;
end;

procedure TTestPersonSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VPerson: TTestPerson;
begin
  VPerson := APID.Entity as TTestPerson;
  VPerson.Name := Driver.ReadString;
  VPerson.Age := Driver.ReadInteger;
  VPerson.Address := Mapper.RetrieveFromDriver(TTestAddress, Driver) as TTestAddress;
  VPerson.City := Mapper.RetrieveFromDriver(TTestCity, Driver) as TTestCity;
  RetrieveList(APID, 'Phones');
  RetrieveList(APID, 'Languages');
end;

procedure TTestPersonSQLMapping.WriteExternalsToDriver(const APID: TJCoreOPFPID);
begin
  StoreList(APID, 'Phones');
  StoreList(APID, 'Languages');
end;

procedure TTestPersonSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VPerson: TTestPerson;
begin
  VPerson := APID.Entity as TTestPerson;
  Driver.WriteString(VPerson.Name);
  Driver.WriteInteger(VPerson.Age);
  Mapper.StoreToDriver(TTestAddress, VPerson.Address, Driver);
  Mapper.StoreToDriver(TTestCity, VPerson.City, Driver);
end;

class function TTestPersonSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestPerson;
end;

{ TTestAddressSQLMapping }

function TTestAddressSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEADDRESS + BuildParams(ASize);
end;

function TTestAddressSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTADDRESS;
end;

function TTestAddressSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTADDRESS;
end;

function TTestAddressSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATEADDRESS;
end;

procedure TTestAddressSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VAddress: TTestAddress;
begin
  VAddress := APID.Entity as TTestAddress;
  VAddress.Street := Driver.ReadString;
  VAddress.ZipCode := Driver.ReadString;
end;

procedure TTestAddressSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VAddress: TTestAddress;
begin
  VAddress := APID.Entity as TTestAddress;
  Driver.WriteString(VAddress.Street);
  Driver.WriteString(VAddress.ZipCode);
end;

class function TTestAddressSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestAddress;
end;

{ TTestCitySQLMapping }

function TTestCitySQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETECITY + BuildParams(ASize);
end;

function TTestCitySQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTCITY;
end;

function TTestCitySQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTCITY;
end;

function TTestCitySQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATECITY;
end;

procedure TTestCitySQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VCity: TTestCity;
begin
  VCity := APID.Entity as TTestCity;
  VCity.Name := Driver.ReadString;
end;

procedure TTestCitySQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
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

function TTestPhoneSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEPHONE + BuildParams(ASize);
end;

function TTestPhoneSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTPHONE;
end;

function TTestPhoneSQLMapping.GenerateSelectCollectionStatement(
  const AOwnerClass: TClass; const AOwnerAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if AOwnerClass = TTestPerson then
    Result := CSQLSELECTPERSON_PHONES
  else
    Result := inherited GenerateSelectCollectionStatement(AOwnerClass, AOwnerAttrMetadata);
end;

function TTestPhoneSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATEPHONE;
end;

procedure TTestPhoneSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VPhone: TTestPhone;
begin
  VPhone := APID.Entity as TTestPhone;
  VPhone.Number := Driver.ReadString;
end;

procedure TTestPhoneSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
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

function TTestLanguageSQLMapping.GenerateInsertStatement(
  const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTLANG;
end;

function TTestLanguageSQLMapping.GenerateSelectCollectionStatement(
  const AOwnerClass: TClass; const AOwnerAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if AOwnerClass = TTestPerson then
    Result := CSQLSELECTPERSON_LANG
  else
    Result := inherited GenerateSelectCollectionStatement(AOwnerClass, AOwnerAttrMetadata);
end;

function TTestLanguageSQLMapping.GenerateUpdateStatement(
  const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATELANG;
end;

procedure TTestLanguageSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VLang: TTestLanguage;
begin
  VLang := APID.Entity as TTestLanguage;
  VLang.Name := Driver.ReadString;
end;

procedure TTestLanguageSQLMapping.WriteInternalsToDriver(
  const APID: TJCoreOPFPID);
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
  RegisterTest('jcore.opf.session', TTestOPFSessionTests);
  RegisterTest('jcore.opf.metadata', TTestOPFMetadataTests);
  RegisterTest('jcore.opf.mapping.core', TTestOPFMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFInsertManualMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFUpdateManualMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFSelectManualMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFDeleteOneManualMappingTests);
  RegisterTest('jcore.opf.mapping.manualmapping', TTestOPFDeleteArrayManualMappingTests);
  RegisterTest('jcore.opf.mapping.cleandirty', TTestOPFCleanDirtyAttributeTests);

end.

