unit TestOPFConfig;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  Classes,
  fgl,
  fpcunit,
  JCoreLogger,
  JCoreList,
  JCoreEntity,
  JCoreOPFDriver,
  JCoreOPFOIDGen,
  JCoreOPFMetadata,
  JCoreOPFMapping,
  JCoreOPFMappingSQL,
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
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
  end;

  { TTestOPFSession }

  TTestOPFSession = class(TJCoreOPFSession, ITestOPFSession)
  private
    class var FCommitCount: Integer;
    class var FLastCommitPIDList: TJCoreOPFPIDList;
  protected
    procedure InternalCommit; override;
  public
    class constructor Create;
    class destructor Destroy;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
    class function ExistPIDInCommitPIDList(const APID: IJCorePID): Boolean;
    class property CommitCount: Integer read FCommitCount write FCommitCount;
    class property LastCommitPIDList: TJCoreOPFPIDList read FLastCommitPIDList;
  end;

  TTestOPFMappingClassArray = array of TJCoreOPFMappingClass;

  TTestOPFSQLDriverMock = class;

  { TTestOPFAbstractTestCase }

  TTestOPFAbstractTestCase = class(TTestCase)
  private
    FDriver: TTestOPFSQLDriverMock;
    FSessionCircularAuto: ITestOPFSession;
    FSessionInvoiceAuto: ITestOPFSession;
    FSessionInvoiceManual: ITestOPFSession;
    FSessionIPIDContactAuto: ITestOPFSession;
    FSessionIPIDContactManual: ITestOPFSession;
    FSessionProxyContact: ITestOPFSession;
    class var FLOG: IJCoreLogger;
    procedure AssertCommands(const AList: TStringList; const AShortName, ALongName: string; const ACommands: array of string);
    function GetDriver: TTestOPFSQLDriverMock;
    function GetSessionCircularAuto: ITestOPFSession;
    function GetSessionInvoiceAuto: ITestOPFSession;
    function GetSessionInvoiceManual: ITestOPFSession;
    function GetSessionIPIDContactAuto: ITestOPFSession;
    function GetSessionIPIDContactManual: ITestOPFSession;
    function GetSessionProxyContact: ITestOPFSession;
  protected
    procedure AssertCommands(const AList: TStringList; const ACommands: array of string);
    procedure AssertExceptionStore(const ASession: IJCoreOPFSession; const AEntity: TObject; const AException: ExceptClass);
    procedure AssertSQLDriverCommands(const ACommands: array of string);
    procedure AssertSQLDriverTransaction(const ATransactions: array of string);
    procedure ConfigAutoMapping(const AConfig: IJCoreOPFConfiguration);
    procedure ConfigIPIDContactMapping(const AConfig: IJCoreOPFConfiguration);
    procedure ConfigIPIDContactModel(const AConfig: IJCoreOPFConfiguration);
    procedure ConfigProxyCircularModel(const AConfig: IJCoreOPFConfiguration);
    procedure ConfigProxyContactMapping(const AConfig: IJCoreOPFConfiguration);
    procedure ConfigProxyContactModel(const AConfig: IJCoreOPFConfiguration);
    procedure ConfigProxyInvoiceMapping(const AConfig: IJCoreOPFConfiguration);
    procedure ConfigProxyInvoiceModel(const AConfig: IJCoreOPFConfiguration);
    function CreateConfiguration: IJCoreOPFConfiguration;
    procedure SetUp; override;
    procedure TearDown; override;
    class property LOG: IJCoreLogger read FLOG;
    property Driver: TTestOPFSQLDriverMock read GetDriver;
    property SessionCircularAuto: ITestOPFSession read GetSessionCircularAuto;
    property SessionInvoiceAuto: ITestOPFSession read GetSessionInvoiceAuto;
    property SessionInvoiceManual: ITestOPFSession read GetSessionInvoiceManual;
    property SessionIPIDContactAuto: ITestOPFSession read GetSessionIPIDContactAuto;
    property SessionIPIDContactManual: ITestOPFSession read GetSessionIPIDContactManual;
    property SessionProxyContact: ITestOPFSession read GetSessionProxyContact;
  end;

  { TTestOPFIPIDContactTestCase }

  TTestOPFIPIDContactTestCase = class(TTestOPFAbstractTestCase)
  protected
    property Session: ITestOPFSession read GetSessionIPIDContactManual;
  end;

  { TTestOPFProxyContactTestCase }

  TTestOPFProxyContactTestCase = class(TTestOPFAbstractTestCase)
  protected
    property Session: ITestOPFSession read GetSessionProxyContact;
  end;

  { TTestOPFInvoiceManualMappingTestCase }

  TTestOPFInvoiceManualMappingTestCase = class(TTestOPFAbstractTestCase)
  protected
    property Session: ITestOPFSession read GetSessionInvoiceManual;
  end;

  { TTestOPFInvoiceAutoMappingTestCase }

  TTestOPFInvoiceAutoMappingTestCase = class(TTestOPFAbstractTestCase)
  protected
    property Session: ITestOPFSession read GetSessionInvoiceAuto;
  end;

  { TTestOPFGeneratorSQLDriver }

  TTestOPFGeneratorSQLDriver = class(TJCoreOPFOIDGeneratorSQLDriver)
  private
    class var FCurrentOID: Integer;
  protected
    procedure InternalGenerateOIDs(const AOIDCount: Integer); override;
  public
    class property CurrentOID: Integer read FCurrentOID;
    class procedure ClearOID;
  end;

  { TTestEmptyDriver }

  TTestEmptyDriver = class(TJCoreOPFSQLDriver)
  protected
    procedure InternalCommit; override;
  public
    class function DriverName: string; override;
    procedure WriteString(const AValue: string); override;
  end;

  { TTestEmptyMapping }

  TTestEmptyMapping = class(TJCoreOPFSQLMapping)
  protected
    procedure InternalInsert(const AMapping: TJCoreOPFADMMapping); override;
    procedure InternalUpdate(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  TTestIntegerList = specialize TFPGList<Integer>;

  { TTestSQLDriver }

  TTestSQLDriver = class(TJCoreOPFSQLDriver)
  private
    procedure AddCommand(const ACommand: string);
    function PopData(const APopFromQueue: Boolean = True): string;
    class var FCommands: TStringList;
    class var FData:  TStringList;
    class var FExpectedResultsets: TTestIntegerList;
    class var FTransaction: TStringList;
  protected
    procedure InternalCommit; override;
    function InternalExecSQL(const ASQL: string): Integer; override;
  public
    class constructor Create;
    class destructor Destroy;
    class function DriverName: string; override;
    function CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator; override;
    function ReadInt32: Integer; override;
    function ReadInt64: Int64; override;
    function ReadNull: Boolean; override;
    function ReadNullAndSkip: Boolean; override;
    function ReadString: string; override;
    procedure WriteInt32(const AValue: Integer); override;
    procedure WriteInt64(const AValue: Int64); override;
    procedure WriteString(const AValue: string); override;
    procedure WriteNull; override;
    class property Commands: TStringList read FCommands;
    class property Data: TStringList read FData;
    class property ExpectedResultsets: TTestIntegerList read FExpectedResultsets write FExpectedResultsets;
    class property Transaction: TStringList read FTransaction;
  end;

  { TTestSQLMapping }

  TTestSQLMapping = class(TJCoreOPFSQLMapping);

  { TTestOPFSQLDriverMock }

  TTestOPFSQLDriverMock = class(TJCoreOPFSQLDriver)
  private
    FCommands: TStringList;
    FParams: TStringList;
  protected
    function InternalCreateQuery(const ASQL: string; const AParams: TJCoreNativeTypeQueue): IJCoreOPFSQLQuery; override;
    procedure InternalCommit; override;
    function InternalExecSQL(const ASQL: string): Integer; override;
  public
    constructor Create(const AParams: TStringList); override;
    destructor Destroy; override;
    function CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator; override;
    class function DriverName: string; override;
    function ReadInt32: Integer; override;
    function ReadInt64: Int64; override;
    function ReadNull: Boolean; override;
    function ReadNullAndSkip: Boolean; override;
    function ReadString: string; override;
    property Commands: TStringList read FCommands;
    property Params: TStringList read FParams;
  end;

  { TTestOPFSQLQueryMock }

  TTestOPFSQLQueryMock = class(TInterfacedObject, IJCoreOPFSQLQuery, IJCoreOPFSQLResultSet)
  private
    FDriver: TTestOPFSQLDriverMock;
    FParams: TJCoreNativeTypeQueue;
    FSQL: string;
    procedure ReadParams;
  private // interfaces
    procedure Close;
    function ExecSQL: Integer;
    function IsClosed: Boolean;
    function OpenCursor: IJCoreOPFSQLResultSet;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadNull: Boolean;
    function ReadString: string;
    function Size: Integer;
  protected
    property Driver: TTestOPFSQLDriverMock read FDriver;
    property Params: TJCoreNativeTypeQueue read FParams;
    property SQL: string read FSQL;
  public
    constructor Create(const ADriver: TTestOPFSQLDriverMock; const ASQL: string; const AParams: TJCoreNativeTypeQueue);
  end;

implementation

uses
  typinfo,
  JCoreMetadata,
  JCoreOPFException,
  JCoreOPFOID,
  TestOPFModelCircular,
  TestOPFModelContact,
  TestOPFModelInvoice,
  TestOPFMappingContact,
  TestOPFMappingInvoice;

{ TTestOPFConfig }

function TTestOPFConfig.InternalCreateSession(const ADriver: TJCoreOPFDriver): IJCoreOPFSession;
begin
  Result := TTestOPFSession.Create(Self, ADriver);
end;

{ TTestOPFSession }

procedure TTestOPFSession.InternalCommit;
var
  I: Integer;
begin
  Inc(FCommitCount);
  LastCommitPIDList.Clear;
  for I := 0 to Pred(InTransactionPIDList.Count) do
    LastCommitPIDList.Add(InTransactionPIDList[I]);
  inherited InternalCommit;
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
  Result := AcquireClassMapping(AClass).Metadata;
end;

class function TTestOPFSession.ExistPIDInCommitPIDList(const APID: IJCorePID): Boolean;
begin
  Result := LastCommitPIDList.IndexOf(APID as TJCoreOPFPID) >= 0;
end;

{ TTestOPFAbstractTestCase }

procedure TTestOPFAbstractTestCase.AssertCommands(const AList: TStringList; const AShortName,
  ALongName: string; const ACommands: array of string);
var
  I: Integer;
begin
  try
    AssertEquals(AShortName + ' count', Length(ACommands), AList.Count);
    for I := Low(ACommands) to High(ACommands) do
      AssertEquals(AShortName + '[' + IntToStr(I) + ']', ACommands[I], AList[I]);
  except
    LOG.Warn('Expected ' + ALongName + ':');
    for I := Low(ACommands) to High(ACommands) do
      LOG.Warn('  ' + IntToStr(I) + ': ' + ACommands[I]);
    LOG.Warn('Executed ' + ALongName + ':');
    for I := 0 to Pred(AList.Count) do
      LOG.Warn('  ' + IntToStr(I) + ': ' + AList[I]);
    raise;
  end;
  AList.Clear;
end;

function TTestOPFAbstractTestCase.GetDriver: TTestOPFSQLDriverMock;
begin
  if not Assigned(FDriver) then
    FDriver := TTestOPFSQLDriverMock.Create(nil);
  Result := FDriver;
end;

function TTestOPFAbstractTestCase.GetSessionCircularAuto: ITestOPFSession;
var
  VConfig: IJCoreOPFConfiguration;
begin
  if not Assigned(FSessionCircularAuto) then
  begin
    VConfig := CreateConfiguration;
    ConfigAutoMapping(VConfig);
    ConfigProxyCircularModel(VConfig);
    FSessionCircularAuto := VConfig.CreateSession as ITestOPFSession;
  end;
  Result := FSessionCircularAuto;
end;

function TTestOPFAbstractTestCase.GetSessionInvoiceAuto: ITestOPFSession;
var
  VConfig: IJCoreOPFConfiguration;
begin
  if not Assigned(FSessionInvoiceAuto) then
  begin
    VConfig := CreateConfiguration;
    ConfigAutoMapping(VConfig);
    ConfigProxyInvoiceModel(VConfig);
    FSessionInvoiceAuto := VConfig.CreateSession as ITestOPFSession;
  end;
  Result := FSessionInvoiceAuto;
end;

function TTestOPFAbstractTestCase.GetSessionInvoiceManual: ITestOPFSession;
var
  VConfig: IJCoreOPFConfiguration;
begin
  if not Assigned(FSessionInvoiceManual) then
  begin
    VConfig := CreateConfiguration;
    ConfigProxyInvoiceMapping(VConfig);
    ConfigProxyInvoiceModel(VConfig);
    FSessionInvoiceManual := VConfig.CreateSession as ITestOPFSession;
  end;
  Result := FSessionInvoiceManual;
end;

function TTestOPFAbstractTestCase.GetSessionIPIDContactAuto: ITestOPFSession;
var
  VConfig: IJCoreOPFConfiguration;
begin
  if not Assigned(FSessionIPIDContactAuto) then
  begin
    VConfig := CreateConfiguration;
    ConfigAutoMapping(VConfig);
    ConfigIPIDContactModel(VConfig);
    FSessionIPIDContactAuto := VConfig.CreateSession as ITestOPFSession;
  end;
  Result := FSessionIPIDContactAuto;
end;

function TTestOPFAbstractTestCase.GetSessionIPIDContactManual: ITestOPFSession;
var
  VConfig: IJCoreOPFConfiguration;
begin
  if not Assigned(FSessionIPIDContactManual) then
  begin
    VConfig := CreateConfiguration;
    ConfigIPIDContactMapping(VConfig);
    ConfigIPIDContactModel(VConfig);
    FSessionIPIDContactManual := VConfig.CreateSession as ITestOPFSession;
  end;
  Result := FSessionIPIDContactManual;
end;

function TTestOPFAbstractTestCase.GetSessionProxyContact: ITestOPFSession;
var
  VConfig: IJCoreOPFConfiguration;
begin
  if not Assigned(FSessionProxyContact) then
  begin
    VConfig := CreateConfiguration;
    ConfigProxyContactMapping(VConfig);
    ConfigProxyContactModel(VConfig);
    FSessionProxyContact := VConfig.CreateSession as ITestOPFSession;
  end;
  Result := FSessionProxyContact;
end;

procedure TTestOPFAbstractTestCase.AssertCommands(const AList: TStringList;
  const ACommands: array of string);
begin
  AssertCommands(AList, 'cmd', 'commands', ACommands);
end;

procedure TTestOPFAbstractTestCase.AssertExceptionStore(const ASession: IJCoreOPFSession;
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

procedure TTestOPFAbstractTestCase.AssertSQLDriverCommands(const ACommands: array of string);
begin
  AssertCommands(TTestSQLDriver.Commands, 'cmd', 'commands', ACommands);
end;

procedure TTestOPFAbstractTestCase.AssertSQLDriverTransaction(const ATransactions: array of string);
begin
  AssertCommands(TTestSQLDriver.Transaction, 'tr', 'transactions', ATransactions);
end;

procedure TTestOPFAbstractTestCase.ConfigAutoMapping(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.AddMappingClass([TTestSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigIPIDContactMapping(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.AddMappingClass([
   TTestIPIDSimpleSQLMapping, TTestIPIDPersonSQLMapping, TTestIPIDEmployeeSQLMapping,
   TTestIPIDAddressSQLMapping, TTestIPIDCitySQLMapping, TTestIPIDPhoneSQLMapping,
   TTestIPIDLanguageSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigIPIDContactModel(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.Model.AddClass([TTestIPIDPerson, TTestIPIDPhone, TTestIPIDLanguage, TTestIPIDAddress, TTestIPIDCity]);
  AConfig.Model.AddGenerics(TTestIPIDPhoneList, TTestIPIDPhone);
  AConfig.Model.AddGenerics(TTestIPIDLanguageList, TTestIPIDLanguage);
  AConfig.Model.AcquireAttrMetadata(TTestIPIDPerson, 'Languages').CompositionType := jctAggregation;
  AConfig.Model.AcquireAttrMetadata(TTestIPIDPerson, 'City').CompositionType := jctAggregation;
end;

procedure TTestOPFAbstractTestCase.ConfigProxyCircularModel(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.Model.AddClass([TCircularPerson]);
  AConfig.Model.AddGenerics(TCircularPersonList, TCircularPerson);
  AConfig.Model.AcquireAttrMetadata(TCircularPerson, 'Dependent').CompositionType := jctAggregation;
end;

procedure TTestOPFAbstractTestCase.ConfigProxyContactMapping(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.AddMappingClass([TTestProxyPhoneSQLMapping, TTestProxyCitySQLMapping, TTestProxyPersonSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyContactModel(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.Model.AddClass([TTestProxyPhone, TTestProxyCity, TTestProxyPerson]);
  AConfig.Model.AddGenerics(TTestProxyPhoneList, TTestProxyPhone);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyInvoiceMapping(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.AddMappingClass([
   TAddressSQLMapping, TClientSQLMapping, TPersonSQLMapping, TCompanySQLMapping, TProductSQLMapping,
   TInvoiceSQLMapping, TInvoiceItemSQLMapping, TInvoiceItemProductSQLMapping,
   TInvoiceItemServiceSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyInvoiceModel(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.Model.AddClass([
   TAddress, TClient, TPerson, TCompany, TProduct, TInvoiceItem, TInvoiceItemProduct, TInvoiceItemService, TInvoice]);
  AConfig.Model.AddGenerics(TInvoiceItemList, TInvoiceItem);
  AConfig.Model.AcquireAttrMetadata(TInvoice, 'Client').CompositionType := jctAggregation;
  AConfig.Model.AcquireAttrMetadata(TInvoiceItemProduct, 'Product').CompositionType := jctAggregation;
end;

function TTestOPFAbstractTestCase.CreateConfiguration: IJCoreOPFConfiguration;
begin
  Result := TTestOPFConfig.Create;
  Result.DriverClass := TTestSQLDriver;
  Result.Model.OIDClass := TJCoreOPFOIDInt64;
  Result.Model.GeneratorName := 'GEN_APP';
end;

procedure TTestOPFAbstractTestCase.SetUp;
begin
  inherited SetUp;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.test.opf');
  AssertEquals(0, TTestSQLDriver.Commands.Count);
  AssertEquals(0, TTestSQLDriver.Data.Count);
  AssertEquals(0, TTestSQLDriver.Transaction.Count);
  AssertEquals(0, TTestOPFGeneratorSQLDriver.CurrentOID);
end;

procedure TTestOPFAbstractTestCase.TearDown;
begin
  inherited TearDown;
  TTestSQLDriver.Commands.Clear;
  TTestSQLDriver.Data.Clear;
  TTestSQLDriver.ExpectedResultsets.Clear;
  TTestSQLDriver.Transaction.Clear;
  TTestOPFGeneratorSQLDriver.ClearOID;
  FreeAndNil(FDriver);
  FSessionCircularAuto := nil;
  FSessionInvoiceAuto := nil;
  FSessionInvoiceManual := nil;
  FSessionIPIDContactAuto := nil;
  FSessionIPIDContactManual := nil;
  FSessionProxyContact := nil;
end;

{ TTestOPFGeneratorSQLDriver }

procedure TTestOPFGeneratorSQLDriver.InternalGenerateOIDs(const AOIDCount: Integer);
var
  I: Integer;
begin
  for I := 0 to Pred(AOIDCount) do
  begin
    Inc(FCurrentOID);
    OIDList.Add(CurrentOID);
  end;
end;

class procedure TTestOPFGeneratorSQLDriver.ClearOID;
begin
  FCurrentOID := 0;
end;

{ TTestEmptyDriver }

procedure TTestEmptyDriver.InternalCommit;
begin
end;

class function TTestEmptyDriver.DriverName: string;
begin
  Result := 'TestEmptyDriver';
end;

procedure TTestEmptyDriver.WriteString(const AValue: string);
begin
end;

{ TTestEmptyMapping }

procedure TTestEmptyMapping.InternalInsert(const AMapping: TJCoreOPFADMMapping);
begin
end;

procedure TTestEmptyMapping.InternalUpdate(const AMapping: TJCoreOPFADMMapping);
begin
end;

procedure TTestEmptyMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
begin
end;

class function TTestEmptyMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := True;
end;

{ TTestSQLDriver }

procedure TTestSQLDriver.AddCommand(const ACommand: string);
begin
  FCommands.Add(ACommand);
end;

function TTestSQLDriver.PopData(const APopFromQueue: Boolean): string;
begin
  if Data.Count = 0 then
    raise Exception.Create('Trying to read an empty data queue');
  Result := Data[0];
  if APopFromQueue then
    Data.Delete(0);
end;

procedure TTestSQLDriver.InternalCommit;
begin
  Transaction.Add('commit');
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
  FTransaction := TStringList.Create;
end;

class destructor TTestSQLDriver.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FCommands);
  FreeAndNil(FData);
  FreeAndNil(FExpectedResultsets);
end;

class function TTestSQLDriver.DriverName: string;
begin
  Result := 'TestSQLDriver';
end;

function TTestSQLDriver.CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator;
begin
  Result := TTestOPFGeneratorSQLDriver.Create(Self, AGeneratorName);
end;

function TTestSQLDriver.ReadInt32: Integer;
begin
  Result := StrToInt(PopData);
end;

function TTestSQLDriver.ReadInt64: Int64;
begin
  Result := StrToInt64(PopData);
end;

function TTestSQLDriver.ReadNull: Boolean;
begin
  Result := PopData(False) = 'null';
  if Result then
    PopData;
end;

function TTestSQLDriver.ReadNullAndSkip: Boolean;
begin
  Result := PopData = 'null';
end;

function TTestSQLDriver.ReadString: string;
begin
  Result := PopData;
end;

procedure TTestSQLDriver.WriteInt32(const AValue: Integer);
begin
  AddCommand('WriteInt32 ' + IntToStr(AValue));
end;

procedure TTestSQLDriver.WriteInt64(const AValue: Int64);
begin
  AddCommand('WriteInt64 ' + IntToStr(AValue));
end;

procedure TTestSQLDriver.WriteString(const AValue: string);
begin
  AddCommand('WriteString ' + AValue);
end;

procedure TTestSQLDriver.WriteNull;
begin
  AddCommand('WriteNull');
end;

{ TTestOPFSQLDriverMock }

function TTestOPFSQLDriverMock.InternalCreateQuery(const ASQL: string;
  const AParams: TJCoreNativeTypeQueue): IJCoreOPFSQLQuery;
begin
  Result := TTestOPFSQLQueryMock.Create(Self, ASQL, AParams);
end;

procedure TTestOPFSQLDriverMock.InternalCommit;
begin
end;

function TTestOPFSQLDriverMock.InternalExecSQL(const ASQL: string): Integer;
begin
  Result := 0;
end;

constructor TTestOPFSQLDriverMock.Create(const AParams: TStringList);
begin
  inherited Create(AParams);
  FCommands := TStringList.Create;
end;

destructor TTestOPFSQLDriverMock.Destroy;
begin
  FreeAndNil(FCommands);
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TTestOPFSQLDriverMock.CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator;
begin
  Result := nil;
end;

class function TTestOPFSQLDriverMock.DriverName: string;
begin
  Result := '';
end;

function TTestOPFSQLDriverMock.ReadInt32: Integer;
begin
  Result := 0;
end;

function TTestOPFSQLDriverMock.ReadInt64: Int64;
begin
  Result := 0;
end;

function TTestOPFSQLDriverMock.ReadNull: Boolean;
begin
  Result := False;
end;

function TTestOPFSQLDriverMock.ReadNullAndSkip: Boolean;
begin
  Result := False;
end;

function TTestOPFSQLDriverMock.ReadString: string;
begin
  Result := '';
end;

{ TTestOPFSQLQueryMock }

procedure TTestOPFSQLQueryMock.ReadParams;
var
  VType: TTypeKind;
begin
  while Params.Count > 0 do
  begin
    VType := Params.PopType;
    case VType of
      tkUnknown: Driver.Commands.Add('WriteNull');
      tkAString: Driver.Commands.Add('WriteString ' + Params.PopString);
      tkInteger: Driver.Commands.Add('WriteInt32 ' + IntToStr(Params.PopInt32));
      tkInt64: Driver.Commands.Add('WriteInt64 ' + IntToStr(Params.PopInt64));
      else raise EJCoreOPFDriver.Create('Unsupported type', [GetEnumName(TypeInfo(TTypeKind), Ord(VType))]);
    end;
  end;
end;

procedure TTestOPFSQLQueryMock.Close;
begin
end;

function TTestOPFSQLQueryMock.ExecSQL: Integer;
begin
  ReadParams;
  Driver.Commands.Add(SQL);
  Result := 0;
end;

function TTestOPFSQLQueryMock.IsClosed: Boolean;
begin
  Result := True;
end;

function TTestOPFSQLQueryMock.OpenCursor: IJCoreOPFSQLResultSet;
begin
  ReadParams;
  Driver.Commands.Add(SQL);
  Result := Self;
end;

function TTestOPFSQLQueryMock.ReadInt32: Integer;
begin
  Result := 0;
end;

function TTestOPFSQLQueryMock.ReadInt64: Int64;
begin
  Result := 0;
end;

function TTestOPFSQLQueryMock.ReadNull: Boolean;
begin
  Result := False;
end;

function TTestOPFSQLQueryMock.ReadString: string;
begin
  Result := '';
end;

function TTestOPFSQLQueryMock.Size: Integer;
begin
  Result := 0;
end;

constructor TTestOPFSQLQueryMock.Create(const ADriver: TTestOPFSQLDriverMock; const ASQL: string;
  const AParams: TJCoreNativeTypeQueue);
begin
  inherited Create;
  FDriver := ADriver;
  FSQL := ASQL;
  FParams := AParams;
end;

end.

