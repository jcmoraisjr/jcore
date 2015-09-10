unit TestOPFConfig;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  Classes,
  fpcunit,
  JCoreClasses,
  JCoreLogger,
  JCoreList,
  JCoreEntity,
  JCoreOPFDriver,
  JCoreOPFOID,
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

  TTestSQLDriver = class;

  { TTestOPFAbstractTestCase }

  TTestOPFAbstractTestCase = class(TTestCase)
  private
    FConfig: IJCoreOPFConfiguration;
    FDriver: TTestSQLDriver;
    FSession: ITestOPFSession;
    class var FLOG: IJCoreLogger;
    procedure AssertCommands(const AList: TStringList; const AShortName, ALongName: string; const ACommands: array of string);
    procedure CreateConfiguration;
    function GetConfig: IJCoreOPFConfiguration;
    function GetDriver: TTestSQLDriver;
    function GetSession: ITestOPFSession;
  protected
    procedure AssertExceptionStore(const ASession: IJCoreOPFSession; const AEntity: TObject; const AException: TJCoreExceptionClass; const ACode: Integer);
    procedure AssertSQLDriverCommands(const ACommands: array of string);
    procedure AssertSQLDriverTransaction(const ATransactions: array of string);
    procedure ConfigAutoMapping;
    procedure ConfigIdContactModel;
    procedure ConfigIPIDContactMapping;
    procedure ConfigIPIDContactModel;
    procedure ConfigProxyCircularModel;
    procedure ConfigProxyContactMapping;
    procedure ConfigProxyContactModel;
    procedure ConfigProxyInvoiceMapping;
    procedure ConfigProxyInvoiceModel;
    procedure CreateConfigIdContactAuto;
    procedure CreateConfigIPIDContactAuto;
    procedure CreateConfigIPIDContactManual;
    procedure CreateConfigProxyCircularAuto;
    procedure CreateConfigProxyContactManual;
    procedure CreateConfigProxyInvoiceAuto;
    procedure CreateConfigProxyInvoiceManual;
    procedure InternalConfigureSession; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
    class property LOG: IJCoreLogger read FLOG;
    property Config: IJCoreOPFConfiguration read GetConfig;
    property Driver: TTestSQLDriver read GetDriver;
    property Session: ITestOPFSession read GetSession;
  end;

  { TTestOPFIPIDContactTestCase }

  TTestOPFIPIDContactTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure InternalConfigureSession; override;
  end;

  { TTestOPFProxyContactTestCase }

  TTestOPFProxyContactTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure InternalConfigureSession; override;
  end;

  { TTestOPFIdContactTestCase }

  TTestOPFIdContactTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure InternalConfigureSession; override;
  end;

  { TTestOPFInvoiceManualMappingTestCase }

  TTestOPFInvoiceManualMappingTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure InternalConfigureSession; override;
  end;

  { TTestOPFInvoiceAutoMappingTestCase }

  TTestOPFInvoiceAutoMappingTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure InternalConfigureSession; override;
  end;

  { TTestOPFOIDGenerator }

  TTestOPFOIDGenerator = class(TInterfacedObject, IJCoreOPFOIDGenerator)
  private
    class var FCurrentOID: Integer;
  public
    function IsPostInsertGenerator: Boolean;
    function ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
    function ReadString(const ADriver: TJCoreOPFDriver): string;
    class property CurrentOID: Integer read FCurrentOID;
    class procedure ClearOID;
  end;

  { TTestEmptyDriver }

  TTestEmptyDriver = class(TJCoreOPFSQLDriver)
  public
    class function DriverName: string; override;
  end;

  { TTestEmptyMapping }

  TTestEmptyMapping = class(TJCoreOPFSQLMapping)
  protected
    procedure InternalInsert(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
    procedure InternalUpdate(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestSQLQuery }

  TTestSQLQuery = class(TInterfacedObject, IJCoreOPFSQLQuery, IJCoreOPFSQLResultSet)
  private
    FDriver: TTestSQLDriver;
    FParams: IJCoreOPFParams;
    FSize: Integer;
    FSQL: string;
    procedure AddCommand(const ACommand: string);
    function PopData(const APopFromQueue: Boolean = True): string;
    procedure ReadParams;
  protected
    property Driver: TTestSQLDriver read FDriver;
  public
    constructor Create(const ADriver: TTestSQLDriver; const ASQL: string; AParams: IJCoreOPFParams);
    procedure Close;
    function ExecSQL: Integer;
    function IsClosed: Boolean;
    function OpenCursor: IJCoreOPFSQLResultSet;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadNull: Boolean;
    function ReadString: string;
    function Size: Integer;
    procedure SkipReading;
  end;

  { TTestSQLDatabase }

  TTestSQLDatabase = class(TJCoreOPFSQLDatabase)
  public
    function AutoincSQL: string; override;
    class function DatabaseName: string; override;
    function SequenceSQL(const ASequenceName: string; const AOIDCount: Integer): string; override;
  end;

  { TTestSQLDriver }

  TTestSQLDriver = class(TJCoreOPFSQLDriver)
  private
    FIgnoreParams: Boolean;
    class var FCommands: TStringList;
    class var FData:  TStringList;
    class var FExpectedResultsets: TJCoreIntegerList;
    class var FTransaction: TStringList;
  protected
    procedure InternalCommit; override;
    function InternalDatabaseClass: TJCoreOPFSQLDatabaseClass; override;
    function InternalCreateQuery(const ASQL: string; const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery; override;
  public
    class constructor Create;
    class destructor Destroy;
    class function DriverName: string; override;
    function HasIgnoreParams: Boolean;
    procedure IgnoreParams;
    class property Commands: TStringList read FCommands;
    class property Data: TStringList read FData;
    class property ExpectedResultsets: TJCoreIntegerList read FExpectedResultsets write FExpectedResultsets;
    class property Transaction: TStringList read FTransaction;
  end;

  { TTestSQLMapping }

  TTestSQLMapping = class(TJCoreOPFSQLMapping);

implementation

uses
  typinfo,
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

procedure TTestOPFAbstractTestCase.CreateConfiguration;
begin
  FConfig := TTestOPFConfig.Create;
  FConfig.DriverClass := TTestSQLDriver;
  FConfig.Model.OIDClass := TJCoreOPFOIDInt64;
  FConfig.Model.OIDGenerator := TTestOPFOIDGenerator.Create;
  FConfig.Model.OrderFieldName := 'SEQ';
  TestContactModel := FConfig.Model;
  TestInvoiceModel := FConfig.Model;
end;

function TTestOPFAbstractTestCase.GetConfig: IJCoreOPFConfiguration;
begin
  if not Assigned(FConfig) then
    InternalConfigureSession;
  Result := FConfig;
end;

function TTestOPFAbstractTestCase.GetDriver: TTestSQLDriver;
begin
  if not Assigned(FDriver) then
    FDriver := TTestSQLDriver.Create(nil);
  Result := FDriver;
end;

function TTestOPFAbstractTestCase.GetSession: ITestOPFSession;
begin
  if not Assigned(FSession) then
    InternalConfigureSession;
  Result := FSession;
end;

procedure TTestOPFAbstractTestCase.AssertExceptionStore(const ASession: IJCoreOPFSession;
  const AEntity: TObject; const AException: TJCoreExceptionClass; const ACode: Integer);
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
      AssertEquals('Exception class', AException.ClassType, E.ClassType);
      AssertEquals('Exception code', ACode, EJCoreException(E).Code);
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

procedure TTestOPFAbstractTestCase.ConfigAutoMapping;
begin
  Config.AddMappingClass([TTestSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigIdContactModel;
begin
  Config.Model.AddClass([TTestIdPerson]);
end;

procedure TTestOPFAbstractTestCase.ConfigIPIDContactMapping;
begin
  Config.AddMappingClass([
   TTestIPIDSimpleSQLMapping, TTestIPIDPersonSQLMapping, TTestIPIDEmployeeSQLMapping,
   TTestIPIDAddressSQLMapping, TTestIPIDCitySQLMapping, TTestIPIDPhoneSQLMapping,
   TTestIPIDLanguageSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigIPIDContactModel;
begin
  Config.Model.AddClass([
   TTestIPIDPerson, TTestIPIDPhone, TTestIPIDLanguage, TTestIPIDAddress, TTestIPIDCity]);
  Config.Model.AddGenerics(TTestIPIDPhoneList, TTestIPIDPhone);
  Config.Model.AddGenerics(TTestIPIDLanguageList, TTestIPIDLanguage);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyCircularModel;
begin
  Config.Model.AddClass([TCircularPerson]);
  Config.Model.AddGenerics(TCircularPersonList, TCircularPerson);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyContactMapping;
begin
  Config.AddMappingClass([
   TTestProxyPhoneSQLMapping, TTestProxyCitySQLMapping, TTestProxyPersonSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyContactModel;
begin
  Config.Model.AddClass([TTestProxyPhone, TTestProxyCity, TTestProxyPerson]);
  Config.Model.AddGenerics(TTestProxyPhoneList, TTestProxyPhone);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyInvoiceMapping;
begin
  Config.AddMappingClass([
   TAddressSQLMapping, TClientSQLMapping, TPersonSQLMapping, TCompanySQLMapping, TProductSQLMapping,
   TInvoiceSQLMapping, TInvoiceItemSQLMapping, TInvoiceItemProductSQLMapping,
   TInvoiceItemServiceSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyInvoiceModel;
begin
  Config.Model.AddClass([
   TAddress, TClient, TPerson, TCompany, TProduct, TInvoiceItem, TInvoiceItemProduct, TInvoiceItemService,
   TInvoice]);
  Config.Model.AddGenerics(TInvoiceItemList, TInvoiceItem);
end;

procedure TTestOPFAbstractTestCase.CreateConfigIdContactAuto;
begin
  CreateConfiguration;
  ConfigAutoMapping;
  ConfigIdContactModel;
  FSession := FConfig.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.CreateConfigIPIDContactAuto;
begin
  CreateConfiguration;
  ConfigAutoMapping;
  ConfigIPIDContactModel;
  FSession := FConfig.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.CreateConfigIPIDContactManual;
begin
  CreateConfiguration;
  ConfigIPIDContactMapping;
  ConfigIPIDContactModel;
  FSession := FConfig.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.CreateConfigProxyCircularAuto;
begin
  CreateConfiguration;
  ConfigAutoMapping;
  ConfigProxyCircularModel;
  FSession := FConfig.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.CreateConfigProxyContactManual;
begin
  CreateConfiguration;
  ConfigProxyContactMapping;
  ConfigProxyContactModel;
  FSession := FConfig.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.CreateConfigProxyInvoiceAuto;
begin
  CreateConfiguration;
  ConfigAutoMapping;
  ConfigProxyInvoiceModel;
  FSession := FConfig.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.CreateConfigProxyInvoiceManual;
begin
  CreateConfiguration;
  ConfigProxyInvoiceMapping;
  ConfigProxyInvoiceModel;
  FSession := FConfig.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.SetUp;
begin
  inherited SetUp;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.test.opf');
  AssertNull(FConfig);
  AssertNull(FSession);
  AssertEquals(0, TTestSQLDriver.Commands.Count);
  AssertEquals(0, TTestSQLDriver.Data.Count);
  AssertEquals(0, TTestSQLDriver.Transaction.Count);
  AssertEquals(0, TTestOPFOIDGenerator.CurrentOID);
end;

procedure TTestOPFAbstractTestCase.TearDown;
begin
  inherited TearDown;
  TestContactModel := nil;
  TestInvoiceModel := nil;
  TTestSQLDriver.Commands.Clear;
  TTestSQLDriver.Data.Clear;
  TTestSQLDriver.ExpectedResultsets.Clear;
  TTestSQLDriver.Transaction.Clear;
  TTestOPFOIDGenerator.ClearOID;
  FreeAndNil(FDriver);
  FSession := nil;
  FConfig := nil;
end;

{ TTestOPFIPIDContactTestCase }

procedure TTestOPFIPIDContactTestCase.InternalConfigureSession;
begin
  CreateConfigIPIDContactManual;
end;

{ TTestOPFProxyContactTestCase }

procedure TTestOPFProxyContactTestCase.InternalConfigureSession;
begin
  CreateConfigProxyContactManual;
end;

{ TTestOPFIdContactTestCase }

procedure TTestOPFIdContactTestCase.InternalConfigureSession;
begin
  CreateConfigIdContactAuto;
end;

{ TTestOPFInvoiceManualMappingTestCase }

procedure TTestOPFInvoiceManualMappingTestCase.InternalConfigureSession;
begin
  CreateConfigProxyInvoiceManual;
end;

{ TTestOPFInvoiceAutoMappingTestCase }

procedure TTestOPFInvoiceAutoMappingTestCase.InternalConfigureSession;
begin
  CreateConfigProxyInvoiceAuto;
end;

{ TTestOPFOIDGenerator }

function TTestOPFOIDGenerator.IsPostInsertGenerator: Boolean;
begin
  Result := False;
end;

function TTestOPFOIDGenerator.ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
begin
  Inc(FCurrentOID);
  Result := CurrentOID;
end;

function TTestOPFOIDGenerator.ReadString(const ADriver: TJCoreOPFDriver): string;
begin
  Result := IntToStr(ReadInt64(ADriver));
end;

class procedure TTestOPFOIDGenerator.ClearOID;
begin
  FCurrentOID := 0;
end;

{ TTestEmptyDriver }

class function TTestEmptyDriver.DriverName: string;
begin
  Result := 'TestEmptyDriver';
end;

{ TTestEmptyMapping }

procedure TTestEmptyMapping.InternalInsert(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
begin
end;

procedure TTestEmptyMapping.InternalUpdate(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
begin
end;

procedure TTestEmptyMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
begin
end;

class function TTestEmptyMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := True;
end;

{ TTestSQLQuery }

procedure TTestSQLQuery.AddCommand(const ACommand: string);
begin
  Driver.Commands.Add(ACommand);
end;

function TTestSQLQuery.PopData(const APopFromQueue: Boolean): string;
begin
  if Driver.Data.Count = 0 then
    raise Exception.Create('Trying to read an empty data queue');
  Result := Driver.Data[0];
  if APopFromQueue then
    Driver.Data.Delete(0);
end;

procedure TTestSQLQuery.ReadParams;
var
  VParamCount: Integer;
  VType: TTypeKind;
  I: Integer;
begin
  VParamCount := 0;
  for I := 1 to Length(FSQL) do
    if FSQL[I] = '?' then
      Inc(VParamCount);
  if Driver.HasIgnoreParams and (VParamCount > FParams.Count) then
    VParamCount := FParams.Count;
  for I := 0 to Pred(VParamCount) do
  begin
    VType := FParams.ReadType;
    case VType of
      tkUnknown: AddCommand('WriteNull');
      tkInteger: AddCommand('WriteInt32 ' + IntToStr(FParams.ReadInt32));
      tkInt64: AddCommand('WriteInt64 ' + IntToStr(FParams.ReadInt64));
      tkAString: AddCommand('WriteString ' + FParams.ReadString);
      else raise Exception.CreateFmt('Unsupported type', [GetEnumName(TypeInfo(TTypeKind), Ord(VType))]);
    end;
  end;
end;

constructor TTestSQLQuery.Create(const ADriver: TTestSQLDriver; const ASQL: string;
  AParams: IJCoreOPFParams);
begin
  inherited Create;
  FDriver := ADriver;
  FSQL := ASQL;
  FParams := AParams;
end;

procedure TTestSQLQuery.Close;
begin
  FSize := -1;
end;

function TTestSQLQuery.ExecSQL: Integer;
begin
  ReadParams;
  Driver.Commands.Add('ExecSQL ' + FSQL);
  if Driver.ExpectedResultsets.Count > 0 then
  begin
    Result := Driver.ExpectedResultsets[0];
    Driver.ExpectedResultsets.Delete(0);
  end else
    Result := 0;
end;

function TTestSQLQuery.IsClosed: Boolean;
begin
  Result := False;
end;

function TTestSQLQuery.OpenCursor: IJCoreOPFSQLResultSet;
begin
  FSize := ExecSQL;
  Result := Self;
end;

function TTestSQLQuery.ReadInt32: Integer;
begin
  Result := StrToInt(PopData);
end;

function TTestSQLQuery.ReadInt64: Int64;
begin
  Result := StrToInt64(PopData);
end;

function TTestSQLQuery.ReadNull: Boolean;
begin
  Result := PopData(False) = 'null';
  if Result then
    PopData;
end;

function TTestSQLQuery.ReadString: string;
begin
  Result := PopData;
end;

function TTestSQLQuery.Size: Integer;
begin
  Result := FSize;
end;

procedure TTestSQLQuery.SkipReading;
begin
  PopData;
end;

{ TTestSQLDatabase }

function TTestSQLDatabase.AutoincSQL: string;
begin
  Result := 'SELECT lastId()';
end;

class function TTestSQLDatabase.DatabaseName: string;
begin
  Result := 'TestDatabase';
end;

function TTestSQLDatabase.SequenceSQL(const ASequenceName: string; const AOIDCount: Integer): string;
begin
  Result := Format('SELECT next(''%s'')', [ASequenceName]);
end;

{ TTestSQLDriver }

procedure TTestSQLDriver.InternalCommit;
begin
  inherited InternalCommit;
  Transaction.Add('commit');
end;

function TTestSQLDriver.InternalDatabaseClass: TJCoreOPFSQLDatabaseClass;
begin
  Result := TTestSQLDatabase;
end;

function TTestSQLDriver.InternalCreateQuery(const ASQL: string;
  const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery;
begin
  Result := TTestSQLQuery.Create(Self, ASQL, AParams);
end;

class constructor TTestSQLDriver.Create;
begin
  FCommands := TStringList.Create;
  FData := TStringList.Create;
  FExpectedResultsets := TJCoreIntegerList.Create;
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

function TTestSQLDriver.HasIgnoreParams: Boolean;
begin
  Result := FIgnoreParams;
end;

procedure TTestSQLDriver.IgnoreParams;
begin
  FIgnoreParams := True;
end;

end.

