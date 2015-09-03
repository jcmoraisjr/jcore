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
    FSessionCircularAuto: ITestOPFSession;
    FSessionInvoiceAuto: ITestOPFSession;
    FSessionInvoiceManual: ITestOPFSession;
    FSessionIPIDContactAuto: ITestOPFSession;
    FSessionIPIDContactManual: ITestOPFSession;
    FSessionProxyContact: ITestOPFSession;
    class var FLOG: IJCoreLogger;
    procedure AssertCommands(const AList: TStringList; const AShortName, ALongName: string; const ACommands: array of string);
    procedure AssertSessionsNull;
    function GetDriver: TTestSQLDriver;
    function GetSessionCircularAuto: ITestOPFSession;
    function GetSessionInvoiceAuto: ITestOPFSession;
    function GetSessionInvoiceManual: ITestOPFSession;
    function GetSessionIPIDContactAuto: ITestOPFSession;
    function GetSessionIPIDContactManual: ITestOPFSession;
    function GetSessionProxyContact: ITestOPFSession;
  protected
    procedure AssertExceptionStore(const ASession: IJCoreOPFSession; const AEntity: TObject; const AException: TJCoreExceptionClass; const ACode: Integer);
    procedure AssertSQLDriverCommands(const ACommands: array of string);
    procedure AssertSQLDriverTransaction(const ATransactions: array of string);
    procedure ConfigAutoMapping;
    procedure ConfigIPIDContactMapping;
    procedure ConfigIPIDContactModel;
    procedure ConfigProxyCircularModel;
    procedure ConfigProxyContactMapping;
    procedure ConfigProxyContactModel;
    procedure ConfigProxyInvoiceMapping;
    procedure ConfigProxyInvoiceModel;
    procedure CreateConfiguration;
    procedure SetUp; override;
    procedure TearDown; override;
    class property LOG: IJCoreLogger read FLOG;
    property Config: IJCoreOPFConfiguration read FConfig;
    property Driver: TTestSQLDriver read GetDriver;
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

procedure TTestOPFAbstractTestCase.AssertSessionsNull;
begin
  AssertNull(FSessionCircularAuto);
  AssertNull(FSessionInvoiceAuto);
  AssertNull(FSessionInvoiceManual);
  AssertNull(FSessionIPIDContactAuto);
  AssertNull(FSessionIPIDContactManual);
  AssertNull(FSessionProxyContact);
end;

function TTestOPFAbstractTestCase.GetDriver: TTestSQLDriver;
begin
  if not Assigned(FDriver) then
    FDriver := TTestSQLDriver.Create(nil);
  Result := FDriver;
end;

function TTestOPFAbstractTestCase.GetSessionCircularAuto: ITestOPFSession;
begin
  if not Assigned(FSessionCircularAuto) then
  begin
    AssertSessionsNull;
    ConfigAutoMapping;
    ConfigProxyCircularModel;
    FSessionCircularAuto := Config.CreateSession as ITestOPFSession;
  end;
  Result := FSessionCircularAuto;
end;

function TTestOPFAbstractTestCase.GetSessionInvoiceAuto: ITestOPFSession;
begin
  if not Assigned(FSessionInvoiceAuto) then
  begin
    AssertSessionsNull;
    ConfigAutoMapping;
    ConfigProxyInvoiceModel;
    FSessionInvoiceAuto := Config.CreateSession as ITestOPFSession;
  end;
  Result := FSessionInvoiceAuto;
end;

function TTestOPFAbstractTestCase.GetSessionInvoiceManual: ITestOPFSession;
begin
  if not Assigned(FSessionInvoiceManual) then
  begin
    AssertSessionsNull;
    ConfigProxyInvoiceMapping;
    ConfigProxyInvoiceModel;
    FSessionInvoiceManual := Config.CreateSession as ITestOPFSession;
  end;
  Result := FSessionInvoiceManual;
end;

function TTestOPFAbstractTestCase.GetSessionIPIDContactAuto: ITestOPFSession;
begin
  if not Assigned(FSessionIPIDContactAuto) then
  begin
    AssertSessionsNull;
    ConfigAutoMapping;
    ConfigIPIDContactModel;
    FSessionIPIDContactAuto := Config.CreateSession as ITestOPFSession;
  end;
  Result := FSessionIPIDContactAuto;
end;

function TTestOPFAbstractTestCase.GetSessionIPIDContactManual: ITestOPFSession;
begin
  if not Assigned(FSessionIPIDContactManual) then
  begin
    AssertSessionsNull;
    ConfigIPIDContactMapping;
    ConfigIPIDContactModel;
    FSessionIPIDContactManual := Config.CreateSession as ITestOPFSession;
  end;
  Result := FSessionIPIDContactManual;
end;

function TTestOPFAbstractTestCase.GetSessionProxyContact: ITestOPFSession;
begin
  if not Assigned(FSessionProxyContact) then
  begin
    AssertSessionsNull;
    ConfigProxyContactMapping;
    ConfigProxyContactModel;
    FSessionProxyContact := Config.CreateSession as ITestOPFSession;
  end;
  Result := FSessionProxyContact;
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

procedure TTestOPFAbstractTestCase.CreateConfiguration;
begin
  FConfig := TTestOPFConfig.Create;
  TestDefaultModel := FConfig.Model;
  FConfig.DriverClass := TTestSQLDriver;
  FConfig.Model.OIDClass := TJCoreOPFOIDInt64;
  FConfig.Model.OIDGenerator := TTestOPFOIDGenerator.Create;
  FConfig.Model.OrderFieldName := 'SEQ';
end;

procedure TTestOPFAbstractTestCase.SetUp;
begin
  inherited SetUp;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.test.opf');
  AssertNull(FConfig);
  AssertSessionsNull;
  AssertEquals(0, TTestSQLDriver.Commands.Count);
  AssertEquals(0, TTestSQLDriver.Data.Count);
  AssertEquals(0, TTestSQLDriver.Transaction.Count);
  AssertEquals(0, TTestOPFOIDGenerator.CurrentOID);
  CreateConfiguration;
end;

procedure TTestOPFAbstractTestCase.TearDown;
begin
  inherited TearDown;
  TestDefaultModel := nil;
  TTestSQLDriver.Commands.Clear;
  TTestSQLDriver.Data.Clear;
  TTestSQLDriver.ExpectedResultsets.Clear;
  TTestSQLDriver.Transaction.Clear;
  TTestOPFOIDGenerator.ClearOID;
  FreeAndNil(FDriver);
  FSessionCircularAuto := nil;
  FSessionInvoiceAuto := nil;
  FSessionInvoiceManual := nil;
  FSessionIPIDContactAuto := nil;
  FSessionIPIDContactManual := nil;
  FSessionProxyContact := nil;
  FConfig := nil;
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

