unit TestOPFConfig;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  Classes,
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

  TTestSQLDriver = class;

  { TTestOPFAbstractTestCase }

  TTestOPFAbstractTestCase = class(TTestCase)
  private
    FDriver: TTestSQLDriver;
    FSessionCircularAuto: ITestOPFSession;
    FSessionInvoiceAuto: ITestOPFSession;
    FSessionInvoiceManual: ITestOPFSession;
    FSessionIPIDContactAuto: ITestOPFSession;
    FSessionIPIDContactManual: ITestOPFSession;
    FSessionProxyContact: ITestOPFSession;
    class var FLOG: IJCoreLogger;
    procedure AssertCommands(const AList: TStringList; const AShortName, ALongName: string; const ACommands: array of string);
    function GetDriver: TTestSQLDriver;
    function GetSessionCircularAuto: ITestOPFSession;
    function GetSessionInvoiceAuto: ITestOPFSession;
    function GetSessionInvoiceManual: ITestOPFSession;
    function GetSessionIPIDContactAuto: ITestOPFSession;
    function GetSessionIPIDContactManual: ITestOPFSession;
    function GetSessionProxyContact: ITestOPFSession;
  protected
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
    property Driver: TTestSQLDriver read GetDriver;
    property SessionCircularAuto: ITestOPFSession read GetSessionCircularAuto;
    property SessionInvoiceAuto: ITestOPFSession read GetSessionInvoiceAuto;
    property SessionInvoiceManual: ITestOPFSession read GetSessionInvoiceManual;
    property SessionIPIDContactAuto: ITestOPFSession read GetSessionIPIDContactAuto;
    property SessionIPIDContactManual: ITestOPFSession read GetSessionIPIDContactManual;
    property SessionProxyContact: ITestOPFSession read GetSessionProxyContact;
  end;

  { TTestOPFSimpleTestCase }

  TTestOPFSimpleTestCase = class(TTestOPFAbstractTestCase)
  private
    FConfig: IJCoreOPFConfiguration;
    FSession: ITestOPFSession;
    function GetConfig: IJCoreOPFConfiguration;
    function GetSession: ITestOPFSession;
  protected
    property Config: IJCoreOPFConfiguration read GetConfig;
    property Session: ITestOPFSession read GetSession;
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
    function InternalCreateQuery(const ASQL: string; const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery; override;
  public
    class constructor Create;
    class destructor Destroy;
    class function DriverName: string; override;
    function CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator; override;
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

function TTestOPFAbstractTestCase.GetDriver: TTestSQLDriver;
begin
  if not Assigned(FDriver) then
    FDriver := TTestSQLDriver.Create(nil);
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
  AConfig.Model.AddClass([
   TTestIPIDPerson, TTestIPIDPhone, TTestIPIDLanguage, TTestIPIDAddress, TTestIPIDCity]);
  AConfig.Model.AddGenerics(TTestIPIDPhoneList, TTestIPIDPhone);
  AConfig.Model.AddGenerics(TTestIPIDLanguageList, TTestIPIDLanguage);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyCircularModel(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.Model.AddClass([TCircularPerson]);
  AConfig.Model.AddGenerics(TCircularPersonList, TCircularPerson);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyContactMapping(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.AddMappingClass([
   TTestProxyPhoneSQLMapping, TTestProxyCitySQLMapping, TTestProxyPersonSQLMapping]);
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
   TAddress, TClient, TPerson, TCompany, TProduct, TInvoiceItem, TInvoiceItemProduct, TInvoiceItemService,
   TInvoice]);
  AConfig.Model.AddGenerics(TInvoiceItemList, TInvoiceItem);
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

{ TTestOPFSimpleTestCase }

function TTestOPFSimpleTestCase.GetConfig: IJCoreOPFConfiguration;
begin
  if not Assigned(FConfig) then
  begin
    FConfig := TTestOPFConfig.Create;
    FConfig.DriverClass := TTestSQLDriver;
    FConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  end;
  Result := FConfig;
end;

function TTestOPFSimpleTestCase.GetSession: ITestOPFSession;
begin
  if not Assigned(FSession) then
    FSession := Config.CreateSession as ITestOPFSession;
  Result := FSession;
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

{ TTestSQLDriver }

procedure TTestSQLDriver.InternalCommit;
begin
  inherited InternalCommit;
  Transaction.Add('commit');
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

function TTestSQLDriver.CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator;
begin
  Result := TTestOPFGeneratorSQLDriver.Create(Self, AGeneratorName);
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

