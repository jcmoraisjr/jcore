unit TestOPFConfig;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  Classes,
  fgl,
  fpcunit,
  JCoreLogger,
  JCoreEntity,
  JCoreOPFDriver,
  JCoreOPFGenerator,
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

  { TTestOPFAbstractTestCase }

  TTestOPFAbstractTestCase = class(TTestCase)
  private
    FSessionCircularAuto: ITestOPFSession;
    FSessionInvoiceAuto: ITestOPFSession;
    FSessionInvoiceManual: ITestOPFSession;
    FSessionIPIDContactAuto: ITestOPFSession;
    FSessionIPIDContactManual: ITestOPFSession;
    FSessionProxyContact: ITestOPFSession;
    class var FLOG: IJCoreLogger;
    procedure AssertCommands(const AList: TStringList; const AShortName, ALongName: string; const ACommands: array of string);
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

  { TTestIntegerGenerator }

  TTestIntegerGenerator = class(TInterfacedObject, IJCoreOPFGenerator)
  private
    class var FCurrentOID: Integer;
  public
    procedure GenerateOIDs(const AOIDCount: Integer);
    function ReadInt64: Int64;
    function ReadString: string;
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
    function CreateCustomGenerator: IJCoreOPFGenerator; override;
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

  TTestSQLMapping = class(TJCoreOPFSQLMapping)
  protected
    function CreateCustomGenerator: IJCoreOPFGenerator; override;
  end;

implementation

uses
  JCoreMetadata,
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
  AConfig.Model.AddClass([TTestIPIDPerson, TTestIPIDPhone, TTestIPIDLanguage, TTestIPIDAddress, TTestIPIDCity]);
  AConfig.Model.AcquireAttrMetadata(TTestIPIDPerson, 'Languages').CompositionType := jctAggregation;
  AConfig.Model.AcquireAttrMetadata(TTestIPIDPerson, 'City').CompositionType := jctAggregation;
end;

procedure TTestOPFAbstractTestCase.ConfigProxyCircularModel(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.Model.AddClass([TCircularPerson]);
  AConfig.Model.AcquireAttrMetadata(TCircularPerson, 'Dependent').CompositionType := jctAggregation;
end;

procedure TTestOPFAbstractTestCase.ConfigProxyContactMapping(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.AddMappingClass([TTestProxyPhoneSQLMapping, TTestProxyCitySQLMapping, TTestProxyPersonSQLMapping]);
end;

procedure TTestOPFAbstractTestCase.ConfigProxyContactModel(const AConfig: IJCoreOPFConfiguration);
begin
  AConfig.Model.AddClass([TTestProxyPhone, TTestProxyCity, TTestProxyPerson]);
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
  AConfig.Model.AcquireAttrMetadata(TInvoice, 'Client').CompositionType := jctAggregation;
  AConfig.Model.AcquireAttrMetadata(TInvoiceItemProduct, 'Product').CompositionType := jctAggregation;
end;

function TTestOPFAbstractTestCase.CreateConfiguration: IJCoreOPFConfiguration;
begin
  Result := TTestOPFConfig.Create;
  Result.DriverClass := TTestSQLDriver;
  Result.Model.OIDClass := TJCoreOPFOIDInt64;
  Result.Model.GeneratorStrategy := jgsCustom;
end;

procedure TTestOPFAbstractTestCase.SetUp;
begin
  inherited SetUp;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.test.opf');
  AssertEquals(0, TTestSQLDriver.Commands.Count);
  AssertEquals(0, TTestSQLDriver.Data.Count);
  AssertEquals(0, TTestSQLDriver.Transaction.Count);
  AssertEquals(0, TTestIntegerGenerator.CurrentOID);
end;

procedure TTestOPFAbstractTestCase.TearDown;
begin
  inherited TearDown;
  TTestSQLDriver.Commands.Clear;
  TTestSQLDriver.Data.Clear;
  TTestSQLDriver.ExpectedResultsets.Clear;
  TTestSQLDriver.Transaction.Clear;
  TTestIntegerGenerator.ClearOID;
  FSessionCircularAuto := nil;
  FSessionInvoiceAuto := nil;
  FSessionInvoiceManual := nil;
  FSessionIPIDContactAuto := nil;
  FSessionIPIDContactManual := nil;
  FSessionProxyContact := nil;
end;

{ TTestIntegerGenerator }

procedure TTestIntegerGenerator.GenerateOIDs(const AOIDCount: Integer);
begin
end;

function TTestIntegerGenerator.ReadInt64: Int64;
begin
  Inc(FCurrentOID);
  Result := FCurrentOID;
end;

function TTestIntegerGenerator.ReadString: string;
begin
  Inc(FCurrentOID);
  Result := IntToStr(FCurrentOID);
end;

class procedure TTestIntegerGenerator.ClearOID;
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

function TTestEmptyMapping.CreateCustomGenerator: IJCoreOPFGenerator;
begin
  Result := TTestIntegerGenerator.Create;
end;

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

{ TTestSQLMapping }

function TTestSQLMapping.CreateCustomGenerator: IJCoreOPFGenerator;
begin
  Result := TTestIntegerGenerator.Create;
end;

end.

