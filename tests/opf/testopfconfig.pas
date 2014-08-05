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
    procedure Commit; override;
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
    FConfiguration: IJCoreOPFConfiguration;
    FSession: ITestOPFSession;
    class var FLOG: IJCoreLogger;
  protected
    procedure AssertExceptionStore(const ASession: IJCoreOPFSession; const AEntity: TObject; const AException: ExceptClass);
    procedure AssertSQLDriverCommands(const ACommands: array of string);
    function CreateConfiguration: IJCoreOPFConfiguration;
    procedure CustomizeConfiguration(const AConfig: TTestOPFConfig); virtual;
    procedure SetUp; override;
    procedure TearDown; override;
    class property LOG: IJCoreLogger read FLOG;
    property Session: ITestOPFSession read FSession;
  end;

  { TTestOPFIPIDContactTestCase }

  TTestOPFIPIDContactTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure CustomizeConfiguration(const AConfig: TTestOPFConfig); override;
  end;

  { TTestOPFProxyContactTestCase }

  TTestOPFProxyContactTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure CustomizeConfiguration(const AConfig: TTestOPFConfig); override;
  end;

  { TTestOPFProxyInvoiceManualMappingTestCase }

  TTestOPFProxyInvoiceManualMappingTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure CustomizeConfiguration(const AConfig: TTestOPFConfig); override;
  end;

  { TTestOPFProxyInvoiceAutoMappingTestCase }

  TTestOPFProxyInvoiceAutoMappingTestCase = class(TTestOPFAbstractTestCase)
  protected
    procedure CustomizeConfiguration(const AConfig: TTestOPFConfig); override;
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
  protected
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
  Result := AcquireClassMapping(AClass).Metadata;
end;

class function TTestOPFSession.ExistPIDInCommitPIDList(const APID: IJCorePID): Boolean;
begin
  Result := LastCommitPIDList.IndexOf(APID as TJCoreOPFPID) >= 0;
end;

{ TTestOPFAbstractTestCase }

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
var
  I: Integer;
begin
  try
    AssertEquals('cmd count', Length(ACommands), TTestSQLDriver.Commands.Count);
    for I := Low(ACommands) to High(ACommands) do
      AssertEquals('cmd' + IntToStr(I), ACommands[I], TTestSQLDriver.Commands[I]);
  except
    LOG.Warn('Expected commands:');
    for I := Low(ACommands) to High(ACommands) do
      LOG.Warn('  ' + IntToStr(I) + ': ' + ACommands[I]);
    LOG.Warn('Executed commands:');
    for I := 0 to Pred(TTestSQLDriver.Commands.Count) do
      LOG.Warn('  ' + IntToStr(I) + ': ' + TTestSQLDriver.Commands[I]);
    raise;
  end;
  TTestSQLDriver.Commands.Clear;
end;

function TTestOPFAbstractTestCase.CreateConfiguration: IJCoreOPFConfiguration;
var
  VConfig: TTestOPFConfig;
begin
  VConfig := TTestOPFConfig.Create(TJCoreOPFModel.Create);
  try
    CustomizeConfiguration(VConfig);
  except
    FreeAndNil(VConfig);
    raise;
  end;
  Result := VConfig;
end;

procedure TTestOPFAbstractTestCase.CustomizeConfiguration(const AConfig: TTestOPFConfig);
begin
  AConfig.DriverClass := TTestSQLDriver;
  AConfig.Model.OIDClass := TJCoreOPFOIDInt64;
  AConfig.Model.GeneratorStrategy := jgsCustom;
end;

procedure TTestOPFAbstractTestCase.SetUp;
begin
  inherited SetUp;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.test.opf');
  AssertEquals(0, TTestSQLDriver.Commands.Count);
  AssertEquals(0, TTestSQLDriver.Data.Count);
  AssertEquals(0, TTestIntegerGenerator.CurrentOID);
  FConfiguration := CreateConfiguration;
  FSession := FConfiguration.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.TearDown;
begin
  inherited TearDown;
  TTestSQLDriver.Commands.Clear;
  TTestSQLDriver.Data.Clear;
  TTestSQLDriver.ExpectedResultsets.Clear;
  TTestIntegerGenerator.ClearOID;
  FSession := nil;
  FConfiguration := nil;
end;

{ TTestOPFIPIDContactTestCase }

procedure TTestOPFIPIDContactTestCase.CustomizeConfiguration(const AConfig: TTestOPFConfig);
begin
  inherited CustomizeConfiguration(AConfig);
  AConfig.AddMappingClass([
   TTestIPIDSimpleSQLMapping, TTestIPIDPersonSQLMapping, TTestIPIDEmployeeSQLMapping,
   TTestIPIDAddressSQLMapping, TTestIPIDCitySQLMapping, TTestIPIDPhoneSQLMapping, TTestIPIDLanguageSQLMapping]);
  AConfig.Model.AddClass([TTestIPIDPerson, TTestIPIDPhone, TTestIPIDLanguage, TTestIPIDAddress, TTestIPIDCity]);
  AConfig.Model.AcquireMetadata(TTestIPIDPerson).AttributeByName('Languages').CompositionType := jctAggregation;
  AConfig.Model.AcquireMetadata(TTestIPIDPerson).AttributeByName('City').CompositionType := jctAggregation;
end;

{ TTestOPFProxyContactTestCase }

procedure TTestOPFProxyContactTestCase.CustomizeConfiguration(const AConfig: TTestOPFConfig);
begin
  inherited CustomizeConfiguration(AConfig);
  AConfig.AddMappingClass([TTestProxyPhoneSQLMapping, TTestProxyCitySQLMapping, TTestProxyPersonSQLMapping]);
  AConfig.Model.AddClass([TTestProxyPhone, TTestProxyCity, TTestProxyPerson]);
end;

{ TTestOPFProxyInvoiceManualMappingTestCase }

procedure TTestOPFProxyInvoiceManualMappingTestCase.CustomizeConfiguration(const AConfig: TTestOPFConfig);
begin
  inherited CustomizeConfiguration(AConfig);
  AConfig.AddMappingClass([
   TClientSQLMapping, TPersonSQLMapping, TCompanySQLMapping, TProductSQLMapping, TInvoiceSQLMapping,
   TInvoiceItemSQLMapping, TInvoiceItemProductSQLMapping, TInvoiceItemServiceSQLMapping]);
  AConfig.Model.AddClass([
   TClient, TPerson, TCompany, TProduct, TInvoiceItem, TInvoiceItemProduct, TInvoiceItemService, TInvoice]);
  AConfig.Model.AcquireMetadata(TInvoiceItemProduct).AttributeByName('Product').CompositionType := jctAggregation;
end;

{ TTestOPFProxyInvoiceAutoMappingTestCase }

procedure TTestOPFProxyInvoiceAutoMappingTestCase.CustomizeConfiguration(const AConfig: TTestOPFConfig);
begin
  inherited CustomizeConfiguration(AConfig);
  AConfig.AddMappingClass([TTestSQLMapping]);
  AConfig.Model.AddClass([
   TClient, TPerson, TCompany, TProduct, TInvoiceItem, TInvoiceItemProduct, TInvoiceItemService, TInvoice]);
  AConfig.Model.AcquireMetadata(TInvoiceItemProduct).AttributeByName('Product').CompositionType := jctAggregation;
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

