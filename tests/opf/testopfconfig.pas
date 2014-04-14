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
    function CreateConfiguration(const ADriverClassArray: array of TJCoreOPFDriverClass; const AMappingClassArray: array of TJCoreOPFMappingClass): IJCoreOPFConfiguration;
    function InternalCreateModel: TJCoreOPFModel; virtual; abstract;
    function InternalMappingClassArray: TTestOPFMappingClassArray; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
    class property LOG: IJCoreLogger read FLOG;
    property Session: ITestOPFSession read FSession;
  end;

  { TTestOPFIPIDTestCase }

  TTestOPFIPIDTestCase = class(TTestOPFAbstractTestCase)
  protected
    function InternalCreateModel: TJCoreOPFModel; override;
    function InternalMappingClassArray: TTestOPFMappingClassArray; override;
  end;

  { TTestOPFProxyTestCase }

  TTestOPFProxyTestCase = class(TTestOPFAbstractTestCase)
  protected
    function InternalCreateModel: TJCoreOPFModel; override;
    function InternalMappingClassArray: TTestOPFMappingClassArray; override;
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

implementation

uses
  TestOPFModelRegistry,
  TestOPFMappingManual;

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
    LOG.Warn('Executed commands:');
    for I := 0 to Pred(TTestSQLDriver.Commands.Count) do
      LOG.Warn('  ' + IntToStr(I) + ': ' + TTestSQLDriver.Commands[I]);
    raise;
  end;
  TTestSQLDriver.Commands.Clear;
end;

function TTestOPFAbstractTestCase.CreateConfiguration(const ADriverClassArray: array of TJCoreOPFDriverClass;
  const AMappingClassArray: array of TJCoreOPFMappingClass): IJCoreOPFConfiguration;
var
  VDriverClass: TJCoreOPFDriverClass;
  VMappingClass: TJCoreOPFMappingClass;
begin
  Result := TTestOPFConfig.Create(InternalCreateModel);
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

procedure TTestOPFAbstractTestCase.SetUp;
begin
  inherited SetUp;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.test.opf');
  AssertEquals(0, TTestSQLDriver.Commands.Count);
  AssertEquals(0, TTestSQLDriver.Data.Count);
  FConfiguration := CreateConfiguration([TTestSQLDriver], InternalMappingClassArray);
  FSession := FConfiguration.CreateSession as ITestOPFSession;
end;

procedure TTestOPFAbstractTestCase.TearDown;
begin
  inherited TearDown;
  FSession := nil;
  FConfiguration := nil;
  TTestAbstractSQLMapping.ClearOID;
  TTestSQLDriver.Commands.Clear;
  TTestSQLDriver.Data.Clear;
end;

{ TTestOPFIPIDTestCase }

function TTestOPFIPIDTestCase.InternalCreateModel: TJCoreOPFModel;
begin
  Result := TTestOPFModelIPIDContact.Create;
end;

function TTestOPFIPIDTestCase.InternalMappingClassArray: TTestOPFMappingClassArray;
begin
  SetLength(Result, 7);
  Result[0] := TTestIPIDSimpleSQLMapping;
  Result[1] := TTestIPIDPersonSQLMapping;
  Result[2] := TTestIPIDEmployeeSQLMapping;
  Result[3] := TTestIPIDAddressSQLMapping;
  Result[4] := TTestIPIDCitySQLMapping;
  Result[5] := TTestIPIDPhoneSQLMapping;
  Result[6] := TTestIPIDLanguageSQLMapping;
end;

{ TTestOPFProxyTestCase }

function TTestOPFProxyTestCase.InternalCreateModel: TJCoreOPFModel;
begin
  Result := TTestOPFModelProxyContact.Create;
end;

function TTestOPFProxyTestCase.InternalMappingClassArray: TTestOPFMappingClassArray;
begin
  SetLength(Result, 3);
  Result[0] := TTestProxyPhoneSQLMapping;
  Result[1] := TTestProxyCitySQLMapping;
  Result[2] := TTestProxyPersonSQLMapping;
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

end.

