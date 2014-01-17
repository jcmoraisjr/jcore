unit TestOPF;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpcunit,
  JCoreLogger,
  JCoreOPFPID,
  JCoreOPFOID,
  JCoreOPFDriver,
  JCoreOPFMapping,
  JCoreOPFSession,
  JCoreOPFConfig;

type

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

  { TTestPerson }

  TTestPerson = class(TTestBase)
  private
    FName: string;
    FAge: Integer;
    FCity: TTestCity;
  public
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property City: TTestCity read FCity write FCity;
  end;

  { TTestSQLDriver }

  TTestSQLDriver = class(TJCoreOPFSQLDriver)
  private
    class var FCommands: TStringList;
  protected
    function InternalExecSQL(const ASQL: string): Integer; override;
  public
    class constructor Create;
    class destructor Destroy;
    class function DriverName: string; override;
    function ReadInteger: Integer; override;
    function ReadString: string; override;
    procedure WriteInteger(const AValue: Integer); override;
    procedure WriteString(const AValue: string); override;
    procedure WriteNull; override;
    class property Commands: TStringList read FCommands;
  end;

  { TTestAbstractSQLMapping }

  TTestAbstractSQLMapping = class(TJCoreOPFSQLMapping)
  private
    class var FCurrentOID: Integer;
  protected
    function CreateOID(const APID: IJCoreOPFPID): TJCoreOPFOID; override;
  public
    class procedure ClearOID;
  end;

  { TTestPersonSQLMapping }

  TTestPersonSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; override;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; override;
    procedure WriteToDriver(const APID: IJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestCitySQLMapping }

  TTestCitySQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; override;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; override;
    procedure WriteToDriver(const APID: IJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

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
  published
    procedure CreatePID;
    procedure DriverNotFound;
    procedure MappingNotFound;
    procedure StoreInsertPersonManualMapping;
    procedure StoreInsertPersonCityManualMapping;
    procedure StoreUpdateCityManualMapping;
    procedure StoreUpdatePersonManualMapping;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreOPFException;

const
  CSQLINSERTCITY = 'INSERT INTO CITY(ID,NAME) VALUES(?,?)';
  CSQLINSERTPERSON = 'INSERT INTO PERSON(ID,NAME,AGE,CITY) VALUES(?,?,?,?)';
  CSQLSELECTCITY = 'SELECT ID,NAME FROM CITY WHERE ID=?';
  CSQLSELECTPERSON = 'SELECT ID,NAME,AGE,CITY FROM PERSON WHERE ID=?';
  CSQLUPDATECITY = 'UPDATE CITY SET NAME=? WHERE ID=?';
  CSQLUPDATEPERSON = 'UPDATE PERSON SET AGE=? WHERE ID=?';

{ TTestPerson }

destructor TTestPerson.Destroy;
begin
  FreeAndNil(FCity);
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

function TTestSQLDriver.InternalExecSQL(const ASQL: string): Integer;
begin
  FCommands.Add('ExecSQL ' + ASQL);
  Result := -1;
end;

class constructor TTestSQLDriver.Create;
begin
  FCommands := TStringList.Create;
end;

class destructor TTestSQLDriver.Destroy;
begin
  FreeAndNil(FCommands);
end;

class function TTestSQLDriver.DriverName: string;
begin
  Result := 'TestSQLDriver';
end;

function TTestSQLDriver.ReadInteger: Integer;
begin
  Result := 1;
  FCommands.Add('ReadInteger ' + IntToStr(Result));
end;

function TTestSQLDriver.ReadString: string;
begin
  Result := 'Test';
  FCommands.Add('ReadString ' + Result);
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

function TTestAbstractSQLMapping.CreateOID(const APID: IJCoreOPFPID
  ): TJCoreOPFOID;
begin
  Inc(FCurrentOID);
  Result := TJCoreOPFIntegerOID.Create(FCurrentOID);
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

function TTestPersonSQLMapping.GenerateUpdateStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLUPDATEPERSON;
end;

procedure TTestPersonSQLMapping.WriteToDriver(const APID: IJCoreOPFPID);
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
  Result := TTestPerson.InheritsFrom(AClass);
end;

{ TTestCitySQLMapping }

function TTestCitySQLMapping.GenerateInsertStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLINSERTCITY;
end;

function TTestCitySQLMapping.GenerateUpdateStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLUPDATECITY;
end;

procedure TTestCitySQLMapping.WriteToDriver(const APID: IJCoreOPFPID);
var
  VCity: TTestCity;
begin
  VCity := APID.Entity as TTestCity;
  Driver.WriteString(VCity.Name);
end;

class function TTestCitySQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := TTestCity.InheritsFrom(AClass);
end;

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
  FConfiguration := CreateConfiguration([TTestSQLDriver], [
   TTestPersonSQLMapping, TTestCitySQLMapping]);
  FSession := FConfiguration.CreateSession;
end;

procedure TTestOPF.TearDown;
begin
  inherited TearDown;
  FSession := nil;
  TTestAbstractSQLMapping.ClearOID;
  TTestSQLDriver.Commands.Clear;
end;

procedure TTestOPF.CreatePID;
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

procedure TTestOPF.DriverNotFound;
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

procedure TTestOPF.MappingNotFound;
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

procedure TTestOPF.StoreInsertPersonManualMapping;
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

procedure TTestOPF.StoreInsertPersonCityManualMapping;
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
    AssertEquals(8, TTestSQLDriver.Commands.Count);
    AssertEquals('WriteInteger 1', TTestSQLDriver.Commands[0]);
    AssertEquals('WriteString SomeName', TTestSQLDriver.Commands[1]);
    AssertEquals('WriteInteger 25', TTestSQLDriver.Commands[2]);
    AssertEquals('WriteInteger 2', TTestSQLDriver.Commands[3]);
    AssertEquals('WriteString CityName', TTestSQLDriver.Commands[4]);
    AssertEquals('ExecSQL ' + CSQLINSERTCITY, TTestSQLDriver.Commands[5]);
    AssertEquals('WriteInteger 2', TTestSQLDriver.Commands[6]);
    AssertEquals('ExecSQL ' + CSQLINSERTPERSON, TTestSQLDriver.Commands[7]);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPF.StoreUpdateCityManualMapping;
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

procedure TTestOPF.StoreUpdatePersonManualMapping;
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

initialization
  RegisterTest(TTestOPF);

end.

