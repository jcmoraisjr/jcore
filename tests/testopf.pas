unit TestOPF;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fgl,
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
  public
    class procedure ClearOID;
  end;

  { TTestPersonSQLMapping }

  TTestPersonSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; override;
    function ReadFromDriver(const AClass: TClass; const AOID: string): TObject; override;
    procedure WriteToDriver(const APID: IJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestCitySQLMapping }

  TTestCitySQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; override;
    function ReadFromDriver(const AClass: TClass; const AOID: string): TObject; override;
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
    procedure SelectCityManualMapping;
    procedure SelectPersonManualMapping;
    procedure SelectPersonNullCityManualMapping;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreOPFException;

const
  CSQLINSERTCITY = 'INSERT INTO CITY(ID,NAME) VALUES(?,?)';
  CSQLINSERTPERSON = 'INSERT INTO PERSON(ID,NAME,AGE,CITY) VALUES(?,?,?,?)';
  CSQLSELECTCITY = 'SELECT NAME FROM CITY WHERE ID=?';
  CSQLSELECTPERSON = 'SELECT NAME,AGE,CITY FROM PERSON WHERE ID=?';
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
  begin
    Inc(FCurrentOID);
    VOID := FCurrentOID;
  end else
    VOID := StrToInt(AOID);
  Result := TJCoreOPFIntegerOID.Create(VOID);
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

function TTestPersonSQLMapping.ReadFromDriver(const AClass: TClass; const AOID: string): TObject;
var
  VPerson: TTestPerson;
begin
  VPerson := TTestPerson.Create;
  try
    VPerson.Name := Driver.ReadString;
    VPerson.Age := Driver.ReadInteger;
    if not Driver.ReadNull then
      VPerson.City := Mapper.Retrieve(TTestCity, IntToStr(Driver.ReadInteger)) as TTestCity;
    Result := VPerson;
  except
    FreeAndNil(VPerson);
    raise;
  end;
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

function TTestCitySQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTCITY;
end;

function TTestCitySQLMapping.GenerateUpdateStatement(const APID: IJCoreOPFPID): string;
begin
  Result := CSQLUPDATECITY;
end;

function TTestCitySQLMapping.ReadFromDriver(const AClass: TClass;
  const AOID: string): TObject;
var
  VCity: TTestCity;
begin
  VCity := TTestCity.Create;
  try
    VCity.Name := Driver.ReadString;
    Result := VCity;
  except
    FreeAndNil(VCity);
    raise;
  end;
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
  AssertEquals(0, TTestSQLDriver.Data.Count);
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
  TTestSQLDriver.Data.Clear;
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

procedure TTestOPF.SelectCityManualMapping;
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

procedure TTestOPF.SelectPersonManualMapping;
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
    AssertEquals(0, TTestSQLDriver.Data.Count);
    AssertEquals(0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertEquals(4, TTestSQLDriver.Commands.Count);
    AssertEquals('WriteInteger 8', TTestSQLDriver.Commands[0]);
    AssertEquals('ExecSQL ' + CSQLSELECTPERSON, TTestSQLDriver.Commands[1]);
    AssertEquals('WriteInteger 5', TTestSQLDriver.Commands[2]);
    AssertEquals('ExecSQL ' + CSQLSELECTCITY, TTestSQLDriver.Commands[3]);
    AssertNotNull(VPerson);
    AssertNotNull(VPerson._PID);
    AssertEquals(8, VPerson._PID.OID.AsInteger);
    AssertEquals('thepersonname', VPerson.Name);
    AssertEquals(30, VPerson.Age);
    VCity := VPerson.City;
    AssertNotNull(VCity);
    AssertNotNull(VCity._PID);
    AssertEquals(5, VCity._PID.OID.AsInteger);
    AssertEquals('nameofcity', VCity.Name);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPF.SelectPersonNullCityManualMapping;
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
    AssertEquals(2, TTestSQLDriver.Commands.Count);
    AssertEquals('WriteInteger 18', TTestSQLDriver.Commands[0]);
    AssertEquals('ExecSQL ' + CSQLSELECTPERSON, TTestSQLDriver.Commands[1]);
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

initialization
  RegisterTest(TTestOPF);

end.

