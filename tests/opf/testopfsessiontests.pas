unit TestOPFSessionTests;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFSession,
  JCoreOPFConfig,
  TestOPFConfig,
  TestOPFModel;

type

  { TTestOPFSessionTests }

  TTestOPFSessionTests = class(TTestOPF)
  published
    procedure DriverNotFound;
    procedure MappingNotFound;
  end;

implementation

uses
  sysutils,
  fpcunit,
  testregistry,
  JCoreOPFException;

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

initialization
  RegisterTest('jcore.opf.session', TTestOPFSessionTests);

end.

