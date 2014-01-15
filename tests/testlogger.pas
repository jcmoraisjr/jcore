unit TestLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpcunit,
  JCoreLogger;

type

  { TTestLogger }

  TTestLogger = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure UseRegisteredLogFactory;
  end;

  { TTestLoggerLogger }

  TTestLoggerLogger = class(TJCoreAbstractLogger)
  private
    class var FCommands: TStringList;
    FLogger: string;
  protected
    procedure InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string); override;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(const ALogger: string);
    class property Commands: TStringList read FCommands;
  end;

  { TTestLogFactory }

  TTestLogFactory = class(TInterfacedObject, IJCoreLogFactory)
  public
    function GetLogger(const ALogger: string): IJCoreLogger;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreDIC;

procedure TTestLogger.SetUp;
begin
end;

procedure TTestLogger.TearDown;
begin
end;

procedure TTestLogger.UseRegisteredLogFactory;
var
  VLogFactory: IJCoreLogFactory;
  VLogger: IJCoreLogger;
begin
  try
    TTestLoggerLogger.Commands.Clear;
    TJCoreDIC.Locate(IJCoreLogFactory, VLogFactory);
    VLogger := VLogFactory.GetLogger('thelogger');
    VLogger.Debug('themsg');
    AssertEquals(0, TTestLoggerLogger.Commands.Count);
    TJCoreDIC.Register(IJCoreLogFactory, TTestLogFactory);
    TJCoreDIC.Locate(IJCoreLogFactory, VLogFactory);
    VLogger := VLogFactory.GetLogger('otherlogger');
    VLogger.Info('othermsg');
    AssertEquals(1, TTestLoggerLogger.Commands.Count);
    AssertEquals('INFO otherlogger othermsg', TTestLoggerLogger.Commands[0]);
  finally
    TTestLoggerLogger.Commands.Clear;
    AssertTrue(TJCoreDIC.Unregister(IJCoreLogFactory, TTestLogFactory));
  end;
end;

{ TTestLoggerLogger }

procedure TTestLoggerLogger.InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string);
begin
  Commands.Add(CJCoreLogLevel[ALevel] + ' ' + FLogger + ' ' + AMsg);
end;

class constructor TTestLoggerLogger.Create;
begin
  FCommands := TStringList.Create;
end;

class destructor TTestLoggerLogger.Destroy;
begin
  FreeAndNil(FCommands);
end;

constructor TTestLoggerLogger.Create(const ALogger: string);
begin
  inherited Create;
  FLogger := ALogger;
end;

{ TTestLogFactory }

function TTestLogFactory.GetLogger(const ALogger: string): IJCoreLogger;
begin
  Result := TTestLoggerLogger.Create(ALogger);
end;

initialization
  RegisterTest(TTestLogger);

end.

