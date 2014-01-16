unit TestLogger;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
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
    procedure AbstractLogFactory;
    procedure CreateLogException;
  end;

  { TTestLoggerLogger }

  TTestLoggerLogger = class(TJCoreAbstractLogger)
  private
    class var FCommands: TStringList;
    FLogger: string;
  protected
    procedure InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string; const AException: Exception); override;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(const ALogger: string);
    class property Commands: TStringList read FCommands;
  end;

  { TTestLogFactory }

  TTestLogFactory = class(TJCoreAbstractLogFactory)
  private
    class var FLoggerCount: Integer;
  protected
    function InternalCreateLogger(const ALogger: string): IJCoreLogger; override;
  public
    class property LoggerCount: Integer read FLoggerCount write FLoggerCount;
  end;

implementation

uses
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

procedure TTestLogger.CreateLogException;
var
  VLogger: IJCoreLogger;
begin
  TJCoreDIC.Register(IJCoreLogFactory, TTestLogFactory);
  try
    TTestLoggerLogger.Commands.Clear;
    VLogger := TJCoreLogger.GetLogger('logger.exception');
    try
      Abort;
    except
      on E: Exception do
      begin
        VLogger.Error('oops', E);
      end;
    end;
    AssertEquals(1, TTestLoggerLogger.Commands.Count);
    AssertEquals('ERROR logger.exception oops EAbort', TTestLoggerLogger.Commands[0]);
  finally
    AssertTrue(TJCoreDIC.Unregister(IJCoreLogFactory, TTestLogFactory));
  end;
end;

procedure TTestLogger.AbstractLogFactory;
var
  VLogger1, VLogger2, VLogger3: IJCoreLogger;
begin
  try
    TTestLogFactory.LoggerCount := 0;
    TTestLoggerLogger.Commands.Clear;
    TJCoreDIC.Register(IJCoreLogFactory, TTestLogFactory);

    VLogger1 := TJCoreLogger.GetLogger('logger1');
    AssertEquals(1, TTestLogFactory.LoggerCount);
    VLogger1.Info('msg');
    AssertEquals(1, TTestLoggerLogger.Commands.Count);

    VLogger2 := TJCoreLogger.GetLogger('logger2');
    AssertEquals(2, TTestLogFactory.LoggerCount);
    VLogger2.Info('other msg');
    AssertEquals(2, TTestLoggerLogger.Commands.Count);
    AssertNotSame(VLogger1, VLogger2);

    VLogger3 := TJCoreLogger.GetLogger('logger1');
    AssertEquals(2, TTestLogFactory.LoggerCount);
    AssertSame(VLogger1, VLogger3);
  finally
    TTestLoggerLogger.Commands.Clear;
    AssertTrue(TJCoreDIC.Unregister(IJCoreLogFactory, TTestLogFactory));
  end;
end;

{ TTestLoggerLogger }

procedure TTestLoggerLogger.InternalLog(const ALevel: TJCoreLogLevel;
  const AMsg: string; const AException: Exception);
var
  VMsg: string;
begin
  VMsg := CJCoreLogLevel[ALevel] + ' ' + FLogger + ' ' + AMsg;
  if Assigned(AException) then
    VMsg := VMsg + ' ' + AException.ClassName;
  Commands.Add(VMsg);
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

function TTestLogFactory.InternalCreateLogger(const ALogger: string): IJCoreLogger;
begin
  Result := TTestLoggerLogger.Create(ALogger);
  Inc(FLoggerCount);
end;

initialization
  RegisterTest(TTestLogger);

end.

