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

  TTestLoggerLogger = class(TInterfacedObject, IJCoreLogger)
  private
    class var FCommands: TStringList;
    FLogger: string;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(const ALogger: string);
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warn(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Fatal(const AMsg: string);
    procedure PrintStackTrace;
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
    AssertEquals('otherlogger Info othermsg', TTestLoggerLogger.Commands[0]);
  finally
    TTestLoggerLogger.Commands.Clear;
    AssertTrue(TJCoreDIC.Unregister(IJCoreLogFactory, TTestLogFactory));
  end;
end;

{ TTestLoggerLogger }

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

procedure TTestLoggerLogger.Debug(const AMsg: string);
begin
  Commands.Add(FLogger + ' Debug ' + AMsg);
end;

procedure TTestLoggerLogger.Info(const AMsg: string);
begin
  Commands.Add(FLogger + ' Info ' + AMsg);
end;

procedure TTestLoggerLogger.Warn(const AMsg: string);
begin
  Commands.Add(FLogger + ' Warn ' + AMsg);
end;

procedure TTestLoggerLogger.Error(const AMsg: string);
begin
  Commands.Add(FLogger + ' Error ' + AMsg);
end;

procedure TTestLoggerLogger.Fatal(const AMsg: string);
begin
  Commands.Add(FLogger + ' Fatal ' + AMsg);
end;

procedure TTestLoggerLogger.PrintStackTrace;
begin
  Commands.Add(FLogger + ' Stack');
end;

{ TTestLogFactory }

function TTestLogFactory.GetLogger(const ALogger: string): IJCoreLogger;
begin
  Result := TTestLoggerLogger.Create(ALogger);
end;

initialization
  RegisterTest(TTestLogger);

end.

