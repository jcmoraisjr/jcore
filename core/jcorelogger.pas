(*
  JCore, Logger Interface
  Copyright (C) 2013 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreLogger;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  sysutils,
  fgl;

type
  TJCoreLogLevel = (jllTrace, jllDebug, jllInfo, jllWarn, jllError, jllFatal);

const
  CJCoreLogLevel: array[TJCoreLogLevel] of string = (
   'TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL');

type

  IJCoreLogger = interface(IInterface)
  ['{786C99E6-9AC9-446C-8DE9-4913C910E90F}']
    procedure Trace(const AMsg: string; const AException: Exception = nil);
    procedure Debug(const AMsg: string; const AException: Exception = nil);
    procedure Info(const AMsg: string; const AException: Exception = nil);
    procedure Warn(const AMsg: string; const AException: Exception = nil);
    procedure Error(const AMsg: string; const AException: Exception = nil);
    procedure Fatal(const AMsg: string; const AException: Exception = nil);
  end;

  IJCoreLogFactory = interface(IInterface)
  ['{C580B89F-3829-4321-AC79-1A85B3D2B0BB}']
    function GetLogger(const ALogger: string): IJCoreLogger;
  end;

  { TJCoreLogger }

  TJCoreLogger = class(TObject)
  public
    class function GetLogger(const ALogger: string): IJCoreLogger;
  end;

  { TJCoreAbstractLogger }

  TJCoreAbstractLogger = class(TInterfacedObject, IJCoreLogger)
  protected
    procedure InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string; const AException: Exception); virtual; abstract;
  public
    procedure Trace(const AMsg: string; const AException: Exception = nil);
    procedure Debug(const AMsg: string; const AException: Exception = nil);
    procedure Info(const AMsg: string; const AException: Exception = nil);
    procedure Warn(const AMsg: string; const AException: Exception = nil);
    procedure Error(const AMsg: string; const AException: Exception = nil);
    procedure Fatal(const AMsg: string; const AException: Exception = nil);
  end;

  TJCoreLoggerMap = specialize TFPGMap<string, IJCoreLogger>;

  { TJCoreAbstractLogFactory }

  TJCoreAbstractLogFactory = class(TInterfacedObject, IJCoreLogFactory)
  private
    FLoggerMap: TJCoreLoggerMap;
  protected
    function InternalCreateLogger(const ALogger: string): IJCoreLogger; virtual; abstract;
  public
    destructor Destroy; override;
    function GetLogger(const ALogger: string): IJCoreLogger;
  end;

  { TJCoreConsoleLogFactory }

  TJCoreConsoleLogFactory = class(TJCoreAbstractLogFactory)
  protected
    function InternalCreateLogger(const ALogger: string): IJCoreLogger; override;
  end;

implementation

uses
  JCoreConsts,
  JCoreUtils,
  JCoreDIC;

type

  { TJCoreConsoleLogger }

  TJCoreConsoleLogger = class(TJCoreAbstractLogger)
  protected
    procedure InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string; const AException: Exception); override;
  end;

  { TJCoreLazyLogger }

  TJCoreLazyLogger = class(TJCoreAbstractLogger)
  protected
    procedure InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string; const AException: Exception); override;
  end;

  { TJCoreLazyLogFactory }

  TJCoreLazyLogFactory = class(TInterfacedObject, IJCoreLogFactory)
  public
    function GetLogger(const ALogger: string): IJCoreLogger;
  end;

{ TJCoreAbstractLogFactory }

destructor TJCoreAbstractLogFactory.Destroy;
begin
  FreeAndNil(FLoggerMap);
  inherited Destroy;
end;

function TJCoreAbstractLogFactory.GetLogger(const ALogger: string): IJCoreLogger;
var
  VIndex: Integer;
begin
  if not Assigned(FLoggerMap) then
  begin
    FLoggerMap := TJCoreLoggerMap.Create;
    FLoggerMap.Sorted := True;
  end;
  if FLoggerMap.Find(ALogger, VIndex) then
    Result := FLoggerMap.Data[VIndex]
  else
  begin
    { TODO : Thread safe }
    VIndex := FLoggerMap.Add(ALogger);
    Result := InternalCreateLogger(ALogger);
    FLoggerMap.Data[VIndex] := Result;
  end;
end;

{ TJCoreLogger }

class function TJCoreLogger.GetLogger(const ALogger: string): IJCoreLogger;
var
  VLogFactory: IJCoreLogFactory;
begin
  TJCoreDIC.Locate(IJCoreLogFactory, VLogFactory);
  Result := VLogFactory.GetLogger(ALogger);
end;

{ TJCoreAbstractLogger }

procedure TJCoreAbstractLogger.Trace(const AMsg: string; const AException: Exception = nil);
begin
  InternalLog(jllTrace, AMsg, AException);
end;

procedure TJCoreAbstractLogger.Debug(const AMsg: string; const AException: Exception = nil);
begin
  InternalLog(jllDebug, AMsg, AException);
end;

procedure TJCoreAbstractLogger.Info(const AMsg: string; const AException: Exception = nil);
begin
  InternalLog(jllInfo, AMsg, AException);
end;

procedure TJCoreAbstractLogger.Warn(const AMsg: string; const AException: Exception = nil);
begin
  InternalLog(jllWarn, AMsg, AException);
end;

procedure TJCoreAbstractLogger.Error(const AMsg: string; const AException: Exception = nil);
begin
  InternalLog(jllError, AMsg, AException);
end;

procedure TJCoreAbstractLogger.Fatal(const AMsg: string; const AException: Exception = nil);
begin
  InternalLog(jllFatal, AMsg, AException);
end;

{ TJCoreConsoleLogFactory }

function TJCoreConsoleLogFactory.InternalCreateLogger(const ALogger: string): IJCoreLogger;
begin
  Result := TJCoreConsoleLogger.Create;
end;

{ TJCoreLoggerConsole }

procedure TJCoreConsoleLogger.InternalLog(const ALevel: TJCoreLogLevel;
  const AMsg: string; const AException: Exception);
begin
  writeln(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) +
   ' | ' + CJCoreLogLevel[ALevel] + ' | ' + AMsg);
  if Assigned(AException) then
  begin
    writeln(Format(SJCoreExceptionRaisedWithMessage, [
     AException.ClassName, AException.Message]));
    writeln(JCoreDumpExceptionStackTrace);
  end;
end;

{ TJCoreLazyLogger }

procedure TJCoreLazyLogger.InternalLog(const ALevel: TJCoreLogLevel;
  const AMsg: string; const AException: Exception);
begin
end;

{ TJCoreLazyLogFactory }

function TJCoreLazyLogFactory.GetLogger(const ALogger: string): IJCoreLogger;
begin
  Result := TJCoreLazyLogger.Create;
end;

initialization
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreLazyLogFactory, jdsApplication);

finalization
  TJCoreDIC.Unregister(IJCoreLogFactory, TJCoreLazyLogFactory);

end.

