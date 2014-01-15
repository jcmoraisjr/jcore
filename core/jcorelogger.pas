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

interface

type
  TJCoreLogLevel = (jllTrace, jllDebug, jllInfo, jllWarn, jllError, jllFatal);

const
  CJCoreLogLevel: array[TJCoreLogLevel] of string = (
   'TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL');

type

  IJCoreLogger = interface(IInterface)
  ['{786C99E6-9AC9-446C-8DE9-4913C910E90F}']
    procedure Trace(const AMsg: string);
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warn(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Fatal(const AMsg: string);
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
    procedure InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string); virtual; abstract;
  public
    procedure Trace(const AMsg: string);
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warn(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Fatal(const AMsg: string);
  end;

implementation

uses
  JCoreDIC;

type

  { TJCoreLazyLogger }

  TJCoreLazyLogger = class(TJCoreAbstractLogger)
  protected
    procedure InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string); override;
  end;

  { TJCoreLazyLogFactory }

  TJCoreLazyLogFactory = class(TInterfacedObject, IJCoreLogFactory)
  public
    function GetLogger(const ALogger: string): IJCoreLogger;
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

procedure TJCoreAbstractLogger.Trace(const AMsg: string);
begin
  InternalLog(jllTrace, AMsg);
end;

procedure TJCoreAbstractLogger.Debug(const AMsg: string);
begin
  InternalLog(jllDebug, AMsg);
end;

procedure TJCoreAbstractLogger.Info(const AMsg: string);
begin
  InternalLog(jllInfo, AMsg);
end;

procedure TJCoreAbstractLogger.Warn(const AMsg: string);
begin
  InternalLog(jllWarn, AMsg);
end;

procedure TJCoreAbstractLogger.Error(const AMsg: string);
begin
  InternalLog(jllError, AMsg);
end;

procedure TJCoreAbstractLogger.Fatal(const AMsg: string);
begin
  InternalLog(jllFatal, AMsg);
end;

{ TJCoreLazyLogger }

procedure TJCoreLazyLogger.InternalLog(const ALevel: TJCoreLogLevel; const AMsg: string);
begin
  writeln(amsg);
end;

{ TJCoreLazyLogFactory }

function TJCoreLazyLogFactory.GetLogger(const ALogger: string): IJCoreLogger;
begin
  Result := TJCoreLazyLogger.Create;
end;

initialization
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreLazyLogFactory);

finalization
  TJCoreDIC.Unregister(IJCoreLogFactory, TJCoreLazyLogFactory);

end.

