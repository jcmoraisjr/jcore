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

  IJCoreLogger = interface(IInterface)
  ['{786C99E6-9AC9-446C-8DE9-4913C910E90F}']
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warn(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Fatal(const AMsg: string);
    procedure PrintStackTrace;
  end;

  IJCoreLogFactory = interface(IInterface)
  ['{C580B89F-3829-4321-AC79-1A85B3D2B0BB}']
    function CreateLogger(const ALogger: string): IJCoreLogger;
  end;

  { TJCoreLogger }

  TJCoreLogger = class(TObject)
  public
    class function CreateLogger(const ALogger: string): IJCoreLogger;
  end;

implementation

uses
  JCoreDIC;

{ TJCoreLogger }

class function TJCoreLogger.CreateLogger(const ALogger: string): IJCoreLogger;
var
  VLogFactory: IJCoreLogFactory;
begin
  TJCoreDIC.Locate(IJCoreLogFactory, VLogFactory);
  Result := VLogFactory.CreateLogger(ALogger);
end;

type

  { TJCoreLazyLogger }

  TJCoreLazyLogger = class(TInterfacedObject, IJCoreLogger)
  public
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warn(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Fatal(const AMsg: string);
    procedure PrintStackTrace;
  end;

  { TJCoreLazyLogFactory }

  TJCoreLazyLogFactory = class(TInterfacedObject, IJCoreLogFactory)
  public
    function CreateLogger(const ALogger: string): IJCoreLogger;
  end;

{ TJCoreLazyLogger }

procedure TJCoreLazyLogger.Debug(const AMsg: string);
begin
end;

procedure TJCoreLazyLogger.Info(const AMsg: string);
begin
end;

procedure TJCoreLazyLogger.Warn(const AMsg: string);
begin
end;

procedure TJCoreLazyLogger.Error(const AMsg: string);
begin
end;

procedure TJCoreLazyLogger.Fatal(const AMsg: string);
begin
end;

procedure TJCoreLazyLogger.PrintStackTrace;
begin
end;

{ TJCoreLazyLogFactory }

function TJCoreLazyLogFactory.CreateLogger(const ALogger: string): IJCoreLogger;
begin
  Result := TJCoreLazyLogger.Create;
end;

initialization
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreLazyLogFactory);

finalization
  TJCoreDIC.Unregister(IJCoreLogFactory, TJCoreLazyLogFactory);

end.

