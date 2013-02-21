(*
  JCore, Configuration Facade Class
  Copyright (C) 2013 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreConfig;

{$I jcore.inc}

interface

uses
  JCoreLogger;

type

  { TJCoreConfig }

  TJCoreConfig = class(TObject)
  private
    class var FLogFactory: IJCoreLogFactory;
    class procedure EnsureLogFactory;
    class function GetLogFactory: IJCoreLogFactory; static;
  public
    class property LogFactory: IJCoreLogFactory read GetLogFactory;
  end;

implementation

uses
  JCoreDIC;

{ TJCoreConfig }

class procedure TJCoreConfig.EnsureLogFactory;
begin
  if not Assigned(FLogFactory) then
    TJCoreDIC.Locate(IJCoreLogFactory, FLogFactory);
end;

class function TJCoreConfig.GetLogFactory: IJCoreLogFactory; static;
begin
  EnsureLogFactory;
  Result := FLogFactory;
end;

end.

