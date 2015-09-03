(*
  JCore, OPF MS SQL Server Driver Class
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFMSSQLServer;

{$I jcore.inc}

interface

uses
  JCoreOPFDriver;

type

  { TJCoreOPFMSSQLServerDatabase }

  TJCoreOPFMSSQLServerDatabase = class(TJCoreOPFSQLDatabase)
  public
    function AutoincSQL: string; override;
    class function DatabaseName: string; override;
  end;

implementation

{ TJCoreOPFMSSQLServerDatabase }

function TJCoreOPFMSSQLServerDatabase.AutoincSQL: string;
begin
  Result := 'SELECT scope_identity()'
end;

class function TJCoreOPFMSSQLServerDatabase.DatabaseName: string;
begin
  Result := 'MS SQL Server';
end;

end.

