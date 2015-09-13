(*
  JCore, OPF MySQL DB Class
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFMySQL;

{$I jcore.inc}

interface

uses
  JCoreOPFDriver;

type

  { TJCoreOPFMySQLDatabase }

  TJCoreOPFMySQLDatabase = class(TJCoreOPFSQLDatabase)
  public
    function AutoincSQL: string; override;
    class function DatabaseName: string; override;
  end;

implementation

{ TJCoreOPFMySQLDatabase }

function TJCoreOPFMySQLDatabase.AutoincSQL: string;
begin
  Result := 'SELECT last_insert_id()';
end;

class function TJCoreOPFMySQLDatabase.DatabaseName: string;
begin
  Result := 'MySQL';
end;

end.

