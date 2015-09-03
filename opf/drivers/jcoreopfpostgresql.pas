(*
  JCore, OPF PostgreSQL Driver Class
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFPostgreSQL;

{$I jcore.inc}

interface

uses
  JCoreOPFDriver;

type

  { TJCoreOPFPostgreSQLDatabase }

  TJCoreOPFPostgreSQLDatabase = class(TJCoreOPFSQLDatabase)
  public
    class function DatabaseName: string; override;
    function SequenceSQL(const ASequenceName: string; const AOIDCount: Integer): string; override;
  end;

implementation

uses
  sysutils;

{ TJCoreOPFPostgreSQLDatabase }

class function TJCoreOPFPostgreSQLDatabase.DatabaseName: string;
begin
  Result := 'PostgreSQL';
end;

function TJCoreOPFPostgreSQLDatabase.SequenceSQL(const ASequenceName: string;
  const AOIDCount: Integer): string;
begin
  if AOIDCount = 1 then
    Result := Format('SELECT nextval(''%s'')', [ASequenceName])
  else
    Result := Format('SELECT nextval(''%s'') FROM generate_series(1,%d)', [ASequenceName, AOIDCount]);
end;

end.

