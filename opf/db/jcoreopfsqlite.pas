(*
  JCore, OPF SQLite DB Class
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFSQLite;

{$I jcore.inc}

interface

uses
  JCoreOPFDriver;

type

  { TJCoreOPFSQLiteDatabase }

  TJCoreOPFSQLiteDatabase = class(TJCoreOPFSQLDatabase)
  public
    function AutoincSQL: string; override;
    class function DatabaseName: string; override;
  end;

implementation

{ TJCoreOPFSQLiteDatabase }

function TJCoreOPFSQLiteDatabase.AutoincSQL: string;
begin
  Result := 'SELECT last_insert_rowid()';
end;

class function TJCoreOPFSQLiteDatabase.DatabaseName: string;
begin
  Result := 'SQLite';
end;

end.

