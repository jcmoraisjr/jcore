(*
  JCore, OPF Interbase/Firebird DB Class
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFFirebird;

{$I jcore.inc}

interface

uses
  JCoreOPFDriver;

type

  { TJCoreOPFFirebirdDatabase }

  TJCoreOPFFirebirdDatabase = class(TJCoreOPFSQLDatabase)
  public
    class function DatabaseName: string; override;
    function SequenceSQL(const ASequenceName: string; const AOIDCount: Integer): string; override;
  end;

implementation

uses
  sysutils;

{ TJCoreOPFFirebirdDatabase }

class function TJCoreOPFFirebirdDatabase.DatabaseName: string;
begin
  Result := 'Firebird';
end;

function TJCoreOPFFirebirdDatabase.SequenceSQL(const ASequenceName: string;
  const AOIDCount: Integer): string;
begin
  Result := Format('SELECT gen_id(%s, %d) FROM rdb$database', [ASequenceName, AOIDCount]);
end;

end.

