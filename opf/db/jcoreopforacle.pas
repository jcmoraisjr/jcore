(*
  JCore, OPF Oracle DB Class
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFOracle;

{$I jcore.inc}

interface

uses
  JCoreOPFDriver;

type

  { TJCoreOPFOracleDatabase }

  TJCoreOPFOracleDatabase = class(TJCoreOPFSQLDatabase)
  public
    class function DatabaseName: string; override;
    function SequenceSQL(const ASequenceName: string; const AOIDCount: Integer): string; override;
  end;

implementation

uses
  sysutils;

{ TJCoreOPFOracleDatabase }

class function TJCoreOPFOracleDatabase.DatabaseName: string;
begin
  Result := 'Oracle';
end;

function TJCoreOPFOracleDatabase.SequenceSQL(const ASequenceName: string;
  const AOIDCount: Integer): string;
begin
  { TODO : AOIDCount }
  Result := Format('SELECT %s.nextval FROM dual', [ASequenceName]);
end;

end.

