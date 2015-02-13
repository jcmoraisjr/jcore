program opf001;

{$mode objfpc}{$H+}

uses
  heaptrc,
  sysutils,
  pqconnection,
  JCoreDIC,
  JCoreLogger,
  JCoreEntity,
  JCoreOPFConfig,
  JCoreOPFSession,
  JCoreOPFMappingSQL,
  JCoreOPFOID,
  JCoreOPFDriverSQLdb;

type

  { TPerson }

  TPerson = class(TJCoreEntity)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

var
  VConfig: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TPerson;

{
  create sequence seq_person;
  create table person (
    id integer,
    name varchar(255)
  );
  alter table person
    add constraint pk_person primary key (id);
}

begin
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreConsoleLogFactory, jdsApplication);
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Params.Values['connection'] := 'PostgreSQL';
  VConfig.Params.Values['hostname'] := 'localhost';
  VConfig.Params.Values['database'] := 'jcore_opf001';
  VConfig.Params.Values['username'] := 'jcore';
  VConfig.Params.Values['password'] := 'jcore';
  VConfig.DriverClass := TJCoreOPFDriverSQLdb;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.GeneratorName := 'seq_person';
  VConfig.Model.OIDClass := TJCoreOPFOIDInt64;
  VSession := VConfig.CreateSession;
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'jack';
    VSession.Store(VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end.

