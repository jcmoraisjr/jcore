program opf007;

{$mode objfpc}{$H+}

uses
  heaptrc,
  sysutils,
  mssqlconn,
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
  create table person (
    id integer identity,
    name varchar(255)
  );
  alter table person
    add constraint pk_person primary key (id);
}

begin
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreConsoleLogFactory, jdsApplication);
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Params.Values['connection'] := 'MSSQLServer';
  VConfig.Params.Values['hostname'] := 'localhost';
  VConfig.Params.Values['database'] := 'jcore_opf007';
  VConfig.Params.Values['username'] := 'jcore';
  VConfig.Params.Values['password'] := 'jcore';
  VConfig.DriverClass := TJCoreOPFDriverSQLdb;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.OIDGenerator := TJCoreOPFOIDGeneratorAutoinc.Create;
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

