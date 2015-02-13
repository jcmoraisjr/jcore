program opf002;

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
  JCoreOPFDriverSQLdb;

type

  TCity = class(TJCoreEntity)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  TPerson = class(TJCoreEntity)
  private
    FName: string;
    FCity: TCity;
  published
    property Name: string read FName write FName;
    property City: TCity read FCity write FCity;
  end;

var
  VConfig: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TPerson;
  VCity: TCity;

{
  create table city (
    id varchar(32),
    name varchar(255)
  );
  create table person (
    id varchar(32),
    name varchar(255),
    city varchar(32)
  );
  alter table city
    add constraint pk_city primary key (id);
  alter table person
    add constraint pk_person primary key (id);
  alter table person
    add constraint fk_person_city foreign key (city) references city (id);
}

begin
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreConsoleLogFactory, jdsApplication);
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Params.Values['connection'] := 'PostgreSQL';
  VConfig.Params.Values['hostname'] := 'localhost';
  VConfig.Params.Values['database'] := 'jcore_opf002';
  VConfig.Params.Values['username'] := 'jcore';
  VConfig.Params.Values['password'] := 'jcore';
  VConfig.DriverClass := TJCoreOPFDriverSQLdb;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.AddClass([TPerson, TCity]);
  VSession := VConfig.CreateSession;
  VPerson := TPerson.Create;
  try
    VCity := TCity.Create;
    try
      VCity.Name := 'dubai';
      VPerson.Name := 'joe';
      VPerson.City := VCity;
      VSession.Store(VPerson);
    finally
      FreeAndNil(VCity);
    end;
  finally
    FreeAndNil(VPerson);
  end;
end.

