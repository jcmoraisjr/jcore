program opf005;

{$mode objfpc}{$H+}

uses
  heaptrc,
  sysutils,
  fgl,
  pqconnection,
  JCoreDIC,
  JCoreLogger,
  JCoreEntity,
  JCoreOPFConfig,
  JCoreOPFSession,
  JCoreOPFOID,
  JCoreOPFMappingSQL,
  JCoreOPFDriverSQLdb;

type

  { TClient }

  TClient = class(TJCoreEntity)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TPerson }

  TPerson = class(TClient)
  private
    FAge: Integer;
  published
    property Age: Integer read FAge write FAge;
  end;

  { TCompany }

  TCompany = class(TClient)
  private
    FEmployees: Integer;
  published
    property Employees: Integer read FEmployees write FEmployees;
  end;

var
  LOG: IJCoreLogger;
  VConfig: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VClient: TClient;
  VPerson: TPerson;
  VCompany: TCompany;
  VClientID, VPersonID, VCompanyID: string;

{
  create table client (
    id varchar(32),
    name varchar(255)
  );
  create table person (
    id varchar(32),
    age integer
  );
  create table company (
    id varchar(32),
    employees integer
  );
  alter table client
    add constraint pk_client primary key (id);
  alter table person
    add constraint pk_person primary key (id);
  alter table person
    add constraint fk_person_client foreign key (id) references client (id);
  alter table company
    add constraint pk_company primary key (id);
  alter table company
    add constraint fk_company_client foreign key (id) references client (id);
}

begin
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreConsoleLogFactory, jdsApplication);
  LOG := TJCoreLogger.GetLogger('demo.opf005');
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Params.Values['connection'] := 'PostgreSQL';
  VConfig.Params.Values['hostname'] := 'localhost';
  VConfig.Params.Values['database'] := 'jcore_opf005';
  VConfig.Params.Values['username'] := 'jcore';
  VConfig.Params.Values['password'] := 'jcore';
  VConfig.DriverClass := TJCoreOPFDriverSQLdb;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.AddClass([TClient, TPerson, TCompany]);
  VSession := VConfig.CreateSession;
  LOG.Info('1 ----- adding some objects');
  VClient := TClient.Create;
  try
    VClient.Name := 'neither';
    VSession.Store(VClient);
    VClientID := VClient._proxy.OID.AsString;
  finally
    FreeAndNil(VClient);
  end;
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'joe';
    VPerson.Age := 48;
    VSession.Store(VPerson);
    VPersonID := VPerson._proxy.OID.AsString;
  finally
    FreeAndNil(VPerson);
  end;
  VCompany := TCompany.Create;
  try
    VCompany.Name := 'tabajara corporation';
    VCompany.Employees := 9;
    VSession.Store(VCompany);
    VCompanyID := VCompany._proxy.OID.AsString;
  finally
    FreeAndNil(VCompany);
  end;
  LOG.Info('2 ----- done, now retrieving "Person" from the concrete class');
  VPerson := VSession.Retrieve(TPerson, VPersonID) as TPerson;
  try
    LOG.Info('Name=' + VPerson.Name + '; Age=' + IntToStr(VPerson.Age));
  finally
    FreeAndNil(VPerson);
  end;
  LOG.Info('3 ----- retrieving "Company" from the abstract class');
  VClient := VSession.Retrieve(TClient, VCompanyID) as TClient;
  try
    if VClient is TCompany then
    begin
      VCompany := TCompany(VClient);
      LOG.Info('Name=' + VCompany.Name + '; employees=' + IntToStr(VCompany.Employees));
    end else
      LOG.Warn('3.1 --- oops, opf retrieved an instance of "' + VClient.ClassName + '"');
  finally
    FreeAndNil(VClient);
  end;
end.

