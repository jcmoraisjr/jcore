program opf006;

{$mode objfpc}{$H+}

uses
  heaptrc,
  sysutils,
  fgl,
  pqconnection,
  JCoreDIC,
  JCoreLogger,
  JCoreEntity,
  JCoreMetadata,
  JCoreOPFConfig,
  JCoreOPFSession,
  JCoreOPFMappingSQL,
  JCoreOPFDriverSQLdb;

type

  TPerson = class;
  TPersonList = specialize TFPGObjectList<TPerson>;

  { TPerson }

  TPerson = class(TJCoreEntity)
  private
    FName: string;
    FDependent: TPersonList;
    function GetDependent: TPersonList;
    procedure SetDependent(AValue: TPersonList);
  public
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Dependent: TPersonList read GetDependent write SetDependent;
  end;

{ TPerson }

function TPerson.GetDependent: TPersonList;
begin
  if not _proxy.Lazyload(@FDependent) then
    FDependent := TPersonList.Create(True);
  Result := FDependent;
end;

procedure TPerson.SetDependent(AValue: TPersonList);
begin
  if AValue <> FDependent then
  begin
    FDependent.Free;
    FDependent := AValue;
  end;
end;

destructor TPerson.Destroy;
begin
  FreeAndNil(FDependent);
  inherited Destroy;
end;

var
  LOG: IJCoreLogger;
  VConfig: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VPerson: TPerson;
  VDependent: TPerson;
  VPersonID: string;

{
  create table person (
    id varchar(32),
    name varchar(255)
  );
  create table person_dependent (
    person varchar(32),
    person_dependent varchar(32)
  );
  alter table person
    add constraint pk_person primary key (id);
  alter table person_dependent
    add constraint pk_person_dependent primary key (person,person_dependent);
  alter table person_dependent
    add constraint fk_person_dependent_person foreign key (person) references person (id);
  alter table person_dependent
    add constraint fk_person_dependent_dependent foreign key (person_dependent) references person (id);
}

procedure Print(const AIndent: string; const APerson: TPerson);
var
  I: Integer;
begin
  LOG.Info(AIndent + 'Name: ' + APerson.Name);
  LOG.Info(AIndent + 'Dependents:');
  for I := 0 to Pred(APerson.Dependent.Count) do
    Print(AIndent + '  ', APerson.Dependent[I]);
end;

begin
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreConsoleLogFactory, jdsApplication);
  LOG := TJCoreLogger.GetLogger('demos.opf006');
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Params.Values['connection'] := 'PostgreSQL';
  VConfig.Params.Values['hostname'] := 'localhost';
  VConfig.Params.Values['database'] := 'jcore';
  VConfig.Params.Values['username'] := 'jcore';
  VConfig.Params.Values['password'] := 'jcore';
  VConfig.DriverClass := TJCoreOPFDriverSQLdb;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.AddClass([TPerson]);
  VConfig.Model.AddGenerics(TPersonList, TPerson);
  VConfig.Model.AcquireAttrMetadata(TPerson, 'Dependent').CompositionType := jctAggregation;
  VSession := VConfig.CreateSession;
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'joe dad';
    VDependent := TPerson.Create;
    VPerson.Dependent.Add(VDependent);
    VDependent.Name := 'joe son';
    VDependent := TPerson.Create;
    VPerson.Dependent.Add(VDependent);
    VDependent.Name := 'jane daughter';
    VDependent := TPerson.Create;
    VPerson.Dependent[0].Dependent.Add(VDependent);
    VDependent.Name := 'jack grandson';
    VSession.Store(VPerson);
    VPersonID := VPerson._proxy.OID.AsString;
  finally
    FreeAndNil(VPerson);
  end;
  VPerson := VSession.Retrieve(TPerson, VPersonID) as TPerson;
  try
    Print(' ', VPerson);
  finally
    FreeAndNil(VPerson);
  end;
end.

