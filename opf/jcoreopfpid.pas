unit JCoreOPFPID;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFOID;

type

  { TODO :
    Sessions from different configurations need different PIDs,
    iow, the same entity may be persistent to a configuration
    and nonpersistent to another one }

  IJCoreOPFPID = interface(IInterface)
  ['{C2E47A60-B063-1FC0-D566-BAAC73195623}']
    procedure AssignOID(const AOID: TJCoreOPFOID);
    function GetEntity: TObject;
    function GetOID: TJCoreOPFOID;
    function IsPersistent: Boolean;
    property Entity: TObject read GetEntity;
    property OID: TJCoreOPFOID read GetOID;
  end;

  { TJCoreOPFPID }

  TJCoreOPFPID = class(TInterfacedObject, IJCoreOPFPID)
  private
    FEntity: TObject;
    FOID: TJCoreOPFOID;
    function GetEntity: TObject;
    function GetOID: TJCoreOPFOID;
  public
    constructor Create(const AEntity: TObject);
    destructor Destroy; override;
    class function AcquirePID(AEntity: TObject): IJCoreOPFPID; virtual;
    procedure AssignOID(const AOID: TJCoreOPFOID);
    function IsPersistent: Boolean;
    property Entity: TObject read GetEntity;
    property OID: TJCoreOPFOID read FOID;
  end;

implementation

uses
  sysutils,
  typinfo,
  JCoreClasses,
  JCoreOPFConsts,
  JCoreOPFException;

{ TJCoreOPFPID }

function TJCoreOPFPID.GetEntity: TObject;
begin
  Result := FEntity;
end;

function TJCoreOPFPID.GetOID: TJCoreOPFOID;
begin
  Result := FOID;
end;

constructor TJCoreOPFPID.Create(const AEntity: TObject);
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FEntity := AEntity;
end;

destructor TJCoreOPFPID.Destroy;
begin
  FreeAndNil(FOID);
  inherited Destroy;
end;

class function TJCoreOPFPID.AcquirePID(AEntity: TObject): IJCoreOPFPID;
var
  VPropInfo: PPropInfo;
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  VPropInfo := GetPropInfo(AEntity, SPID);
  if not Assigned(VPropInfo) then
    raise EJCoreOPFPersistentIDFieldNotFound.Create(AEntity.ClassName);
  Result := GetInterfaceProp(AEntity, VPropInfo) as IJCoreOPFPID;
  if not Assigned(Result) then
  begin
    Result := TJCoreOPFPID.Create(AEntity);
    SetInterfaceProp(AEntity, VPropInfo, Result);
  end;
end;

procedure TJCoreOPFPID.AssignOID(const AOID: TJCoreOPFOID);
begin
  if IsPersistent then
    raise EJCoreOPFCannotAssignOIDPersistent.Create;
  FOID := AOID;
end;

function TJCoreOPFPID.IsPersistent: Boolean;
begin
  Result := Assigned(OID);
end;

end.

