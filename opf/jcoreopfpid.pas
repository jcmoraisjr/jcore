(*
  JCore, OPF Persistence Identity Class
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFPID;

{$I jcore.inc}

interface

uses
  JCoreOPFID;

type

  { TODO :
    Sessions from different configurations need different PIDs,
    iow, the same entity may be persistent to a configuration
    and nonpersistent to another one }

  { TJCoreOPFPID }

  TJCoreOPFPID = class(TInterfacedObject, IJCoreOPFPID)
  private
    FEntity: TObject;
    FOID: TJCoreOPFOID;
    FOwner: IJCoreOPFPID;
    function GetEntity: TObject;
    function GetOID: TJCoreOPFOID;
    function GetOwner: IJCoreOPFPID;
    procedure SetOwner(const AValue: IJCoreOPFPID);
  public
    constructor Create(const AEntity: TObject);
    destructor Destroy; override;
    procedure AssignOID(const AOID: TJCoreOPFOID);
    function IsPersistent: Boolean;
    property Entity: TObject read GetEntity;
    property OID: TJCoreOPFOID read FOID;
    property Owner: IJCoreOPFPID read GetOwner write SetOwner;
  end;

implementation

uses
  sysutils,
  JCoreClasses,
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

function TJCoreOPFPID.GetOwner: IJCoreOPFPID;
begin
  Result := FOwner;
end;

procedure TJCoreOPFPID.SetOwner(const AValue: IJCoreOPFPID);
begin
  if not Assigned(AValue) then
    FOwner := nil
  else if not Assigned(FOwner) then
  begin
    { TODO : Check circular reference }
    FOwner := AValue;
  end else if FOwner <> AValue then
    raise EJCoreOPFObjectAlreadyOwned.Create(Entity.ClassName, FOwner.Entity.ClassName);
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

