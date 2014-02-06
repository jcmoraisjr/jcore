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
  typinfo,
  JCoreOPFID,
  JCoreOPFEDM;

type

  { TODO :
    Sessions from different configurations need different PIDs,
    iow, the same entity may be persistent to a configuration
    and nonpersistent to another one }

  IJCoreOPFPIDManager = interface
    function AcquireEDMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFEDMClass;
  end;

  { TJCoreOPFPID }

  TJCoreOPFPID = class(TInterfacedObject, IJCoreOPFPID)
  private
    FEDMMap: TJCoreOPFEDMMap;
    FEntity: TObject;
    FIsPersistent: Boolean;
    FOID: TJCoreOPFOID;
    FOwner: IJCoreOPFPID;
    FPIDManager: IJCoreOPFPIDManager;
    function AcquireEDM(const AAttributeName: string): TJCoreOPFEDM;
    function CreateEDM(const AAttributeName: string): TJCoreOPFEDM;
    function GetEntity: TObject;
    function GetIsPersistent: Boolean;
    function GetOID: TJCoreOPFOID;
    function GetOwner: IJCoreOPFPID;
    procedure SetOwner(const AValue: IJCoreOPFPID);
  protected
    property PIDManager: IJCoreOPFPIDManager read FPIDManager;
  public
    constructor Create(const APIDManager: IJCoreOPFPIDManager; const AEntity: TObject);
    destructor Destroy; override;
    procedure AssignOID(const AOID: TJCoreOPFOID);
    procedure Commit;
    function IsDirty(const AAttributeName: string): Boolean;
    procedure ReleaseOID(const AOID: TJCoreOPFOID);
    procedure UpdateCache(const AAttributeNameArray: array of string);
    property IsPersistent: Boolean read GetIsPersistent;
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

function TJCoreOPFPID.AcquireEDM(const AAttributeName: string): TJCoreOPFEDM;
var
  VIndex: Integer;
begin
  VIndex := FEDMMap.IndexOf(AAttributeName);
  if VIndex = -1 then
    VIndex := FEDMMap.Add(AAttributeName, CreateEDM(AAttributeName));
  Result := FEDMMap.Data[VIndex];
end;

function TJCoreOPFPID.CreateEDM(const AAttributeName: string): TJCoreOPFEDM;
var
  VAttrPropInfo: PPropInfo;
  VEDMClass: TJCoreOPFEDMClass;
begin
  { TODO : delegate propinfo and mediator search to the attribute metadata.
           Search metadata here, metadata search everything ONCE and
           save a reference }
  VAttrPropInfo := GetPropInfo(FEntity, AAttributeName);
  if not Assigned(VAttrPropInfo) then
    raise EJCoreOPFAttributeNotFound.Create(FEntity.ClassName, AAttributeName);
  VEDMClass := PIDManager.AcquireEDMClass(VAttrPropInfo^.PropType);
  Result := VEDMClass.Create(FEntity, VAttrPropInfo);
end;

function TJCoreOPFPID.GetEntity: TObject;
begin
  Result := FEntity;
end;

function TJCoreOPFPID.GetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
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

constructor TJCoreOPFPID.Create(const APIDManager: IJCoreOPFPIDManager;
  const AEntity: TObject);
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FPIDManager := APIDManager;
  FEntity := AEntity;
  FIsPersistent := False;
  FEDMMap := TJCoreOPFEDMMap.Create;
end;

destructor TJCoreOPFPID.Destroy;
var
  I: Integer;
begin
  for I := 0 to Pred(FEDMMap.Count) do
    FEDMMap.Data[I].Free;
  FreeAndNil(FEDMMap);
  FreeAndNil(FOID);
  inherited Destroy;
end;

procedure TJCoreOPFPID.AssignOID(const AOID: TJCoreOPFOID);
begin
  if IsPersistent then
    raise EJCoreOPFCannotAssignOIDPersistent.Create;
  FreeAndNil(FOID);
  FOID := AOID;
end;

procedure TJCoreOPFPID.Commit;
begin
  FIsPersistent := Assigned(OID);
end;

function TJCoreOPFPID.IsDirty(const AAttributeName: string): Boolean;
begin
  Result := AcquireEDM(AAttributeName).IsDirty;
end;

procedure TJCoreOPFPID.ReleaseOID(const AOID: TJCoreOPFOID);
begin
  { TODO : Used to release the OID if an exception raises just after the OID
           was assigned. A refcounted object (intf or a jcore managed obj) is
           a better approach }
  if FOID = AOID then
    FOID := nil;
end;

procedure TJCoreOPFPID.UpdateCache(const AAttributeNameArray: array of string);
var
  VAttributeName: string;
begin
  { TODO : call all attributes if array is empty }
  for VAttributeName in AAttributeNameArray do
    AcquireEDM(VAttributeName).UpdateCache;
end;

end.

