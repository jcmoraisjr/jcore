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
  fgl,
  JCoreOPFEntity,
  JCoreOPFOID,
  JCoreOPFMetadata;

type

  { TODO :
    Sessions from different configurations need different PIDs,
    iow, the same entity may be persistent to a configuration
    and nonpersistent to another one }

  { TJCoreOPFPID }

  TJCoreOPFPID = class(TInterfacedObject, IJCoreOPFPID)
  private
    FADMMap: TJCoreOPFADMMap;
    FEntity: TObject;
    FIsPersistent: Boolean;
    FMetadata: TJCoreOPFClassMetadata;
    FOID: TJCoreOPFOID;
    FOwner: TJCoreOPFPID;
    function GetEntity: TObject;
    function GetIsPersistent: Boolean;
    function GetOIDIntf: IJCoreOPFOID;
    function GetOwnerIntf: IJCoreOPFPID;
    procedure SetOwner(const AValue: TJCoreOPFPID);
    function IJCoreOPFPID.OID = GetOIDIntf;
    function IJCoreOPFPID.Owner = GetOwnerIntf;
  protected
    property Metadata: TJCoreOPFClassMetadata read FMetadata;
  public
    constructor Create(const AMetadata: TJCoreOPFClassMetadata; const AEntity: TObject);
    destructor Destroy; override;
    function AcquireADM(const AAttributeName: string): TJCoreOPFADM;
    function ADMByName(const AAttributeName: string): IJCoreOPFADM;
    procedure AssignOID(const AOID: TJCoreOPFOID);
    procedure Commit;
    procedure ReleaseOID(const AOID: TJCoreOPFOID);
    property IsPersistent: Boolean read GetIsPersistent;
    property Entity: TObject read GetEntity;
    property OID: TJCoreOPFOID read FOID;
    property Owner: TJCoreOPFPID read FOwner write SetOwner;
  end;

  TJCoreOPFPIDList = specialize TFPGObjectList<TJCoreOPFPID>;
  TJCoreOPFPIDArray = array of TJCoreOPFPID;

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

function TJCoreOPFPID.GetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
end;

function TJCoreOPFPID.GetOIDIntf: IJCoreOPFOID;
begin
  Result := FOID as IJCoreOPFOID
end;

function TJCoreOPFPID.GetOwnerIntf: IJCoreOPFPID;
begin
  Result := FOwner as IJCoreOPFPID;
end;

procedure TJCoreOPFPID.SetOwner(const AValue: TJCoreOPFPID);
begin
  if not Assigned(AValue) then
    FreeAndNil(FOwner)
  else if not Assigned(FOwner) then
  begin
    { TODO : Check circular reference }
    FOwner := AValue;
  end else if FOwner <> AValue then
    raise EJCoreOPFObjectAlreadyOwned.Create(Entity.ClassName, FOwner.Entity.ClassName);
end;

constructor TJCoreOPFPID.Create(const AMetadata: TJCoreOPFClassMetadata;
  const AEntity: TObject);
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FEntity := AEntity;
  FMetadata := AMetadata;
  FIsPersistent := False;
  FADMMap := TJCoreOPFADMMap.Create;
end;

destructor TJCoreOPFPID.Destroy;
var
  I: Integer;
begin
  for I := 0 to Pred(FADMMap.Count) do
    FADMMap.Data[I].Free;
  FreeAndNil(FADMMap);
  FreeAndNil(FOID);
  inherited Destroy;
end;

function TJCoreOPFPID.AcquireADM(const AAttributeName: string): TJCoreOPFADM;
var
  VIndex: Integer;
begin
  VIndex := FADMMap.IndexOf(AAttributeName);
  if VIndex = -1 then
    VIndex := FADMMap.Add(AAttributeName,
     Metadata.AttributeByName(AAttributeName).CreateADM(Entity));
  Result := FADMMap.Data[VIndex];
end;

function TJCoreOPFPID.ADMByName(const AAttributeName: string): IJCoreOPFADM;
begin
  Result := AcquireADM(AAttributeName) as IJCoreOPFADM;
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
  FIsPersistent := Assigned(FOID);
end;

procedure TJCoreOPFPID.ReleaseOID(const AOID: TJCoreOPFOID);
begin
  { TODO : Used to release the OID if an exception raises just after the OID
           was assigned. A refcounted object (intf or a jcore managed obj) is
           a better approach }
  if FOID = AOID then
    FOID := nil;
end;

end.

