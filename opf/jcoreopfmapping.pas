(*
  JCore, OPF Mapping Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFMapping;

{$I jcore.inc}

interface

uses
  typinfo,
  fgl,
  JCoreClasses,
  JCoreOPFEntity,
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMetadata;

type

  { IJCoreOPFMapper }

  IJCoreOPFMapper = interface
    procedure AddInTransactionPID(const APID: TJCoreOPFPID);
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveListPID(const AListBaseClass: TClass; const AOwnerPID: TJCoreOPFPID): TJCoreObjectList;
    procedure StoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection; const AItems: TJCoreOPFPIDArray);
    procedure StorePID(const APID: TJCoreOPFPID);
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
  end;

  { TJCoreOPFMapping }

  TJCoreOPFMapping = class(TObject, IJCoreOPFPIDMapping)
  private
    FDriver: TJCoreOPFDriver;
    FMapper: IJCoreOPFMapper;
    FModel: TJCoreOPFModel;
  protected
    function CreatePID(const AEntity: TObject): TJCoreOPFPID; virtual;
    procedure WriteNullOIDToDriver(const AClass: TClass); virtual;
  protected
    function CreateOIDFromString(const AOID: string): TJCoreOPFOID; virtual; abstract;
    function CreateOIDFromDriver(const ADriver: TJCoreOPFDriver): TJCoreOPFOID; virtual; abstract;
    procedure InternalDispose(const AClass: TClass; const AOIDArray: array of TJCoreOPFOID); virtual; abstract;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; virtual; abstract;
    function InternalRetrieveList(const AListBaseClass: TClass; const AOwnerPID: TJCoreOPFPID): TJCoreObjectList; virtual; abstract;
    procedure InternalStore(const APID: TJCoreOPFPID); virtual; abstract;
    procedure InternalStoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection; const AItems: TJCoreOPFPIDArray); virtual; abstract;
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
    property Model: TJCoreOPFModel read FModel;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver); virtual;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    class function Apply(const AClass: TClass): Boolean; virtual; abstract;
    procedure Dispose(const APID: TJCoreOPFPID);
    procedure DisposeFromString(const AClass: TClass; const AOID: string);
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveFromString(const AClass: TClass; const AStringOID: string): TObject;
    function RetrieveList(const AListBaseClass: TClass; const AOwnerPID: TJCoreOPFPID): TJCoreObjectList;
    procedure Store(const APID: TJCoreOPFPID);
    procedure StoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection; const AItems: TJCoreOPFPIDArray);
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
  end;

  TJCoreOPFMappingList = specialize TFPGObjectList<TJCoreOPFMapping>;

  TJCoreOPFMappingClass = class of TJCoreOPFMapping;

  TJCoreOPFMappingClassList = specialize TFPGList<TJCoreOPFMappingClass>;

  { TJCoreOPFSQLMapping }

  TJCoreOPFSQLMapping = class(TJCoreOPFMapping)
  private
    { TODO : Generics? }
    FSQLDriver: TJCoreOPFSQLDriver;
    function EnsureCollectionAttribute(const APID: TJCoreOPFPID; const AAttributeName: string): TJCoreOPFADMCollection;
  protected
    function CreateEntity(const AClass: TClass): TObject; virtual;
    function GenerateDeleteExternalLinksStatement(const ACompositionClass: TClass): string; virtual;
    function GenerateInsertExternalLinksStatement(const ACompositionClass: TClass): string; virtual;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; virtual; abstract;
    function GenerateSelectListFromStatement(const AListBaseClass: TClass): string; virtual;
    function GenerateSelectStatement(const AClass: TClass): string; virtual; abstract;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; virtual; abstract;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); virtual;
    procedure WriteExternalLinksToDriver(const AOwnerPID: TJCoreOPFPID; const ACompositionClass: TClass; const AItems: TJCoreOPFPIDArray); virtual;
    procedure WriteExternalsToDriver(const APID: TJCoreOPFPID); virtual;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); virtual;
  protected
    function BuildParams(const ASize: Integer): string;
    function CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
    procedure InternalDispose(const AClass: TClass; const AOIDArray: array of TJCoreOPFOID); override;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; override;
    function InternalRetrieveList(const AListBaseClass: TClass; const AOwnerPID: TJCoreOPFPID): TJCoreObjectList; override;
    procedure InternalStore(const APID: TJCoreOPFPID); override;
    procedure InternalStoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection; const AItems: TJCoreOPFPIDArray); override;
    procedure InternalStoreList(const APID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection); virtual;
  protected
    function RetrieveListPID(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
    procedure StoreList(const APID: TJCoreOPFPID; const AAttributeName: string);
    property Driver: TJCoreOPFSQLDriver read FSQLDriver;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver); override;
  end;

implementation

uses
  sysutils,
  JCoreMetadata,
  JCoreOPFConsts,
  JCoreOPFException;

{ TJCoreOPFMapping }

function TJCoreOPFMapping.AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
begin
  Result := Model.AcquireMetadata(AClass);
end;

function TJCoreOPFMapping.CreatePID(const AEntity: TObject): TJCoreOPFPID;
begin
  Result := TJCoreOPFPID.Create(Self, AEntity, AcquireMetadata(AEntity.ClassType));
end;

procedure TJCoreOPFMapping.WriteNullOIDToDriver(const AClass: TClass);
begin
  { TODO : Evaluate after attr metadata implementation }
  Driver.WriteNull;
end;

constructor TJCoreOPFMapping.Create(const AMapper: IJCoreOPFMapper;
  const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver);
begin
  if not Assigned(ADriver) or not Assigned(AMapper) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FMapper := AMapper;
  FModel := AModel;
  FDriver := ADriver;
end;

function TJCoreOPFMapping.AcquirePID(const AEntity: TObject): TJCoreOPFPID;
var
  VPID: IJCoreOPFPID;
  VPropInfo: PPropInfo;
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  VPropInfo := GetPropInfo(AEntity, SPID);
  if not Assigned(VPropInfo) then
    raise EJCoreOPFPersistentIDFieldNotFound.Create(AEntity.ClassName);
  VPID := GetInterfaceProp(AEntity, VPropInfo) as IJCoreOPFPID;
  if Assigned(VPID) then
  begin
    Result := VPID as TJCoreOPFPID;
  end else
  begin
    Result := CreatePID(AEntity);
    VPID := Result as IJCoreOPFPID;
    SetInterfaceProp(AEntity, VPropInfo, VPID);
  end;
  Mapper.AddInTransactionPID(Result);
end;

procedure TJCoreOPFMapping.Dispose(const APID: TJCoreOPFPID);
begin
  InternalDispose(APID.Entity.ClassType, [APID.OID]);
end;

procedure TJCoreOPFMapping.DisposeFromString(const AClass: TClass;
  const AOID: string);
var
  VOID: TJCoreOPFOID;
begin
  VOID := CreateOIDFromString(AOID);
  try
    InternalDispose(AClass, [VOID]);
  finally
    FreeAndNil(VOID);
  end;
end;

function TJCoreOPFMapping.RetrieveFromDriver(const AClass: TClass;
  const ADriverOID: TJCoreOPFDriver): TObject;
var
  VOID: TJCoreOPFOID;
begin
  if not ADriverOID.ReadNull then
  begin
    VOID := CreateOIDFromDriver(ADriverOID);
    try
      Result := InternalRetrieve(AClass, VOID);
    except
      FreeAndNil(VOID);
      raise;
    end;
  end else
    Result := nil;
end;

function TJCoreOPFMapping.RetrieveFromString(const AClass: TClass;
  const AStringOID: string): TObject;
var
  VOID: TJCoreOPFOID;
begin
  if AStringOID <> '' then
  begin
    VOID := CreateOIDFromString(AStringOID);
    try
      Result := InternalRetrieve(AClass, VOID);
    except
      FreeAndNil(VOID);
      raise;
    end;
  end else
    Result := nil;
end;

function TJCoreOPFMapping.RetrieveList(const AListBaseClass: TClass;
  const AOwnerPID: TJCoreOPFPID): TJCoreObjectList;
begin
  Result := InternalRetrieveList(AListBaseClass, AOwnerPID);
end;

procedure TJCoreOPFMapping.Store(const APID: TJCoreOPFPID);
begin
  if APID.IsDirty then
  begin
    InternalStore(APID);
    APID.Stored;
  end;
end;

procedure TJCoreOPFMapping.StoreElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection; const AItems: TJCoreOPFPIDArray);
begin
  InternalStoreElements(AOwnerPID, AOwnerADM, AItems);
end;

procedure TJCoreOPFMapping.StoreToDriver(const AClass: TClass;
  const AEntity: TObject; const ADriver: TJCoreOPFDriver);
var
  VPID: TJCoreOPFPID;
begin
  if Assigned(AEntity) then
  begin
    VPID := AcquirePID(AEntity);
    Store(VPID);
    VPID.OID.WriteToDriver(ADriver);
  end else
    WriteNullOIDToDriver(AClass);
end;

{ TJCoreOPFSQLMapping }

function TJCoreOPFSQLMapping.EnsureCollectionAttribute(const APID: TJCoreOPFPID;
  const AAttributeName: string): TJCoreOPFADMCollection;
var
  VADM: TJCoreOPFADM;
begin
  VADM := APID.AcquireADM(AAttributeName);
  if not (VADM is TJCoreOPFADMCollection) then
    raise EJCoreOPFCollectionADMExpected.Create(
     APID.Entity.ClassName, AAttributeName);
  Result := TJCoreOPFADMCollection(VADM);
end;

function TJCoreOPFSQLMapping.CreateEntity(const AClass: TClass): TObject;
begin
  Result := AClass.Create;
end;

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinksStatement(
  const ACompositionClass: TClass): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateInsertExternalLinksStatement(
  const ACompositionClass: TClass): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateSelectListFromStatement(
  const AListBaseClass: TClass): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

procedure TJCoreOPFSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
begin
end;

procedure TJCoreOPFSQLMapping.WriteExternalLinksToDriver(
  const AOwnerPID: TJCoreOPFPID; const ACompositionClass: TClass;
  const AItems: TJCoreOPFPIDArray);
var
  VInsertStatement: string;
  I: Integer;
begin
  VInsertStatement := GenerateInsertExternalLinksStatement(ACompositionClass);
  for I := Low(AItems) to High(AItems) do
  begin
    AOwnerPID.OID.WriteToDriver(Driver);
    AItems[I].OID.WriteToDriver(Driver);
    Driver.ExecSQL(VInsertStatement);
  end;
end;

procedure TJCoreOPFSQLMapping.WriteExternalsToDriver(const APID: TJCoreOPFPID);
begin
end;

procedure TJCoreOPFSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
begin
end;

function TJCoreOPFSQLMapping.BuildParams(const ASize: Integer): string;
var
  I: Integer;
begin
  if ASize > 1 then
  begin
    { TODO : allocate once, at the start }
    Result := ' IN (?';
    for I := 2 to ASize do
      Result := Result + ',?';
    Result := Result + ')';
  end else if ASize = 1 then
    Result := '=?'
  else
    Result := '';
end;

function TJCoreOPFSQLMapping.CreatePIDArray(
  const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AItems));
  for I := Low(AItems) to High(AItems) do
    Result[I] := AcquirePID(AItems[I]);
end;

procedure TJCoreOPFSQLMapping.InternalDispose(const AClass: TClass;
  const AOIDArray: array of TJCoreOPFOID);
begin
  { TODO : Implement }
end;

function TJCoreOPFSQLMapping.InternalRetrieve(const AClass: TClass;
  const AOID: TJCoreOPFOID): TObject;
var
  VPID: TJCoreOPFPID;
begin
  AOID.WriteToDriver(Driver);
  Driver.ExecSQL(GenerateSelectStatement(AClass), 1);
  Result := CreateEntity(AClass);
  try
    VPID := AcquirePID(Result);
    try
      VPID.AssignOID(AOID);
      ReadFromDriver(VPID);
    except
      VPID.ReleaseOID(AOID);
      raise;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJCoreOPFSQLMapping.InternalRetrieveList(const AListBaseClass: TClass;
  const AOwnerPID: TJCoreOPFPID): TJCoreObjectList;
var
  VPID: TJCoreOPFPID;
  VOID: TJCoreOPFOID;
  VCount: Integer;
  I: Integer;
begin
  AOwnerPID.OID.WriteToDriver(Driver);
  VCount := Driver.ExecSQL(GenerateSelectListFromStatement(AListBaseClass));
  Result := TJCoreObjectList.Create(True);
  try
    for I := 1 to VCount do
    begin
      VOID := CreateOIDFromDriver(Driver);
      try
        Result.Add(CreateEntity(AListBaseClass));
        VPID := AcquirePID(Result.Last);
        try
          VPID.AssignOID(VOID);
          ReadFromDriver(VPID);
        except
          VPID.ReleaseOID(VOID);
          raise;
        end;
      except
        FreeAndNil(VOID);
        raise;
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TJCoreOPFSQLMapping.InternalStore(const APID: TJCoreOPFPID);
begin
  if APID.IsPersistent then
  begin
    WriteInternalsToDriver(APID);
    APID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateUpdateStatement(APID));
    WriteExternalsToDriver(APID);
  end else
  begin
    APID.AssignOID(CreateOIDFromString(''));
    APID.OID.WriteToDriver(Driver);
    WriteInternalsToDriver(APID);
    Driver.ExecSQL(GenerateInsertStatement(APID));
    WriteExternalsToDriver(APID);
  end;
end;

procedure TJCoreOPFSQLMapping.InternalStoreElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection; const AItems: TJCoreOPFPIDArray);
var
  I: Integer;
begin
  for I := Low(AItems) to High(AItems) do
  begin
    AItems[I].AssignOwner(AOwnerPID, AOwnerADM);
    if (AOwnerADM.Metadata.CompositionType = jctComposition) or
     not AItems[I].IsPersistent then
    begin
      if Apply(AItems[I].Entity.ClassType) then
        Store(AItems[I])
      else
        Mapper.StorePID(AItems[I]);
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.InternalStoreList(const APID: TJCoreOPFPID;
  const AADM: TJCoreOPFADMCollection);
var
  VMetadata: TJCoreOPFAttrMetadata;
  VItems: TJCoreOPFPIDArray;
begin
  { TODO : Implement partial update of external links }
  VMetadata := AADM.Metadata;

  // remove old links
  if (VMetadata.CompositionLinkType = jcltExternal) and APID.IsPersistent then
  begin
    APID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateDeleteExternalLinksStatement(VMetadata.CompositionClass));
  end;

  // update items
  VItems := CreatePIDArray(AADM.CreateArray);
  Mapper.StoreElements(APID, AADM, VItems);

  // add new links
  if VMetadata.CompositionLinkType = jcltExternal then
    WriteExternalLinksToDriver(APID, VMetadata.CompositionClass, VItems);
end;

function TJCoreOPFSQLMapping.RetrieveListPID(const AListBaseClass: TClass;
  const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
begin
  Result := Mapper.RetrieveListPID(AListBaseClass, AOwnerPID as TJCoreOPFPID);
end;

{ Sequence diagram - include in the API doc
    user mapping
    -> Mapping.StoreList
      -> Session(from Mapper intf).StoreElements
        -> Mapping(of the CompositionClass).StoreElements
          -> Mapping.InternalStoreElements (loop)
    Conventions:
      StoreList == the attribute list, in the owner mapping
      StoreElements == the elements of the list, in the base class list mapping }
procedure TJCoreOPFSQLMapping.StoreList(const APID: TJCoreOPFPID;
  const AAttributeName: string);
var
  VADM: TJCoreOPFADMCollection;
begin
  { TODO : Improve the change analyzer }
  VADM := EnsureCollectionAttribute(APID, AAttributeName);
  if VADM.IsDirty then
    InternalStoreList(APID, VADM);
end;

constructor TJCoreOPFSQLMapping.Create(const AMapper: IJCoreOPFMapper;
  const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver);
begin
  if not (ADriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPFUnsupportedDriver.Create(ADriver.ClassName);
  inherited Create(AMapper, AModel, ADriver);
  FSQLDriver := TJCoreOPFSQLDriver(ADriver);
end;

end.

