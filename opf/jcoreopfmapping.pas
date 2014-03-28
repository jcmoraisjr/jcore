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
  JCoreEntity,
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMetadata;

type

  { IJCoreOPFMapper }

  IJCoreOPFMapper = interface
    procedure AddInTransactionPID(const APID: TJCoreOPFPID);
    procedure DisposeFromDriver(const AMetadata: TJCoreOPFClassMetadata; const ADriverOID: TJCoreOPFDriver; const ACount: Integer);
    procedure RetrieveElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    procedure StoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
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
    function CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
    procedure InternalDispose(const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID); virtual; abstract;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; virtual; abstract;
    procedure InternalRetrieveElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection); virtual; abstract;
    procedure InternalStore(const APID: TJCoreOPFPID); virtual; abstract;
    procedure InternalStoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection); virtual; abstract;
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
    property Model: TJCoreOPFModel read FModel;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver); virtual;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    class function Apply(const AClass: TClass): Boolean; virtual; abstract;
    procedure Dispose(const APID: TJCoreOPFPID);
    procedure DisposeFromDriver(const AMetadata: TJCoreOPFClassMetadata; const ADriverOID: TJCoreOPFDriver; const ACount: Integer);
    procedure DisposeFromString(const AClass: TClass; const AOIDArray: array of string);
    procedure RetrieveElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveFromString(const AClass: TClass; const AStringOID: string): TObject;
    procedure Store(const APID: TJCoreOPFPID);
    procedure StoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
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
    function GenerateDeleteExternalLinkIDsStatement(const ACompositionClass: TClass; const ASize: Integer): string; virtual;
    function GenerateDeleteExternalLinksStatement(const ACompositionClass: TClass; const ASize: Integer): string; virtual;
    function GenerateDeleteStatement(const ASize: Integer): string; virtual; abstract;
    function GenerateInsertExternalLinksStatement(const ACompositionClass: TClass): string; virtual;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; virtual; abstract;
    function GenerateSelectCompositionsForDeleteStatement(const AClass: TClass; const ASize: Integer): string; virtual;
    function GenerateSelectForDeleteStatement(const ACompositionClass: TClass; const ASize: Integer): string; virtual;
    function GenerateSelectListFromStatement(const AListBaseClass: TClass): string; virtual;
    function GenerateSelectStatement(const AClass: TClass): string; virtual; abstract;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; virtual; abstract;
    function ListEntityCompositionMetadatas(const AClassMetadata: TJCoreOPFClassMetadata): TJCoreOPFAttrMetadataArray; virtual;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); virtual;
    procedure WriteDisposeCollectionToDriver(const AMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of TJCoreOPFOID); virtual;
    procedure WriteDisposeEntityCompositionsToDriver(const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID); virtual;
    procedure WriteDisposeExternalsToDriver(const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID); virtual;
    procedure WriteDisposeToDriver(const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID); virtual;
    procedure WriteExternalLinksToDriver(const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection); virtual;
    procedure WriteExternalsToDriver(const APID: TJCoreOPFPID); virtual;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); virtual;
  protected
    function BuildParams(const ASize: Integer): string;
    function CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
    procedure InternalDispose(const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID); override;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; override;
    procedure InternalRetrieveElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection); override;
    procedure InternalRetrieveList(const APID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection); virtual;
    procedure InternalStore(const APID: TJCoreOPFPID); override;
    procedure InternalStoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection); override;
    procedure InternalStoreList(const APID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection); virtual;
  protected
    procedure RetrieveList(const APID: TJCoreOPFPID; const AAttributeName: string);
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

function TJCoreOPFMapping.CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AItems));
  for I := Low(Result) to High(Result) do
    Result[I] := AcquirePID(AItems[I]);
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
  VPID: IJCorePID;
  VPropInfo: PPropInfo;
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  VPropInfo := GetPropInfo(AEntity, SPID);
  if not Assigned(VPropInfo) then
    raise EJCoreOPFPersistentIDFieldNotFound.Create(AEntity.ClassName);
  VPID := GetInterfaceProp(AEntity, VPropInfo) as IJCorePID;
  if Assigned(VPID) then
  begin
    Result := VPID as TJCoreOPFPID;
  end else
  begin
    Result := CreatePID(AEntity);
    VPID := Result as IJCorePID;
    SetInterfaceProp(AEntity, VPropInfo, VPID);
  end;
  Mapper.AddInTransactionPID(Result);
end;

procedure TJCoreOPFMapping.Dispose(const APID: TJCoreOPFPID);
begin
  InternalDispose(Model.AcquireMetadata(APID.Entity.ClassType), [APID.OID]);
end;

procedure TJCoreOPFMapping.DisposeFromDriver(const AMetadata: TJCoreOPFClassMetadata;
  const ADriverOID: TJCoreOPFDriver; const ACount: Integer);
var
  VOIDArray: TJCoreOPFOIDArray;
  I, VCount: Integer;
begin
  SetLength(VOIDArray, ACount);
  try
    VCount := 0;
    for I := 0 to Pred(ACount) do
    begin
      VOIDArray[VCount] := CreateOIDFromDriver(ADriverOID);
      if Assigned(VOIDArray[VCount]) then
        Inc(VCount);
    end;
    SetLength(VOIDArray, VCount);
    if VCount > 0 then
      InternalDispose(AMetadata, VOIDArray);
  finally
    for I := 0 to Pred(VCount) do
      FreeAndNil(VOIDArray[I]);
  end;
end;

procedure TJCoreOPFMapping.DisposeFromString(const AClass: TClass;
  const AOIDArray: array of string);
var
  VOIDArray: TJCoreOPFOIDArray;
  I: Integer;
begin
  SetLength(VOIDArray, Length(AOIDArray));
  try
    for I := Low(AOIDArray) to High(AOIDArray) do
      VOIDArray[I] := CreateOIDFromString(AOIDArray[I]);
    InternalDispose(Model.AcquireMetadata(AClass), VOIDArray);
  finally
    for I := Low(VOIDArray) to High(VOIDArray) do
      FreeAndNil(VOIDArray[I]);
  end;
end;

procedure TJCoreOPFMapping.RetrieveElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  InternalRetrieveElements(AOwnerPID, AOwnerADM);
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

procedure TJCoreOPFMapping.Store(const APID: TJCoreOPFPID);
begin
  if APID.IsDirty then
  begin
    InternalStore(APID);
    APID.Stored;
  end;
end;

procedure TJCoreOPFMapping.StoreElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  InternalStoreElements(AOwnerPID, AOwnerADM);
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

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinkIDsStatement(
  const ACompositionClass: TClass; const ASize: Integer): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinksStatement(
  const ACompositionClass: TClass; const ASize: Integer): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateInsertExternalLinksStatement(
  const ACompositionClass: TClass): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateSelectCompositionsForDeleteStatement(
  const AClass: TClass; const ASize: Integer): string;
begin
  raise EJCoreOPFUnsupportedSelectForDeleteOperation.Create(AClass);
end;

function TJCoreOPFSQLMapping.GenerateSelectForDeleteStatement(
  const ACompositionClass: TClass; const ASize: Integer): string;
begin
  raise EJCoreOPFUnsupportedSelectForDeleteOperation.Create(ACompositionClass);
end;

function TJCoreOPFSQLMapping.GenerateSelectListFromStatement(
  const AListBaseClass: TClass): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.ListEntityCompositionMetadatas(
  const AClassMetadata: TJCoreOPFClassMetadata): TJCoreOPFAttrMetadataArray;
var
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VCount, I: Integer;
begin
  SetLength(Result, AClassMetadata.AttributeCount);
  VCount := 0;
  for I := 0 to Pred(AClassMetadata.AttributeCount) do
  begin
    VAttrMetadata := AClassMetadata.Attributes[I];
    if (VAttrMetadata.CompositionType = jctComposition) and (VAttrMetadata.AttributeType = jatEntity) then
    begin
      Result[VCount] := VAttrMetadata;
      Inc(VCount);
    end;
  end;
  SetLength(Result, VCount);
end;

procedure TJCoreOPFSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
begin
end;

procedure TJCoreOPFSQLMapping.WriteDisposeCollectionToDriver(
  const AMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of TJCoreOPFOID);
var
  VCompositionMetadata: TJCoreOPFClassMetadata;
  VOID: TJCoreOPFOID;
  VSelectStmt: string;
  VDeleteStmt: string;
  VCount: Integer;
begin
  {
    Collections might be:
    * compositions (owned) with embedded link
      -- select external IDs
      -- delete objects
    * compositions (owned) with external link
      -- select external IDs
      -- delete links
      -- delete objects
    * aggregations (shared) with external link
      -- delete links
  }
  VCompositionMetadata := AMetadata.CompositionMetadata;
  VCount := 0;
  if AMetadata.CompositionType = jctComposition then
  begin
    // Select external IDs
    VSelectStmt := GenerateSelectForDeleteStatement(VCompositionMetadata.TheClass, Length(AOIDArray));
    for VOID in AOIDArray do
      VOID.WriteToDriver(Driver);
    VCount := Driver.ExecSQL(VSelectStmt);
  end;
  if (AMetadata.CompositionLinkType = jcltExternal) and
   ((VCount > 0) or (AMetadata.CompositionType = jctAggregation)) then
  begin
    // Delete external links
    VDeleteStmt := GenerateDeleteExternalLinksStatement(VCompositionMetadata.TheClass, Length(AOIDArray));
    for VOID in AOIDArray do
      VOID.WriteToDriver(Driver);
    Driver.ExecSQL(VDeleteStmt);
  end;
  if (VCount > 0) and (AMetadata.CompositionType = jctComposition) then
  begin
    // Delete external objects
    Mapper.DisposeFromDriver(VCompositionMetadata, Driver, VCount);
  end;
end;

procedure TJCoreOPFSQLMapping.WriteDisposeEntityCompositionsToDriver(
  const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID);
var
  VCompositionMetadatas: TJCoreOPFAttrMetadataArray;
  VOID: TJCoreOPFOID;
  VSelectStmt: string;
begin
  VCompositionMetadatas := ListEntityCompositionMetadatas(AMetadata);
  { TODO : Implement reading of more than one entity composition }
  if Length(VCompositionMetadatas) = 1 then
  begin
    VSelectStmt := GenerateSelectCompositionsForDeleteStatement(AMetadata.TheClass, Length(AOIDArray));
    for VOID in AOIDArray do
      VOID.WriteToDriver(Driver);
    Driver.ExecSQL(VSelectStmt, Length(AOIDArray));
    Mapper.DisposeFromDriver(VCompositionMetadatas[0].CompositionMetadata, Driver, Length(AOIDArray));
  end;
end;

procedure TJCoreOPFSQLMapping.WriteDisposeExternalsToDriver(
  const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID);
var
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VHasEntityComposition: Boolean;
  I: Integer;
begin
  VHasEntityComposition := False;
  for I := 0 to Pred(AMetadata.AttributeCount) do
  begin
    VAttrMetadata := AMetadata.Attributes[I];
    if VAttrMetadata.AttributeType = jatCollection then
      WriteDisposeCollectionToDriver(VAttrMetadata, AOIDArray)
    else if (VAttrMetadata.AttributeType = jatEntity) and (VAttrMetadata.CompositionType = jctComposition) then
      VHasEntityComposition := True;
  end;
  if VHasEntityComposition then
    WriteDisposeEntityCompositionsToDriver(AMetadata, AOIDArray);
end;

procedure TJCoreOPFSQLMapping.WriteDisposeToDriver(
  const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID);
var
  VOID: TJCoreOPFOID;
begin
  for VOID in AOIDArray do
    VOID.WriteToDriver(Driver);
  Driver.ExecSQL(GenerateDeleteStatement(Length(AOIDArray)));
end;

procedure TJCoreOPFSQLMapping.WriteExternalLinksToDriver(
  const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
var
  VInsertStmt: string;
  VPIDs: TJCoreOPFPIDArray;
  VPID: TJCoreOPFPID;
begin
  VInsertStmt := GenerateInsertExternalLinksStatement(AADM.Metadata.CompositionClass);
  VPIDs := AADM.PIDAdded;
  for VPID in VPIDs do
  begin
    AOwnerPID.OID.WriteToDriver(Driver);
    VPID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(VInsertStmt);
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

procedure TJCoreOPFSQLMapping.InternalDispose(
  const AMetadata: TJCoreOPFClassMetadata; const AOIDArray: array of TJCoreOPFOID);
begin
  WriteDisposeExternalsToDriver(AMetadata, AOIDArray);
  WriteDisposeToDriver(AMetadata, AOIDArray);
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

procedure TJCoreOPFSQLMapping.InternalRetrieveElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
var
  VElementsArray: TJCoreObjectArray;
  VListClass: TClass;
  VOID: TJCoreOPFOID;
  VPID: TJCoreOPFPID;
  VCount: Integer;
  I: Integer;
begin
  AOwnerPID.OID.WriteToDriver(Driver);
  VListClass := AOwnerADM.Metadata.CompositionClass;
  VCount := Driver.ExecSQL(GenerateSelectListFromStatement(VListClass));
  SetLength(VElementsArray, VCount);
  try
    for I := Low(VElementsArray) to High(VElementsArray) do
    begin
      VElementsArray[I] := CreateEntity(VListClass);
      VOID := CreateOIDFromDriver(Driver);
      try
        VPID := AcquirePID(VElementsArray[I]);
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
    AOwnerADM.AssignArray(VElementsArray);
  except
    for I := Low(VElementsArray) to High(VElementsArray) do
      FreeAndNil(VElementsArray[I]);
    raise;
  end;
end;

procedure TJCoreOPFSQLMapping.InternalRetrieveList(const APID: TJCoreOPFPID;
  const AADM: TJCoreOPFADMCollection);
begin
  Mapper.RetrieveElements(APID, AADM);
end;

procedure TJCoreOPFSQLMapping.InternalStore(const APID: TJCoreOPFPID);
begin
  if not APID.IsPersistent then
  begin
    APID.AssignOID(CreateOIDFromString(''));
    APID.OID.WriteToDriver(Driver);
    WriteInternalsToDriver(APID);
    Driver.ExecSQL(GenerateInsertStatement(APID));
  end else if APID.IsInternalsDirty then
  begin
    WriteInternalsToDriver(APID);
    APID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateUpdateStatement(APID));
  end;
  WriteExternalsToDriver(APID);
end;

procedure TJCoreOPFSQLMapping.InternalStoreElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
var
  VOIDs: TJCoreOPFOIDArray;
  VOID: TJCoreOPFOID;
  VPIDs: TJCoreOPFPIDArray;
  VPID: TJCoreOPFPID;
begin
  if AOwnerADM.Metadata.CompositionType = jctComposition then
  begin
    VOIDs := AOwnerADM.OIDRemoved;
    if Length(VOIDs) > 0 then
    begin
      for VOID in VOIDs do
        VOID.WriteToDriver(Driver);
      Driver.ExecSQL(GenerateDeleteStatement(Length(VOIDs)));
    end;
  end;

  VPIDs := AOwnerADM.PIDArray;
  for VPID in VPIDs do
  begin
    VPID.AssignOwner(AOwnerPID, AOwnerADM);
    if (AOwnerADM.Metadata.CompositionType = jctComposition) or
     not VPID.IsPersistent then
    begin
      if Apply(VPID.Entity.ClassType) then
        Store(VPID)
      else
        Mapper.StorePID(VPID);
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.InternalStoreList(const APID: TJCoreOPFPID;
  const AADM: TJCoreOPFADMCollection);
var
  VMetadata: TJCoreOPFAttrMetadata;
  VOIDs: TJCoreOPFOIDArray;
  VOID: TJCoreOPFOID;
begin
  VMetadata := AADM.Metadata;

  // remove old links
  if (VMetadata.CompositionLinkType = jcltExternal) and APID.IsPersistent then
  begin
    VOIDs := AADM.OIDRemoved;
    if Length(VOIDs) > 0 then
    begin
      APID.OID.WriteToDriver(Driver);
      for VOID in VOIDs do
        VOID.WriteToDriver(Driver);
      Driver.ExecSQL(GenerateDeleteExternalLinkIDsStatement(VMetadata.CompositionClass, Length(VOIDs)));
    end;
  end;

  // update items
  Mapper.StoreElements(APID, AADM);

  // add new links
  if VMetadata.CompositionLinkType = jcltExternal then
    WriteExternalLinksToDriver(APID, AADM);
end;

procedure TJCoreOPFSQLMapping.RetrieveList(const APID: TJCoreOPFPID;
  const AAttributeName: string);
var
  VADM: TJCoreOPFADMCollection;
begin
  VADM := EnsureCollectionAttribute(APID, AAttributeName);
  InternalRetrieveList(APID, VADM);
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

