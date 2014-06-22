(*
  JCore, OPF SQL Mapping Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFMappingSQL;

{$I jcore.inc}

interface

uses
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMetadata,
  JCoreOPFMapping;

type

  { TJCoreOPFSQLMapping }

  TJCoreOPFSQLMapping = class(TJCoreOPFMapping)
  private
    FSQLDriver: TJCoreOPFSQLDriver;
  protected
    //// Abstract entry points ////
    // Mandatory sql generators
    function GenerateDeleteStatement(const AOIDCount: Integer): string; virtual;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; virtual;
    function GenerateSelectStatement(const ABaseMap: TJCoreOPFMap; const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; virtual;
    // Composition/Collection related sql generators
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; virtual;
    function GenerateSelectCompositionsForDeleteStatement(const AOIDCount: Integer): string; virtual;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    // direct attribute -> field mapping
    procedure WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping); virtual;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); virtual;
  protected
    //// Support ////
    procedure WriteDisposeCollectionToDriver(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of TJCoreOPFOID);
    procedure WriteDisposeEntityCompositionsToDriver(const AOIDArray: array of TJCoreOPFOID);
    procedure WriteDisposeExternalLinksToDriver(const AOwnerMapping: TJCoreOPFADMMapping; const AADM: TJCoreOPFADMCollection);
    procedure WriteInsertExternalLinksToDriver(const AOwnerMapping: TJCoreOPFADMMapping; const AADM: TJCoreOPFADMCollection);
    function BuildParams(const AOIDCount: Integer): string;
    property Driver: TJCoreOPFSQLDriver read FSQLDriver;
  protected
    //// Facade internals ////
    procedure InternalDispose(const AOIDArray: array of TJCoreOPFOID); override;
    function InternalRetrieveCollectionToDriver(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection): Integer; override;
    procedure InternalRetrieveEntityToDriver(const AOIDArray: array of TJCoreOPFOID; const ABaseMap: TJCoreOPFMap); override;
    procedure InternalStore(const AMapping: TJCoreOPFADMMapping); override;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap); override;
  end;

  { TJCoreOPFSQLManualMapping }

  TJCoreOPFSQLManualMapping = class(TJCoreOPFSQLMapping)
  protected
    function EnsureCollectionAttribute(const AMapping: TJCoreOPFADMMapping; const AAttributeName: string): TJCoreOPFADMCollection;
    function EnsureCollectionAttribute(const APID: TJCoreOPFPID; const AAttributeName: string): TJCoreOPFADMCollection;
    function EnsureEntityAttribute(const AMapping: TJCoreOPFADMMapping; const AAttributeName: string): TJCoreOPFADMEntity;
    function EnsureEntityAttribute(const APID: TJCoreOPFPID; const AAttributeName: string): TJCoreOPFADMEntity;
  protected
    procedure ReadCollection(const APID: TJCoreOPFPID; const AAttributeName: string);
    function ReadEntity(const AClass: TClass): TObject;
    procedure ReadLazyEntity(const APID: TJCoreOPFPID; const AAttributeName: string);
    procedure WriteCollection(const AMapping: TJCoreOPFADMMapping; const AAttributeName: string);
    procedure WriteEntity(const AClass: TClass; const AEntity: TObject);
    procedure WriteOwnerOIDToDriver(const AMapping: TJCoreOPFADMMapping);
  end;

implementation

uses
  JCoreMetadata,
  JCoreOPFException;

{ TJCoreOPFSQLMapping }

{$warn 5033 off}
function TJCoreOPFSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'delete');
end;

function TJCoreOPFSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'insert');
end;

function TJCoreOPFSQLMapping.GenerateSelectStatement(const ABaseMap: TJCoreOPFMap;
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'select');
end;

function TJCoreOPFSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'update');
end;

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinkIDsStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'delete external link IDs');
end;

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'delete external links');
end;

function TJCoreOPFSQLMapping.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'insert external links');
end;

function TJCoreOPFSQLMapping.GenerateSelectCompositionsForDeleteStatement(const AOIDCount: Integer): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'select compositions for delete');
end;

function TJCoreOPFSQLMapping.GenerateSelectForDeleteStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  raise EJCoreOPFUnsupportedDMLOperation.Create(Map.Metadata.TheClass, 'select for delete');
end;
{$warn 5033 on}

procedure TJCoreOPFSQLMapping.WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
end;

procedure TJCoreOPFSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
end;

procedure TJCoreOPFSQLMapping.WriteDisposeCollectionToDriver(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of TJCoreOPFOID);
var
  VOID: TJCoreOPFOID;
  VSelectStmt: string;
  VDeleteStmt: string;
  VOIDCount: Integer;
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
  VOIDCount := 0;
  if AAttrMetadata.CompositionType = jctComposition then
  begin
    // Select external IDs
    VSelectStmt := GenerateSelectForDeleteStatement(AAttrMetadata, Length(AOIDArray));
    for VOID in AOIDArray do
      VOID.WriteToDriver(Driver);
    VOIDCount := Driver.ExecSQL(VSelectStmt);
  end;
  if (AAttrMetadata.CompositionLinkType = jcltExternal) and
   ((VOIDCount > 0) or (AAttrMetadata.CompositionType = jctAggregation)) then
  begin
    // Delete external links
    VDeleteStmt := GenerateDeleteExternalLinksStatement(AAttrMetadata, Length(AOIDArray));
    for VOID in AOIDArray do
      VOID.WriteToDriver(Driver);
    Driver.ExecSQL(VDeleteStmt);
  end;
  if (VOIDCount > 0) and (AAttrMetadata.CompositionType = jctComposition) then
  begin
    // Delete external objects
    Mapper.AcquireClassMapping(AAttrMetadata.CompositionClass).DisposeFromDriverInternal(VOIDCount);
  end;
end;

procedure TJCoreOPFSQLMapping.WriteDisposeEntityCompositionsToDriver(
  const AOIDArray: array of TJCoreOPFOID);
var
  VClassMetadata: TJCoreOPFClassMetadata;
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VCompositionMetadatas: TJCoreOPFAttrMetadataArray;
  VCompositionMetadata: TJCoreOPFClassMetadata;
  VOID: TJCoreOPFOID;
  VSelectStmt: string;
  VCount: Integer;
  I: Integer;
begin
  VClassMetadata := Map.Metadata;
  SetLength(VCompositionMetadatas, VClassMetadata.AttributeCount);
  VCount := 0;
  for I := 0 to Pred(VClassMetadata.AttributeCount) do
  begin
    VAttrMetadata := VClassMetadata.Attributes[I];
    if (VAttrMetadata.CompositionType = jctComposition) and (VAttrMetadata.AttributeType = jatEntity) then
    begin
      VCompositionMetadatas[VCount] := VAttrMetadata;
      Inc(VCount);
    end;
  end;
  SetLength(VCompositionMetadatas, VCount);
  { TODO : Implement reading of more than one entity composition }
  if Length(VCompositionMetadatas) = 1 then
  begin
    VSelectStmt := GenerateSelectCompositionsForDeleteStatement(Length(AOIDArray));
    for VOID in AOIDArray do
      VOID.WriteToDriver(Driver);
    Driver.ExecSQL(VSelectStmt, Length(AOIDArray));
    VCompositionMetadata := VCompositionMetadatas[0].CompositionMetadata;
    Mapper.AcquireClassMapping(VCompositionMetadata.TheClass).DisposeFromDriverInternal(Length(AOIDArray));
  end;
end;

procedure TJCoreOPFSQLMapping.WriteDisposeExternalLinksToDriver(
  const AOwnerMapping: TJCoreOPFADMMapping; const AADM: TJCoreOPFADMCollection);
var
  VOIDs: TJCoreOPFOIDArray;
  VOID: TJCoreOPFOID;
begin
  if (AADM.Metadata.CompositionLinkType = jcltExternal) and AOwnerMapping.PID.IsPersistent then
  begin
    VOIDs := AADM.OIDRemoved;
    if Length(VOIDs) > 0 then
    begin
      AOwnerMapping.PID.OID.WriteToDriver(Driver);
      for VOID in VOIDs do
        VOID.WriteToDriver(Driver);
      Driver.ExecSQL(GenerateDeleteExternalLinkIDsStatement(AADM.Metadata, Length(VOIDs)));
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.WriteInsertExternalLinksToDriver(
  const AOwnerMapping: TJCoreOPFADMMapping; const AADM: TJCoreOPFADMCollection);
var
  VInsertStmt: string;
  VPIDs: TJCoreOPFPIDArray;
  VPID: TJCoreOPFPID;
begin
  if AADM.Metadata.CompositionLinkType = jcltExternal then
  begin
    VInsertStmt := GenerateInsertExternalLinksStatement(AADM.Metadata);
    VPIDs := AADM.PIDAdded;
    for VPID in VPIDs do
    begin
      AOwnerMapping.PID.OID.WriteToDriver(Driver);
      VPID.OID.WriteToDriver(Driver);
      Driver.ExecSQL(VInsertStmt);
    end;
  end;
end;

function TJCoreOPFSQLMapping.BuildParams(const AOIDCount: Integer): string;
var
  I: Integer;
begin
  if AOIDCount > 1 then
  begin
    { TODO : allocate once, at the start }
    Result := ' IN (?';
    for I := 2 to AOIDCount do
      Result := Result + ',?';
    Result := Result + ')';
  end else if AOIDCount = 1 then
    Result := '=?'
  else
    Result := '';
end;

procedure TJCoreOPFSQLMapping.InternalDispose(const AOIDArray: array of TJCoreOPFOID);
var
  VClassMetadata: TJCoreOPFClassMetadata;
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VHasEntityComposition: Boolean;
  I: Integer;
begin
  VClassMetadata := Map.Metadata;
  VHasEntityComposition := False;
  for I := 0 to Pred(VClassMetadata.AttributeCount) do
  begin
    VAttrMetadata := VClassMetadata.Attributes[I];
    if VAttrMetadata.AttributeType = jatCollection then
      WriteDisposeCollectionToDriver(VAttrMetadata, AOIDArray)
    else if (VAttrMetadata.AttributeType = jatEntity) and (VAttrMetadata.CompositionType = jctComposition) then
      VHasEntityComposition := True;
  end;
  if VHasEntityComposition then
    WriteDisposeEntityCompositionsToDriver(AOIDArray);
  for I := Low(AOIDArray) to High(AOIDArray) do
    AOIDArray[I].WriteToDriver(Driver);
  Driver.ExecSQL(GenerateDeleteStatement(Length(AOIDArray)));
end;

function TJCoreOPFSQLMapping.InternalRetrieveCollectionToDriver(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection): Integer;
begin
  AOwnerPID.OID.WriteToDriver(Driver);
  Result := Driver.ExecSQL(GenerateSelectStatement(nil, AOwnerPID.Metadata, AOwnerADM.Metadata, 1));
end;

procedure TJCoreOPFSQLMapping.InternalRetrieveEntityToDriver(const AOIDArray: array of TJCoreOPFOID;
  const ABaseMap: TJCoreOPFMap);
var
  VOID: TJCoreOPFOID;
begin
  for VOID in AOIDArray do
    VOID.WriteToDriver(Driver);
  Driver.ExecSQL(GenerateSelectStatement(ABaseMap, nil, nil, Length(AOIDArray)), Length(AOIDArray));
end;

procedure TJCoreOPFSQLMapping.InternalStore(const AMapping: TJCoreOPFADMMapping);
var
  VPID: TJCoreOPFPID;
begin
  VPID := AMapping.PID;
  if not VPID.IsPersistent then
  begin
    VPID.OID.WriteToDriver(Driver);
    WriteInternalsToDriver(AMapping);
    Driver.ExecSQL(GenerateInsertStatement(AMapping));
  end else if AMapping.IsInternalsDirty then
  begin
    WriteInternalsToDriver(AMapping);
    VPID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateUpdateStatement(AMapping));
  end;
  WriteExternalsToDriver(AMapping);
end;

constructor TJCoreOPFSQLMapping.Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap);
var
  VDriver: TJCoreOPFDriver;
begin
  VDriver := AMapper.Driver;
  if not (VDriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPFUnsupportedDriver.Create(VDriver.ClassName);
  inherited Create(AMapper, AMap);
  FSQLDriver := TJCoreOPFSQLDriver(VDriver);
end;

{ TJCoreOPFSQLManualMapping }

function TJCoreOPFSQLManualMapping.EnsureCollectionAttribute(const AMapping: TJCoreOPFADMMapping;
  const AAttributeName: string): TJCoreOPFADMCollection;
begin
  Result := EnsureCollectionAttribute(AMapping.PID, AAttributeName);
end;

function TJCoreOPFSQLManualMapping.EnsureCollectionAttribute(const APID: TJCoreOPFPID;
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

function TJCoreOPFSQLManualMapping.EnsureEntityAttribute(const AMapping: TJCoreOPFADMMapping;
  const AAttributeName: string): TJCoreOPFADMEntity;
begin
  Result := EnsureEntityAttribute(AMapping.PID, AAttributeName);
end;

function TJCoreOPFSQLManualMapping.EnsureEntityAttribute(const APID: TJCoreOPFPID;
  const AAttributeName: string): TJCoreOPFADMEntity;
var
  VADM: TJCoreOPFADM;
begin
  VADM := APID.AcquireADM(AAttributeName);
  if not (VADM is TJCoreOPFADMEntity) then
    raise EJCoreOPFEntityADMExpected.Create(APID.Entity.ClassName, AAttributeName);
  Result := TJCoreOPFADMEntity(VADM);
end;

procedure TJCoreOPFSQLManualMapping.ReadCollection(const APID: TJCoreOPFPID; const AAttributeName: string);
var
  VADM: TJCoreOPFADMCollection;
begin
  VADM := EnsureCollectionAttribute(APID, AAttributeName);
  Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).RetrieveCollectionInternal(APID, VADM);
end;

function TJCoreOPFSQLManualMapping.ReadEntity(const AClass: TClass): TObject;
begin
  Result := Mapper.AcquireClassMapping(AClass).RetrieveEntityFromDriverInternal;
end;

procedure TJCoreOPFSQLManualMapping.ReadLazyEntity(const APID: TJCoreOPFPID; const AAttributeName: string);
var
  VADM: TJCoreOPFADMEntity;
begin
  VADM := EnsureEntityAttribute(APID, AAttributeName);
  Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).RetrieveLazyEntityFromDriverInternal(VADM);
end;

procedure TJCoreOPFSQLManualMapping.WriteCollection(const AMapping: TJCoreOPFADMMapping;
  const AAttributeName: string);
var
  VADM: TJCoreOPFADMCollection;
begin
  { TODO : Improve the change analyzer }
  VADM := EnsureCollectionAttribute(AMapping, AAttributeName);
  if VADM.IsDirty then
  begin
    WriteDisposeExternalLinksToDriver(AMapping, VADM);
    Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).StoreCollectionInternal(AMapping, VADM);
    WriteInsertExternalLinksToDriver(AMapping, VADM);
  end;
end;

procedure TJCoreOPFSQLManualMapping.WriteEntity(const AClass: TClass; const AEntity: TObject);
var
  VPID: TJCoreOPFPID;
begin
  if Assigned(AEntity) then
  begin
    VPID := Mapper.AcquirePID(AEntity);
    Mapper.AcquireClassMapping(AEntity.ClassType).StorePID(VPID);
    VPID.OID.WriteToDriver(Driver);
  end else
    Model.AcquireMetadata(AClass).OIDClass.WriteNull(Driver);
end;

procedure TJCoreOPFSQLManualMapping.WriteOwnerOIDToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  AMapping.PID.Owner.OID.WriteToDriver(Driver);
end;

end.

