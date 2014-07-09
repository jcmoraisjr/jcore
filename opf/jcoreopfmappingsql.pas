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
  protected
    //// Support ////
    procedure WriteDisposeCollectionToDriver(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of TJCoreOPFOID);
    procedure WriteDisposeEntityCompositionsToDriver(const AOIDArray: array of TJCoreOPFOID);
    procedure WriteDisposeExternalLinksToDriver(const AOwnerMapping: TJCoreOPFADMMapping; const AADM: TJCoreOPFADMCollection);
    procedure WriteInsertExternalLinksToDriver(const AOwnerMapping: TJCoreOPFADMMapping; const AADM: TJCoreOPFADMCollection);
    function BuildFieldParams(const AFieldCount: Integer): string;
    function BuildOIDCondition(const AOIDNameArray: array of string; const AOIDCount: Integer): string;
    property Driver: TJCoreOPFSQLDriver read FSQLDriver;
  protected
    //// Facade internals ////
    procedure InternalDispose(const AOIDArray: array of TJCoreOPFOID); override;
    procedure InternalInsert(const AMapping: TJCoreOPFADMMapping); override;
    function InternalRetrieveCollectionToDriver(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection): Integer; override;
    procedure InternalRetrieveEntityToDriver(const AOIDArray: array of TJCoreOPFOID; const ABaseMap: TJCoreOPFMap); override;
    procedure InternalUpdate(const AMapping: TJCoreOPFADMMapping); override;
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

  { TJCoreOPFSQLAutoMapping }

  TJCoreOPFSQLAutoMapping = class(TJCoreOPFSQLMapping)
  private
    function BuildDeleteCondition(const AOIDCount: Integer): string;
    function BuildInsertFieldNames(const AMapping: TJCoreOPFADMMapping): string;
    function BuildInsertParamNames(const AMapping: TJCoreOPFADMMapping): string;
    function BuildSelectCondition: string;
    function BuildSelectFieldNames: string;
    function BuildSelectFrom: string;
    function BuildUpdateCondition(const AMapping: TJCoreOPFADMMapping): string;
    function BuildUpdateNames(const AMapping: TJCoreOPFADMMapping): string;
  protected
    function CreateEntityFromDriver: TObject; override;
    // Mandatory sql generators
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectStatement(const ABaseMap: TJCoreOPFMap; const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    // Composition/Collection related sql generators
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateSelectCompositionsForDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    // direct attribute <-> field mapping
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

implementation

uses
  sysutils,
  Classes,
  JCoreClasses,
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

function TJCoreOPFSQLMapping.BuildFieldParams(const AFieldCount: Integer): string;
var
  VFieldParams: PChar;
  I: Integer;
begin
  if AFieldCount > 0 then
  begin
    SetLength(Result, AFieldCount * 2 - 1);
    VFieldParams := PChar(Result);
    VFieldParams[0] := '?';
    for I := 1 to Pred(AFieldCount) do
    begin
      VFieldParams[2*I-1] := ',';
      VFieldParams[2*I] := '?';
    end;
  end else
    Result := '';
end;

function TJCoreOPFSQLMapping.BuildOIDCondition(const AOIDNameArray: array of string;
  const AOIDCount: Integer): string;
var
  VOIDClause: string;
  I: Integer;
begin
  { TODO : allocate once, at the start }
  if Length(AOIDNameArray) = 1 then
  begin
    if AOIDCount > 1 then
    begin
      Result := AOIDNameArray[0] + ' IN (?';
      for I := 2 to AOIDCount do
        Result := Result + ',?';
      Result := Result + ')';
    end else if AOIDCount = 1 then
      Result := AOIDNameArray[0] + '=?'
    else
      Result := '';
  end else
  begin
    VOIDClause := '(';
    for I := Low(AOIDNameArray) to High(AOIDNameArray) do
      VOIDClause := AOIDNameArray[I] + '=? AND ';
    SetLength(VOIDClause, Length(VOIDClause) - 4);
    VOIDClause[Length(VOIDClause)] := ')';
    Result := '';
    for I := 0 to Pred(AOIDCount) do
      Result := Result + VOIDClause + ' OR ';
    SetLength(Result, Length(Result) - 4);
  end;
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

procedure TJCoreOPFSQLMapping.InternalInsert(const AMapping: TJCoreOPFADMMapping);
begin
  Driver.ExecSQL(GenerateInsertStatement(AMapping));
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

procedure TJCoreOPFSQLMapping.InternalUpdate(const AMapping: TJCoreOPFADMMapping);
begin
  Driver.ExecSQL(GenerateUpdateStatement(AMapping));
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

{ TJCoreOPFSQLAutoMapping }

function TJCoreOPFSQLAutoMapping.BuildDeleteCondition(const AOIDCount: Integer): string;
begin
  Result := BuildOIDCondition(Map.OIDName, AOIDCount);
end;

function TJCoreOPFSQLAutoMapping.BuildInsertFieldNames(const AMapping: TJCoreOPFADMMapping): string;
var
  VOIDName: TJCoreStringArray;
  VADMChanged: TJCoreOPFADMArray;
  I: Integer;
begin
  VOIDName := Map.OIDName;
  Result := '';
  for I := Low(VOIDName) to High(VOIDName) do
    Result := Result + VOIDName[I] + ',';
  VADMChanged := AMapping.ADMChanged;
  for I := Low(VADMChanged) to High(VADMChanged) do
    Result := Result + VADMChanged[I].Metadata.PersistentFieldName + ',';
  SetLength(Result, Length(Result) - 1);
end;

function TJCoreOPFSQLAutoMapping.BuildInsertParamNames(const AMapping: TJCoreOPFADMMapping): string;
begin
  // Insert use "admChanged" because there is a single "writeToDriver" method,
  // shared between inserts and updates.
  Result := BuildFieldParams(Length(Map.OIDName) + Length(AMapping.ADMChanged));
end;

function TJCoreOPFSQLAutoMapping.BuildSelectCondition: string;
begin
  Result := BuildOIDCondition(Map.OIDName, 1);
end;

function TJCoreOPFSQLAutoMapping.BuildSelectFieldNames: string;
var
  VOIDName: TJCoreStringArray;
  I: Integer;
begin
  VOIDName := Map.OIDName;
  Result := '';
  for I := Low(VOIDName) to High(VOIDName) do
    Result := Result + VOIDName[I] + ',';
  for I := 0 to Pred(Map.Count) do
    Result := Result + Map[I].PersistentFieldName + ',';
  SetLength(Result, Length(Result) - 1);
end;

function TJCoreOPFSQLAutoMapping.BuildSelectFrom: string;
begin
  Result := Map.TableName;
end;

function TJCoreOPFSQLAutoMapping.BuildUpdateCondition(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := BuildOIDCondition(Map.OIDName, 1);
end;

function TJCoreOPFSQLAutoMapping.BuildUpdateNames(const AMapping: TJCoreOPFADMMapping): string;
var
  VADMChanged: TJCoreOPFADMArray;
  I: Integer;
begin
  VADMChanged := AMapping.ADMChanged;
  Result := '';
  for I := Low(VADMChanged) to High(VADMChanged) do
    Result := Result + VADMChanged[I].Metadata.PersistentFieldName + '=?,';
  SetLength(Result, Length(Result)-1);
end;

function TJCoreOPFSQLAutoMapping.CreateEntityFromDriver: TObject;
begin
  { TODO : Implement }
  Result := Map.Metadata.TheClass.Create;
end;

function TJCoreOPFSQLAutoMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := Format('DELETE FROM %s WHERE %s', [Map.TableName, BuildDeleteCondition(AOIDCount)]);
end;

function TJCoreOPFSQLAutoMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := Format('INSERT INTO %s (%s) VALUES (%s)', [
   Map.TableName, BuildInsertFieldNames(AMapping), BuildInsertParamNames(AMapping)]);
end;

function TJCoreOPFSQLAutoMapping.GenerateSelectStatement(const ABaseMap: TJCoreOPFMap;
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
begin
  Result := Format('SELECT %s FROM %s WHERE %s', [
   BuildSelectFieldNames(), BuildSelectFrom(), BuildSelectCondition()]);
end;

function TJCoreOPFSQLAutoMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := Format('UPDATE %s SET %s WHERE %s', [
   Map.TableName, BuildUpdateNames(AMapping), BuildUpdateCondition(AMapping)]);
end;

function TJCoreOPFSQLAutoMapping.GenerateDeleteExternalLinkIDsStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  { TODO : Implement }
  Result := 'DELETE';
end;

function TJCoreOPFSQLAutoMapping.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  { TODO : Implement }
  Result := 'DELETE';
end;

function TJCoreOPFSQLAutoMapping.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  { TODO : Implement }
  Result := 'INSERT';
end;

function TJCoreOPFSQLAutoMapping.GenerateSelectCompositionsForDeleteStatement(
  const AOIDCount: Integer): string;
begin
  { TODO : Implement }
  Result := 'SELECT';
end;

function TJCoreOPFSQLAutoMapping.GenerateSelectForDeleteStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  { TODO : Implement }
  Result := 'SELECT';
end;

procedure TJCoreOPFSQLAutoMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  I: Integer;
begin
  for I := 0 to Pred(AMapping.Count) do
    AMapping[I].ReadFromDriver(Driver);
end;

procedure TJCoreOPFSQLAutoMapping.WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  { TODO : Implement }
end;

procedure TJCoreOPFSQLAutoMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VADMChanged: TJCoreOPFADMArray;
  I: Integer;
begin
  VADMChanged := AMapping.ADMChanged;
  for I := Low(VADMChanged) to High(VADMChanged) do
    VADMChanged[I].WriteToDriver(Driver);
end;

class function TJCoreOPFSQLAutoMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := True;
end;

end.

