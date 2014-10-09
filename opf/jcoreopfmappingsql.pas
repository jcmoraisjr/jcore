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
  JCoreClasses,
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMetadata,
  JCoreOPFMapping;

type

  TJCoreOPFTablePrefixType = (jtptNone, jtptMainMap, jtptSubMap);

  { TJCoreOPFSQLGenerator }

  TJCoreOPFSQLGenerator = class(TObject)
  private
    FMap: TJCoreOPFMap;
    FMapIndex: Integer;
    FMaps: TJCoreOPFMaps;
    FSubMaps: TJCoreOPFMaps;
  protected
    // Internal Support
    function BuildFieldName(const AMaps: TJCoreOPFMaps; const AMapIndex, AFieldIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildFieldNames(const ABaseMapIdx: Integer; const AUseTablePrefix: Boolean): string;
    function BuildInsertFields(const AMapping: TJCoreOPFADMMapping): TJCoreStringArray; virtual;
    function BuildOIDName(const AMaps: TJCoreOPFMaps; const AMapIndex, AOIDIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDName(const AOIDNameArray: array of string; const AMapIndex, AOIDIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDNames(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildTableName(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function MapIndexByMap(const AMap: TJCoreOPFMap): Integer;
  protected
    // SQL fragments
    function BuildDeleteCondition(const AOIDCount: Integer): string; virtual;
    function BuildInsertFieldNames(const AFields: TJCoreStringArray): string; virtual;
    function BuildSelectBaseFieldNames(const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectBaseFrom(const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectComplementaryFieldNames(const ABaseMapIdx: Integer; const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectComplementaryFrom(const ABaseMapIdx: Integer; const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectCondition(const AOIDCount: Integer; const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectFieldNames(const AAttributes: TJCoreOPFAttrMetadataArray): string;
    function BuildSelectJoinCondition(const ALeftMaps: TJCoreOPFMaps; const ALeftIndex: Integer; const ALeftTablePrefixType: TJCoreOPFTablePrefixType; const ARightMaps: TJCoreOPFMaps; const ARightIndex: Integer; const ARightTablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildUpdateCondition(const AMapping: TJCoreOPFADMMapping): string; virtual;
    function BuildUpdateNames(const AMapping: TJCoreOPFADMMapping): string; virtual;
    property Map: TJCoreOPFMap read FMap;
    property MapIndex: Integer read FMapIndex;
    property Maps: TJCoreOPFMaps read FMaps;
    property SubMaps: TJCoreOPFMaps read FSubMaps;
  public
    constructor Create(const AMap: TJCoreOPFMap); virtual;
    function GenerateDeleteStatement(const AOIDCount: Integer): string; virtual;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; virtual;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; virtual;
    function GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap; const AOIDCount: Integer): string; virtual;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; virtual;
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; virtual;
    function GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string; virtual;
    function GenerateSelectCompositionsForDeleteStatement(const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDCount: Integer): string; virtual;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    // Support
    function BuildFieldParams(const AFieldCount: Integer): string; virtual;
    function BuildOIDCondition(const AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDCondition(const AMaps: TJCoreOPFMaps; const AMapIndex, AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDCondition(const AOIDNameArray: array of string; const AOIDCount: Integer): string; virtual;
    function BuildOIDCondition(const AOIDNameArray: array of string; const AMapIndex, AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
  end;

  TJCoreOPFSQLGeneratorClass = class of TJCoreOPFSQLGenerator;

  { TJCoreOPFSQLMapping }

  TJCoreOPFSQLMapping = class(TJCoreOPFMapping)
  private
    FSQLDriver: TJCoreOPFSQLDriver;
    FSQLGenerator: TJCoreOPFSQLGenerator;
  protected
    function CreateEntityFromDriver: TObject; override;
    function SQLGeneratorClass: TJCoreOPFSQLGeneratorClass; virtual;
    // Mandatory sql generators
    function GenerateDeleteStatement(const AOIDCount: Integer): string; virtual;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; virtual;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; virtual;
    function GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap; const AOIDCount: Integer): string; virtual;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; virtual;
    // Composition/Collection related sql generators
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; virtual;
    function GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string; virtual;
    function GenerateSelectCompositionsForDeleteStatement(const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDCount: Integer): string; virtual;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
  protected
    // Support
    procedure WriteDisposeCollectionToDriver(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of TJCoreOPFOID);
    procedure WriteDisposeEntityCompositionsToDriver(const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDArray: array of TJCoreOPFOID);
    procedure WriteDisposeExternalLinksToDriver(const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
    procedure WriteInsertExternalLinksToDriver(const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
    property Driver: TJCoreOPFSQLDriver read FSQLDriver;
    property SQLGenerator: TJCoreOPFSQLGenerator read FSQLGenerator;
  protected
    // Facade internals
    procedure InternalDispose(const AOIDArray: array of TJCoreOPFOID); override;
    procedure InternalInsert(const AMapping: TJCoreOPFADMMapping); override;
    function InternalRetrieveCollectionToDriver(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection): Integer; override;
    procedure InternalRetrieveEntityToDriver(const AOIDArray: array of TJCoreOPFOID; const ABaseMap: TJCoreOPFMap); override;
    procedure InternalUpdate(const AMapping: TJCoreOPFADMMapping); override;
    // Direct attribute <-> field mapping
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteCollectionsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  protected
    // Manual mapping helpers
    function BuildOIDCondition(const AOIDNameArray: array of string; const AOIDCount: Integer): string;
    function EnsureCollectionAttribute(const AADM: TJCoreOPFADM): TJCoreOPFADMCollection;
    function EnsureEntityAttribute(const AADM: TJCoreOPFADM): TJCoreOPFADMEntity;
    procedure ReadCollection(const AADM: TJCoreOPFADM);
    function ReadEntity(const AClass: TClass): TObject;
    procedure ReadLazyEntity(const AADM: TJCoreOPFADM);
    procedure WriteCollection(const AADM: TJCoreOPFADM);
    procedure WriteEntity(const AClass: TClass; const AEntity: TObject; const AComposition: Boolean);
    procedure WriteEntity(const AADM: TJCoreOPFADM);
    procedure WriteOwnerOID(const AMapping: TJCoreOPFADMMapping);
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap); override;
    destructor Destroy; override;
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

implementation

uses
  sysutils,
  Classes,
  JCoreMetadata,
  JCoreOPFException;

{ TJCoreOPFSQLGenerator }

const
  { TODO : Review the aproach after "subclass type strategy" implementation }
  CTablePrefix: array[TJCoreOPFTablePrefixType] of string = ('', 'T_', 'TS_');
  CMainMapPrefix: array[Boolean] of TJCoreOPFTablePrefixType = (jtptNone, jtptMainMap);
  CSubMapPrefix: array[Boolean] of TJCoreOPFTablePrefixType = (jtptNone, jtptSubMap);

function TJCoreOPFSQLGenerator.BuildFieldName(const AMaps: TJCoreOPFMaps; const AMapIndex,
  AFieldIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string;
var
  VFieldName: string;
begin
  VFieldName := AMaps[AMapIndex][AFieldIndex].PersistentFieldName;
  if ATablePrefixType <> jtptNone then
    Result := Format('%s%d.%s', [CTablePrefix[ATablePrefixType], AMapIndex, VFieldName])
  else
    Result := VFieldName;
end;

function TJCoreOPFSQLGenerator.BuildFieldNames(const ABaseMapIdx: Integer;
  const AUseTablePrefix: Boolean): string;
var
  VMap: TJCoreOPFMap;
  I, J: Integer;
begin
  Result := '';
  for I := ABaseMapIdx to Pred(Maps.Count) do
  begin
    VMap := Maps[I];
    for J := 0 to Pred(VMap.Count) do
      if VMap[J].AttributeType <> jatCollection then
        Result := Result + BuildFieldName(Maps, I, J, CMainMapPrefix[AUseTablePrefix]) + ',';
  end;
end;

function TJCoreOPFSQLGenerator.BuildInsertFields(const AMapping: TJCoreOPFADMMapping): TJCoreStringArray;
var
  VOIDName: TJCoreStringArray;
  VOwnerOIDName: TJCoreStringArray;
  VADMChanged: TJCoreOPFADMArray;
  VIndex, I: Integer;
begin
  VOIDName := Map.OIDName;
  VOwnerOIDName := Map.OwnerOIDName;
  VADMChanged := AMapping.ADMAttributeChanged;
  SetLength(Result, Length(VOIDName) + Length(VOwnerOIDName) + Length(VADMChanged));
  VIndex := 0;
  for I := Low(VOIDName) to High(VOIDName) do
  begin
    Result[VIndex] := VOIDName[I];
    Inc(VIndex);
  end;
  for I := Low(VOwnerOIDName) to High(VOwnerOIDName) do
  begin
    Result[VIndex] := VOwnerOIDName[I];
    Inc(VIndex);
  end;
  for I := Low(VADMChanged) to High(VADMChanged) do
  begin
    Result[VIndex] := VADMChanged[I].Metadata.PersistentFieldName;
    Inc(VIndex);
  end;
end;

function TJCoreOPFSQLGenerator.BuildOIDName(const AMaps: TJCoreOPFMaps; const AMapIndex,
  AOIDIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string;
begin
  Result := BuildOIDName(AMaps[AMapIndex].OIDName, AMapIndex, AOIDIndex, ATablePrefixType);
end;

function TJCoreOPFSQLGenerator.BuildOIDName(const AOIDNameArray: array of string; const AMapIndex,
  AOIDIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string;
var
  VFieldName: string;
begin
  VFieldName := AOIDNameArray[AOIDIndex];
  if ATablePrefixType <> jtptNone then
    Result := Format('%s%d.%s', [CTablePrefix[ATablePrefixType], AMapIndex, VFieldName])
  else
    Result := VFieldName;
end;

function TJCoreOPFSQLGenerator.BuildOIDNames(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer;
  const ATablePrefixType: TJCoreOPFTablePrefixType): string;
var
  VOIDNameArray: TJCoreStringArray;
  I: Integer;
begin
  VOIDNameArray := AMaps[AMapIndex].OIDName;
  Result := '';
  for I := Low(VOIDNameArray) to High(VOIDNameArray) do
    Result := Result + BuildOIDName(VOIDNameArray, AMapIndex, I, ATablePrefixType) + ',';
  SetLength(Result, Length(Result) - 1);
end;

function TJCoreOPFSQLGenerator.BuildTableName(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer;
  const ATablePrefixType: TJCoreOPFTablePrefixType): string;
var
  VMap: TJCoreOPFMap;
begin
  VMap := AMaps[AMapIndex];
  if ATablePrefixType <> jtptNone then
    Result := Format('%s %s%d', [VMap.TableName, CTablePrefix[ATablePrefixType], AMapIndex])
  else
    Result := VMap.TableName;
end;

function TJCoreOPFSQLGenerator.MapIndexByMap(const AMap: TJCoreOPFMap): Integer;
begin
  for Result := 0 to Pred(Maps.Count) do
    if Maps[Result] = AMap then
      Exit;
  raise EJCoreOPFMappingNotFound.Create(AMap.Metadata.TheClass.ClassName);
end;

function TJCoreOPFSQLGenerator.BuildDeleteCondition(const AOIDCount: Integer): string;
begin
  Result := BuildOIDCondition(AOIDCount, jtptNone);
end;

function TJCoreOPFSQLGenerator.BuildInsertFieldNames(const AFields: TJCoreStringArray): string;
var
  I: Integer;
begin
  Result := '';
  if Length(AFields) > 0 then
  begin
    for I := Low(AFields) to Pred(High(AFields)) do
      Result := Result + AFields[I] + ',';
    Result := Result + AFields[High(AFields)];
  end;
end;

function TJCoreOPFSQLGenerator.BuildSelectBaseFieldNames(const AUseTablePrefix: Boolean): string;
var
  I: Integer;
begin
  Result := BuildOIDNames(Maps, 0, CMainMapPrefix[AUseTablePrefix]) + ',';
  for I := 0 to Pred(SubMaps.Count) do
    Result := Result + BuildOIDName(SubMaps, I, 0, CSubMapPrefix[AUseTablePrefix]) + ',';
  Result := Result + BuildFieldNames(0, AUseTablePrefix);
  SetLength(Result, Length(Result) - 1);
end;

function TJCoreOPFSQLGenerator.BuildSelectBaseFrom(const AUseTablePrefix: Boolean): string;
var
  I: Integer;
begin
  Result := BuildTableName(Maps, 0, CMainMapPrefix[AUseTablePrefix]);
  for I := 1 to Pred(Maps.Count) do
    Result := Format('%s INNER JOIN %s ON %s', [
     Result,
     BuildTableName(Maps, I, jtptMainMap),
     BuildSelectJoinCondition(Maps, 0, jtptMainMap, Maps, I, jtptMainMap)]);
  for I := 0 to Pred(SubMaps.Count) do
    Result := Format('%s LEFT OUTER JOIN %s ON %s', [
     Result,
     BuildTableName(SubMaps, I, jtptSubMap),
     BuildSelectJoinCondition(Maps, 0, jtptMainMap, SubMaps, I, jtptSubMap)]);
end;

function TJCoreOPFSQLGenerator.BuildSelectComplementaryFieldNames(const ABaseMapIdx: Integer;
  const AUseTablePrefix: Boolean): string;
begin
  Result := BuildOIDNames(Maps, 0, CMainMapPrefix[AUseTablePrefix]) + ',';
  Result := Result + BuildFieldNames(ABaseMapIdx, AUseTablePrefix);
  SetLength(Result, Length(Result) - 1);
end;

function TJCoreOPFSQLGenerator.BuildSelectComplementaryFrom(const ABaseMapIdx: Integer;
  const AUseTablePrefix: Boolean): string;
var
  I: Integer;
begin
  Result := BuildTableName(Maps, MapIndex, CMainMapPrefix[AUseTablePrefix]);
  for I := ABaseMapIdx to Pred(Maps.Count) do
    if Maps[I] <> Map then
      Result := Format('%s INNER JOIN %s ON %s', [
       Result,
       BuildTableName(Maps, I, CMainMapPrefix[AUseTablePrefix]),
       BuildSelectJoinCondition(Maps, MapIndex, jtptMainMap, Maps, I, jtptMainMap)]);
end;

function TJCoreOPFSQLGenerator.BuildSelectCondition(const AOIDCount: Integer;
  const AUseTablePrefix: Boolean): string;
begin
  Result := BuildOIDCondition(Maps, 0, AOIDCount, CMainMapPrefix[AUseTablePrefix]);
end;

function TJCoreOPFSQLGenerator.BuildSelectFieldNames(const AAttributes: TJCoreOPFAttrMetadataArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(AAttributes) to High(AAttributes) do
    Result := Result + AAttributes[I].PersistentFieldName + ',';
  SetLength(Result, Length(Result) - 1);
end;

function TJCoreOPFSQLGenerator.BuildSelectJoinCondition(const ALeftMaps: TJCoreOPFMaps;
  const ALeftIndex: Integer; const ALeftTablePrefixType: TJCoreOPFTablePrefixType;
  const ARightMaps: TJCoreOPFMaps; const ARightIndex: Integer;
  const ARightTablePrefixType: TJCoreOPFTablePrefixType): string;
var
  VLeftMap: TJCoreOPFMap;
  I: Integer;
begin
  VLeftMap := ALeftMaps[ALeftIndex];
  Result := '';
  { TODO : Validate OID length among related classes }
  for I := Low(VLeftMap.OIDName) to High(VLeftMap.OIDName) do
    Result := Format('%s%s=%s AND ', [
     Result,
     BuildOIDName(ALeftMaps, ALeftIndex, I, ALeftTablePrefixType),
     BuildOIDName(ARightMaps, ARightIndex, I, ARightTablePrefixType)]);
  SetLength(Result, Length(Result) - 5);
end;

function TJCoreOPFSQLGenerator.BuildUpdateCondition(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := BuildOIDCondition(1, jtptNone);
end;

function TJCoreOPFSQLGenerator.BuildUpdateNames(const AMapping: TJCoreOPFADMMapping): string;
var
  VADMChanged: TJCoreOPFADMArray;
  I: Integer;
begin
  VADMChanged := AMapping.ADMAttributeChanged;
  Result := '';
  for I := Low(VADMChanged) to High(VADMChanged) do
    Result := Result + VADMChanged[I].Metadata.PersistentFieldName + '=?,';
  SetLength(Result, Length(Result) - 1);
end;

constructor TJCoreOPFSQLGenerator.Create(const AMap: TJCoreOPFMap);
begin
  inherited Create;
  FMap := AMap;
  FMaps := Map.Metadata.Maps;
  FMapIndex := MapIndexByMap(AMap);
  FSubMaps := Map.SubMaps;
end;

function TJCoreOPFSQLGenerator.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := Format('DELETE FROM %s WHERE %s', [Map.TableName, BuildDeleteCondition(AOIDCount)]);
end;

function TJCoreOPFSQLGenerator.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
var
  VFields: TJCoreStringArray;
begin
  VFields := BuildInsertFields(AMapping);
  Result := Format('INSERT INTO %s (%s) VALUES (%s)', [
   Map.TableName, BuildInsertFieldNames(VFields), BuildFieldParams(Length(VFields))]);
end;

function TJCoreOPFSQLGenerator.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
var
  VIsMultiMap: Boolean;
begin
  VIsMultiMap := (SubMaps.Count > 0) or (Maps.Count > 1);
  Result := Format('SELECT %s FROM %s WHERE %s', [
   BuildSelectBaseFieldNames(VIsMultiMap),
   BuildSelectBaseFrom(VIsMultiMap),
   BuildSelectCondition(AOIDCount, VIsMultiMap)]);
end;

function TJCoreOPFSQLGenerator.GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap;
  const AOIDCount: Integer): string;
var
  VBaseMapIdx: Integer;
  VIsMultiMap: Boolean;
begin
  VBaseMapIdx := MapIndexByMap(ABaseMap) + 1;
  VIsMultiMap := VBaseMapIdx < Pred(Maps.Count);
  Result := Format('SELECT %s FROM %s WHERE %s', [
   BuildSelectComplementaryFieldNames(VBaseMapIdx, VIsMultiMap),
   BuildSelectComplementaryFrom(VBaseMapIdx, VIsMultiMap),
   BuildSelectCondition(AOIDCount, VIsMultiMap)]);
end;

function TJCoreOPFSQLGenerator.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := Format('UPDATE %s SET %s WHERE %s', [
   Map.TableName, BuildUpdateNames(AMapping), BuildUpdateCondition(AMapping)]);
end;

function TJCoreOPFSQLGenerator.GenerateDeleteExternalLinkIDsStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  { TODO : Implement }
  Result := 'DELETE';
end;

function TJCoreOPFSQLGenerator.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  { TODO : Implement }
  Result := 'DELETE';
end;

function TJCoreOPFSQLGenerator.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  { TODO : Implement }
  Result := 'INSERT';
end;

function TJCoreOPFSQLGenerator.GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata;
  const AOwnerAttr: TJCoreOPFAttrMetadata): string;
var
  VIsMultiMap: Boolean;
begin
  VIsMultiMap := (SubMaps.Count > 0) or (Maps.Count > 1);
  Result := Format('SELECT %s FROM %s WHERE %s', [
   BuildSelectBaseFieldNames(VIsMultiMap),
   BuildSelectBaseFrom(VIsMultiMap),
   BuildOIDCondition(Map.OwnerOIDName, 0, 1, CMainMapPrefix[VIsMultiMap])]);
end;

function TJCoreOPFSQLGenerator.GenerateSelectCompositionsForDeleteStatement(
  const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDCount: Integer): string;
begin
  { TODO : Rename/document - simple compositions, reference from the instance itself }
  Result := Format('SELECT %s FROM %s WHERE %s', [
   BuildSelectFieldNames(ACompositionMetadatas),
   Map.TableName,
   BuildSelectCondition(AOIDCount, False)]);
end;

function TJCoreOPFSQLGenerator.GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
var
  VMaps: TJCoreOPFMaps;
begin
  { TODO : Rename/document - collections, external references }
  VMaps := AAttrMetadata.CompositionMetadata.Maps;
  Result := Format('SELECT %s FROM %s WHERE %s', [
   BuildOIDNames(VMaps, 0, jtptNone),
   VMaps[0].TableName,
   BuildOIDCondition(VMaps[0].OwnerOIDName, 0, 1, jtptNone)]);
end;

function TJCoreOPFSQLGenerator.BuildFieldParams(const AFieldCount: Integer): string;
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

function TJCoreOPFSQLGenerator.BuildOIDCondition(const AOIDCount: Integer;
  const ATablePrefixType: TJCoreOPFTablePrefixType): string;
begin
  Result := BuildOIDCondition(Maps, MapIndex, AOIDCount, ATablePrefixType);
end;

function TJCoreOPFSQLGenerator.BuildOIDCondition(const AMaps: TJCoreOPFMaps; const AMapIndex, AOIDCount: Integer;
  const ATablePrefixType: TJCoreOPFTablePrefixType): string;
begin
  Result := BuildOIDCondition(AMaps[AMapIndex].OIDName, AMapIndex, AOIDCount, ATablePrefixType);
end;

function TJCoreOPFSQLGenerator.BuildOIDCondition(const AOIDNameArray: array of string;
  const AOIDCount: Integer): string;
begin
  Result := BuildOIDCondition(AOIDNameArray, 0, AOIDCount, jtptNone);
end;

function TJCoreOPFSQLGenerator.BuildOIDCondition(const AOIDNameArray: array of string;
  const AMapIndex, AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string;
var
  VOIDClause: string;
  I: Integer;
begin
  { TODO : allocate once, at the start }
  if Length(AOIDNameArray) = 1 then
  begin
    if AOIDCount > 1 then
    begin
      Result := BuildOIDName(AOIDNameArray, AMapIndex, 0, ATablePrefixType) + ' IN (?';
      for I := 2 to AOIDCount do
        Result := Result + ',?';
      Result := Result + ')';
    end else if AOIDCount = 1 then
      Result := BuildOIDName(AOIDNameArray, AMapIndex, 0, ATablePrefixType) + '=?'
    else
      Result := '';
  end else
  begin
    VOIDClause := '(';
    for I := Low(AOIDNameArray) to High(AOIDNameArray) do
      VOIDClause := BuildOIDName(AOIDNameArray, AMapIndex, I, ATablePrefixType) + '=? AND ';
    SetLength(VOIDClause, Length(VOIDClause) - 4);
    VOIDClause[Length(VOIDClause)] := ')';
    Result := '';
    for I := 0 to Pred(AOIDCount) do
      Result := Result + VOIDClause + ' OR ';
    SetLength(Result, Length(Result) - 4);
  end;
end;

{ TJCoreOPFSQLMapping }

function TJCoreOPFSQLMapping.CreateEntityFromDriver: TObject;
var
  VClass: TClass;
begin
  VClass := SelectClassFromDriver(Map.SubClasses, nil);
  if Assigned(VClass) then
    Result := VClass.Create
  else
    Result := nil;
end;

function TJCoreOPFSQLMapping.SQLGeneratorClass: TJCoreOPFSQLGeneratorClass;
begin
  Result := TJCoreOPFSQLGenerator;
end;

function TJCoreOPFSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.GenerateDeleteStatement(AOIDCount);
end;

function TJCoreOPFSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := SQLGenerator.GenerateInsertStatement(AMapping);
end;

function TJCoreOPFSQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.GenerateSelectBaseStatement(AOIDCount);
end;

function TJCoreOPFSQLMapping.GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap;
  const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.GenerateSelectComplementaryStatement(ABaseMap, AOIDCount);
end;

function TJCoreOPFSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := SQLGenerator.GenerateUpdateStatement(AMapping);
end;

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinkIDsStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.GenerateDeleteExternalLinkIDsStatement(AAttrMetadata, AOIDCount);
end;

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.GenerateDeleteExternalLinksStatement(AAttrMetadata, AOIDCount);
end;

function TJCoreOPFSQLMapping.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  Result := SQLGenerator.GenerateInsertExternalLinksStatement(AAttrMetadata);
end;

function TJCoreOPFSQLMapping.GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata;
  const AOwnerAttr: TJCoreOPFAttrMetadata): string;
begin
  Result := SQLGenerator.GenerateSelectCollectionStatement(AOwnerClass, AOwnerAttr);
end;

function TJCoreOPFSQLMapping.GenerateSelectCompositionsForDeleteStatement(
  const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.GenerateSelectCompositionsForDeleteStatement(ACompositionMetadatas, AOIDCount);
end;

function TJCoreOPFSQLMapping.GenerateSelectForDeleteStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.GenerateSelectForDeleteStatement(AAttrMetadata, AOIDCount);
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
    * compositions (owned) with external link (currently unsupported)
      -- select external IDs
      -- delete links
      -- delete objects
    * aggregations (shared) with external link
      -- delete links
  }
  case AAttrMetadata.CompositionType of
    jctComposition:
    begin
      VOIDCount := 0;
      // Select external IDs
      // DisposeFromDriverInternal internally uses InternalDispose, which need the
      // object ID to dispose it's own compositions
      { TODO : select for delete isn't necessary if deleted objects doesn't have compositions }
      { TODO : external links of owned compositions }
      VSelectStmt := GenerateSelectForDeleteStatement(AAttrMetadata, Length(AOIDArray));
      for VOID in AOIDArray do
        VOID.WriteToDriver(Driver);
      VOIDCount := Driver.ExecSQL(VSelectStmt);
      // Delete external objects
      if VOIDCount > 0 then
        Mapper.AcquireClassMapping(AAttrMetadata.CompositionClass).DisposeFromDriverInternal(VOIDCount);
    end;
    jctAggregation:
    begin
      // Delete external links
      VDeleteStmt := GenerateDeleteExternalLinksStatement(AAttrMetadata, Length(AOIDArray));
      for VOID in AOIDArray do
        VOID.WriteToDriver(Driver);
      Driver.ExecSQL(VDeleteStmt);
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.WriteDisposeEntityCompositionsToDriver(
  const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDArray: array of TJCoreOPFOID);
var
  VOID: TJCoreOPFOID;
  VSelectStmt: string;
begin
  { TODO : Implement reading of more than one entity composition }
  if Length(ACompositionMetadatas) = 1 then
  begin
    VSelectStmt := GenerateSelectCompositionsForDeleteStatement(ACompositionMetadatas, Length(AOIDArray));
    for VOID in AOIDArray do
      VOID.WriteToDriver(Driver);
    Driver.ExecSQL(VSelectStmt, Length(AOIDArray));
    Mapper.AcquireClassMapping(
     ACompositionMetadatas[0].CompositionMetadata.TheClass).DisposeFromDriverInternal(Length(AOIDArray));
  end;
end;

procedure TJCoreOPFSQLMapping.WriteDisposeExternalLinksToDriver(
  const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
var
  VOIDs: TJCoreOPFOIDArray;
  VOID: TJCoreOPFOID;
begin
  if AADM.Metadata.HasExternalLink and AOwnerPID.IsPersistent then
  begin
    VOIDs := AADM.OIDRemoved;
    if Length(VOIDs) > 0 then
    begin
      AOwnerPID.OID.WriteToDriver(Driver);
      for VOID in VOIDs do
        VOID.WriteToDriver(Driver);
      Driver.ExecSQL(GenerateDeleteExternalLinkIDsStatement(AADM.Metadata, Length(VOIDs)));
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.WriteInsertExternalLinksToDriver(
  const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
var
  VInsertStmt: string;
  VPIDs: TJCoreOPFPIDArray;
  VPID: TJCoreOPFPID;
begin
  if AADM.Metadata.HasExternalLink then
  begin
    VInsertStmt := GenerateInsertExternalLinksStatement(AADM.Metadata);
    VPIDs := AADM.PIDAdded;
    for VPID in VPIDs do
    begin
      AOwnerPID.OID.WriteToDriver(Driver);
      VPID.OID.WriteToDriver(Driver);
      Driver.ExecSQL(VInsertStmt);
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.InternalDispose(const AOIDArray: array of TJCoreOPFOID);
var
  VCompositionMetadatas: TJCoreOPFAttrMetadataArray;
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VCount: Integer;
  I: Integer;
begin
  SetLength(VCompositionMetadatas, Map.Count);
  VCount := 0;
  for I := 0 to Pred(Map.Count) do
  begin
    VAttrMetadata := Map[I];
    if VAttrMetadata.AttributeType = jatCollection then
      WriteDisposeCollectionToDriver(VAttrMetadata, AOIDArray)
    else if (VAttrMetadata.AttributeType = jatEntity) and (VAttrMetadata.CompositionType = jctComposition) then
    begin
      VCompositionMetadatas[VCount] := VAttrMetadata;
      Inc(VCount);
    end;
  end;
  SetLength(VCompositionMetadatas, VCount);
  if VCount > 0 then
    WriteDisposeEntityCompositionsToDriver(VCompositionMetadatas, AOIDArray);
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
  Result := Driver.ExecSQL(GenerateSelectCollectionStatement(AOwnerPID.Metadata, AOwnerADM.Metadata));
end;

procedure TJCoreOPFSQLMapping.InternalRetrieveEntityToDriver(const AOIDArray: array of TJCoreOPFOID;
  const ABaseMap: TJCoreOPFMap);
var
  VOID: TJCoreOPFOID;
  VSelectStmt: string;
begin
  for VOID in AOIDArray do
    VOID.WriteToDriver(Driver);
  if not Assigned(ABaseMap) then
    VSelectStmt := GenerateSelectBaseStatement(Length(AOIDArray))
  else
    VSelectStmt := GenerateSelectComplementaryStatement(ABaseMap, Length(AOIDArray));
  Driver.ExecSQL(VSelectStmt, Length(AOIDArray));
end;

procedure TJCoreOPFSQLMapping.InternalUpdate(const AMapping: TJCoreOPFADMMapping);
begin
  Driver.ExecSQL(GenerateUpdateStatement(AMapping));
end;

procedure TJCoreOPFSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  I: Integer;
begin
  for I := 0 to Pred(AMapping.Count) do
    AMapping.ADM[I].ReadFromDriver(Driver);
end;

procedure TJCoreOPFSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPID: TJCoreOPFPID;
  VADMChanged: TJCoreOPFADMArray;
  I: Integer;
begin
  VPID := AMapping.PID;
  if (Length(Map.OwnerOIDName) > 0) and not AMapping.PID.IsPersistent then
  begin
    if Assigned(VPID.Owner) then
      VPID.Owner.OID.WriteToDriver(Driver)
    else
      Map.Metadata.OIDClass.WriteNull(Driver);
  end;
  VADMChanged := AMapping.ADMAttributeChanged;
  for I := Low(VADMChanged) to High(VADMChanged) do
    VADMChanged[I].WriteToDriver(Driver);
end;

procedure TJCoreOPFSQLMapping.WriteCollectionsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VADMChanged: TJCoreOPFADMArray;
  VPID: TJCoreOPFPID;
  I: Integer;
  VADM: TJCoreOPFADMCollection;
begin
  VADMChanged := AMapping.ADMCollectionChanged;
  VPID := AMapping.PID;
  for I := Low(VADMChanged) to High(VADMChanged) do
  begin
    VADM := VADMChanged[I] as TJCoreOPFADMCollection;
    WriteDisposeExternalLinksToDriver(VPID, VADM);
    Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).StoreCollectionInternal(VPID, VADM);
    WriteInsertExternalLinksToDriver(VPID, VADM);
  end;
end;

function TJCoreOPFSQLMapping.BuildOIDCondition(const AOIDNameArray: array of string;
  const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.BuildOIDCondition(AOIDNameArray, AOIDCount);
end;

function TJCoreOPFSQLMapping.EnsureCollectionAttribute(const AADM: TJCoreOPFADM): TJCoreOPFADMCollection;
begin
  if not (AADM is TJCoreOPFADMCollection) then
    raise EJCoreOPFCollectionADMExpected.Create(AADM.PID.Entity.ClassName, AADM.Metadata.Name);
  Result := TJCoreOPFADMCollection(AADM);
end;

function TJCoreOPFSQLMapping.EnsureEntityAttribute(const AADM: TJCoreOPFADM): TJCoreOPFADMEntity;
begin
  if not (AADM is TJCoreOPFADMEntity) then
    raise EJCoreOPFEntityADMExpected.Create(AADM.PID.Entity.ClassName, AADM.Metadata.Name);
  Result := TJCoreOPFADMEntity(AADM);
end;

procedure TJCoreOPFSQLMapping.ReadCollection(const AADM: TJCoreOPFADM);
var
  VADM: TJCoreOPFADMCollection;
begin
  VADM := EnsureCollectionAttribute(AADM);
  Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).RetrieveCollectionInternal(AADM.PID, VADM);
end;

function TJCoreOPFSQLMapping.ReadEntity(const AClass: TClass): TObject;
begin
  Result := Mapper.AcquireClassMapping(AClass).RetrieveEntityFromDriverInternal;
end;

procedure TJCoreOPFSQLMapping.ReadLazyEntity(const AADM: TJCoreOPFADM);
var
  VADM: TJCoreOPFADMEntity;
begin
  VADM := EnsureEntityAttribute(AADM);
  Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).RetrieveLazyEntityFromDriverInternal(VADM);
end;

procedure TJCoreOPFSQLMapping.WriteCollection(const AADM: TJCoreOPFADM);
var
  VADM: TJCoreOPFADMCollection;
  VPID: TJCoreOPFPID;
begin
  VADM := EnsureCollectionAttribute(AADM);
  VPID := AADM.PID;
  { TODO : Improve the change analyzer }
  if VADM.IsDirty then
  begin
    WriteDisposeExternalLinksToDriver(VPID, VADM);
    Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).StoreCollectionInternal(VPID, VADM);
    WriteInsertExternalLinksToDriver(VPID, VADM);
  end;
end;

procedure TJCoreOPFSQLMapping.WriteEntity(const AClass: TClass; const AEntity: TObject;
  const AComposition: Boolean);
var
  VPID: TJCoreOPFPID;
begin
  if Assigned(AEntity) then
  begin
    VPID := Mapper.AcquirePID(AEntity);
    if AComposition or not Assigned(VPID.OID) then
      Mapper.AcquireClassMapping(AEntity.ClassType).StorePID(VPID);
    VPID.OID.WriteToDriver(Driver);
  end else
    Model.AcquireMetadata(AClass).OIDClass.WriteNull(Driver);
end;

procedure TJCoreOPFSQLMapping.WriteEntity(const AADM: TJCoreOPFADM);
var
  VADM: TJCoreOPFADMEntity;
  VEntity: TObject;
  VPID: TJCoreOPFPID;
begin
  VADM := EnsureEntityAttribute(AADM);
  VEntity := VADM.Value;
  if Assigned(VEntity) then
  begin
    VPID := Mapper.AcquirePID(VEntity);
    if not Assigned(VPID.OID) or (VADM.Metadata.CompositionType = jctComposition) then
      Mapper.AcquireClassMapping(VEntity.ClassType).StorePID(VPID);
    VPID.OID.WriteToDriver(Driver);
  end else
    VADM.Metadata.CompositionMetadata.OIDClass.WriteNull(Driver);
end;

procedure TJCoreOPFSQLMapping.WriteOwnerOID(const AMapping: TJCoreOPFADMMapping);
begin
  AMapping.PID.Owner.OID.WriteToDriver(Driver);
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
  { TODO : Use cache of SQL Generator }
  FSQLGenerator := SQLGeneratorClass.Create(Map);
end;

destructor TJCoreOPFSQLMapping.Destroy;
begin
  FreeAndNil(FSQLGenerator);
  inherited Destroy;
end;

class function TJCoreOPFSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := True;
end;

end.

