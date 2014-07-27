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
    function BuildOIDName(const AMaps: TJCoreOPFMaps; const AMapIndex, AOIDIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDNames(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildTableName(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function MapIndexByMap(const AMap: TJCoreOPFMap): Integer;
  protected
    // SQL fragments
    function BuildDeleteCondition(const AOIDCount: Integer): string; virtual;
    function BuildInsertFieldNames(const AMapping: TJCoreOPFADMMapping): string; virtual;
    function BuildInsertParamNames(const AMapping: TJCoreOPFADMMapping): string; virtual;
    function BuildSelectBaseFieldNames(const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectBaseFrom(const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectComplementaryFieldNames(const ABaseMapIdx: Integer; const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectComplementaryFrom(const ABaseMapIdx: Integer; const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectCondition(const AOIDCount: Integer; const AUseTablePrefix: Boolean): string; virtual;
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
    function GenerateSelectCompositionsForDeleteStatement(const AOIDCount: Integer): string; virtual;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
    // Support
    function BuildFieldParams(const AFieldCount: Integer): string; virtual;
    function BuildOIDCondition(const AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDCondition(const AMaps: TJCoreOPFMaps; const AMapIndex, AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
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
    function GenerateSelectCompositionsForDeleteStatement(const AOIDCount: Integer): string; virtual;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; virtual;
  protected
    // Support
    procedure WriteDisposeCollectionToDriver(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of TJCoreOPFOID);
    procedure WriteDisposeEntityCompositionsToDriver(const AOIDArray: array of TJCoreOPFOID);
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
    procedure WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
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
  JCoreClasses,
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

function TJCoreOPFSQLGenerator.BuildOIDName(const AMaps: TJCoreOPFMaps; const AMapIndex,
  AOIDIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string;
var
  VFieldName: string;
begin
  VFieldName := AMaps[AMapIndex].OIDName[AOIDIndex];
  if ATablePrefixType <> jtptNone then
    Result := Format('%s%d.%s', [CTablePrefix[ATablePrefixType], AMapIndex, VFieldName])
  else
    Result := VFieldName;
end;

function TJCoreOPFSQLGenerator.BuildOIDNames(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer;
  const ATablePrefixType: TJCoreOPFTablePrefixType): string;
var
  VMap: TJCoreOPFMap;
  I: Integer;
begin
  VMap := AMaps[AMapIndex];
  Result := '';
  for I := Low(VMap.OIDName) to High(VMap.OIDName) do
    Result := Result + BuildOIDName(AMaps, AMapIndex, I, ATablePrefixType) + ',';
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

function TJCoreOPFSQLGenerator.BuildInsertFieldNames(const AMapping: TJCoreOPFADMMapping): string;
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

function TJCoreOPFSQLGenerator.BuildInsertParamNames(const AMapping: TJCoreOPFADMMapping): string;
begin
  // Insert use "admChanged" because there is a single "writeToDriver" method,
  // shared between inserts and updates.
  Result := BuildFieldParams(Length(Map.OIDName) + Length(AMapping.ADMChanged));
end;

function TJCoreOPFSQLGenerator.BuildSelectBaseFieldNames(const AUseTablePrefix: Boolean): string;
var
  VMap: TJCoreOPFMap;
  I, J: Integer;
begin
  Result := BuildOIDNames(Maps, 0, CMainMapPrefix[AUseTablePrefix]);
  for I := 0 to Pred(SubMaps.Count) do
    Result := Result + BuildOIDName(SubMaps, I, 0, CSubMapPrefix[AUseTablePrefix]) + ',';
  for I := 0 to Pred(Maps.Count) do
  begin
    VMap := Maps[I];
    for J := 0 to Pred(VMap.Count) do
      Result := Result + BuildFieldName(Maps, I, J, CMainMapPrefix[AUseTablePrefix]) + ',';
  end;
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
var
  VMap: TJCoreOPFMap;
  I, J: Integer;
begin
  Result := BuildOIDNames(Maps, 0, CMainMapPrefix[AUseTablePrefix]);
  for I := ABaseMapIdx to Pred(Maps.Count) do
  begin
    VMap := Maps[I];
    for J := 0 to Pred(VMap.Count) do
      Result := Result + BuildFieldName(Maps, I, J, CMainMapPrefix[AUseTablePrefix]) + ',';
  end;
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
  VADMChanged := AMapping.ADMChanged;
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
begin
  Result := Format('INSERT INTO %s (%s) VALUES (%s)', [
   Map.TableName, BuildInsertFieldNames(AMapping), BuildInsertParamNames(AMapping)]);
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
begin
  { TODO : Implement }
  Result := 'SELECT';
end;

function TJCoreOPFSQLGenerator.GenerateSelectCompositionsForDeleteStatement(
  const AOIDCount: Integer): string;
begin
  { TODO : Implement }
  Result := 'SELECT';
end;

function TJCoreOPFSQLGenerator.GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
begin
  { TODO : Implement }
  Result := 'SELECT';
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
var
  VOIDName: TJCoreStringArray;
  VOIDClause: string;
  I: Integer;
begin
  { TODO : allocate once, at the start }
  VOIDName := AMaps[AMapIndex].OIDName;
  if Length(VOIDName) = 1 then
  begin
    if AOIDCount > 1 then
    begin
      Result := BuildOIDName(AMaps, AMapIndex, 0, ATablePrefixType) + ' IN (?';
      for I := 2 to AOIDCount do
        Result := Result + ',?';
      Result := Result + ')';
    end else if AOIDCount = 1 then
      Result := BuildOIDName(AMaps, AMapIndex, 0, ATablePrefixType) + '=?'
    else
      Result := '';
  end else
  begin
    VOIDClause := '(';
    for I := Low(VOIDName) to High(VOIDName) do
      VOIDClause := BuildOIDName(AMaps, AMapIndex, I, ATablePrefixType) + '=? AND ';
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

function TJCoreOPFSQLMapping.GenerateSelectCompositionsForDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := SQLGenerator.GenerateSelectCompositionsForDeleteStatement(AOIDCount);
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
  const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
var
  VOIDs: TJCoreOPFOIDArray;
  VOID: TJCoreOPFOID;
begin
  if (AADM.Metadata.CompositionLinkType = jcltExternal) and AOwnerPID.IsPersistent then
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
  if AADM.Metadata.CompositionLinkType = jcltExternal then
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

procedure TJCoreOPFSQLMapping.WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  { TODO : Implement }
end;

procedure TJCoreOPFSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VADMChanged: TJCoreOPFADMArray;
  I: Integer;
begin
  VADMChanged := AMapping.ADMChanged;
  for I := Low(VADMChanged) to High(VADMChanged) do
    VADMChanged[I].WriteToDriver(Driver);
end;

function TJCoreOPFSQLMapping.BuildOIDCondition(const AOIDNameArray: array of string;
  const AOIDCount: Integer): string;
var
  VOIDClause: string;
  I: Integer;
begin
  { TODO : Fix duplicated code. Used on manual mapping of external links }
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

