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
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  JCoreClasses,
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMetadata,
  JCoreOPFCriteria,
  JCoreOPFCriteriaSQL,
  JCoreOPFMapping;

type

  TJCoreOPFTablePrefixType = (jtptNone, jtptMainMap, jtptSubMap, jtptExternalLink);

  { TJCoreOPFSQLGenerator }

  TJCoreOPFSQLGenerator = class(TObject)
  private
    FMap: TJCoreOPFMap;
    FMapIndex: Integer;
    FMaps: TJCoreOPFMaps;
    FSubMaps: TJCoreOPFMaps;
  protected
    property Map: TJCoreOPFMap read FMap;
    property MapIndex: Integer read FMapIndex;
    property Maps: TJCoreOPFMaps read FMaps;
    property SubMaps: TJCoreOPFMaps read FSubMaps;
  public
    constructor Create(const AMap: TJCoreOPFMap; const AMapIndex: Integer); virtual;
    // Support
    function BuildFieldName(const AFieldName: string; const AMapIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildFieldName(const AMaps: TJCoreOPFMaps; const AMapIndex, AFieldIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildFieldNames(const ABaseMapIdx: Integer; const AUseTablePrefix: Boolean): string;
    function BuildFieldParams(const AFieldCount: Integer): string; virtual;
    function BuildInsertFields(const AMapping: TJCoreOPFADMMapping): TJCoreStringArray; virtual;
    function BuildInsertLinkFields(const AAttrMetadata: TJCoreOPFAttrMetadata): TJCoreStringArray; virtual;
    function BuildOIDCondition(const AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDCondition(const AMaps: TJCoreOPFMaps; const AMapIndex, AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDCondition(const AOIDNameArray: array of string; const AOIDCount: Integer): string; virtual;
    function BuildOIDCondition(const AOIDNameArray: array of string; const AMapIndex, AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDName(const AMaps: TJCoreOPFMaps; const AMapIndex, AOIDIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDName(const AOIDNameArray: array of string; const AMapIndex, AOIDIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildOIDNames(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildTableName(const AMaps: TJCoreOPFMaps; const AMapIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildTableName(const ATableName: string; const AMapIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    // SQL fragments
    function BuildDeleteCondition(const AOIDCount: Integer): string; virtual;
    function BuildInsertFieldNames(const AFields: TJCoreStringArray): string; virtual;
    function BuildSelectBaseFieldNames(const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectBaseFrom(const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectComplementaryFieldNames(const ABaseMapIdx: Integer; const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectComplementaryFrom(const ABaseMapIdx: Integer; const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectCondition(const AOIDCount: Integer; const AUseTablePrefix: Boolean): string; virtual;
    function BuildSelectExternalLinksCondition(const AOwnerAttr: TJCoreOPFAttrMetadata): string; virtual;
    function BuildSelectExternalLinksFrom(const AOwnerAttr: TJCoreOPFAttrMetadata): string; virtual;
    function BuildSelectFieldNames(const AAttributes: TJCoreOPFAttrMetadataArray): string;
    function BuildSelectJoinCondition(const ALeftMaps: TJCoreOPFMaps; const ALeftIndex: Integer; const ALeftTablePrefixType: TJCoreOPFTablePrefixType; const ARightMaps: TJCoreOPFMaps; const ARightIndex: Integer; const ARightTablePrefixType: TJCoreOPFTablePrefixType): string; virtual;
    function BuildUpdateCondition(const AMapping: TJCoreOPFADMMapping): string; virtual;
    function BuildUpdateOrderField(const AAttrMetadata: TJCoreOPFAttrMetadata): string; virtual;
    function BuildUpdateOrderCondition(const AAttrMetadata: TJCoreOPFAttrMetadata): string; virtual;
    function BuildUpdateNames(const AMapping: TJCoreOPFADMMapping): string; virtual;
  end;

  TJCoreOPFSQLGeneratorClass = class of TJCoreOPFSQLGenerator;

  { TJCoreOPFSQLMapping }

  TJCoreOPFSQLMapping = class(TJCoreOPFMapping)
  private
    FMapIndex: Integer;
    FMaps: TJCoreOPFMaps;
    FSQLDriver: TJCoreOPFSQLDriver;
    FSQLGen: TJCoreOPFSQLGenerator;
  private
    function MapIndexByMap(const AMap: TJCoreOPFMap): Integer;
  protected
    function CreateEntityFromResultSet(const AResultSet: IJCoreOPFResultSet): TObject; override;
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
    function GenerateUpdateOrderFieldStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; virtual;
  protected
    // Support
    procedure WriteDisposeCollection(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of IJCoreOPFOID);
    procedure WriteDisposeEntityCompositions(const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDArray: array of IJCoreOPFOID);
    procedure WriteDisposeExternalLinks(const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
    procedure WriteInsertExternalLinks(const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
    procedure WriteOrder(const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
    property Driver: TJCoreOPFSQLDriver read FSQLDriver;
    property MapIndex: Integer read FMapIndex;
    property Maps: TJCoreOPFMaps read FMaps;
    property SQLGen: TJCoreOPFSQLGenerator read FSQLGen;
  protected
    // Facade internals
    function InternalCreateCriteria(const ARetriever: IJCoreOPFCriteriaRetriever): IJCoreOPFSQLCriteria; override;
    procedure InternalDispose(const AOIDArray: array of IJCoreOPFOID); override;
    procedure InternalInsert(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
    function InternalRetrieveCollection(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection): IJCoreOPFResultSet; override;
    function InternalRetrieveEntity(const AOIDArray: array of IJCoreOPFOID; const ABaseMap: TJCoreOPFMap): IJCoreOPFResultSet; override;
    procedure InternalUpdate(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
    // Direct attribute <-> field mapping
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteCollections(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteLinkAttributesToParams(const AOwnerPID, APID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection; const AParams: IJCoreOPFParams); virtual;
  protected
    // Manual mapping helpers
    function BuildOIDCondition(const AOIDNameArray: array of string; const AOIDCount: Integer): string;
    function EnsureCollectionAttribute(const AADM: TJCoreOPFADM): TJCoreOPFADMCollection;
    function EnsureEntityAttribute(const AADM: TJCoreOPFADM): TJCoreOPFADMEntity;
    procedure ReadCollection(const AADM: TJCoreOPFADM);
    function ReadEntity(const AResultSet: IJCoreOPFResultSet; const AClass: TClass): TObject;
    procedure ReadLazyEntity(const AResultSet: IJCoreOPFResultSet; const AADM: TJCoreOPFADM);
    procedure WriteCollection(const AADM: TJCoreOPFADM);
    procedure WriteEntity(const AParams: IJCoreOPFParams; const AClass: TClass; const AEntity: TObject; const AComposition: Boolean);
    procedure WriteEntity(const AParams: IJCoreOPFParams; const AADM: TJCoreOPFADM);
    procedure WriteOwnerOID(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping);
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap; const AIsBaseMapping: Boolean); override;
    destructor Destroy; override;
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

implementation

uses
  sysutils,
  Classes,
  JCoreConsts,
  JCoreMetadata;

{ TJCoreOPFSQLGenerator }

const
  { TODO : Review the aproach after "subclass type strategy" implementation }
  CTablePrefix: array[TJCoreOPFTablePrefixType] of string = ('', 'T_', 'TS_', 'TL_');
  CMainMapPrefix: array[Boolean] of TJCoreOPFTablePrefixType = (jtptNone, jtptMainMap);
  CSubMapPrefix: array[Boolean] of TJCoreOPFTablePrefixType = (jtptNone, jtptSubMap);

constructor TJCoreOPFSQLGenerator.Create(const AMap: TJCoreOPFMap; const AMapIndex: Integer);
begin
  inherited Create;
  FMap := AMap;
  FMaps := Map.Metadata.Maps;
  FMapIndex := AMapIndex;
  FSubMaps := Map.SubMaps;
end;

function TJCoreOPFSQLGenerator.BuildFieldName(const AFieldName: string; const AMapIndex: Integer;
  const ATablePrefixType: TJCoreOPFTablePrefixType): string;
begin
  if ATablePrefixType <> jtptNone then
    Result := Format('%s%d.%s', [CTablePrefix[ATablePrefixType], AMapIndex, AFieldName])
  else
    Result := AFieldName;
end;

function TJCoreOPFSQLGenerator.BuildFieldName(const AMaps: TJCoreOPFMaps; const AMapIndex,
  AFieldIndex: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string;
begin
  Result := BuildFieldName(AMaps[AMapIndex][AFieldIndex].PersistentFieldName, AMapIndex, ATablePrefixType);
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

function TJCoreOPFSQLGenerator.BuildInsertFields(const AMapping: TJCoreOPFADMMapping): TJCoreStringArray;
var
  VOIDName: TJCoreStringArray;
  VOwnerOIDName: TJCoreStringArray;
  VADMChanged: TJCoreOPFADMArray;
  VLength, VIndex, I: Integer;
begin
  VOIDName := Map.OIDName;
  VOwnerOIDName := Map.OwnerOIDName;
  VADMChanged := AMapping.ADMAttributeChanged;
  VLength := Length(VOIDName) + Length(VOwnerOIDName) + Length(VADMChanged);
  if Map.HasOrderField then
    Inc(VLength);
  SetLength(Result, VLength);
  VIndex := 0;
  for I := Low(VOIDName) to High(VOIDName) do
  begin
    Result[VIndex] := VOIDName[I];
    Inc(VIndex);
  end;
  if Map.HasOrderField then
  begin
    Result[VIndex] := Map.OrderFieldName;
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

function TJCoreOPFSQLGenerator.BuildInsertLinkFields(
  const AAttrMetadata: TJCoreOPFAttrMetadata): TJCoreStringArray;
var
  VHasOrderField: Boolean;
begin
  VHasOrderField := AAttrMetadata.OrderFieldName <> '';
  if VHasOrderField then
    SetLength(Result, 3)
  else
    SetLength(Result, 2);
  Result[0] := AAttrMetadata.ExternalLinkLeftFieldName;
  Result[1] := AAttrMetadata.ExternalLinkRightFieldName;
  if VHasOrderField then
    Result[2] := AAttrMetadata.OrderFieldName;
end;

function TJCoreOPFSQLGenerator.BuildOIDCondition(const AOIDCount: Integer;
  const ATablePrefixType: TJCoreOPFTablePrefixType): string;
begin
  Result := BuildOIDCondition(Maps, MapIndex, AOIDCount, ATablePrefixType);
end;

function TJCoreOPFSQLGenerator.BuildOIDCondition(const AMaps: TJCoreOPFMaps; const AMapIndex,
  AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string;
begin
  Result := BuildOIDCondition(AMaps[AMapIndex].OIDName, AMapIndex, AOIDCount, ATablePrefixType);
end;

function TJCoreOPFSQLGenerator.BuildOIDCondition(const AOIDNameArray: array of string;
  const AOIDCount: Integer): string;
begin
  Result := BuildOIDCondition(AOIDNameArray, 0, AOIDCount, jtptNone);
end;

function TJCoreOPFSQLGenerator.BuildOIDCondition(const AOIDNameArray: array of string; const AMapIndex,
  AOIDCount: Integer; const ATablePrefixType: TJCoreOPFTablePrefixType): string;
var
  I: Integer;
begin
  { TODO : allocate once, at the start }
  if Length(AOIDNameArray) in [1,2] then
  begin
    if AOIDCount > 1 then
    begin
      Result := BuildOIDName(AOIDNameArray[High(AOIDNameArray)], AMapIndex, 0, ATablePrefixType) + ' IN (?';
      for I := 2 to AOIDCount do
        Result := Result + ',?';
      Result := Result + ')';
    end else if AOIDCount = 1 then
      Result := BuildOIDName(AOIDNameArray[High(AOIDNameArray)], AMapIndex, 0, ATablePrefixType) + '=?'
    else
      Result := '';
    if (AOIDCount > 0) and (Length(AOIDNameArray) = 2) then
      Result := BuildOIDName(AOIDNameArray[0], AMapIndex, 0, ATablePrefixType) + '=? AND ' + Result;
  end else if Length(AOIDNameArray) = 0 then
    Result := ''
  else
    raise EJCoreOPF.Create(2126, S2126_UnsupportedOIDArray, []);
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
begin
  Result := BuildTableName(AMaps[AMapIndex].TableName, AMapIndex, ATablePrefixType);
end;

function TJCoreOPFSQLGenerator.BuildTableName(const ATableName: string; const AMapIndex: Integer;
  const ATablePrefixType: TJCoreOPFTablePrefixType): string;
begin
  if ATablePrefixType <> jtptNone then
    Result := Format('%s %s%d', [ATableName, CTablePrefix[ATablePrefixType], AMapIndex])
  else
    Result := ATableName;
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
  if Map.HasOrderField then
    Result := Result + BuildFieldName(Map.OrderFieldName, 0, CMainMapPrefix[AUseTablePrefix]) + ',';
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

function TJCoreOPFSQLGenerator.BuildSelectExternalLinksCondition(
  const AOwnerAttr: TJCoreOPFAttrMetadata): string;
begin
  Result := BuildOIDCondition([AOwnerAttr.ExternalLinkLeftFieldName], 0, 1, jtptExternalLink);
end;

function TJCoreOPFSQLGenerator.BuildSelectExternalLinksFrom(
  const AOwnerAttr: TJCoreOPFAttrMetadata): string;
begin
  Result := Format('%s INNER JOIN %s ON %s=%s', [
   BuildSelectBaseFrom(True),
   BuildTableName(AOwnerAttr.ExternalLinkTableName, 0, jtptExternalLink),
   BuildOIDName(Maps, 0, 0, CMainMapPrefix[True]),
   BuildOIDName([AOwnerAttr.ExternalLinkRightFieldName], 0, 0, jtptExternalLink)]);
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

function TJCoreOPFSQLGenerator.BuildUpdateOrderField(const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  Result := AAttrMetadata.OrderFieldName + '=?';
end;

function TJCoreOPFSQLGenerator.BuildUpdateOrderCondition(const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if AAttrMetadata.HasExternalLink then
    Result := BuildOIDCondition([
     AAttrMetadata.ExternalLinkLeftFieldName, AAttrMetadata.ExternalLinkRightFieldName], 1)
  else
    Result := BuildOIDCondition(AAttrMetadata.CompositionMetadata.OIDName, 1);
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

{ TJCoreOPFSQLMapping }

function TJCoreOPFSQLMapping.MapIndexByMap(const AMap: TJCoreOPFMap): Integer;
begin
  for Result := 0 to Pred(Maps.Count) do
    if Maps[Result] = AMap then
      Exit;
  raise EJCoreOPF.Create(2116, S2116_MappingNotFound, [AMap.Metadata.TheClass.ClassName]);
end;

function TJCoreOPFSQLMapping.CreateEntityFromResultSet(const AResultSet: IJCoreOPFResultSet): TObject;
var
  VClass: TClass;
begin
  VClass := SelectClassFromResultSet(AResultSet, Map.SubClasses, nil);
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
  Result := Format('DELETE FROM %s WHERE %s', [Map.TableName, SQLGen.BuildDeleteCondition(AOIDCount)]);
end;

function TJCoreOPFSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
var
  VFields: TJCoreStringArray;
begin
  VFields := SQLGen.BuildInsertFields(AMapping);
  Result := Format('INSERT INTO %s (%s) VALUES (%s)', [
   Map.TableName,
   SQLGen.BuildInsertFieldNames(VFields),
   SQLGen.BuildFieldParams(Length(VFields))]);
end;

function TJCoreOPFSQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
var
  VIsMultiMap: Boolean;
begin
  VIsMultiMap := (Map.SubMaps.Count > 0) or (Maps.Count > 1);
  Result := Format('SELECT %s FROM %s WHERE %s', [
   SQLGen.BuildSelectBaseFieldNames(VIsMultiMap),
   SQLGen.BuildSelectBaseFrom(VIsMultiMap),
   SQLGen.BuildSelectCondition(AOIDCount, VIsMultiMap)]);
end;

function TJCoreOPFSQLMapping.GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap;
  const AOIDCount: Integer): string;
var
  VBaseMapIdx: Integer;
  VIsMultiMap: Boolean;
begin
  VBaseMapIdx := MapIndexByMap(ABaseMap) + 1;
  VIsMultiMap := VBaseMapIdx < Pred(Maps.Count);
  Result := Format('SELECT %s FROM %s WHERE %s', [
   SQLGen.BuildSelectComplementaryFieldNames(VBaseMapIdx, VIsMultiMap),
   SQLGen.BuildSelectComplementaryFrom(VBaseMapIdx, VIsMultiMap),
   SQLGen.BuildSelectCondition(AOIDCount, VIsMultiMap)]);
end;

function TJCoreOPFSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := Format('UPDATE %s SET %s WHERE %s', [
   Map.TableName,
   SQLGen.BuildUpdateNames(AMapping),
   SQLGen.BuildUpdateCondition(AMapping)]);
end;

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinkIDsStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  Result := Format('DELETE FROM %s WHERE %s', [
   AAttrMetadata.ExternalLinkTableName,
   BuildOIDCondition([
    AAttrMetadata.ExternalLinkLeftFieldName,
    AAttrMetadata.ExternalLinkRightFieldName],
   AOIDCount)]);
end;

function TJCoreOPFSQLMapping.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  Result := Format('DELETE FROM %s WHERE %s', [
   AAttrMetadata.ExternalLinkTableName,
   BuildOIDCondition([AAttrMetadata.ExternalLinkRightFieldName], AOIDCount)]);
end;

function TJCoreOPFSQLMapping.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
var
  VFields: TJCoreStringArray;
begin
  VFields := SQLGen.BuildInsertLinkFields(AAttrMetadata);
  Result := Format('INSERT INTO %s (%s) VALUES (%s)', [
   AAttrMetadata.ExternalLinkTableName,
   SQLGen.BuildInsertFieldNames(VFields),
   SQLGen.BuildFieldParams(Length(VFields))]);
end;

function TJCoreOPFSQLMapping.GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata;
  const AOwnerAttr: TJCoreOPFAttrMetadata): string;
var
  VIsMultiMap: Boolean;
begin
  if not AOwnerAttr.HasExternalLink then
  begin
    VIsMultiMap := (Map.SubMaps.Count > 0) or (Maps.Count > 1);
    Result := Format('SELECT %s FROM %s WHERE %s', [
     SQLGen.BuildSelectBaseFieldNames(VIsMultiMap),
     SQLGen.BuildSelectBaseFrom(VIsMultiMap),
     SQLGen.BuildOIDCondition(Map.OwnerOIDName, 0, 1, CMainMapPrefix[VIsMultiMap])]);
  end else
  begin
    Result := Format('SELECT %s FROM %s WHERE %s', [
     SQLGen.BuildSelectBaseFieldNames(True),
     SQLGen.BuildSelectExternalLinksFrom(AOwnerAttr),
     SQLGen.BuildSelectExternalLinksCondition(AOwnerAttr)]);
  end;
end;

function TJCoreOPFSQLMapping.GenerateSelectCompositionsForDeleteStatement(
  const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDCount: Integer): string;
begin
  { TODO : Rename/document - simple compositions, reference from the instance itself }
  Result := Format('SELECT %s FROM %s WHERE %s', [
   SQLGen.BuildSelectFieldNames(ACompositionMetadatas),
   Map.TableName,
   SQLGen.BuildSelectCondition(AOIDCount, False)]);
end;

function TJCoreOPFSQLMapping.GenerateSelectForDeleteStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
var
  VMaps: TJCoreOPFMaps;
begin
  { TODO : Rename/document - collections, external references }
  VMaps := AAttrMetadata.CompositionMetadata.Maps;
  Result := Format('SELECT %s FROM %s WHERE %s', [
   SQLGen.BuildOIDNames(VMaps, 0, jtptNone),
   VMaps[0].TableName,
   SQLGen.BuildOIDCondition(VMaps[0].OwnerOIDName, 0, 1, jtptNone)]);
end;

function TJCoreOPFSQLMapping.GenerateUpdateOrderFieldStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  Result := Format('UPDATE %s SET %s WHERE %s', [
   AAttrMetadata.ExternalLinkTableName,
   SQLGen.BuildUpdateOrderField(AAttrMetadata),
   SQLGen.BuildUpdateOrderCondition(AAttrMetadata)]);
end;

procedure TJCoreOPFSQLMapping.WriteDisposeCollection(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDArray: array of IJCoreOPFOID);
var
  VStmt: IJCoreOPFSQLStatement;
  VResultSet: IJCoreOPFSQLResultSet;
  VOID: IJCoreOPFOID;
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
      // Select external IDs
      // DisposeFromResultSetInternal internally uses InternalDispose, which need the
      // object ID to dispose it's own compositions
      { TODO : select for delete isn't necessary if deleted objects doesn't have compositions }
      { TODO : external links of owned compositions }
      VStmt := Driver.CreateStatement;
      for VOID in AOIDArray do
        VOID.WriteToParams(VStmt.Params);
      VStmt.SQL := GenerateSelectForDeleteStatement(AAttrMetadata, Length(AOIDArray));;
      VResultSet := VStmt.OpenCursor;
      // Delete external objects
      if VResultSet.Size > 0 then
        Mapper.AcquireClassMapping(AAttrMetadata.CompositionClass).DisposeFromResultSetInternal(VResultSet);
    end;
    jctAggregation:
    begin
      // Delete external links
      VStmt := Driver.CreateStatement;
      for VOID in AOIDArray do
        VOID.WriteToParams(VStmt.Params);
      VStmt.SQL := GenerateDeleteExternalLinksStatement(AAttrMetadata, Length(AOIDArray));
      VStmt.ExecSQL;
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.WriteDisposeEntityCompositions(
  const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDArray: array of IJCoreOPFOID);
var
  VStmt: IJCoreOPFSQLStatement;
  VResultSet: IJCoreOPFSQLResultSet;
  VOID: IJCoreOPFOID;
begin
  { TODO : Implement reading of more than one entity composition }
  if Length(ACompositionMetadatas) = 1 then
  begin
    VStmt := Driver.CreateStatement;
    for VOID in AOIDArray do
      VOID.WriteToParams(VStmt.Params);
    VStmt.SQL := GenerateSelectCompositionsForDeleteStatement(ACompositionMetadatas, Length(AOIDArray));
    VResultSet := VStmt.OpenCursor(Length(AOIDArray));
    Mapper.AcquireClassMapping(
     ACompositionMetadatas[0].CompositionMetadata.TheClass).DisposeFromResultSetInternal(VResultSet);
  end;
end;

procedure TJCoreOPFSQLMapping.WriteDisposeExternalLinks(
  const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
var
  VStmt: IJCoreOPFSQLStatement;
  VOIDs: TJCoreOPFOIDArray;
  VOID: IJCoreOPFOID;
begin
  if AADM.Metadata.HasExternalLink and AOwnerPID.IsPersistent then
  begin
    VOIDs := AADM.OIDRemoved;
    if Length(VOIDs) > 0 then
    begin
      VStmt := Driver.CreateStatement;
      AOwnerPID.OID.WriteToParams(VStmt.Params);
      for VOID in VOIDs do
        VOID.WriteToParams(VStmt.Params);
      VStmt.SQL := GenerateDeleteExternalLinkIDsStatement(AADM.Metadata, Length(VOIDs));
      VStmt.ExecSQL;
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.WriteInsertExternalLinks(
  const AOwnerPID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection);
var
  VStmt: IJCoreOPFSQLStatement;
  VPIDs: TJCoreOPFPIDArray;
  VPID: TJCoreOPFPID;
begin
  if AADM.Metadata.HasExternalLink then
  begin
    VStmt := Driver.CreateStatement;
    VStmt.SQL := GenerateInsertExternalLinksStatement(AADM.Metadata);
    VPIDs := AADM.PIDAdded;
    for VPID in VPIDs do
    begin
      WriteLinkAttributesToParams(AOwnerPID, VPID, AADM, VStmt.Params);
      VStmt.ExecSQL;
    end;
  end;
end;

procedure TJCoreOPFSQLMapping.WriteOrder(const AOwnerPID: TJCoreOPFPID;
  const AADM: TJCoreOPFADMCollection);
var
  VStmt: IJCoreOPFSQLStatement;
  VPIDReorder: TJCoreOPFPIDArray;
  VHasExternalLink: Boolean;
  I: Integer;
begin
  if AOwnerPID.IsPersistent and AADM.OrderIsDirty then
  begin
    VHasExternalLink := AADM.Metadata.HasExternalLink;
    VStmt := Driver.CreateStatement;
    VStmt.SQL := GenerateUpdateOrderFieldStatement(AADM.Metadata);
    VPIDReorder := AADM.PIDReorder;
    for I := Low(VPIDReorder) to High(VPIDReorder) do
    begin
      VPIDReorder[I].WriteSequenceField(VStmt.Params);
      if VHasExternalLink then
        AOwnerPID.OID.WriteToParams(VStmt.Params);
      VPIDReorder[I].OID.WriteToParams(VStmt.Params);
      VStmt.ExecSQL;
    end;
  end;
end;

function TJCoreOPFSQLMapping.InternalCreateCriteria(
  const ARetriever: IJCoreOPFCriteriaRetriever): IJCoreOPFSQLCriteria;
begin
  Result := TJCoreOPFSQLCriteria.Create(Map.Metadata, Driver, ARetriever);
end;

procedure TJCoreOPFSQLMapping.InternalDispose(const AOIDArray: array of IJCoreOPFOID);
var
  VCompositionMetadatas: TJCoreOPFAttrMetadataArray;
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VStmt: IJCoreOPFSQLStatement;
  VCount: Integer;
  I: Integer;
begin
  SetLength(VCompositionMetadatas, Map.Count);
  VCount := 0;
  for I := 0 to Pred(Map.Count) do
  begin
    VAttrMetadata := Map[I];
    if VAttrMetadata.AttributeType = jatCollection then
      WriteDisposeCollection(VAttrMetadata, AOIDArray)
    else if (VAttrMetadata.AttributeType = jatEntity) and (VAttrMetadata.CompositionType = jctComposition) then
    begin
      VCompositionMetadatas[VCount] := VAttrMetadata;
      Inc(VCount);
    end;
  end;
  SetLength(VCompositionMetadatas, VCount);
  if VCount > 0 then
    WriteDisposeEntityCompositions(VCompositionMetadatas, AOIDArray);
  VStmt := Driver.CreateStatement;
  for I := Low(AOIDArray) to High(AOIDArray) do
    AOIDArray[I].WriteToParams(VStmt.Params);
  VStmt.SQL := GenerateDeleteStatement(Length(AOIDArray));
  VStmt.ExecSQL;
end;

procedure TJCoreOPFSQLMapping.InternalInsert(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement(AParams);
  VStmt.SQL := GenerateInsertStatement(AMapping);
  VStmt.ExecSQL;
end;

function TJCoreOPFSQLMapping.InternalRetrieveCollection(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection): IJCoreOPFResultSet;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  AOwnerPID.OID.WriteToParams(VStmt.Params);
  VStmt.SQL := GenerateSelectCollectionStatement(AOwnerPID.Metadata, AOwnerADM.Metadata);
  Result := VStmt.OpenCursor;
end;

function TJCoreOPFSQLMapping.InternalRetrieveEntity(const AOIDArray: array of IJCoreOPFOID;
  const ABaseMap: TJCoreOPFMap): IJCoreOPFResultSet;
var
  VStmt: IJCoreOPFSQLStatement;
  VOID: IJCoreOPFOID;
begin
  VStmt := Driver.CreateStatement;
  for VOID in AOIDArray do
    VOID.WriteToParams(VStmt.Params);
  if not Assigned(ABaseMap) then
    VStmt.SQL := GenerateSelectBaseStatement(Length(AOIDArray))
  else
    VStmt.SQL := GenerateSelectComplementaryStatement(ABaseMap, Length(AOIDArray));
  Result := VStmt.OpenCursor(Length(AOIDArray));
end;

procedure TJCoreOPFSQLMapping.InternalUpdate(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement(AParams);
  VStmt.SQL := GenerateUpdateStatement(AMapping);
  VStmt.ExecSQL;
end;

procedure TJCoreOPFSQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  I: Integer;
begin
  if IsBaseMapping and Map.HasOrderField then
    AMapping.PID.ReadSequenceField(AResultSet);
  for I := 0 to Pred(AMapping.Count) do
    AMapping.ADM[I].ReadFromResultSet(AResultSet);
end;

procedure TJCoreOPFSQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VPID: TJCoreOPFPID;
  VADMChanged: TJCoreOPFADMArray;
  I: Integer;
begin
  VPID := AMapping.PID;
  if not AMapping.PID.IsPersistent then
  begin
    if Map.HasOrderField then
      VPID.WriteSequenceField(AParams);
    if Map.HasOwnerOID then
    begin
      if Assigned(VPID.Owner) then
        VPID.Owner.OID.WriteToParams(AParams)
      else
        Map.Metadata.OIDClass.WriteNull(AParams);
    end;
  end;
  VADMChanged := AMapping.ADMAttributeChanged;
  for I := Low(VADMChanged) to High(VADMChanged) do
    VADMChanged[I].WriteToParams(AParams);
end;

procedure TJCoreOPFSQLMapping.WriteCollections(const AMapping: TJCoreOPFADMMapping);
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
    WriteDisposeExternalLinks(VPID, VADM);
    Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).StoreCollectionInternal(VPID, VADM);
    WriteInsertExternalLinks(VPID, VADM);
    WriteOrder(VPID, VADM);
  end;
end;

procedure TJCoreOPFSQLMapping.WriteLinkAttributesToParams(const AOwnerPID, APID: TJCoreOPFPID;
  const AADM: TJCoreOPFADMCollection; const AParams: IJCoreOPFParams);
begin
  AOwnerPID.OID.WriteToParams(AParams);
  APID.OID.WriteToParams(AParams);
  if AADM.Metadata.OrderFieldName <> '' then
    APID.WriteSequenceField(AParams);
end;

function TJCoreOPFSQLMapping.BuildOIDCondition(const AOIDNameArray: array of string;
  const AOIDCount: Integer): string;
begin
  Result := SQLGen.BuildOIDCondition(AOIDNameArray, AOIDCount);
end;

function TJCoreOPFSQLMapping.EnsureCollectionAttribute(const AADM: TJCoreOPFADM): TJCoreOPFADMCollection;
begin
  if not (AADM is TJCoreOPFADMCollection) then
    raise EJCoreOPF.Create(2125, S2125_CollectionADMExpected, [
     AADM.PID.Entity.ClassName, AADM.Metadata.Name]);
  Result := TJCoreOPFADMCollection(AADM);
end;

function TJCoreOPFSQLMapping.EnsureEntityAttribute(const AADM: TJCoreOPFADM): TJCoreOPFADMEntity;
begin
  if not (AADM is TJCoreOPFADMEntity) then
    raise EJCoreOPF.Create(2123, S2123_EntityADMExpected, [AADM.PID.Entity.ClassName, AADM.Metadata.Name]);
  Result := TJCoreOPFADMEntity(AADM);
end;

procedure TJCoreOPFSQLMapping.ReadCollection(const AADM: TJCoreOPFADM);
var
  VADM: TJCoreOPFADMCollection;
begin
  VADM := EnsureCollectionAttribute(AADM);
  Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).RetrieveCollectionInternal(AADM.PID, VADM);
end;

function TJCoreOPFSQLMapping.ReadEntity(const AResultSet: IJCoreOPFResultSet;
  const AClass: TClass): TObject;
begin
  Result := Mapper.AcquireClassMapping(AClass).RetrieveEntityFromResultSetInternal(AResultSet);
end;

procedure TJCoreOPFSQLMapping.ReadLazyEntity(const AResultSet: IJCoreOPFResultSet;
  const AADM: TJCoreOPFADM);
var
  VADM: TJCoreOPFADMEntity;
begin
  VADM := EnsureEntityAttribute(AADM);
  Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).
   RetrieveLazyEntityFromResultSetInternal(AResultSet, VADM);
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
    WriteDisposeExternalLinks(VPID, VADM);
    Mapper.AcquireClassMapping(VADM.Metadata.CompositionClass).StoreCollectionInternal(VPID, VADM);
    WriteInsertExternalLinks(VPID, VADM);
  end;
end;

procedure TJCoreOPFSQLMapping.WriteEntity(const AParams: IJCoreOPFParams; const AClass: TClass;
  const AEntity: TObject; const AComposition: Boolean);
var
  VPID: TJCoreOPFPID;
begin
  if Assigned(AEntity) then
  begin
    VPID := Mapper.AcquirePID(AEntity);
    if AComposition or not Assigned(VPID.OID) then
      Mapper.AcquireClassMapping(AEntity.ClassType).StorePID(VPID);
    VPID.OID.WriteToParams(AParams);
  end else
    Model.AcquireMetadata(AClass).OIDClass.WriteNull(AParams);
end;

procedure TJCoreOPFSQLMapping.WriteEntity(const AParams: IJCoreOPFParams; const AADM: TJCoreOPFADM);
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
    VPID.OID.WriteToParams(AParams);
  end else
    VADM.Metadata.CompositionMetadata.OIDClass.WriteNull(AParams);
end;

procedure TJCoreOPFSQLMapping.WriteOwnerOID(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
begin
  AMapping.PID.Owner.OID.WriteToParams(AParams);
end;

constructor TJCoreOPFSQLMapping.Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap;
  const AIsBaseMapping: Boolean);
var
  VDriver: TJCoreOPFDriver;
begin
  VDriver := AMapper.Driver;
  if not (VDriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPF.Create(2109, S2109_UnsupportedDriver, [VDriver.ClassName]);
  inherited Create(AMapper, AMap, AIsBaseMapping);
  FMaps := Map.Metadata.Maps;
  FMapIndex := MapIndexByMap(Map);
  FSQLDriver := TJCoreOPFSQLDriver(VDriver);
  { TODO : Use cache of SQL Generator }
  FSQLGen := SQLGeneratorClass.Create(Map, MapIndex);
end;

destructor TJCoreOPFSQLMapping.Destroy;
begin
  FreeAndNil(FSQLGen);
  inherited Destroy;
end;

class function TJCoreOPFSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := True;
end;

end.

