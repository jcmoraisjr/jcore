(*
  JCore, OPF SQL Criteria Classes
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFCriteriaSQL;

{$I jcore.inc}

interface

uses
  Classes,
  variants,
  contnrs,
  fgl,
  JCoreClasses,
  JCoreOPFDriver,
  JCoreOPFMetadata,
  JCoreOPFCriteria;

type

  { TJCoreOPFSQLCriterionList }

  TJCoreOPFSQLCriterionList = class(TObject)
  private
    FList: TInterfaceList;
  protected
    property List: TInterfaceList read FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ACriterion: IJCoreOPFSQLCriterion);
    function GetSQL(const AResolver: IJCoreOPFSQLCriteriaResolver): string;
  end;

  TJCoreOPFSQLCriteriaJoinType = (jcjtInnerJoin, jcjtLeftOuterJoin);

  { TJCoreOPFSQLCriteriaAlias }

  TJCoreOPFSQLCriteriaAlias = class(TObject)
  private
    FAliasName: string;
    FIncludeFields: Boolean;
    FIncludeOID: Boolean;
    FJoinAlias: TJCoreOPFSQLCriteriaAlias;
    FJoinType: TJCoreOPFSQLCriteriaJoinType;
    FMap: TJCoreOPFMap;
    FTableName: string;
  public
    constructor Create(const AMap: TJCoreOPFMap; const AAliasName: string; const AIncludeOID, AIncludeFields: Boolean);
    constructor Create(const AMap: TJCoreOPFMap; const AAliasName: string; const AJoinType: TJCoreOPFSQLCriteriaJoinType; const AJoinAlias: TJCoreOPFSQLCriteriaAlias; const AIncludeOID, AIncludeFields: Boolean);
    function BuildFieldNames(const AUseTableAlias: Boolean): string;
    function BuildOIDNames(const AUseTableAlias: Boolean): string;
    function FindAttribute(const AAttributeName: string): TJCoreOPFAttrMetadata;
    function JoinCondition: string;
    function JoinTypeName: string;
    function OID: TJCoreStringArray;
    function TableName(const AUseTableAlias: Boolean): string;
    property AliasName: string read FAliasName;
    property JoinType: TJCoreOPFSQLCriteriaJoinType read FJoinType;
  end;

  TJCoreOPFSQLCriteriaAbstractFetchStrategy = (jcfsLinkSubMaps, jcfsFetchSubMaps);

  { TJCoreOPFSQLCriteriaAliasList }

  TJCoreOPFSQLCriteriaAliasList = class(specialize TFPGObjectList<TJCoreOPFSQLCriteriaAlias>)
  protected
    function BuildAliasName: string;
  public
    function AddInnerAlias(const AMap: TJCoreOPFMap; const AJoinAlias: TJCoreOPFSQLCriteriaAlias): TJCoreOPFSQLCriteriaAlias;
    function AddMainAlias(const AMap: TJCoreOPFMap): TJCoreOPFSQLCriteriaAlias;
    function AddSubMapAlias(const AMap: TJCoreOPFMap; const AJoinAlias: TJCoreOPFSQLCriteriaAlias; const AFetchStrategy: TJCoreOPFSQLCriteriaAbstractFetchStrategy): TJCoreOPFSQLCriteriaAlias;
  end;

  { TJCoreOPFSQLCriteria }

  TJCoreOPFSQLCriteria = class(TInterfacedObject, IJCoreOPFSQLCriteria, IJCoreOPFSQLCriteriaResolver)
  private
    FAliasList: TJCoreOPFSQLCriteriaAliasList;
    FCriterionList: TJCoreOPFSQLCriterionList;
    FDriver: TJCoreOPFSQLDriver;
    FMetadata: TJCoreOPFClassMetadata;
    FParams: IJCoreOPFParams;
    FRetriever: IJCoreOPFCriteriaRetriever;
    function GetMetadata: TJCoreOPFClassMetadata;
    function GetParams: IJCoreOPFParams;
  protected
    function BuildSQLFrom: string;
    function BuildSQLSelect: string;
    function BuildStatement: string;
    procedure CreateDefaultAliasList(const AMetadata: TJCoreOPFClassMetadata);
    function FieldNameByAttributeName(const AAttributeName: string): string;
    function UseTableAlias: Boolean;
    property AliasList: TJCoreOPFSQLCriteriaAliasList read FAliasList;
    property CriterionList: TJCoreOPFSQLCriterionList read FCriterionList;
    property Driver: TJCoreOPFSQLDriver read FDriver;
    property Metadata: TJCoreOPFClassMetadata read FMetadata;
    property Params: IJCoreOPFParams read GetParams;
    property Retriever: IJCoreOPFCriteriaRetriever read FRetriever;
    function IJCoreOPFSQLCriteria.Metadata = GetMetadata;
    function IJCoreOPFSQLCriteriaResolver.Params = GetParams;
  public
    constructor Create(const AMetadata: TJCoreOPFClassMetadata; const ADriver: TJCoreOPFSQLDriver; const ARetriever: IJCoreOPFCriteriaRetriever);
    destructor Destroy; override;
    function Add(const ACriterion: IJCoreOPFSQLCriterion): IJCoreOPFSQLCriteria;
    function RetrieveList: TObjectList;
    function RetrieveResultSet: IJCoreOPFSQLResultSet;
    function RetrieveUnique: TObject;
  end;

  { TJCoreOPFCriterionValueExpression }

  TJCoreOPFCriterionValueExpression = class(TInterfacedObject, IJCoreOPFSQLCriterion)
  private
    FAttribute: String;
    FOperation: String;
    FValue: Variant;
  protected
    function GetSQL(const AResolver: IJCoreOPFSQLCriteriaResolver): string;
  public
    constructor Create(const AAttribute: string; const AValue: Variant; const AOperation: string);
  end;

  { TJCoreOPFCriterionSQLExpression }

  TJCoreOPFCriterionSQLExpression = class(TInterfacedObject, IJCoreOPFSQLCriterion)
  private
    FParams: TJCoreVariantArray;
    FQuery: String;
  protected
    function GetSQL(const AResolver: IJCoreOPFSQLCriteriaResolver): string;
  public
    constructor Create(const AQuery: string; AParams: TJCoreVariantArray);
  end;

  { TJCoreOPFCriteriaRestriction }

  TJCoreOPFCriteriaRestriction = class(TObject)
  public
    class function Diff(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
    class function Eq(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
    class function Ge(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
    class function Gt(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
    class function Le(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
    class function Like(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
    class function Lt(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
    class function SQL(const AQuery: string; const AParams: TJCoreVariantArray): IJCoreOPFSQLCriterion;
  end;

implementation

uses
  sysutils,
  JCoreConsts;

{ TJCoreOPFSQLCriterionList }

constructor TJCoreOPFSQLCriterionList.Create;
begin
  inherited Create;
  FList := TInterfaceList.Create;
end;

destructor TJCoreOPFSQLCriterionList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TJCoreOPFSQLCriterionList.Add(const ACriterion: IJCoreOPFSQLCriterion);
begin
  List.Add(ACriterion);
end;

function TJCoreOPFSQLCriterionList.GetSQL(const AResolver: IJCoreOPFSQLCriteriaResolver): string;
var
  I: Integer;
begin
  if List.Count > 0 then
  begin
    Result := IJCoreOPFSQLCriterion(List[0]).GetSQL(AResolver);
    for I := 1 to Pred(List.Count) do
      Result := ' AND ' + IJCoreOPFSQLCriterion(List[I]).GetSQL(AResolver);
  end else
    Result := '';
end;

{ TJCoreOPFSQLCriteriaAlias }

constructor TJCoreOPFSQLCriteriaAlias.Create(const AMap: TJCoreOPFMap; const AAliasName: string;
  const AIncludeOID, AIncludeFields: Boolean);
begin
  inherited Create;
  FMap := AMap;
  FAliasName := AAliasName;
  FIncludeOID := AIncludeOID;
  FIncludeFields := AIncludeFields;
end;

constructor TJCoreOPFSQLCriteriaAlias.Create(const AMap: TJCoreOPFMap; const AAliasName: string;
  const AJoinType: TJCoreOPFSQLCriteriaJoinType; const AJoinAlias: TJCoreOPFSQLCriteriaAlias;
  const AIncludeOID, AIncludeFields: Boolean);
begin
  inherited Create;
  FMap := AMap;
  FAliasName := AAliasName;
  FJoinType := AJoinType;
  FJoinAlias := AJoinAlias;
  FIncludeOID := AIncludeOID;
  FIncludeFields := AIncludeFields;
end;

function TJCoreOPFSQLCriteriaAlias.BuildFieldNames(const AUseTableAlias: Boolean): string;
var
  VPrefix: string;
  I: Integer;
begin
  Result := '';
  if FIncludeFields and Assigned(FMap) then
  begin
    if AUseTableAlias then
      VPrefix := AliasName + '.'
    else
      VPrefix := '';
    for I := 0 to Pred(FMap.Count) do
      Result := Result + VPrefix + FMap[I].PersistentFieldName + ',';
  end;
end;

function TJCoreOPFSQLCriteriaAlias.BuildOIDNames(const AUseTableAlias: Boolean): string;
var
  VOIDArray: TJCoreStringArray;
  VPrefix: string;
  I: Integer;
begin
  Result := '';
  if FIncludeOID and Assigned(FMap) then
  begin
    VOIDArray := FMap.OIDName;
    if AUseTableAlias then
      VPrefix := AliasName + '.'
    else
      VPrefix := '';
    for I := Low(VOIDArray) to High(VOIDArray) do
      Result := Result + VPrefix + VOIDArray[I] + ',';
  end;
end;

function TJCoreOPFSQLCriteriaAlias.FindAttribute(const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  if Assigned(FMap) then
    Result := FMap.Metadata.FindAttribute(AAttributeName)
  else
    Result := nil;
end;

function TJCoreOPFSQLCriteriaAlias.JoinCondition: string;
var
  VJoinOID: TJCoreStringArray;
  VOID: TJCoreStringArray;
  I: Integer;
begin
  if not Assigned(FJoinAlias) then
    raise EJCoreOPF.Create(2128, S2128_UndefinedJoinAlias, [AliasName]);
  VJoinOID := FJoinAlias.OID;
  VOID := OID;
  if Length(VJoinOID) <> Length(VOID) then
    raise EJCoreOPF.Create(2129, S2129_IncompatibleJoinOID, [AliasName]);
  Result := '';
  for I := Low(VOID) to High(VOID) do
    Result := Result + Format('%s.%s = %s.%s AND ', [FJoinAlias.AliasName, VJoinOID[I], AliasName, VOID[I]]);
  SetLength(Result, Length(Result) - 5);
end;

function TJCoreOPFSQLCriteriaAlias.JoinTypeName: string;
const
  CJoinType: array[TJCoreOPFSQLCriteriaJoinType] of string = ('INNER JOIN', 'LEFT OUTER JOIN');
begin
  Result := CJoinType[JoinType];
end;

function TJCoreOPFSQLCriteriaAlias.OID: TJCoreStringArray;
begin
  if Assigned(FMap) then
    Result := FMap.OIDName
  else
    SetLength(Result, 0);
end;

function TJCoreOPFSQLCriteriaAlias.TableName(const AUseTableAlias: Boolean): string;
var
  VTableName: string;
begin
  if Assigned(FMap) then
    VTableName := FMap.TableName
  else
    VTableName := FTableName;
  if AUseTableAlias then
    Result := Format('%s %s', [VTableName, AliasName])
  else
    Result := VTableName;
end;

{ TJCoreOPFSQLCriteriaAliasList }

function TJCoreOPFSQLCriteriaAliasList.BuildAliasName: string;
begin
  Result := 't' + IntToStr(Count);
end;

function TJCoreOPFSQLCriteriaAliasList.AddInnerAlias(const AMap: TJCoreOPFMap;
  const AJoinAlias: TJCoreOPFSQLCriteriaAlias): TJCoreOPFSQLCriteriaAlias;
begin
  Result := TJCoreOPFSQLCriteriaAlias.Create(AMap, BuildAliasName, jcjtInnerJoin, AJoinAlias, False, True);
  Add(Result);
end;

function TJCoreOPFSQLCriteriaAliasList.AddMainAlias(const AMap: TJCoreOPFMap): TJCoreOPFSQLCriteriaAlias;
begin
  Result := TJCoreOPFSQLCriteriaAlias.Create(AMap, BuildAliasName, True, True);
  Add(Result);
end;

function TJCoreOPFSQLCriteriaAliasList.AddSubMapAlias(const AMap: TJCoreOPFMap;
  const AJoinAlias: TJCoreOPFSQLCriteriaAlias;
  const AFetchStrategy: TJCoreOPFSQLCriteriaAbstractFetchStrategy): TJCoreOPFSQLCriteriaAlias;
begin
  Result := TJCoreOPFSQLCriteriaAlias.Create(AMap, BuildAliasName, jcjtLeftOuterJoin, AJoinAlias,
   AFetchStrategy = jcfsLinkSubMaps, AFetchStrategy = jcfsFetchSubMaps);
  Add(Result);
end;

{ TJCoreOPFSQLCriteria }

function TJCoreOPFSQLCriteria.GetParams: IJCoreOPFParams;
begin
  if not Assigned(FParams) then
    FParams := TJCoreOPFParams.Create;
  Result := FParams;
end;

function TJCoreOPFSQLCriteria.BuildSQLFrom: string;
var
  VUsePrefix: Boolean;
  VAlias: TJCoreOPFSQLCriteriaAlias;
  I: Integer;
begin
  VUsePrefix := UseTableAlias;
  Result := AliasList[0].TableName(VUsePrefix);
  for I := 1 to Pred(AliasList.Count) do
  begin
    VAlias := AliasList[I];
    Result := Format('%s %s %s ON %s', [
     Result,
     VAlias.JoinTypeName,
     VAlias.TableName(VUsePrefix),
     VAlias.JoinCondition]);
  end;
end;

function TJCoreOPFSQLCriteria.BuildSQLSelect: string;
var
  VUsePrefix: Boolean;
  I: Integer;
begin
  VUsePrefix := UseTableAlias;
  Result := '';
  for I := 0 to Pred(AliasList.Count) do
    Result := Result + AliasList[I].BuildOIDNames(VUsePrefix);
  for I := 0 to Pred(AliasList.Count) do
    Result := Result + AliasList[I].BuildFieldNames(VUsePrefix);
  SetLength(Result, Length(Result) - 1);
end;

function TJCoreOPFSQLCriteria.GetMetadata: TJCoreOPFClassMetadata;
begin
  Result := FMetadata;
end;

function TJCoreOPFSQLCriteria.BuildStatement: string;
var
  VCondition: string;
begin
  Result := Format('SELECT %s FROM %s', [BuildSQLSelect, BuildSQLFrom]);
  VCondition := CriterionList.GetSQL(Self);
  if VCondition <> '' then
    Result := Format('%s WHERE %s', [Result, VCondition]);
end;

procedure TJCoreOPFSQLCriteria.CreateDefaultAliasList(const AMetadata: TJCoreOPFClassMetadata);
var
  VMaps: TJCoreOPFMaps;
  VSubMaps: TJCoreOPFMaps;
  I: Integer;
  VMainAlias: TJCoreOPFSQLCriteriaAlias;
begin
  VMaps := AMetadata.Maps;
  VSubMaps := AMetadata.SubMaps;
  VMainAlias := AliasList.AddMainAlias(VMaps[0]);
  for I := 1 to Pred(VMaps.Count) do
    AliasList.AddInnerAlias(VMaps[I], VMainAlias);
  for I := 0 to Pred(VSubMaps.Count) do
    AliasList.AddSubMapAlias(VSubMaps[I], VMainAlias, jcfsLinkSubMaps);
end;

function TJCoreOPFSQLCriteria.FieldNameByAttributeName(const AAttributeName: string): string;
var
  VAttr: TJCoreOPFAttrMetadata;
  I: Integer;
begin
  for I := 0 to Pred(AliasList.Count) do
  begin
    VAttr := AliasList[I].FindAttribute(AAttributeName);
    if Assigned(VAttr) then
    begin
      if UseTableAlias then
        Result := AliasList[I].AliasName + '.' + VAttr.PersistentFieldName
      else
        Result := VAttr.PersistentFieldName;
      Exit;
    end;
  end;
  raise EJCoreOPF.Create(2127, S2127_AttributeNotFound, [Metadata.TheClass.ClassName, AAttributeName]);
end;

function TJCoreOPFSQLCriteria.UseTableAlias: Boolean;
begin
  Result := AliasList.Count > 1;
end;

constructor TJCoreOPFSQLCriteria.Create(const AMetadata: TJCoreOPFClassMetadata;
  const ADriver: TJCoreOPFSQLDriver; const ARetriever: IJCoreOPFCriteriaRetriever);
begin
  inherited Create;
  FMetadata := AMetadata;
  FDriver := ADriver;
  FRetriever := ARetriever;
  FAliasList := TJCoreOPFSQLCriteriaAliasList.Create(True);
  FCriterionList := TJCoreOPFSQLCriterionList.Create;
  CreateDefaultAliasList(AMetadata);
end;

destructor TJCoreOPFSQLCriteria.Destroy;
begin
  FreeAndNil(FAliasList);
  FreeAndNil(FCriterionList);
  inherited Destroy;
end;

function TJCoreOPFSQLCriteria.Add(const ACriterion: IJCoreOPFSQLCriterion): IJCoreOPFSQLCriteria;
begin
  CriterionList.Add(ACriterion);
  Result := Self;
end;

function TJCoreOPFSQLCriteria.RetrieveList: TObjectList;
var
  VResultSet: IJCoreOPFSQLResultSet;
  VObjectArray: TJCoreObjectArray;
  I: Integer;
begin
  VResultSet := RetrieveResultSet;
  VObjectArray := Retriever.RetrieveResultSet(Metadata.TheClass, VResultSet);
  Result := TObjectList.Create(True);
  try
    for I := Low(VObjectArray) to High(VObjectArray) do
    begin
      Result.Add(VObjectArray[I]);
      VObjectArray[I] := nil;
    end;
  except
    FreeAndNil(Result);
    for I := Low(VObjectArray) to High(VObjectArray) do
      VObjectArray[I].Free;
  end;
end;

function TJCoreOPFSQLCriteria.RetrieveResultSet: IJCoreOPFSQLResultSet;
var
  VQuery: string;
  VStmt: IJCoreOPFSQLStatement;
begin
  VQuery := BuildStatement;
  VStmt := Driver.CreateStatement(Params);
  VStmt.SQL := VQuery;
  Result := VStmt.OpenCursor;
end;

function TJCoreOPFSQLCriteria.RetrieveUnique: TObject;
var
  VObjectList: TObjectList;
begin
  VObjectList := RetrieveList;
  try
    if VObjectList.Count <> 1 then
      raise EJCoreOPF.Create(2113, S2113_UnexpectedResultSetSize, [1, VObjectList.Count]);
    Result := VObjectList[0];
    VObjectList.Extract(Result);
  finally
    FreeAndNil(VObjectList);
  end;
end;

{ TJCoreOPFCriterionValueExpression }

function TJCoreOPFCriterionValueExpression.GetSQL(const AResolver: IJCoreOPFSQLCriteriaResolver): string;
var
  VFieldName: string;
begin
  VFieldName := AResolver.FieldNameByAttributeName(FAttribute);
  AResolver.Params.WriteVariant(FValue);
  Result := Format('%s %s ?', [VFieldName, FOperation]);
end;

constructor TJCoreOPFCriterionValueExpression.Create(const AAttribute: string; const AValue: Variant;
  const AOperation: string);
begin
  inherited Create;
  FAttribute := AAttribute;
  FValue := AValue;
  FOperation := AOperation;
end;

{ TJCoreOPFCriterionSQLExpression }

function TJCoreOPFCriterionSQLExpression.GetSQL(const AResolver: IJCoreOPFSQLCriteriaResolver): string;
var
  VResolverParams: IJCoreOPFParams;
  I: Integer;
begin
  Result := FQuery;
  VResolverParams := AResolver.Params;
  for I := Low(FParams) to High(FParams) do
    VResolverParams.WriteVariant(FParams[I]);
end;

constructor TJCoreOPFCriterionSQLExpression.Create(const AQuery: string; AParams: TJCoreVariantArray);
begin
  inherited Create;
  FQuery := AQuery;
  FParams := AParams;
end;

{ TJCoreOPFCriteriaRestriction }

class function TJCoreOPFCriteriaRestriction.Diff(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
begin
  Result := TJCoreOPFCriterionValueExpression.Create(AAttributeName, AValue, '<>');
end;

class function TJCoreOPFCriteriaRestriction.Eq(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
begin
  Result := TJCoreOPFCriterionValueExpression.Create(AAttributeName, AValue, '=');
end;

class function TJCoreOPFCriteriaRestriction.Ge(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
begin
  Result := TJCoreOPFCriterionValueExpression.Create(AAttributeName, AValue, '>=');
end;

class function TJCoreOPFCriteriaRestriction.Gt(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
begin
  Result := TJCoreOPFCriterionValueExpression.Create(AAttributeName, AValue, '>');
end;

class function TJCoreOPFCriteriaRestriction.Le(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
begin
  Result := TJCoreOPFCriterionValueExpression.Create(AAttributeName, AValue, '<=');
end;

class function TJCoreOPFCriteriaRestriction.Like(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
begin
  Result := TJCoreOPFCriterionValueExpression.Create(AAttributeName, AValue, 'LIKE');
end;

class function TJCoreOPFCriteriaRestriction.Lt(const AAttributeName, AValue: Variant): IJCoreOPFSQLCriterion;
begin
  Result := TJCoreOPFCriterionValueExpression.Create(AAttributeName, AValue, '<');
end;

class function TJCoreOPFCriteriaRestriction.SQL(const AQuery: string; const AParams: TJCoreVariantArray): IJCoreOPFSQLCriterion;
begin
  Result := TJCoreOPFCriterionSQLExpression.Create(AQuery, AParams);
end;

end.

