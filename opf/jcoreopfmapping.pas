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
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  typinfo,
  fgl,
  JCoreClasses,
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFOIDGen,
  JCoreOPFMetadata,
  JCoreOPFCriteria;

type

  TJCoreOPFClassMapping = class;

  { IJCoreOPFMapper }

  IJCoreOPFMapper = interface
    function AcquireClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    procedure AddInTransactionPID(const APID: TJCoreOPFPID);
    function Driver: TJCoreOPFDriver;
    function Model: TJCoreOPFModel;
  end;

  TJCoreOPFMappingClass = class of TJCoreOPFMapping;

  { TJCoreOPFMappingClassRef }

  TJCoreOPFMappingClassRef = class(TObject)
  private
    FMap: TJCoreOPFMap;
    FMappingClass: TJCoreOPFMappingClass;
  public
    constructor Create(const AMappingClass: TJCoreOPFMappingClass; AMap: TJCoreOPFMap);
    property Map: TJCoreOPFMap read FMap;
    property MappingClass: TJCoreOPFMappingClass read FMappingClass;
  end;

  TJCoreOPFMappingClassRefList = specialize TFPGObjectList<TJCoreOPFMappingClassRef>;
  TJCoreOPFMappingClassRefListMap = specialize TFPGMap<Pointer, TJCoreOPFMappingClassRefList>;

  TJCoreOPFMappingClassList = specialize TFPGList<TJCoreOPFMappingClass>;

  { TJCoreOPFMappingClassFactory }

  TJCoreOPFMappingClassFactory = class(specialize TJCoreFactory<TJCoreOPFMappingClass>)
  private
    FMappingClassList: TJCoreOPFMappingClassList;
    FMappingClassRefListMap: TJCoreOPFMappingClassRefListMap;
    FModel: TJCoreOPFModel;
  protected
    function CreateMappingClassRef(const AMap: TJCoreOPFMap): TJCoreOPFMappingClassRef;
    function CreateMappingClassRefList(const AClass: TClass): TJCoreOPFMappingClassRefList;
    property MappingClassList: TJCoreOPFMappingClassList read FMappingClassList;
    property MappingClassRefListMap: TJCoreOPFMappingClassRefListMap read FMappingClassRefListMap;
  public
    constructor Create(const AModel: TJCoreOPFModel);
    destructor Destroy; override;
    function AcquireMappingClassRefList(const AClass: TClass): TJCoreOPFMappingClassRefList;
    procedure AddMappingClass(const AMappingClassArray: array of TJCoreOPFMappingClass);
    property Model: TJCoreOPFModel read FModel;
  end;

  TJCoreOPFMapping = class;
  TJCoreOPFMappingList = specialize TFPGObjectList<TJCoreOPFMapping>;
  TJCoreOPFMappingListMap = specialize TFPGMap<Pointer, TJCoreOPFMappingList>;

  { TJCoreOPFMappingFactory }

  TJCoreOPFMappingFactory = class(TObject)
  private
    FDriver: TJCoreOPFDriver;
    FMapper: IJCoreOPFMapper;
    FMappingClassFactory: TJCoreOPFMappingClassFactory;
    FMappingListMap: TJCoreOPFMappingListMap;
  protected
    function CreateMappingList(const AClass: TClass): TJCoreOPFMappingList;
    property MappingClassFactory: TJCoreOPFMappingClassFactory read FMappingClassFactory;
    property MappingListMap: TJCoreOPFMappingListMap read FMappingListMap;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AMappingClassFactory: TJCoreOPFMappingClassFactory; const ADriver: TJCoreOPFDriver);
    destructor Destroy; override;
    function AcquireMappingList(const AClass: TClass): TJCoreOPFMappingList;
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
  end;

  { TJCoreOPFComplementaryClassMapping }

  TJCoreOPFComplementaryClassMapping = class(TObject)
  private
    FClass: TClass;
    FPIDList: TJCoreOPFPIDList;
  protected
    property PIDList: TJCoreOPFPIDList read FPIDList;
  public
    constructor Create(const AClass: TClass);
    destructor Destroy; override;
    procedure AddPID(const APID: TJCoreOPFPID);
    function CreateOIDArray: TJCoreOPFOIDArray;
    function PIDByOID(const AOID: IJCoreOPFOID): TJCoreOPFPID;
    property TheClass: TClass read FClass;
  end;

  TJCoreOPFComplementaryClassMappingMap = specialize TFPGMap<Pointer, TJCoreOPFComplementaryClassMapping>;

  { TJCoreOPFComplementaryMappingManager }

  TJCoreOPFComplementaryMappingManager = class(TObject)
  private
    FClassMappingMap: TJCoreOPFComplementaryClassMappingMap;
    function GetClassMappings(const AIndex: Integer): TJCoreOPFComplementaryClassMapping;
  protected
    function AcquireClassMapping(const AClass: TClass): TJCoreOPFComplementaryClassMapping;
    property ClassMappingMap: TJCoreOPFComplementaryClassMappingMap read FClassMappingMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPID(const APID: TJCoreOPFPID);
    function ClassMappingCount: Integer;
    procedure Clear;
    property ClassMappings[const AIndex: Integer]: TJCoreOPFComplementaryClassMapping read GetClassMappings; default;
  end;

  { TJCoreOPFClassMapping }

  TJCoreOPFClassMapping = class(TObject, IJCoreOPFCriteriaRetriever)
  // Class mappings of a single class metadata, without parents.
  // Based on TJCoreOPFMaps and synchronized with TJCoreOPFADMMapping ADMs.
  private
    FDriver: TJCoreOPFDriver;
    FGenerator: IJCoreOPFOIDGenerator;
    FMapper: IJCoreOPFMapper;
    FMapping: TJCoreOPFMapping;
    FMappingList: TJCoreOPFMappingList;
    FMetadata: TJCoreOPFClassMetadata;
    function AcquireClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
    function CreateOIDFromResultSet(const AResultSet: IJCoreOPFResultSet): IJCoreOPFOID;
    function CreateOIDFromString(const AOID: string): IJCoreOPFOID;
    procedure Dispose(const AOIDArray: array of IJCoreOPFOID);
    procedure EnsureMappingConsistency(const APID: TJCoreOPFPID);
    function GetGenerator: IJCoreOPFOIDGenerator;
    function MapIndexByMap(const ABaseMap: TJCoreOPFMap): Integer;
    function Retrieve(const AOIDArray: array of IJCoreOPFOID): TJCoreObjectArray;
    function RetrieveResultSet(const AClass: TClass; const AResultSet: IJCoreOPFResultSet): TJCoreObjectArray;
    function RetrieveFromResultSet(const AResultSet: IJCoreOPFResultSet): TJCoreObjectArray;
  protected
    property Driver: TJCoreOPFDriver read FDriver;
    property Generator: IJCoreOPFOIDGenerator read GetGenerator;
    property Mapper: IJCoreOPFMapper read FMapper;
    property MappingList: TJCoreOPFMappingList read FMappingList;
  public
    // Internal mapping implementations, mapped from other classes
    procedure DisposeFromResultSetInternal(const AResultSet: IJCoreOPFResultSet);
    procedure DisposeFromOIDInternal(const AOIDArray: TJCoreOPFOIDArray);
    function RetrieveEntityFromResultSetInternal(const AResultSet: IJCoreOPFResultSet): TObject;
    procedure RetrieveMapsInternal(const AResultSet: IJCoreOPFResultSet; const APID: TJCoreOPFPID; const ABaseMap: TJCoreOPFMap);
    procedure RetrieveCollectionInternal(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure RetrieveEntityInternal(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMEntity);
    procedure RetrieveLazyEntityFromResultSetInternal(const AResultSet: IJCoreOPFResultSet; const ALazyADM: TJCoreOPFADMEntity);
    procedure StoreCollectionInternal(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
  public
    // Public facades, mapped from session facades
    constructor Create(const AMapper: IJCoreOPFMapper; const AMetadata: TJCoreOPFClassMetadata; AMappingList: TJCoreOPFMappingList);
    function CreateCriteria: IJCoreOPFSQLCriteria;
    procedure DisposeOID(const AStringOIDArray: array of string);
    procedure DisposePID(const APIDArray: array of TJCoreOPFPID);
    function RetrieveOID(const AStringOID: string): TObject;
    procedure StorePID(const APID: TJCoreOPFPID);
    property Mapping: TJCoreOPFMapping read FMapping;
    property Metadata: TJCoreOPFClassMetadata read FMetadata;
  end;

  TJCoreOPFClassMappingMap = specialize TFPGMap<Pointer, TJCoreOPFClassMapping>;

  { TJCoreOPFMapping }

  TJCoreOPFMapping = class(TObject)
  private
    FDriver: TJCoreOPFDriver;
    FIsBaseMapping: Boolean;
    FMap: TJCoreOPFMap;
    FMapper: IJCoreOPFMapper;
    FModel: TJCoreOPFModel;
  protected
    // CreateEntityFromResultSet should analyze the storage mechanism in order
    // to create an instance of the correct class, eg:
    //
    //   TObject
    //   +-TSomeAbstract
    //     +-TSomeConcrete
    //       +-TConcreteA
    //       +-TConcreteB
    //
    // The mapping being called depends on the dependency mapping, eg, if
    // TAnother.Some references TSomeConcrete, the TSomeConcrete mapping will
    // be called. If a TConcreteB is actually stored, a TConcreteB instance
    // should be created.
    //
    // CreateEntityFromResultSet is called after retrieving (GenerateSelectStatement
    // on sql mapping), after reading of the mandatory ID field(s), and before
    // ReadFromResultSet. Use the retrieving implementation to retrieve
    // information about the class of the instance actually stored, read them
    // inside CreateEntityFromResultSet. The SelectClassFromResultSet should be of
    // some help.
    function CreateEntityFromResultSet(const AResultSet: IJCoreOPFResultSet): TObject; virtual;
    function CreateParams: IJCoreOPFParams; virtual;
    // abstract facades
    function InternalCreateCriteria(const ARetriever: IJCoreOPFCriteriaRetriever): IJCoreOPFSQLCriteria; virtual; abstract;
    function InternalCreateOIDArray(const AGenerator: IJCoreOPFOIDGenerator; const AOIDCount: Integer): TJCoreOPFOIDArray;
    procedure InternalDispose(const AOIDArray: array of IJCoreOPFOID); virtual; abstract;
    procedure InternalInsert(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); virtual; abstract;
    function InternalRetrieveCollection(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection): IJCoreOPFResultSet; virtual; abstract;
    function InternalRetrieveEntity(const AOIDArray: array of IJCoreOPFOID; const ABaseMap: TJCoreOPFMap): IJCoreOPFResultSet; virtual; abstract;
    procedure InternalUpdate(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); virtual; abstract;
    // direct field <-> attribute mapping
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); virtual;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); virtual;
    procedure WriteCollections(const AMapping: TJCoreOPFADMMapping); virtual;
    // support
    function InternalCreateGenerator: IJCoreOPFOIDGenerator; virtual;
    function SelectClassFromResultSet(const AResultSet: IJCoreOPFResultSet; const ASubClassArray: array of TClass; const ADefaultClass: TClass): TClass;
    property Driver: TJCoreOPFDriver read FDriver;
    property IsBaseMapping: Boolean read FIsBaseMapping;
    property Map: TJCoreOPFMap read FMap;
    property Mapper: IJCoreOPFMapper read FMapper;
    property Model: TJCoreOPFModel read FModel;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap; const AIsBaseMapping: Boolean); virtual;
    class function Apply(const AMap: TJCoreOPFMap): Boolean; virtual; abstract;
    function CreateCriteria(const ARetriever: IJCoreOPFCriteriaRetriever): IJCoreOPFSQLCriteria;
    function CreateEntity(const AResultSet: IJCoreOPFResultSet): TObject;
    function CreateGenerator: IJCoreOPFOIDGenerator;
    function CreateOID(const AGenerator: IJCoreOPFOIDGenerator): IJCoreOPFOID;
    procedure Dispose(const AOIDArray: array of IJCoreOPFOID);
    function RetrieveCollection(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection): IJCoreOPFResultSet;
    function RetrieveEntity(const AOIDArray: array of IJCoreOPFOID; const ABaseMap: TJCoreOPFMap): IJCoreOPFResultSet;
    procedure RetrieveMappingFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping);
    procedure Store(const AMapping: TJCoreOPFADMMapping);
  end;

implementation

uses
  sysutils,
  JCoreConsts,
  JCoreMetadata;

{ TJCoreOPFMappingClassRef }

constructor TJCoreOPFMappingClassRef.Create(const AMappingClass: TJCoreOPFMappingClass; AMap: TJCoreOPFMap);
begin
  inherited Create;
  FMap := AMap;
  FMappingClass := AMappingClass;
end;

{ TJCoreOPFMappingClassFactory }

function TJCoreOPFMappingClassFactory.CreateMappingClassRef(
  const AMap: TJCoreOPFMap): TJCoreOPFMappingClassRef;
var
  VActualMappingClass: TJCoreOPFMappingClass;
  VMappingClass: TJCoreOPFMappingClass;
begin
  VActualMappingClass := nil;
  for VMappingClass in MappingClassList do
    if VMappingClass.Apply(AMap) then
      VActualMappingClass := Choose(VActualMappingClass, VMappingClass);
  if not Assigned(VActualMappingClass) then
    raise EJCoreOPF.Create(2116, S2116_MappingNotFound, [AMap.Metadata.TheClass.ClassName]);
  Result := TJCoreOPFMappingClassRef.Create(VActualMappingClass, AMap);
end;

function TJCoreOPFMappingClassFactory.CreateMappingClassRefList(
  const AClass: TClass): TJCoreOPFMappingClassRefList;
var
  VMappingClassRefList: TJCoreOPFMappingClassRefList;
  VMaps: TJCoreOPFMaps;
  VMap: TJCoreOPFMap;
begin
  VMappingClassRefList := TJCoreOPFMappingClassRefList.Create(True);
  try
    VMaps := Model.AcquireMetadata(AClass).Maps;
    for VMap in VMaps do
      VMappingClassRefList.Add(CreateMappingClassRef(VMap));
  except
    FreeAndNil(VMappingClassRefList);
    raise;
  end;
  Result := VMappingClassRefList;
end;

constructor TJCoreOPFMappingClassFactory.Create(const AModel: TJCoreOPFModel);
begin
  inherited Create;
  FModel := AModel;
  FMappingClassList := TJCoreOPFMappingClassList.Create;
  FMappingClassRefListMap := TJCoreOPFMappingClassRefListMap.Create;
end;

destructor TJCoreOPFMappingClassFactory.Destroy;
var
  I: Integer;
begin
  for I := 0 to Pred(FMappingClassRefListMap.Count) do
    FMappingClassRefListMap.Data[I].Free;
  FreeAndNil(FMappingClassRefListMap);
  FreeAndNil(FMappingClassList);
  inherited Destroy;
end;

function TJCoreOPFMappingClassFactory.AcquireMappingClassRefList(
  const AClass: TClass): TJCoreOPFMappingClassRefList;
var
  VIndex: Integer;
begin
  VIndex := MappingClassRefListMap.IndexOf(AClass);
  if VIndex = -1 then
    VIndex := MappingClassRefListMap.Add(AClass, CreateMappingClassRefList(AClass));
  Result := MappingClassRefListMap.Data[VIndex];
end;

procedure TJCoreOPFMappingClassFactory.AddMappingClass(
  const AMappingClassArray: array of TJCoreOPFMappingClass);
var
  I: Integer;
begin
  for I := Low(AMappingClassArray) to High(AMappingClassArray) do
    MappingClassList.Add(AMappingClassArray[I]);
end;

{ TJCoreOPFMappingFactory }

function TJCoreOPFMappingFactory.CreateMappingList(
  const AClass: TClass): TJCoreOPFMappingList;
var
  VMappingClassRefList: TJCoreOPFMappingClassRefList;
  VMappingList: TJCoreOPFMappingList;
  VMappingClassRef: TJCoreOPFMappingClassRef;
  I: Integer;
begin
  VMappingClassRefList := MappingClassFactory.AcquireMappingClassRefList(AClass);
  VMappingList := TJCoreOPFMappingList.Create(True);
  try
    for I := 0 to Pred(VMappingClassRefList.Count) do
    begin
      VMappingClassRef := VMappingClassRefList[I];
      VMappingList.Add(VMappingClassRef.MappingClass.Create(Mapper, VMappingClassRef.Map, I = 0));
    end;
  except
    FreeAndNil(VMappingList);
    raise;
  end;
  Result := VMappingList;
end;

constructor TJCoreOPFMappingFactory.Create(const AMapper: IJCoreOPFMapper;
  const AMappingClassFactory: TJCoreOPFMappingClassFactory; const ADriver: TJCoreOPFDriver);
begin
  inherited Create;
  FMapper := AMapper;
  FMappingClassFactory := AMappingClassFactory;
  FDriver := ADriver;
  FMappingListMap := TJCoreOPFMappingListMap.Create;
end;

destructor TJCoreOPFMappingFactory.Destroy;
var
  I: Integer;
begin
  for I := 0 to Pred(FMappingListMap.Count) do
    FMappingListMap.Data[I].Free;
  FreeAndNil(FMappingListMap);
  inherited Destroy;
end;

function TJCoreOPFMappingFactory.AcquireMappingList(const AClass: TClass): TJCoreOPFMappingList;
var
  VIndex: Integer;
begin
  VIndex := MappingListMap.IndexOf(AClass);
  if VIndex = -1 then
    VIndex := MappingListMap.Add(AClass, CreateMappingList(AClass));
  Result := MappingListMap.Data[VIndex];
end;

{ TJCoreOPFComplementaryClassMapping }

constructor TJCoreOPFComplementaryClassMapping.Create(const AClass: TClass);
begin
  inherited Create;
  FClass := AClass;
  FPIDList := TJCoreOPFPIDList.Create(False);
end;

destructor TJCoreOPFComplementaryClassMapping.Destroy;
begin
  FreeAndNil(FPIDList);
  inherited Destroy;
end;

procedure TJCoreOPFComplementaryClassMapping.AddPID(const APID: TJCoreOPFPID);
begin
  PIDList.Add(APID);
end;

function TJCoreOPFComplementaryClassMapping.CreateOIDArray: TJCoreOPFOIDArray;
var
  I: Integer;
begin
  SetLength(Result, PIDList.Count);
  for I := Low(Result) to High(Result) do
    Result[I] := PIDList[I].OID;
end;

function TJCoreOPFComplementaryClassMapping.PIDByOID(const AOID: IJCoreOPFOID): TJCoreOPFPID;
begin
  { TODO : binary search }
  for Result in PIDList do
    if Result.OID.EqualsOID(AOID) then
      Exit;
  raise EJCoreOPF.Create(2114, S2114_ObjectNotFound, [AOID.AsString]);
end;

{ TJCoreOPFComplementaryMappingManager }

function TJCoreOPFComplementaryMappingManager.GetClassMappings(
  const AIndex: Integer): TJCoreOPFComplementaryClassMapping;
begin
  Result := ClassMappingMap.Data[AIndex];
end;

function TJCoreOPFComplementaryMappingManager.AcquireClassMapping(
  const AClass: TClass): TJCoreOPFComplementaryClassMapping;
var
  VIndex: Integer;
begin
  VIndex := ClassMappingMap.IndexOf(AClass);
  if VIndex = -1 then
    VIndex := ClassMappingMap.Add(AClass, TJCoreOPFComplementaryClassMapping.Create(AClass));
  Result := ClassMappingMap.Data[VIndex];
end;

constructor TJCoreOPFComplementaryMappingManager.Create;
begin
  inherited Create;
  FClassMappingMap := TJCoreOPFComplementaryClassMappingMap.Create;
end;

destructor TJCoreOPFComplementaryMappingManager.Destroy;
begin
  Clear;
  FreeAndNil(FClassMappingMap);
  inherited Destroy;
end;

procedure TJCoreOPFComplementaryMappingManager.AddPID(const APID: TJCoreOPFPID);
begin
  AcquireClassMapping(APID.Entity.ClassType).AddPID(APID);
end;

function TJCoreOPFComplementaryMappingManager.ClassMappingCount: Integer;
begin
  Result := ClassMappingMap.Count;
end;

procedure TJCoreOPFComplementaryMappingManager.Clear;
var
  I: Integer;
begin
  for I := 0 to Pred(ClassMappingMap.Count) do
    ClassMappingMap.Data[I].Free;
  ClassMappingMap.Clear;
end;

{ TJCoreOPFClassMapping }

function TJCoreOPFClassMapping.AcquireClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
begin
  if Metadata.TheClass = AClass then
    Result := Self
  else
    Result := Mapper.AcquireClassMapping(AClass);
end;

function TJCoreOPFClassMapping.CreateOIDFromResultSet(const AResultSet: IJCoreOPFResultSet): IJCoreOPFOID;
begin
  if not AResultSet.ReadNull then
    Result := Metadata.OIDClass.CreateFromResultSet(AResultSet)
  else
    Result := nil;
end;

function TJCoreOPFClassMapping.CreateOIDFromString(const AOID: string): IJCoreOPFOID;
begin
  Result := Metadata.OIDClass.CreateFromString(AOID);
end;

procedure TJCoreOPFClassMapping.Dispose(const AOIDArray: array of IJCoreOPFOID);
var
  VSubClasses: TJCoreClassArray;
  I: Integer;
begin
  if Length(AOIDArray) > 0 then
  begin
    for I := 0 to Pred(MappingList.Count) do
      MappingList[I].Dispose(AOIDArray);
    VSubClasses := Mapping.Map.SubClasses;
    for I := Low(VSubClasses) to High(VSubClasses) do
      Mapper.AcquireClassMapping(VSubClasses[I]).Mapping.Dispose(AOIDArray);
  end;
end;

procedure TJCoreOPFClassMapping.EnsureMappingConsistency(const APID: TJCoreOPFPID);
begin
  if MappingList.Count <> APID.ADMMappingCount then
    raise EJCoreOPF.Create(2117, S2117_InconsistentMappingSizes, [Metadata.TheClass.ClassName]);
end;

function TJCoreOPFClassMapping.GetGenerator: IJCoreOPFOIDGenerator;
begin
  { TODO : Implement multiple generators per class mapping }
  if not Assigned(FGenerator) then
    FGenerator := Mapping.CreateGenerator;
  Result := FGenerator;
end;

function TJCoreOPFClassMapping.MapIndexByMap(const ABaseMap: TJCoreOPFMap): Integer;
begin
  for Result := 0 to Pred(MappingList.Count) do
    if MappingList[Result].Map = ABaseMap then
      Exit;
  raise EJCoreOPF.Create(2116, S2116_MappingNotFound, [ABaseMap.Metadata.TheClass.ClassName]);
end;

function TJCoreOPFClassMapping.Retrieve(const AOIDArray: array of IJCoreOPFOID): TJCoreObjectArray;
var
  VResultSet: IJCoreOPFResultSet;
begin
  VResultSet := Mapping.RetrieveEntity(AOIDArray, nil);
  Result := RetrieveFromResultSet(VResultSet);
end;

function TJCoreOPFClassMapping.RetrieveResultSet(const AClass: TClass;
  const AResultSet: IJCoreOPFResultSet): TJCoreObjectArray;
begin
  Result := AcquireClassMapping(AClass).RetrieveFromResultSet(AResultSet);
end;

function TJCoreOPFClassMapping.RetrieveFromResultSet(
  const AResultSet: IJCoreOPFResultSet): TJCoreObjectArray;
var
  VCompMapping: TJCoreOPFComplementaryMappingManager;
  VCompClass: TJCoreOPFComplementaryClassMapping;
  VCompResultSet: IJCoreOPFResultSet;
  VObjectArray: TJCoreObjectArray;
  VClassMapping: TJCoreOPFClassMapping;
  VPID: TJCoreOPFPID;
  VOIDClass: TJCoreOPFOIDClass;
  VOIDArray: TJCoreOPFOIDArray;
  VOID: IJCoreOPFOID;
  I, J: Integer;
begin
  SetLength(VObjectArray, AResultSet.Size);
  VOIDClass := Mapping.Map.OIDClass;
  VCompMapping := nil;
  try
    try
      // Reading base mappings
      for I := 0 to Pred(AResultSet.Size) do
      begin
        VOID := VOIDClass.CreateFromResultSet(AResultSet);
        VObjectArray[I] := Mapping.CreateEntity(AResultSet);
        VPID := Mapper.AcquirePID(VObjectArray[I]);
        VPID.OID := VOID;
        RetrieveMapsInternal(AResultSet, VPID, nil);
        if VPID.ADMMappingCount <> MappingList.Count then
        begin
          if not Assigned(VCompMapping) then
            VCompMapping := TJCoreOPFComplementaryMappingManager.Create;
          VCompMapping.AddPID(VPID);
        end;
      end;

      // Reading complementary mappings
      if Assigned(VCompMapping) then
      begin
        for I := 0 to Pred(VCompMapping.ClassMappingCount) do
        begin
          // [I] iterates mappings of distinct complementary classes
          // [J] iterates instances of a single class
          VCompClass := VCompMapping[I];
          VClassMapping := Mapper.AcquireClassMapping(VCompClass.TheClass);
          VOIDArray := VCompClass.CreateOIDArray;
          VCompResultSet := VClassMapping.Mapping.RetrieveEntity(VOIDArray, Mapping.Map);
          for J := Low(VOIDArray) to High(VOIDArray) do
          begin
            // Retrieving the mandatory OID field(s) from result set. The
            // complementary resultset may be in a different order of the
            // base resultset
            VOID := VOIDClass.CreateFromResultSet(VCompResultSet);
            VPID := VCompClass.PIDByOID(VOID);
            VClassMapping.RetrieveMapsInternal(VCompResultSet, VPID, Mapping.Map);
          end;
        end;
      end;
    except
      for I := 0 to Pred(AResultSet.Size) do
        VObjectArray[I].Free;
      raise;
    end;
  finally
    FreeAndNil(VCompMapping);
  end;
  Result := VObjectArray;
end;

procedure TJCoreOPFClassMapping.DisposeFromResultSetInternal(const AResultSet: IJCoreOPFResultSet);
var
  VOIDArray: TJCoreOPFOIDArray;
  I, VOIDCount: Integer;
begin
  SetLength(VOIDArray, AResultSet.Size);
  VOIDCount := 0;
  for I := 0 to Pred(AResultSet.Size) do
  begin
    VOIDArray[VOIDCount] := CreateOIDFromResultSet(AResultSet);
    if Assigned(VOIDArray[VOIDCount]) then
      Inc(VOIDCount);
  end;
  SetLength(VOIDArray, VOIDCount);
  Dispose(VOIDArray);
end;

procedure TJCoreOPFClassMapping.DisposeFromOIDInternal(const AOIDArray: TJCoreOPFOIDArray);
begin
  Dispose(AOIDArray);
end;

function TJCoreOPFClassMapping.RetrieveEntityFromResultSetInternal(
  const AResultSet: IJCoreOPFResultSet): TObject;
var
  VOID: IJCoreOPFOID;
begin
  VOID := CreateOIDFromResultSet(AResultSet);
  if Assigned(VOID) then
    Result := Retrieve([VOID])[0]
  else
    Result := nil;
end;

procedure TJCoreOPFClassMapping.RetrieveMapsInternal(const AResultSet: IJCoreOPFResultSet;
  const APID: TJCoreOPFPID; const ABaseMap: TJCoreOPFMap);
var
  VBaseIndex: Integer;
  I: Integer;
begin
  if Assigned(ABaseMap) then
  // BaseMap assigned means complementary retrieving, otherwise
  // Mapping X ADMMapping may be inconsistent.
  // TJCoreOPFClassMapping.RetrieveFromResultSet takes care of the remainders
  begin
    EnsureMappingConsistency(APID);
    VBaseIndex := MapIndexByMap(ABaseMap) + 1;
  end else
    VBaseIndex := 0;
  for I := VBaseIndex to Pred(MappingList.Count) do
    MappingList[I].RetrieveMappingFromResultSet(AResultSet, APID[I]);
end;

procedure TJCoreOPFClassMapping.RetrieveCollectionInternal(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
var
  VObjectArray: TJCoreObjectArray;
  VResultSet: IJCoreOPFResultSet;
  I: Integer;
begin
  VResultSet := Mapping.RetrieveCollection(AOwnerPID, AOwnerADM);
  VObjectArray := RetrieveFromResultSet(VResultSet);
  try
    AOwnerADM.AssignArray(VObjectArray);
  except
    { TODO : Move responsibility to AssignArray? }
    for I := Low(VObjectArray) to High(VObjectArray) do
      VObjectArray[I].Free;
    raise;
  end;
end;

procedure TJCoreOPFClassMapping.RetrieveEntityInternal(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMEntity);
var
  VOID: IJCoreOPFOID;
  VComposite: TObject;
begin
  VOID := AOwnerADM.CompositionOID;
  if not Assigned(VOID) then
    raise EJCoreOPF.Create(2115, S2115_EmptyOID, []);
  VComposite := Retrieve([VOID])[0];
  AOwnerADM.AssignComposition(VComposite);
end;

procedure TJCoreOPFClassMapping.RetrieveLazyEntityFromResultSetInternal(const AResultSet: IJCoreOPFResultSet;
  const ALazyADM: TJCoreOPFADMEntity);
begin
  ALazyADM.CompositionOID := CreateOIDFromResultSet(AResultSet);
end;

procedure TJCoreOPFClassMapping.StoreCollectionInternal(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
var
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VOIDs: TJCoreOPFOIDArray;
  VPIDs: TJCoreOPFPIDArray;
  VPID: TJCoreOPFPID;
  VLastMapping: TJCoreOPFClassMapping;
  VLastClass: TClass;
begin
  VAttrMetadata := AOwnerADM.Metadata;
  //// This method does not update external links!
  // delete old links (before)
  // dispose removed and store inserted/updated objects (this method)
  // insert new links (after)
  if VAttrMetadata.CompositionType = jctComposition then
  begin
    VOIDs := AOwnerADM.OIDRemoved;
    if Length(VOIDs) > 0 then
      AcquireClassMapping(VAttrMetadata.CompositionClass).DisposeFromOIDInternal(VOIDs);
  end;
  VLastMapping := nil;
  VLastClass := nil;
  VPIDs := AOwnerADM.PIDArray;
  for VPID in VPIDs do
  begin
    VPID.AssignOwner(AOwnerPID, AOwnerADM);
    if (VAttrMetadata.CompositionType = jctComposition) or not VPID.IsPersistent then
    { TODO : Document this approach: non composition attributes are stored if they are not persistent }
    begin
      if not Assigned(VLastMapping) or (VPID.Entity.ClassType <> VLastClass) then
      begin
        // this will save some time when iterating a list of a class
        // with lots of instances of one subclass
        VLastClass := VPID.Entity.ClassType;
        VLastMapping := Mapper.AcquireClassMapping(VLastClass);
      end;
      VLastMapping.StorePID(VPID);
    end;
  end;
end;

constructor TJCoreOPFClassMapping.Create(const AMapper: IJCoreOPFMapper;
  const AMetadata: TJCoreOPFClassMetadata; AMappingList: TJCoreOPFMappingList);
begin
  inherited Create;
  FMapper := AMapper;
  FDriver := FMapper.Driver;
  FMetadata := AMetadata;
  FMappingList := AMappingList;
  FMapping := FMappingList.Last;
end;

function TJCoreOPFClassMapping.CreateCriteria: IJCoreOPFSQLCriteria;
begin
  Result := Mapping.CreateCriteria(Self);
end;

procedure TJCoreOPFClassMapping.DisposeOID(const AStringOIDArray: array of string);
var
  VOIDArray: TJCoreOPFOIDArray;
  I: Integer;
begin
  SetLength(VOIDArray, Length(AStringOIDArray));
  for I := Low(AStringOIDArray) to High(AStringOIDArray) do
    VOIDArray[I] := CreateOIDFromString(AStringOIDArray[I]);
  Dispose(VOIDArray);
end;

procedure TJCoreOPFClassMapping.DisposePID(const APIDArray: array of TJCoreOPFPID);
var
  VOIDArray: TJCoreOPFOIDArray;
  I: Integer;
begin
  SetLength(VOIDArray, Length(APIDArray));
  for I := 0 to Pred(Length(VOIDArray)) do
    VOIDArray[I] := APIDArray[I].OID;
  Dispose(VOIDArray);
  for I := 0 to Pred(Length(APIDArray)) do
  begin
    APIDArray[I].OID := nil;
    Mapper.AddInTransactionPID(APIDArray[I]);
  end;
end;

function TJCoreOPFClassMapping.RetrieveOID(const AStringOID: string): TObject;
var
  VOID: IJCoreOPFOID;
begin
  if AStringOID <> '' then
  begin
    VOID := CreateOIDFromString(AStringOID);
    try
      Result := Retrieve([VOID])[0];
    except
      on E: EJCoreOPF do
        if E.Code = 2110 then
          raise EJCoreOPF.Create(2114, S2114_ObjectNotFound, [AStringOID])
        else
          raise;
    end;
  end else
    Result := nil;
end;

procedure TJCoreOPFClassMapping.StorePID(const APID: TJCoreOPFPID);
var
  VOID: IJCoreOPFOID;
  I: Integer;
begin
  EnsureMappingConsistency(APID);
  if not APID.IsPersistent then
  begin
    VOID := Mapping.CreateOID(Generator);
    APID.OID := VOID;
  end;
  for I := 0 to Pred(MappingList.Count) do
    MappingList[I].Store(APID[I]);
  Mapper.AddInTransactionPID(APID);
end;

{ TJCoreOPFMapping }

function TJCoreOPFMapping.CreateEntityFromResultSet(const AResultSet: IJCoreOPFResultSet): TObject;
begin
  Result := nil;
end;

function TJCoreOPFMapping.CreateParams: IJCoreOPFParams;
begin
  Result := TJCoreOPFParams.Create;
end;

function TJCoreOPFMapping.InternalCreateOIDArray(const AGenerator: IJCoreOPFOIDGenerator;
  const AOIDCount: Integer): TJCoreOPFOIDArray;
var
  I: Integer;
begin
  AGenerator.GenerateOIDs(AOIDCount);
  SetLength(Result, AOIDCount);
  for I := Low(Result) to High(Result) do
    Result[I] := Map.OIDClass.CreateFromGenerator(AGenerator);
end;

procedure TJCoreOPFMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
begin
end;

procedure TJCoreOPFMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
begin
end;

procedure TJCoreOPFMapping.WriteCollections(const AMapping: TJCoreOPFADMMapping);
begin
end;

function TJCoreOPFMapping.InternalCreateGenerator: IJCoreOPFOIDGenerator;
var
  VGeneratorName: string;
begin
  VGeneratorName := Map.GeneratorName;
  if VGeneratorName <> '' then
    Result := Driver.CreateGenerator(VGeneratorName)
  else
    Result := TJCoreOPFOIDGeneratorGUID.Create;
end;

function TJCoreOPFMapping.SelectClassFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const ASubClassArray: array of TClass; const ADefaultClass: TClass): TClass;
var
  VSubClass: TClass;
begin
  Result := nil;
  for VSubClass in ASubClassArray do
  begin
    if not AResultSet.ReadNull then
    begin
      AResultSet.SkipReading;
      if not Assigned(Result) then
        Result := VSubClass
      else
        raise EJCoreOPF.Create(201, S0201_AmbiguousImplementation, [Result.ClassName,
         VSubClass.ClassName]);
    end;
  end;
  if not Assigned(Result) then
    Result := ADefaultClass;
end;

constructor TJCoreOPFMapping.Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap;
  const AIsBaseMapping: Boolean);
begin
  inherited Create;
  FMapper := AMapper;
  FMap := AMap;
  FModel := Mapper.Model;
  FDriver := Mapper.Driver;
  FIsBaseMapping := AIsBaseMapping;
end;

function TJCoreOPFMapping.CreateCriteria(const ARetriever: IJCoreOPFCriteriaRetriever): IJCoreOPFSQLCriteria;
begin
  Result := InternalCreateCriteria(ARetriever);
end;

function TJCoreOPFMapping.CreateEntity(const AResultSet: IJCoreOPFResultSet): TObject;
begin
  Result := CreateEntityFromResultSet(AResultSet);
  if not Assigned(Result) then
    Result := Map.Metadata.TheClass.Create;
end;

function TJCoreOPFMapping.CreateGenerator: IJCoreOPFOIDGenerator;
begin
  Result := InternalCreateGenerator;
end;

function TJCoreOPFMapping.CreateOID(const AGenerator: IJCoreOPFOIDGenerator): IJCoreOPFOID;
begin
  Result := InternalCreateOIDArray(AGenerator, 1)[0];
end;

procedure TJCoreOPFMapping.Dispose(const AOIDArray: array of IJCoreOPFOID);
begin
  InternalDispose(AOIDArray);
end;

function TJCoreOPFMapping.RetrieveCollection(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection): IJCoreOPFResultSet;
begin
  Result := InternalRetrieveCollection(AOwnerPID, AOwnerADM);
end;

function TJCoreOPFMapping.RetrieveEntity(const AOIDArray: array of IJCoreOPFOID;
  const ABaseMap: TJCoreOPFMap): IJCoreOPFResultSet;
begin
  Result := InternalRetrieveEntity(AOIDArray, ABaseMap);
end;

procedure TJCoreOPFMapping.RetrieveMappingFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
begin
  ReadFromResultSet(AResultSet, AMapping);
end;

procedure TJCoreOPFMapping.Store(const AMapping: TJCoreOPFADMMapping);
var
  VPID: TJCoreOPFPID;
  VParams: IJCoreOPFParams;
begin
  if AMapping.IsDirty then
  begin
    VPID := AMapping.PID;
    VParams := CreateParams;
    if not VPID.IsPersistent then
    begin
      VPID.OID.WriteToParams(VParams);
      WriteAttributesToParams(VParams, AMapping);
      InternalInsert(VParams, AMapping);
    end else if AMapping.IsAttributesDirty then
    begin
      WriteAttributesToParams(VParams, AMapping);
      VPID.OID.WriteToParams(VParams);
      InternalUpdate(VParams, AMapping);
    end;
    WriteCollections(AMapping);
  end;
end;

end.

