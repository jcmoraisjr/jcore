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
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMetadata;

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
    FClass: TJCoreOPFMappingClass;
    FMap: TJCoreOPFMap;
  public
    constructor Create(const AClass: TJCoreOPFMappingClass; AMap: TJCoreOPFMap);
    property Map: TJCoreOPFMap read FMap;
    property TheClass: TJCoreOPFMappingClass read FClass;
  end;

  TJCoreOPFMappingClassRefList = specialize TFPGObjectList<TJCoreOPFMappingClassRef>;
  TJCoreOPFMappingClassRefListMap = specialize TFPGMap<Pointer, TJCoreOPFMappingClassRefList>;

  TJCoreOPFMappingClassList = specialize TFPGList<TJCoreOPFMappingClass>;

  { TJCoreOPFMappingClassFactory }

  TJCoreOPFMappingClassFactory = class(specialize TJCoreFactory<TJCoreOPFMappingClass>)
  // Class mappings of a single class metadata, without parents.
  // Based on TJCoreOPFMaps and synchronized with TJCoreOPFADMMapping ADMs.
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

  { TJCoreOPFClassMapping }

  TJCoreOPFClassMapping = class(TObject)
  private
    FDriver: TJCoreOPFDriver;
    FMapper: IJCoreOPFMapper;
    FMapping: TJCoreOPFMapping;
    FMappingList: TJCoreOPFMappingList;
    FMetadata: TJCoreOPFClassMetadata;
    function AcquireClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
    function CreateOIDFromDriver: TJCoreOPFOID;
    function CreateOIDFromString(const AOID: string): TJCoreOPFOID;
    procedure Dispose(const AOIDArray: array of TJCoreOPFOID);
    procedure EnsureMappingConsistency(const APID: TJCoreOPFPID);
    function Retrieve(const AOID: TJCoreOPFOID): TObject;
  protected
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
    property MappingList: TJCoreOPFMappingList read FMappingList;
  public
    // Internal mapping implementations, mapped from other classes
    procedure DisposeFromDriverInternal(const AOIDCount: Integer);
    procedure DisposeFromOIDInternal(const AOIDArray: TJCoreOPFOIDArray);
    function RetrieveEntityFromDriverInternal: TObject;
    procedure RetrieveMapsInternal(const APID: TJCoreOPFPID);
    procedure RetrieveCollectionInternal(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure RetrieveEntityInternal(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMEntity);
    procedure RetrieveLazyEntityFromDriverInternal(const ALazyADM: TJCoreOPFADMEntity);
    procedure StoreCollectionInternal(const AOwnerMapping: TJCoreOPFADMMapping; const AOwnerADM: TJCoreOPFADMCollection);
  public
    // Public facades, mapped from session facades
    constructor Create(const AMapper: IJCoreOPFMapper; const AMetadata: TJCoreOPFClassMetadata; AMappingList: TJCoreOPFMappingList);
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
    FMap: TJCoreOPFMap;
    FMapper: IJCoreOPFMapper;
    FModel: TJCoreOPFModel;
  protected
    // CreateEntity should analyze the storage mechanism in order to create an
    // instance of the correct class, eg:
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
    // CreateEntity is called after GenerateSelectStatement and
    // GenerateSelectCollectionStatement, and before ReadFromDriver. Use these
    // SQL generators to retrieve information about the class of the instance
    // actually stored, read them inside CreateEntity. The
    // SelectClassFromDriver should be of some help.
    //
    // Note also that CreateEntity need to be aligned with select generators.
    // If you extend a class and override some select generator, you should
    // also override CreateEntity or make the generic CreateEntity method
    // aligned with all select generators descendants.
    function CreateEntityFromDriver: TObject; virtual;
    // abstract facades
    function InternalCreateOIDArray(const AOIDCount: Integer): TJCoreOPFOIDArray; virtual; abstract;
    procedure InternalDispose(const AOIDArray: array of TJCoreOPFOID); virtual; abstract;
    procedure InternalRetrieveCollectionToDriver(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection); virtual; abstract;
    procedure InternalRetrieveEntityToDriver(const AOID: TJCoreOPFOID; const ABaseMap: TJCoreOPFMap); virtual; abstract;
    procedure InternalStore(const AMapping: TJCoreOPFADMMapping); virtual; abstract;
    // direct field -> attribute mapping
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); virtual;
    // support
    function SelectClassFromDriver(const ASubClassArray: array of TClass; const ADefaultClass: TClass): TClass;
    property Driver: TJCoreOPFDriver read FDriver;
    property Map: TJCoreOPFMap read FMap;
    property Mapper: IJCoreOPFMapper read FMapper;
    property Model: TJCoreOPFModel read FModel;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap); virtual;
    class function Apply(const AMap: TJCoreOPFMap): Boolean; virtual; abstract;
    function CreateEntity: TObject;
    function CreateOID: TJCoreOPFOID;
    procedure Dispose(const AOIDArray: array of TJCoreOPFOID);
    procedure RetrieveCollectionToDriver(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure RetrieveEntityToDriver(const AOID: TJCoreOPFOID; const ABaseMap: TJCoreOPFMap);
    procedure RetrieveMappingFromDriver(const AMapping: TJCoreOPFADMMapping);
    procedure Store(const AMapping: TJCoreOPFADMMapping);
  end;

implementation

uses
  sysutils,
  JCoreMetadata,
  JCoreOPFException;

{ TJCoreOPFMappingClassRef }

constructor TJCoreOPFMappingClassRef.Create(const AClass: TJCoreOPFMappingClass; AMap: TJCoreOPFMap);
begin
  inherited Create;
  FClass := AClass;
  FMap := AMap;
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
    raise EJCoreOPFMappingNotFound.Create(AMap.Metadata.TheClass.ClassName);
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
begin
  VMappingClassRefList := MappingClassFactory.AcquireMappingClassRefList(AClass);
  VMappingList := TJCoreOPFMappingList.Create(True);
  try
    for VMappingClassRef in VMappingClassRefList do
      VMappingList.Add(VMappingClassRef.TheClass.Create(Mapper, VMappingClassRef.Map));
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

{ TJCoreOPFClassMapping }

function TJCoreOPFClassMapping.AcquireClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
begin
  if Metadata.TheClass = AClass then
    Result := Self
  else
    Result := Mapper.AcquireClassMapping(AClass);
end;

function TJCoreOPFClassMapping.CreateOIDFromDriver: TJCoreOPFOID;
begin
  if not Driver.ReadNull then
    Result := Metadata.OIDClass.CreateFromDriver(Driver)
  else
    Result := nil;
end;

function TJCoreOPFClassMapping.CreateOIDFromString(const AOID: string): TJCoreOPFOID;
begin
  Result := Metadata.OIDClass.CreateFromString(AOID);
end;

procedure TJCoreOPFClassMapping.Dispose(const AOIDArray: array of TJCoreOPFOID);
var
  I: Integer;
begin
  if Length(AOIDArray) > 0 then
    for I := 0 to Pred(MappingList.Count) do
      MappingList[I].Dispose(AOIDArray);
end;

procedure TJCoreOPFClassMapping.EnsureMappingConsistency(const APID: TJCoreOPFPID);
begin
  if MappingList.Count <> APID.ADMMappingCount then
    raise EJCoreOPFInconsistentMappingSizes.Create(MappingList.Count, APID.ADMMappingCount);
end;

function TJCoreOPFClassMapping.Retrieve(const AOID: TJCoreOPFOID): TObject;
var
  VEntity: TObject;
  VPID: TJCoreOPFPID;
  VClassMapping: TJCoreOPFClassMapping;
begin
  Mapping.RetrieveEntityToDriver(AOID, nil);
  VEntity := Mapping.CreateEntity;
  try
    VPID := Mapper.AcquirePID(VEntity);
    VPID.AssignOID(AOID);
    if MappingList.Count = VPID.ADMMappingCount then
    begin
      VClassMapping := Self
    end else
    begin
      VClassMapping := Mapper.AcquireClassMapping(VPID.Entity.ClassType);
      VClassMapping.Mapping.RetrieveEntityToDriver(VPID.OID, Mapping.Map);
    end;
    VClassMapping.RetrieveMapsInternal(VPID);
  except
    FreeAndNil(VEntity);
    raise;
  end;
  Result := VPID.Entity;
end;

procedure TJCoreOPFClassMapping.DisposeFromDriverInternal(const AOIDCount: Integer);
var
  VOIDArray: TJCoreOPFOIDArray;
  I, VOIDCount: Integer;
begin
  SetLength(VOIDArray, AOIDCount);
  try
    VOIDCount := 0;
    for I := 0 to Pred(AOIDCount) do
    begin
      VOIDArray[VOIDCount] := CreateOIDFromDriver;
      if Assigned(VOIDArray[VOIDCount]) then
        Inc(VOIDCount);
    end;
    SetLength(VOIDArray, VOIDCount);
    Dispose(VOIDArray);
  finally
    for I := 0 to Pred(VOIDCount) do
      FreeAndNil(VOIDArray[I]);
  end;
end;

procedure TJCoreOPFClassMapping.DisposeFromOIDInternal(const AOIDArray: TJCoreOPFOIDArray);
begin
  Dispose(AOIDArray);
end;

function TJCoreOPFClassMapping.RetrieveEntityFromDriverInternal: TObject;
var
  VOID: TJCoreOPFOID;
begin
  VOID := CreateOIDFromDriver;
  if Assigned(VOID) then
  begin
    try
      Result := Retrieve(VOID);
    finally
      FreeAndNil(VOID);
    end;
  end else
    Result := nil;
end;

procedure TJCoreOPFClassMapping.RetrieveMapsInternal(const APID: TJCoreOPFPID);
var
  I: Integer;
begin
  EnsureMappingConsistency(APID);
  for I := 0 to Pred(MappingList.Count) do
    MappingList[I].RetrieveMappingFromDriver(APID[I]);
end;

procedure TJCoreOPFClassMapping.RetrieveCollectionInternal(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  Mapping.RetrieveCollectionToDriver(AOwnerPID, AOwnerADM);
end;

procedure TJCoreOPFClassMapping.RetrieveEntityInternal(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMEntity);
var
  VOID: TJCoreOPFOID;
  VComposite: TObject;
begin
  VOID := AOwnerADM.CompositionOID;
  if not Assigned(VOID) then
    raise EJCoreOPFEmptyOID.Create;
  VComposite := Retrieve(VOID);
  AOwnerADM.AssignComposition(VComposite);
end;

procedure TJCoreOPFClassMapping.RetrieveLazyEntityFromDriverInternal(const ALazyADM: TJCoreOPFADMEntity);
begin
  ALazyADM.CompositionOID := CreateOIDFromDriver;
end;

procedure TJCoreOPFClassMapping.StoreCollectionInternal(const AOwnerMapping: TJCoreOPFADMMapping;
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
    VPID.AssignOwner(AOwnerMapping.PID, AOwnerADM);
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
  FMapping := MappingList.Last;
end;

procedure TJCoreOPFClassMapping.DisposeOID(const AStringOIDArray: array of string);
var
  VOIDArray: TJCoreOPFOIDArray;
  I: Integer;
begin
  SetLength(VOIDArray, Length(AStringOIDArray));
  try
    for I := Low(AStringOIDArray) to High(AStringOIDArray) do
      VOIDArray[I] := CreateOIDFromString(AStringOIDArray[I]);
    Dispose(VOIDArray);
  finally
    for I := Low(VOIDArray) to High(VOIDArray) do
      FreeAndNil(VOIDArray[I]);
  end;
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
    APIDArray[I].AssignOID(nil);
    Mapper.AddInTransactionPID(APIDArray[I]);
  end;
end;

function TJCoreOPFClassMapping.RetrieveOID(const AStringOID: string): TObject;
var
  VOID: TJCoreOPFOID;
begin
  if AStringOID <> '' then
  begin
    VOID := CreateOIDFromString(AStringOID);
    try
      Result := Retrieve(VOID);
    finally
      FreeAndNil(VOID);
    end;
  end else
    Result := nil;
end;

procedure TJCoreOPFClassMapping.StorePID(const APID: TJCoreOPFPID);
var
  VOID: TJCoreOPFOID;
  I: Integer;
begin
  EnsureMappingConsistency(APID);
  if not APID.IsPersistent then
  begin
    VOID := Mapping.CreateOID;
    try
      APID.AssignOID(VOID);
    finally
      FreeAndNil(VOID);
    end;
  end;
  for I := 0 to Pred(MappingList.Count) do
    MappingList[I].Store(APID[I]);
  Mapper.AddInTransactionPID(APID);
end;

{ TJCoreOPFMapping }

function TJCoreOPFMapping.CreateEntityFromDriver: TObject;
begin
  Result := nil;
end;

procedure TJCoreOPFMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
begin
end;

function TJCoreOPFMapping.SelectClassFromDriver(const ASubClassArray: array of TClass;
  const ADefaultClass: TClass): TClass;
var
  VSubClass: TClass;
begin
  Result := nil;
  for VSubClass in ASubClassArray do
  begin
    if not Driver.ReadNullAndSkip then
    begin
      if not Assigned(Result) then
        Result := VSubClass
      else
        raise EJCoreOPFAmbiguousInstanceClass.Create(Result, VSubClass);
    end;
  end;
  if not Assigned(Result) then
    Result := ADefaultClass;
end;

constructor TJCoreOPFMapping.Create(const AMapper: IJCoreOPFMapper; const AMap: TJCoreOPFMap);
begin
  inherited Create;
  FMapper := AMapper;
  FMap := AMap;
  FModel := Mapper.Model;
  FDriver := Mapper.Driver;
end;

function TJCoreOPFMapping.CreateEntity: TObject;
begin
  Result := CreateEntityFromDriver;
  if not Assigned(Result) then
    Result := Map.Metadata.TheClass.Create;
end;

function TJCoreOPFMapping.CreateOID: TJCoreOPFOID;
begin
  Result := InternalCreateOIDArray(1)[0];
end;

procedure TJCoreOPFMapping.Dispose(const AOIDArray: array of TJCoreOPFOID);
begin
  InternalDispose(AOIDArray);
end;

procedure TJCoreOPFMapping.RetrieveCollectionToDriver(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  InternalRetrieveCollectionToDriver(AOwnerPID, AOwnerADM);
end;

procedure TJCoreOPFMapping.RetrieveEntityToDriver(const AOID: TJCoreOPFOID; const ABaseMap: TJCoreOPFMap);
begin
  InternalRetrieveEntityToDriver(AOID, ABaseMap);
end;

procedure TJCoreOPFMapping.RetrieveMappingFromDriver(const AMapping: TJCoreOPFADMMapping);
begin
  ReadFromDriver(AMapping);
end;

procedure TJCoreOPFMapping.Store(const AMapping: TJCoreOPFADMMapping);
begin
  if AMapping.IsDirty then
    InternalStore(AMapping);
end;

end.

