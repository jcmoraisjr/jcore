(*
  JCore, OPF Mediator and Metadata Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFMetadata;

{$I jcore.inc}

interface

uses
  typinfo,
  fgl,
  JCoreClasses,
  JCoreMetadata,
  JCoreOPFEntity,
  JCoreOPFOID;

type

  TJCoreOPFPID = class;
  TJCoreOPFPIDList = specialize TFPGObjectList<TJCoreOPFPID>;
  TJCoreOPFPIDArray = array of TJCoreOPFPID;

  IJCoreOPFPIDMapping = interface
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    function CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
  end;

  TJCoreOPFAttrMetadata = class;

  { TJCoreOPFADM }

  TJCoreOPFADM = class(TObject, IJCoreOPFADM)
  private
    FAttrPropInfo: PPropInfo;
    FCacheUpdated: Boolean;
    FEntity: TObject;
    FMapping: IJCoreOPFPIDMapping;
    FMetadata: TJCoreOPFAttrMetadata;
  protected
    function InternalIsDirty: Boolean; virtual; abstract;
    procedure InternalUpdateCache; virtual; abstract;
    property AttrPropInfo: PPropInfo read FAttrPropInfo;
    property Entity: TObject read FEntity;
    property Mapping: IJCoreOPFPIDMapping read FMapping;
  public
    constructor Create(const AMapping: IJCoreOPFPIDMapping; const AEntity: TObject; const AMetadata: TJCoreOPFAttrMetadata); virtual;
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; virtual; abstract;
    function IsDirty: Boolean;
    procedure TransactionClosing(const ACommit: Boolean); virtual;
    procedure UpdateCache;
    property Metadata: TJCoreOPFAttrMetadata read FMetadata;
  end;

  TJCoreOPFADMClass = class of TJCoreOPFADM;
  TJCoreOPFADMClassList = specialize TFPGList<TJCoreOPFADMClass>;
  TJCoreOPFADMMap = specialize TFPGMap<string, TJCoreOPFADM>;

  { TJCoreOPFADMType32 }

  TJCoreOPFADMType32 = class(TJCoreOPFADM)
  private
    FCache: Longint;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMType64 }

  TJCoreOPFADMType64 = class(TJCoreOPFADM)
  private
    FCache: Int64;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMFloat }

  TJCoreOPFADMFloat = class(TJCoreOPFADM)
  private
    FCache: Extended;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMAnsiString }

  TJCoreOPFADMAnsiString = class(TJCoreOPFADM)
  private
    FCache: AnsiString;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMEntity }

  TJCoreOPFADMEntity = class(TJCoreOPFADM)
  private
    FOIDCache: TJCoreOPFOID;
    procedure SetOIDCache(AValue: TJCoreOPFOID);
  protected
    function AcquirePID: TJCoreOPFPID;
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
    property OIDCache: TJCoreOPFOID read FOIDCache write SetOIDCache;
  public
    destructor Destroy; override;
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMCollection }

  TJCoreOPFADMCollection = class(TJCoreOPFADM)
  private
    { TODO : Thread safety between arrays initialization and finish the transaction }
    FChangesUpdated: Boolean;
    FItemsArray: TJCoreObjectArray;
    FItemsArrayUpdated: Boolean;
    FOIDCache: TJCoreOPFOIDArray;
    FOIDRemoved: TJCoreOPFOIDArray;
    FPIDAdded: TJCoreOPFPIDArray;
    FPIDArray: TJCoreOPFPIDArray;
    FPIDArrayUpdated: Boolean;
    function ArrayContentIsDirty(const APIDArray: TJCoreOPFPIDArray): Boolean;
    function ArrayOrderIsDirty(const APIDArray: TJCoreOPFPIDArray): Boolean;
    function ArraySizeIsDirty(const AItems: TJCoreObjectArray): Boolean;
    function GetItemsArray: TJCoreObjectArray;
    function GetOIDRemoved: TJCoreOPFOIDArray;
    function GetPIDAdded: TJCoreOPFPIDArray;
    function GetPIDArray: TJCoreOPFPIDArray;
    function HasOIDInCache(const AOID: TJCoreOPFOID): Boolean;
    function HasOIDInCollection(const APIDArray: TJCoreOPFPIDArray; const AOID: TJCoreOPFOID): Boolean;
    procedure ReleaseOIDCache;
    procedure SetOIDCache(AValue: TJCoreOPFOIDArray);
    procedure UpdateChanges;
  protected
    function InternalCreateItemsArray: TJCoreObjectArray; virtual; abstract;
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
    property ItemsArray: TJCoreObjectArray read GetItemsArray;
    property OIDCache: TJCoreOPFOIDArray read FOIDCache write SetOIDCache;
  public
    destructor Destroy; override;
    procedure TransactionClosing(const ACommit: Boolean); override;
    property OIDRemoved: TJCoreOPFOIDArray read GetOIDRemoved;
    property PIDAdded: TJCoreOPFPIDArray read GetPIDAdded;
    property PIDArray: TJCoreOPFPIDArray read GetPIDArray;
  end;

  { TJCoreOPFADMFPSListCollection }

  TJCoreOPFADMFPSListCollection = class(TJCoreOPFADMCollection)
  protected
    function InternalCreateItemsArray: TJCoreObjectArray; override;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  TJCoreOPFClassMetadata = class;

  { TJCoreOPFPID }

  { TODO :
    Sessions from different configurations need different PIDs,
    iow, the same entity may be persistent to a configuration
    and nonpersistent to another one }

  TJCoreOPFPID = class(TInterfacedObject, IJCoreOPFPID)
  private
    FADMMap: TJCoreOPFADMMap;
    FEntity: TObject;
    FIsPersistent: Boolean;
    FMapping: IJCoreOPFPIDMapping;
    FMetadata: TJCoreOPFClassMetadata;
    FOID: TJCoreOPFOID;
    FOwner: TJCoreOPFPID;
    FOwnerADM: TJCoreOPFADMCollection;
    FStored: Boolean;
    procedure CreateADMs;
    function GetEntity: TObject;
    function GetIsPersistent: Boolean;
    function GetOIDIntf: IJCoreOPFOID;
    function GetOwnerIntf: IJCoreOPFPID;
    function IJCoreOPFPID.Entity = GetEntity;
    function IJCoreOPFPID.OID = GetOIDIntf;
    function IJCoreOPFPID.Owner = GetOwnerIntf;
  protected
    function InternalIsDirty(const AIncludeExternals: Boolean): Boolean; virtual;
    property ADMMap: TJCoreOPFADMMap read FADMMap;
    property Mapping: IJCoreOPFPIDMapping read FMapping;
    property Metadata: TJCoreOPFClassMetadata read FMetadata;
  public
    constructor Create(const AMapping: IJCoreOPFPIDMapping; const AEntity: TObject; const AMetadata: TJCoreOPFClassMetadata);
    destructor Destroy; override;
    function AcquireADM(const AAttributeName: string): TJCoreOPFADM;
    function ADMByName(const AAttributeName: string): IJCoreOPFADM;
    procedure AssignOID(const AOID: TJCoreOPFOID);
    procedure AssignOwner(const AOwner: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure Commit;
    function IsDirty: Boolean;
    function IsInternalsDirty: Boolean;
    procedure ReleaseOID(const AOID: TJCoreOPFOID);
    procedure Stored;
    property IsPersistent: Boolean read GetIsPersistent;
    property Entity: TObject read FEntity;
    property OID: TJCoreOPFOID read FOID;
    property Owner: TJCoreOPFPID read FOwner;
  end;

  TJCoreOPFModel = class;

  TJCoreOPFMetadataCompositionLinkType = (jcltEmbedded, jcltExternal);

  { TJCoreOPFAttrMetadata }

  TJCoreOPFAttrMetadata = class(TJCoreAttrMetadata)
  private
    FADMClass: TJCoreOPFADMClass;
    FCompositionLinkType: TJCoreOPFMetadataCompositionLinkType;
    FIsExternal: Boolean;
    FModel: TJCoreOPFModel;
    function ReadGenericsComposition(const AClassName: string): TClass;
  protected
    property Model: TJCoreOPFModel read FModel;
  public
    constructor Create(const AModel: TJCoreOPFModel; const APropInfo: PPropInfo);
    function CreateADM(const AMapping: IJCoreOPFPIDMapping; const AEntity: TObject): TJCoreOPFADM;
    property CompositionLinkType: TJCoreOPFMetadataCompositionLinkType read FCompositionLinkType write FCompositionLinkType;
    property IsExternal: Boolean read FIsExternal write FIsExternal;
  end;

  { TJCoreOPFClassMetadata }

  TJCoreOPFClassMetadata = class(TJCoreClassMetadata)
  private
    function GetAttributes(const AIndex: Integer): TJCoreOPFAttrMetadata;
  public
    { TODO : Generics? }
    function AttributeByName(const AAttributeName: string): TJCoreOPFAttrMetadata;
    property Attributes[const AIndex: Integer]: TJCoreOPFAttrMetadata read GetAttributes; default;
  end;

  { TJCoreOPFModel }

  TJCoreOPFModel = class(TJCoreModel)
  private
    FADMClassList: TJCoreOPFADMClassList;
  protected
    procedure AddADMClass(const AADMClass: TJCoreOPFADMClass);
    function CreateAttribute(const APropInfo: PPropInfo): TJCoreAttrMetadata; override;
    function CreateMetadata(const AClass: TClass): TJCoreClassMetadata; override;
    procedure Finit; override;
    procedure InitRegistry; override;
    function IsReservedAttr(const AAttrName: ShortString): Boolean; override;
    property ADMClassList: TJCoreOPFADMClassList read FADMClassList;
  public
    constructor Create; override;
    function AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
  end;

implementation

uses
  sysutils,
  JCoreOPFConsts,
  JCoreOPFException;

{ TJCoreOPFADM }

constructor TJCoreOPFADM.Create(const AMapping: IJCoreOPFPIDMapping;
  const AEntity: TObject; const AMetadata: TJCoreOPFAttrMetadata);
begin
  inherited Create;
  FMapping := AMapping;
  FEntity := AEntity;
  FAttrPropInfo := AMetadata.PropInfo;
  FCacheUpdated := False;
  FMetadata := AMetadata;
end;

function TJCoreOPFADM.IsDirty: Boolean;
begin
  { TODO : Implement IsDirty cache while transaction is active }
  Result := not FCacheUpdated or InternalIsDirty;
end;

procedure TJCoreOPFADM.TransactionClosing(const ACommit: Boolean);
begin
end;

procedure TJCoreOPFADM.UpdateCache;
begin
  InternalUpdateCache;
  FCacheUpdated := True;
end;

{ TJCoreOPFADMType32 }

function TJCoreOPFADMType32.InternalIsDirty: Boolean;
begin
  Result := GetOrdProp(Entity, AttrPropInfo) <> FCache;
end;

procedure TJCoreOPFADMType32.InternalUpdateCache;
begin
  FCache := GetOrdProp(Entity, AttrPropInfo);
end;

class function TJCoreOPFADMType32.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind in [tkInteger, tkChar, tkEnumeration, tkBool];
end;

{ TJCoreOPFADMType64 }

function TJCoreOPFADMType64.InternalIsDirty: Boolean;
begin
  Result := GetInt64Prop(Entity, AttrPropInfo) <> FCache;
end;

procedure TJCoreOPFADMType64.InternalUpdateCache;
begin
  FCache := GetInt64Prop(Entity, AttrPropInfo);
end;

class function TJCoreOPFADMType64.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind in [tkInt64, tkQWord];
end;

{ TJCoreOPFADMFloat }

function TJCoreOPFADMFloat.InternalIsDirty: Boolean;
begin
  Result := GetFloatProp(Entity, AttrPropInfo) <> FCache;
end;

procedure TJCoreOPFADMFloat.InternalUpdateCache;
begin
  FCache := GetFloatProp(Entity, AttrPropInfo);
end;

class function TJCoreOPFADMFloat.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkFloat;
end;

{ TJCoreOPFADMAnsiString }

function TJCoreOPFADMAnsiString.InternalIsDirty: Boolean;
begin
  { TODO : use hash for long strings }
  Result := GetStrProp(Entity, AttrPropInfo) <> FCache;
end;

procedure TJCoreOPFADMAnsiString.InternalUpdateCache;
begin
  { TODO : use hash for long strings }
  FCache := GetStrProp(Entity, AttrPropInfo);
end;

class function TJCoreOPFADMAnsiString.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkAString;
end;

{ TJCoreOPFADMEntity }

procedure TJCoreOPFADMEntity.SetOIDCache(AValue: TJCoreOPFOID);
begin
  if FOIDCache <> AValue then
  begin;
    FreeAndNil(FOIDCache);
    FOIDCache := AValue;
    if Assigned(FOIDCache) then
      FOIDCache.AddRef;
  end;
end;

function TJCoreOPFADMEntity.AcquirePID: TJCoreOPFPID;
var
  VObject: TObject;
begin
  VObject := GetObjectProp(Entity, AttrPropInfo, nil);
  if Assigned(VObject) then
    Result := Mapping.AcquirePID(VObject)
  else
    Result := nil;
end;

function TJCoreOPFADMEntity.InternalIsDirty: Boolean;
var
  VPID: TJCoreOPFPID;
  VOID: TJCoreOPFOID;
begin
  VPID := AcquirePID;
  if Assigned(VPID) then
    VOID := VPID.OID
  else
    VOID := nil;
  Result := OIDCache <> VOID;
  if not Result and Assigned(VPID) and (Metadata.CompositionType = jctComposition) then
    Result := VPID.IsDirty;
end;

procedure TJCoreOPFADMEntity.InternalUpdateCache;
var
  VPID: TJCoreOPFPID;
begin
  VPID := AcquirePID;
  if Assigned(VPID) then
    OIDCache := VPID.OID
  else
    OIDCache := nil;
end;

destructor TJCoreOPFADMEntity.Destroy;
begin
  FreeAndNil(FOIDCache);
  inherited Destroy;
end;

class function TJCoreOPFADMEntity.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  if AAttrTypeInfo^.Kind = tkClass then
    Result := AModel.IsEntityClass(GetTypeData(AAttrTypeInfo)^.ClassType)
  else
    Result := False;
end;

{ TJCoreOPFADMCollection }

function TJCoreOPFADMCollection.ArrayContentIsDirty(const APIDArray: TJCoreOPFPIDArray): Boolean;
var
  VPID: TJCoreOPFPID;
begin
  if Metadata.CompositionType = jctComposition then
  begin
    Result := True;
    for VPID in APIDArray do
      if VPID.IsDirty then
        Exit;
  end;
  Result := False;
end;

function TJCoreOPFADMCollection.ArrayOrderIsDirty(const APIDArray: TJCoreOPFPIDArray): Boolean;
var
  I: Integer;
begin
  // SizeIsDirty checks that APIDArray and FOIDCache has the same size
  Result := True;
  for I := Low(APIDArray) to High(APIDArray) do
    if APIDArray[I].OID <> OIDCache[I] then
      Exit;
  Result := False;
end;

function TJCoreOPFADMCollection.ArraySizeIsDirty(const AItems: TJCoreObjectArray): Boolean;
begin
  Result := Length(OIDCache) <> Length(AItems);
end;

function TJCoreOPFADMCollection.GetItemsArray: TJCoreObjectArray;
begin
  if not FItemsArrayUpdated then
  begin
    FItemsArray := InternalCreateItemsArray;
    { TODO : Fix cache outside transaction control }
    //FItemsArrayUpdated := True;
  end;
  Result := FItemsArray;
end;

function TJCoreOPFADMCollection.GetOIDRemoved: TJCoreOPFOIDArray;
begin
  UpdateChanges;
  Result := FOIDRemoved;
end;

function TJCoreOPFADMCollection.GetPIDAdded: TJCoreOPFPIDArray;
begin
  UpdateChanges;
  Result := FPIDAdded;
end;

function TJCoreOPFADMCollection.GetPIDArray: TJCoreOPFPIDArray;
begin
  if not FPIDArrayUpdated then
  begin
    FPIDArray := Mapping.CreatePIDArray(ItemsArray);
    { TODO : Fix cache outside transaction control }
    //FPIDArrayUpdated := True;
  end;
  Result := FPIDArray;
end;

function TJCoreOPFADMCollection.HasOIDInCache(const AOID: TJCoreOPFOID): Boolean;
var
  VOID: TJCoreOPFOID;
begin
  Result := True;
  for VOID in OIDCache do
    if AOID = VOID then
      Exit;
  Result := False;
end;

function TJCoreOPFADMCollection.HasOIDInCollection(
  const APIDArray: TJCoreOPFPIDArray; const AOID: TJCoreOPFOID): Boolean;
var
  VPID: TJCoreOPFPID;
begin
  Result := True;
  for VPID in APIDArray do
    if VPID.OID = AOID then
      Exit;
  Result := False;
end;

procedure TJCoreOPFADMCollection.ReleaseOIDCache;
var
  VOID: TJCoreOPFOID;
begin
  for VOID in OIDCache do
    FreeAndNil(VOID);
end;

procedure TJCoreOPFADMCollection.SetOIDCache(AValue: TJCoreOPFOIDArray);
var
  VOID: TJCoreOPFOID;
begin
  ReleaseOIDCache;
  FOIDCache := AValue;
  for VOID in FOIDCache do
    VOID.AddRef;
end;

procedure TJCoreOPFADMCollection.UpdateChanges;
var
  VPIDArray: TJCoreOPFPIDArray;
  VPIDAddedArray: TJCoreOPFPIDArray;
  VOIDRemovedArray: TJCoreOPFOIDArray;
  I, VMinSize, VMaxSize, VTmpSize, VPIDSize, VOIDSize: Integer;
  VAddedCount, VRemovedCount: Integer;
begin
  { TODO : Reduce npath }
  if FChangesUpdated then
    Exit;

  // Initializing vars
  VPIDArray := PIDArray;
  VPIDSize := Length(VPIDArray);
  VOIDSize := Length(OIDCache);
  VMinSize := VPIDSize;
  if VOIDSize < VMinSize then
  begin
    VMinSize := VOIDSize;
    VMaxSize := VPIDSize;
  end else
    VMaxSize := VOIDSize;

  // populating temp arrays
  SetLength(VPIDAddedArray, VMaxSize);
  SetLength(VOIDRemovedArray, VMaxSize);
  VTmpSize := 0;
  for I := 0 to Pred(VMinSize) do
  begin
    if VPIDArray[I].OID <> OIDCache[I] then
    begin
      VPIDAddedArray[VTmpSize] := VPIDArray[I];
      VOIDRemovedArray[VTmpSize] := OIDCache[I];
      Inc(VTmpSize);
    end;
  end;
  for I := VMinSize to Pred(VPIDSize) do
  begin
    VPIDAddedArray[VTmpSize] := VPIDArray[I];
    VOIDRemovedArray[VTmpSize] := nil;
    Inc(VTmpSize);
  end;
  for I := VMinSize to Pred(VOIDSize) do
  begin
    VPIDAddedArray[VTmpSize] := nil;
    VOIDRemovedArray[VTmpSize] := OIDCache[I];
    Inc(VTmpSize);
  end;

  // populating added/removed arrays
  SetLength(FPIDAdded, VTmpSize);
  SetLength(FOIDRemoved, VTmpSize);
  VAddedCount := 0;
  VRemovedCount := 0;
  for I := 0 to Pred(VTmpSize) do
  begin
    if Assigned(VPIDAddedArray[I]) and not HasOIDInCache(VPIDAddedArray[I].OID) then
    begin
      FPIDAdded[VAddedCount] := VPIDAddedArray[I];
      Inc(VAddedCount);
    end;
    if Assigned(VOIDRemovedArray[I]) and not HasOIDInCollection(VPIDArray, VOIDRemovedArray[I]) then
    begin
      FOIDRemoved[VRemovedCount] := VOIDRemovedArray[I];
      Inc(VRemovedCount);
    end;
  end;
  SetLength(FPIDAdded, VAddedCount);
  SetLength(FOIDRemoved, VRemovedCount);
  FChangesUpdated := True;
end;

function TJCoreOPFADMCollection.InternalIsDirty: Boolean;
var
  VItems: TJCoreObjectArray;
  VPIDArray: TJCoreOPFPIDArray;
begin
  { TODO : evaluate after lazy loading implementation }
  VItems := ItemsArray;
  Result := ArraySizeIsDirty(VItems);
  if not Result then
  begin
    VPIDArray := PIDArray;
    Result := ArrayOrderIsDirty(VPIDArray) or ArrayContentIsDirty(VPIDArray);
  end;
end;

procedure TJCoreOPFADMCollection.InternalUpdateCache;
var
  VPIDArray: TJCoreOPFPIDArray;
  VOIDCache: TJCoreOPFOIDArray;
  VOID: TJCoreOPFOID;
  I, J: Integer;
begin
  { TODO : evaluate after lazy loading implementation }
  VPIDArray := PIDArray;
  SetLength(VOIDCache, Length(VPIDArray));
  J := 0;
  for I := Low(VPIDArray) to High(VPIDArray) do
  begin
    VOID := VPIDArray[I].OID;
    if Assigned(VOID) then
    begin
      VOIDCache[J] := VOID;
      Inc(J);
    end;
  end;
  SetLength(VOIDCache, J);
  OIDCache := VOIDCache;
end;

destructor TJCoreOPFADMCollection.Destroy;
begin
  ReleaseOIDCache;
  inherited Destroy;
end;

procedure TJCoreOPFADMCollection.TransactionClosing(const ACommit: Boolean);
begin
  inherited;
  FChangesUpdated := False;
  FItemsArrayUpdated := False;
  FPIDArrayUpdated := False;
end;

{ TJCoreOPFADMFPSListCollection }

function TJCoreOPFADMFPSListCollection.InternalCreateItemsArray: TJCoreObjectArray;
var
  VItems: TFPSList;
  I: Integer;
begin
  { TODO : evaluate after lazy loading implementation }
  VItems := TFPSList(GetObjectProp(Entity, AttrPropInfo, TFPSList));
  if Assigned(VItems) then
  begin
    SetLength(Result, VItems.Count);
    { TODO : Thread safe }
    for I := 0 to Pred(VItems.Count) do
      Result[I] := TObject(VItems[I]^);
  end else
    SetLength(Result, 0);
end;

class function TJCoreOPFADMFPSListCollection.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  if AAttrTypeInfo^.Kind = tkClass then
    Result := GetTypeData(AAttrTypeInfo)^.ClassType.InheritsFrom(TFPSList)
  else
    Result := False;
end;

{ TJCoreOPFPID }

procedure TJCoreOPFPID.CreateADMs;
var
  VAttrMetadata: TJCoreOPFAttrMetadata;
  I: Integer;
begin
  for I := 0 to Pred(Metadata.AttributeCount) do
  begin
    VAttrMetadata := Metadata.Attributes[I];
    ADMMap.Add(VAttrMetadata.Name, TJCoreOPFADM(VAttrMetadata.CreateADM(Mapping, Entity)));
  end;
end;

function TJCoreOPFPID.GetEntity: TObject;
begin
  Result := FEntity;
end;

function TJCoreOPFPID.GetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
end;

function TJCoreOPFPID.GetOIDIntf: IJCoreOPFOID;
begin
  Result := FOID as IJCoreOPFOID
end;

function TJCoreOPFPID.GetOwnerIntf: IJCoreOPFPID;
begin
  Result := FOwner as IJCoreOPFPID;
end;

function TJCoreOPFPID.InternalIsDirty(const AIncludeExternals: Boolean): Boolean;
var
  VADM: TJCoreOPFADM;
  I: Integer;
begin
  Result := True;
  for I := 0 to Pred(ADMMap.Count) do
  begin
    VADM := ADMMap.Data[I];
    if VADM.Metadata.IsExternal then
    begin
      if AIncludeExternals and VADM.IsDirty then
        Exit;
    end else if VADM.IsDirty then
      Exit;
  end;
  Result := False;
end;

constructor TJCoreOPFPID.Create(const AMapping: IJCoreOPFPIDMapping;
  const AEntity: TObject; const AMetadata: TJCoreOPFClassMetadata);
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FMapping := AMapping;
  FEntity := AEntity;
  FMetadata := AMetadata;
  FIsPersistent := False;
  FADMMap := TJCoreOPFADMMap.Create;
  CreateADMs;
end;

destructor TJCoreOPFPID.Destroy;
var
  I: Integer;
begin
  for I := 0 to Pred(FADMMap.Count) do
    FADMMap.Data[I].Free;
  FreeAndNil(FADMMap);
  FreeAndNil(FOID);
  inherited Destroy;
end;

function TJCoreOPFPID.AcquireADM(const AAttributeName: string): TJCoreOPFADM;
var
  VIndex: Integer;
begin
  VIndex := FADMMap.IndexOf(AAttributeName);
  if VIndex = -1 then
    raise EJCoreAttributeNotFound.Create(Entity.ClassName, AAttributeName);
  Result := FADMMap.Data[VIndex];
end;

function TJCoreOPFPID.ADMByName(const AAttributeName: string): IJCoreOPFADM;
begin
  Result := AcquireADM(AAttributeName) as IJCoreOPFADM;
end;

procedure TJCoreOPFPID.AssignOID(const AOID: TJCoreOPFOID);
begin
  if IsPersistent then
    raise EJCoreOPFCannotAssignOIDPersistent.Create;
  FreeAndNil(FOID);
  FOID := AOID;
end;

procedure TJCoreOPFPID.AssignOwner(const AOwner: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  if AOwnerADM.Metadata.CompositionType = jctComposition then
  begin
    if not Assigned(AOwner) then
    begin
      FOwner := nil;
      FOwnerADM := nil;
    end else if not Assigned(FOwner) then
    begin
      { TODO : Check circular reference }
      FOwner := AOwner;
      FOwnerADM := AOwnerADM;
    end else if (AOwner <> FOwner) or (AOwnerADM <> FOwnerADM) then
      { TODO : Check duplication in the same admcollection }
      raise EJCoreOPFObjectAlreadyOwned.Create(Entity.ClassName, FOwner.Entity.ClassName);
  end;
end;

procedure TJCoreOPFPID.Commit;
var
  VADM: TJCoreOPFADM;
  I: Integer;
begin
  if FStored then
  begin
    FIsPersistent := Assigned(FOID);
    for I := 0 to Pred(ADMMap.Count) do
    begin
      VADM := ADMMap.Data[I];
      VADM.UpdateCache;
      VADM.TransactionClosing(True);
    end;
    FStored := False;
  end;
end;

function TJCoreOPFPID.IsDirty: Boolean;
begin
  Result := InternalIsDirty(True);
end;

function TJCoreOPFPID.IsInternalsDirty: Boolean;
begin
  Result := InternalIsDirty(False);
end;

procedure TJCoreOPFPID.ReleaseOID(const AOID: TJCoreOPFOID);
begin
  { TODO : Used to release the OID if an exception raises just after the OID
           was assigned. A refcounted object (intf or a jcore managed obj) is
           a better approach }
  if FOID = AOID then
    FOID := nil;
end;

procedure TJCoreOPFPID.Stored;
begin
  FStored := True;
end;

{ TJCoreOPFAttrMetadata }

function TJCoreOPFAttrMetadata.ReadGenericsComposition(const AClassName: string): TClass;
var
  VClassName: string;
  VPos: Integer;
begin
  // Sample of ClassName: TFPGList$TListType
  VPos := Pos('$', AClassName);
  if VPos > 0 then
  begin
    VClassName := Copy(AClassName, VPos + 1, Length(AClassName));
    Result := Model.FindClass(VClassName);
  end else
    Result := nil;
end;

constructor TJCoreOPFAttrMetadata.Create(const AModel: TJCoreOPFModel;
  const APropInfo: PPropInfo);
begin
  inherited Create(APropInfo);
  FModel := AModel;
  CompositionClass := ReadGenericsComposition(APropInfo^.PropType^.Name);
  FCompositionLinkType := jcltEmbedded;
  FIsExternal := APropInfo^.PropType^.Kind = tkClass;
end;

function TJCoreOPFAttrMetadata.CreateADM(const AMapping: IJCoreOPFPIDMapping;
  const AEntity: TObject): TJCoreOPFADM;
begin
  if not Assigned(FADMClass) then
    FADMClass := Model.AcquireADMClass(PropInfo^.PropType);
  Result := FADMClass.Create(AMapping, AEntity, Self);
end;

{ TJCoreOPFClassMetadata }

function TJCoreOPFClassMetadata.GetAttributes(const AIndex: Integer): TJCoreOPFAttrMetadata;
begin
  Result := inherited Attributes[AIndex] as TJCoreOPFAttrMetadata;
end;

function TJCoreOPFClassMetadata.AttributeByName(
  const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  Result := inherited AttributeByName(AAttributeName) as TJCoreOPFAttrMetadata;
end;

{ TJCoreOPFModel }

procedure TJCoreOPFModel.AddADMClass(const AADMClass: TJCoreOPFADMClass);
begin
  ADMClassList.Add(AADMClass);
end;

function TJCoreOPFModel.CreateAttribute(const APropInfo: PPropInfo): TJCoreAttrMetadata;
begin
  Result := TJCoreOPFAttrMetadata.Create(Self, APropInfo);
end;

function TJCoreOPFModel.CreateMetadata(const AClass: TClass): TJCoreClassMetadata;
begin
  Result := TJCoreOPFClassMetadata.Create(AClass);
end;

procedure TJCoreOPFModel.Finit;
begin
  FreeAndNil(FADMClassList);
  inherited Finit;
end;

procedure TJCoreOPFModel.InitRegistry;
begin
  inherited InitRegistry;
  AddADMClass(TJCoreOPFADMType32);
  AddADMClass(TJCoreOPFADMType64);
  AddADMClass(TJCoreOPFADMFloat);
  AddADMClass(TJCoreOPFADMAnsiString);
  AddADMClass(TJCoreOPFADMEntity);
  AddADMClass(TJCoreOPFADMFPSListCollection);
end;

function TJCoreOPFModel.IsReservedAttr(const AAttrName: ShortString): Boolean;
begin
  Result := SameText(SPID, AAttrName) or inherited IsReservedAttr(AAttrName);
end;

constructor TJCoreOPFModel.Create;
begin
  FADMClassList := TJCoreOPFADMClassList.Create;
  inherited Create;
end;

function TJCoreOPFModel.AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
begin
  for Result in ADMClassList do
    if Result.Apply(Self, AAttrTypeInfo) then
      Exit;
  raise EJCoreOPFUnsupportedAttributeType.Create(AAttrTypeInfo);
end;

function TJCoreOPFModel.AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
begin
  Result := inherited AcquireMetadata(AClass) as TJCoreOPFClassMetadata;
end;

end.

