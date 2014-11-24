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
  JCoreEntity,
  JCoreMetadata,
  JCoreOPFDriver,
  JCoreOPFOID;

type

  TJCoreOPFADMEntity = class;
  TJCoreOPFADMCollection = class;

  TJCoreOPFPID = class;
  TJCoreOPFPIDClass = class of TJCoreOPFPID;
  TJCoreOPFPIDList = specialize TFPGObjectList<TJCoreOPFPID>;
  TJCoreOPFPIDArray = array of TJCoreOPFPID;

  IJCoreOPFPIDManager = interface
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    function CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
    procedure LoadEntity(const APID: TJCoreOPFPID; const AADM: TJCoreOPFADMEntity);
    procedure LoadCollection(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure StorePID(const APID: TJCoreOPFPID);
  end;

  TJCoreOPFAttributeType = (jatSimple, jatEntity, jatCollection);
  TJCoreOPFAttributeTypeSet = set of TJCoreOPFAttributeType;

  TJCoreOPFAttrMetadata = class;

  { TJCoreOPFADM }

  TJCoreOPFADM = class(TObject, IJCoreADM)
  private
    FAttrAddr: Pointer;
    FAttrPropInfo: PPropInfo;
    FCacheUpdated: Boolean;
    FLoaded: Boolean;
    FMetadata: TJCoreOPFAttrMetadata;
    FPID: TJCoreOPFPID;
  protected
    procedure InternalCommit; virtual;
    procedure InternalGetter; virtual; abstract;
    function InternalIsDirty: Boolean; virtual; abstract;
    procedure InternalLoad; virtual;
    procedure InternalUpdateCache; virtual; abstract;
    property AttrAddr: Pointer read FAttrAddr;
    property AttrPropInfo: PPropInfo read FAttrPropInfo;
  public
    constructor Create(const APID: TJCoreOPFPID; const AMetadata: TJCoreOPFAttrMetadata); virtual;
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; virtual; abstract;
    class function AttributeType: TJCoreOPFAttributeType; virtual; abstract;
    procedure Commit;
    function IsDirty: Boolean;
    procedure Load;
    procedure ReadFromDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
    procedure UpdateAttrAddr(const AAttrAddrRef: PPPointer);
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
    property Metadata: TJCoreOPFAttrMetadata read FMetadata;
    property PID: TJCoreOPFPID read FPID;
  end;

  TJCoreOPFADMClass = class of TJCoreOPFADM;
  TJCoreOPFADMClassList = specialize TFPGList<TJCoreOPFADMClass>;
  TJCoreOPFADMArray = array of TJCoreOPFADM;
  TJCoreOPFADMMap = specialize TFPGMap<string, TJCoreOPFADM>;

  { TJCoreOPFADMSimple }

  TJCoreOPFADMSimple = class(TJCoreOPFADM)
  public
    class function AttributeType: TJCoreOPFAttributeType; override;
  end;

  { TJCoreOPFADMType32 }

  TJCoreOPFADMType32 = class(TJCoreOPFADMSimple)
  private
    FCache: Longint;
    function GetValue: Longint;
    procedure SetValue(AValue: Longint);
    function UseGetter: Longint;
    procedure UseSetter(const AValue: Longint);
  protected
    procedure InternalGetter; override;
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
    property Value: Longint read GetValue write SetValue;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
    procedure ReadFromDriver(const ADriver: TJCoreOPFDriver); override;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
  end;

  { TJCoreOPFADMType64 }

  TJCoreOPFADMType64 = class(TJCoreOPFADMSimple)
  private
    FCache: Int64;
    function GetValue: Int64;
    procedure SetValue(const AValue: Int64);
    function UseGetter: Int64;
    procedure UseSetter(const AValue: Int64);
  protected
    procedure InternalGetter; override;
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
    property Value: Int64 read GetValue write SetValue;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
    procedure ReadFromDriver(const ADriver: TJCoreOPFDriver); override;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
  end;

  { TJCoreOPFADMFloat }

  TJCoreOPFADMFloat = class(TJCoreOPFADMSimple)
  private
    FCache: Extended;
    function GetGetter: Extended;
    function GetValue: Extended;
  protected
    procedure InternalGetter; override;
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
    property Value: Extended read GetValue;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMAnsiString }

  TJCoreOPFADMAnsiString = class(TJCoreOPFADMSimple)
  private
    FCache: AnsiString;
    function GetValue: AnsiString;
    procedure SetValue(AValue: AnsiString);
    function UseGetter: AnsiString;
    procedure UseSetter(const AValue: AnsiString);
  protected
    procedure InternalGetter; override;
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
    property Value: AnsiString read GetValue write SetValue;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
    procedure ReadFromDriver(const ADriver: TJCoreOPFDriver); override;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
  end;

  { TJCoreOPFADMObject }

  TJCoreOPFADMObject = class(TJCoreOPFADM)
  private
    function GetValue: TObject;
    procedure SetValue(const AValue: TObject);
    function UseGetter: TObject;
    procedure UseSetter(const AValue: TObject);
  protected
    procedure InternalGetter; override;
  public
    property Value: TObject read GetValue write SetValue;
  end;

  { TJCoreOPFADMEntity }

  TJCoreOPFADMEntity = class(TJCoreOPFADMObject)
  private
    FCompositionOID: TJCoreOPFOID;
    FOIDCache: TJCoreOPFOID;
    procedure SetCompositionOID(AValue: TJCoreOPFOID);
    procedure SetOIDCache(AValue: TJCoreOPFOID);
  protected
    function AcquirePID: TJCoreOPFPID;
    function InternalIsDirty: Boolean; override;
    procedure InternalLoad; override;
    procedure InternalUpdateCache; override;
    property OIDCache: TJCoreOPFOID read FOIDCache write SetOIDCache;
  public
    destructor Destroy; override;
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
    procedure AssignComposition(const AComposite: TObject);
    class function AttributeType: TJCoreOPFAttributeType; override;
    procedure ReadFromDriver(const ADriver: TJCoreOPFDriver); override;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
    property CompositionOID: TJCoreOPFOID read FCompositionOID write SetCompositionOID;
  end;

  { TJCoreOPFADMCollection }

  TJCoreOPFADMCollection = class(TJCoreOPFADMObject)
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
    procedure InternalAssignArray(const AArray: TJCoreObjectArray); virtual; abstract;
    procedure InternalCommit; override;
    function InternalCreateItemsArray: TJCoreObjectArray; virtual; abstract;
    function InternalIsDirty: Boolean; override;
    procedure InternalLoad; override;
    procedure InternalUpdateCache; override;
    property ItemsArray: TJCoreObjectArray read GetItemsArray;
    property OIDCache: TJCoreOPFOIDArray read FOIDCache write SetOIDCache;
  public
    destructor Destroy; override;
    procedure AssignArray(const AArray: TJCoreObjectArray);
    class function AttributeType: TJCoreOPFAttributeType; override;
    procedure ReadFromDriver(const ADriver: TJCoreOPFDriver); override;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
    property OIDRemoved: TJCoreOPFOIDArray read GetOIDRemoved;
    property PIDAdded: TJCoreOPFPIDArray read GetPIDAdded;
    property PIDArray: TJCoreOPFPIDArray read GetPIDArray;
  end;

  { TJCoreOPFADMFPSListCollection }

  TJCoreOPFADMFPSListCollection = class(TJCoreOPFADMCollection)
  private
    function GetList: TFPSList;
  protected
    procedure InternalAssignArray(const AArray: TJCoreObjectArray); override;
    function InternalCreateItemsArray: TJCoreObjectArray; override;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  TJCoreOPFClassMetadata = class;

  { TJCoreOPFADMMapping }

  TJCoreOPFADMMapping = class(TJCoreOPFADMMap)
  // ADMs of a single class metadata, without parents.
  // Based on TJCoreOPFMaps and synchronized with TJCoreOPFMappingClassFactory's mappings.
  private
    FADMAttributeChanged: TJCoreOPFADMArray;
    FADMCollectionChanged: TJCoreOPFADMArray;
    FPID: TJCoreOPFPID;
    function BuildChangedADMArray(const AAttributeType: TJCoreOPFAttributeTypeSet): TJCoreOPFADMArray;
    function GetADM(const AIndex: Integer): TJCoreOPFADM;
    function GetADMAttributeChanged: TJCoreOPFADMArray;
    function GetADMByName(const AAttributeName: string): TJCoreOPFADM;
    function GetADMCollectionChanged: TJCoreOPFADMArray;
  protected
    function InternalIsDirty(const AIncludeCollections: Boolean): Boolean;
  public
    constructor Create(const APID: TJCoreOPFPID);
    function AcquireADM(const AAttributeName: string): TJCoreOPFADM;
    procedure Commit;
    function IsAttributesDirty: Boolean;
    function IsDirty: Boolean;
    property ADM[const AIndex: Integer]: TJCoreOPFADM read GetADM;
    property ADMByName[const AAttributeName: string]: TJCoreOPFADM read GetADMByName; default;
    property ADMAttributeChanged: TJCoreOPFADMArray read GetADMAttributeChanged;
    property ADMCollectionChanged: TJCoreOPFADMArray read GetADMCollectionChanged;
    property PID: TJCoreOPFPID read FPID;
  end;

  TJCoreOPFADMMappingMap = specialize TFPGMap<Pointer, TJCoreOPFADMMapping>;

  TJCoreOPFMap = class;
  TJCoreOPFMaps = specialize TFPGObjectList<TJCoreOPFMap>;
  TJCoreOPFMapMap = specialize TFPGMap<string, TJCoreOPFMap>;

  { TJCoreOPFPID }

  { TODO :
    Sessions from different configurations need different PIDs,
    iow, the same entity may be persistent to a configuration
    and nonpersistent to another one }

  TJCoreOPFPID = class(TInterfacedObject, IJCorePID)
  private
    // Stores and owns all ADMs
    FADMMap: TJCoreOPFADMMap;
    // Stores maps of ADMs, synchronized with TJCoreOPFMaps
    FADMMappingMap: TJCoreOPFADMMappingMap;
    FAttrAddrRef: PPointer;
    FEntity: TObject;
    FIsPersistent: Boolean;
    FPIDManager: IJCoreOPFPIDManager;
    FMetadata: TJCoreOPFClassMetadata;
    FOID: TJCoreOPFOID;
    FOwner: TJCoreOPFPID;
    FOwnerADM: TJCoreOPFADMCollection;
    function CreateADM(const AAttrMetadata: TJCoreOPFAttrMetadata): TJCoreOPFADM;
    procedure CreateADMs(const AMaps: TJCoreOPFMaps);
    function GetADMMapping(const AIndex: Integer): TJCoreOPFADMMapping;
    function IGetEntity: TObject;
    function IGetIsPersistent: Boolean;
    function IGetOID: IJCoreOID;
    function IGetOwner: IJCorePID;
    function IJCorePID.Entity = IGetEntity;
    function IJCorePID.IsPersistent = IGetIsPersistent;
    function IJCorePID.OID = IGetOID;
    function IJCorePID.Owner = IGetOwner;
  protected
    function AcquireADMByAttrAddr(const AAttrAddr: Pointer): TJCoreOPFADM;
    property ADMMap: TJCoreOPFADMMap read FADMMap;
    property ADMMappingMap: TJCoreOPFADMMappingMap read FADMMappingMap;
    property PIDManager: IJCoreOPFPIDManager read FPIDManager;
  public
    constructor Create(const APIDManager: IJCoreOPFPIDManager; const AEntity: TObject; const AMetadata: TJCoreOPFClassMetadata);
    destructor Destroy; override;
    function AcquireADM(const AAttributeName: string): TJCoreOPFADM;
    function ADMByName(const AAttributeName: string): IJCoreADM;
    function ADMMappingCount: Integer;
    procedure AssignOID(const AOID: TJCoreOPFOID);
    procedure AssignOwner(const AOwner: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure Commit;
    function IsDirty: Boolean;
    function Lazyload(const AAttrAddr: Pointer): Boolean;
    property ADMMapping[const AIndex: Integer]: TJCoreOPFADMMapping read GetADMMapping; default;
    property IsPersistent: Boolean read FIsPersistent;
    property Entity: TObject read FEntity;
    property Metadata: TJCoreOPFClassMetadata read FMetadata;
    property OID: TJCoreOPFOID read FOID;
    property Owner: TJCoreOPFPID read FOwner;
  end;

  TJCoreOPFModel = class;

  { TJCoreOPFAttrMetadata }

  TJCoreOPFAttrMetadata = class(TJCoreAttrMetadata)
  private
    FADMClass: TJCoreOPFADMClass;
    FAttributeType: TJCoreOPFAttributeType;
    FExternalLinkLeftFieldName: string;
    FExternalLinkRightFieldName: string;
    FExternalLinkTableName: string;
    FHasLazyload: Boolean;
    FPersistentFieldName: string;
    function GetCompositionMetadata: TJCoreOPFClassMetadata;
    function GetHasExternalLink: Boolean;
    function GetModel: TJCoreOPFModel;
    function ReadComposition(const ATypeInfo: PTypeInfo): TClass;
  protected
    property Model: TJCoreOPFModel read GetModel;
  public
    constructor Create(const AModel: TJCoreModel; const AOwner: TJCoreClassMetadata; const APropInfo: PPropInfo); override;
    function CreateADM(const APID: TJCoreOPFPID): TJCoreOPFADM;
    function IsCollection: Boolean; override;
    procedure NoLazyload;
    property ADMClass: TJCoreOPFADMClass read FADMClass;
    property AttributeType: TJCoreOPFAttributeType read FAttributeType;
    property CompositionMetadata: TJCoreOPFClassMetadata read GetCompositionMetadata;
    property ExternalLinkLeftFieldName: string read FExternalLinkLeftFieldName write FExternalLinkLeftFieldName;
    property ExternalLinkRightFieldName: string read FExternalLinkRightFieldName write FExternalLinkRightFieldName;
    property ExternalLinkTableName: string read FExternalLinkTableName write FExternalLinkTableName;
    property HasExternalLink: Boolean read GetHasExternalLink;
    property HasLazyload: Boolean read FHasLazyload;
    property PersistentFieldName: string read FPersistentFieldName;
  end;

  TJCoreOPFAttrMetadataList = specialize TFPGObjectList<TJCoreOPFAttrMetadata>;
  TJCoreOPFAttrMetadataArray = array of TJCoreOPFAttrMetadata;

  { TJCoreOPFMap }

  TJCoreOPFMap = class(TJCoreOPFAttrMetadataList)
  // Map is a list of attributes that share the same persistence structure, eg a
  // table on a RDBMS)
  //
  // In the current version these attributes must be of the same class or a
  // parent class.
  private
    FGeneratorName: string;
    FMetadata: TJCoreOPFClassMetadata;
    FOIDClass: TJCoreOPFOIDClass;
    FOIDName: TJCoreStringArray;
    FOwnerOIDName: TJCoreStringArray;
    FSubClasses: TJCoreClassArray;
    FSubMaps: TJCoreOPFMaps;
    FTableName: string;
  public
    constructor Create(const AMetadata: TJCoreOPFClassMetadata);
    property GeneratorName: string read FGeneratorName;
    property Metadata: TJCoreOPFClassMetadata read FMetadata;
    property OIDClass: TJCoreOPFOIDClass read FOIDClass;
    property OIDName: TJCoreStringArray read FOIDName;
    property OwnerOIDName: TJCoreStringArray read FOwnerOIDName;
    property SubClasses: TJCoreClassArray read FSubClasses;
    property SubMaps: TJCoreOPFMaps read FSubMaps;
    property TableName: string read FTableName;
  end;

  { TJCoreOPFClassMetadata }

  TJCoreOPFClassMetadata = class(TJCoreClassMetadata)
  private
    FGeneratorName: string; // model initializes
    FMaps: TJCoreOPFMaps;
    FOIDClass: TJCoreOPFOIDClass; // model initializes
    FOIDName: TJCoreStringArray; // model initializes
    FSubMaps: TJCoreOPFMaps;
    FTableName: string; // model initializes
    function GetAttributes(const AIndex: Integer): TJCoreOPFAttrMetadata;
    function GetMaps: TJCoreOPFMaps;
    function GetModel: TJCoreOPFModel;
    function GetParent: TJCoreOPFClassMetadata;
    function GetSubMaps: TJCoreOPFMaps;
  protected
    property Model: TJCoreOPFModel read GetModel;
  public
    { TODO : Generics? }
    constructor Create(const AModel: TJCoreModel; const AClass: TClass; const AParent: TJCoreClassMetadata); override;
    destructor Destroy; override;
    function AttributeByName(const AAttributeName: string): TJCoreOPFAttrMetadata;
    property Attributes[const AIndex: Integer]: TJCoreOPFAttrMetadata read GetAttributes; default;
    property GeneratorName: string read FGeneratorName write FGeneratorName;
    property Maps: TJCoreOPFMaps read GetMaps;
    property OIDClass: TJCoreOPFOIDClass read FOIDClass;
    property OIDName: TJCoreStringArray read FOIDName;
    property Parent: TJCoreOPFClassMetadata read GetParent;
    property SubMaps: TJCoreOPFMaps read GetSubMaps;
    property TableName: string read FTableName;
  end;

  { TJCoreOPFModel }

  TJCoreOPFModel = class(TJCoreModel)
  { TODO : Model, map and metadata threadsafe }
  private
    FADMClassList: TJCoreOPFADMClassList;
    FGeneratorName: string;
    FMapMap: TJCoreOPFMapMap;
    FOIDClass: TJCoreOPFOIDClass;
    procedure AcquireMaps(const AMetadata: TJCoreOPFClassMetadata; const AMaps: TJCoreOPFMaps);
    procedure AcquireSubMaps(const AMetadata: TJCoreOPFClassMetadata; const AMaps: TJCoreOPFMaps);
  protected
    procedure AddADMClass(const AADMClassArray: array of TJCoreOPFADMClass);
    function AcquireMap(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMap;
    function AttributeMetadataClass: TJCoreAttrMetadataClass; override;
    function ClassMetadataClass: TJCoreClassMetadataClass; override;
    function CreateAttrMetadata(const AMetadata: TJCoreClassMetadata; const APropInfo: PPropInfo): TJCoreAttrMetadata; override;
    procedure Finit; override;
    procedure InitRegistry; override;
    function IsReservedAttr(const AAttrName: ShortString): Boolean; override;
    procedure RefineClassMetadata(const AClassMetadata: TJCoreClassMetadata); override;
    property ADMClassList: TJCoreOPFADMClassList read FADMClassList;
    property MapMap: TJCoreOPFMapMap read FMapMap;
  public
    constructor Create; override;
    function AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
    function AcquireAttrMetadata(const AClass: TClass; const AAttributeName: string): TJCoreOPFAttrMetadata;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
    function CreateMaps(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMaps;
    function CreateSubMaps(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMaps;
    property GeneratorName: string read FGeneratorName write FGeneratorName;
    property OIDClass: TJCoreOPFOIDClass read FOIDClass write FOIDClass;
  end;

implementation

uses
  sysutils,
  JCoreOPFConsts,
  JCoreOPFException;

{ TJCoreOPFADM }

procedure TJCoreOPFADM.InternalCommit;
begin
end;

procedure TJCoreOPFADM.InternalLoad;
begin
  raise EJCoreOPFUnsupportedLoadOperation.Create(Metadata.PropInfo^.PropType^.Name);
end;

constructor TJCoreOPFADM.Create(const APID: TJCoreOPFPID; const AMetadata: TJCoreOPFAttrMetadata);
begin
  inherited Create;
  FPID := APID;
  FAttrPropInfo := AMetadata.PropInfo;
  FCacheUpdated := False;
  FMetadata := AMetadata;
  FLoaded := False;
end;

procedure TJCoreOPFADM.Commit;
begin
  InternalUpdateCache;
  FCacheUpdated := True;
  InternalCommit;
end;

function TJCoreOPFADM.IsDirty: Boolean;
begin
  { TODO : Implement IsDirty cache while transaction is active }
  Result := not FCacheUpdated or InternalIsDirty;
end;

procedure TJCoreOPFADM.Load;
begin
  if not FLoaded then
  begin
    InternalLoad;
    FLoaded := True;
  end;
end;

procedure TJCoreOPFADM.UpdateAttrAddr(const AAttrAddrRef: PPPointer);
begin
  // AAttrAddrRef^, which references ADM.AttrAddr address, will be updated
  // with the entity's attribute address by the Lazyload method.
  AAttrAddrRef^ := @FAttrAddr;
  InternalGetter;
  if not Assigned(FAttrAddr) then
    Metadata.NoLazyload;
end;

{ TJCoreOPFADMSimple }

class function TJCoreOPFADMSimple.AttributeType: TJCoreOPFAttributeType;
begin
  Result := jatSimple;
end;

{ TJCoreOPFADMType32 }

function TJCoreOPFADMType32.GetValue: Longint;
begin
  if Assigned(AttrAddr) then
    Result := Longint(AttrAddr^)
  else
    Result := UseGetter;
end;

procedure TJCoreOPFADMType32.SetValue(AValue: Longint);
begin
  if Assigned(AttrAddr) then
    Longint(AttrAddr^) := AValue
  else
    UseSetter(AValue);
end;

function TJCoreOPFADMType32.UseGetter: Longint;
begin
  Result := GetOrdProp(PID.Entity, AttrPropInfo);
end;

procedure TJCoreOPFADMType32.UseSetter(const AValue: Longint);
begin
  SetOrdProp(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMType32.InternalGetter;
begin
  UseGetter;
end;

function TJCoreOPFADMType32.InternalIsDirty: Boolean;
begin
  Result := Value <> FCache;
end;

procedure TJCoreOPFADMType32.InternalUpdateCache;
begin
  FCache := Value;
end;

class function TJCoreOPFADMType32.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind in [tkInteger, tkChar, tkEnumeration, tkBool];
end;

procedure TJCoreOPFADMType32.ReadFromDriver(const ADriver: TJCoreOPFDriver);
begin
  Value := ADriver.ReadInt32;
end;

procedure TJCoreOPFADMType32.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteInt32(Value);
end;

{ TJCoreOPFADMType64 }

function TJCoreOPFADMType64.GetValue: Int64;
begin
  if Assigned(AttrAddr) then
  begin
    case AttrPropInfo^.PropType^.Kind of
      tkInt64: Result := PInt64(AttrAddr)^;
      tkQWord: Result := PQWord(AttrAddr)^;
      else raise EJCoreOPFUnsupportedAttributeType.Create(AttrPropInfo^.PropType);
    end;
  end else
    Result := UseGetter;
end;

procedure TJCoreOPFADMType64.SetValue(const AValue: Int64);
begin
  UseSetter(AValue);
end;

function TJCoreOPFADMType64.UseGetter: Int64;
begin
  Result := GetInt64Prop(PID.Entity, AttrPropInfo);
end;

procedure TJCoreOPFADMType64.UseSetter(const AValue: Int64);
begin
  SetInt64Prop(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMType64.InternalGetter;
begin
  UseGetter;
end;

function TJCoreOPFADMType64.InternalIsDirty: Boolean;
begin
  Result := Value <> FCache;
end;

procedure TJCoreOPFADMType64.InternalUpdateCache;
begin
  FCache := Value;
end;

class function TJCoreOPFADMType64.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind in [tkInt64, tkQWord];
end;

procedure TJCoreOPFADMType64.ReadFromDriver(const ADriver: TJCoreOPFDriver);
begin
  Value := ADriver.ReadInt64;
end;

procedure TJCoreOPFADMType64.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteInt64(Value);
end;

{ TJCoreOPFADMFloat }

function TJCoreOPFADMFloat.GetGetter: Extended;
begin
  Result := GetFloatProp(PID.Entity, AttrPropInfo);
end;

function TJCoreOPFADMFloat.GetValue: Extended;
begin
  if Assigned(AttrAddr) then
  begin
    case GetTypeData(AttrPropInfo^.PropType)^.FloatType of
      ftSingle: Result := PSingle(AttrAddr)^;
      ftDouble: Result := PDouble(AttrAddr)^;
      ftExtended: Result := PExtended(AttrAddr)^;
      ftComp: Result := PComp(AttrAddr)^;
      ftCurr: Result := PCurrency(AttrAddr)^;
      else raise EJCoreOPFUnsupportedAttributeType.Create(AttrPropInfo^.PropType);
    end;
  end else
    Result := GetGetter;
end;

procedure TJCoreOPFADMFloat.InternalGetter;
begin
  GetGetter;
end;

function TJCoreOPFADMFloat.InternalIsDirty: Boolean;
begin
  Result := Value <> FCache;
end;

procedure TJCoreOPFADMFloat.InternalUpdateCache;
begin
  FCache := Value;
end;

class function TJCoreOPFADMFloat.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkFloat;
end;

{ TJCoreOPFADMAnsiString }

function TJCoreOPFADMAnsiString.GetValue: AnsiString;
begin
  if Assigned(AttrAddr) then
    Result := PAnsiString(AttrAddr)^
  else
    Result := UseGetter;
end;

procedure TJCoreOPFADMAnsiString.SetValue(AValue: AnsiString);
begin
  if Assigned(AttrAddr) then
    PAnsiString(AttrAddr)^ := AValue
  else
    UseSetter(AValue);
end;

function TJCoreOPFADMAnsiString.UseGetter: AnsiString;
begin
  Result := GetStrProp(PID.Entity, AttrPropInfo);
end;

procedure TJCoreOPFADMAnsiString.UseSetter(const AValue: AnsiString);
begin
  SetStrProp(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMAnsiString.InternalGetter;
begin
  UseGetter;
end;

function TJCoreOPFADMAnsiString.InternalIsDirty: Boolean;
begin
  { TODO : use hash for long strings }
  Result := Value <> FCache;
end;

procedure TJCoreOPFADMAnsiString.InternalUpdateCache;
begin
  { TODO : use hash for long strings }
  FCache := Value;
end;

class function TJCoreOPFADMAnsiString.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkAString;
end;

procedure TJCoreOPFADMAnsiString.ReadFromDriver(const ADriver: TJCoreOPFDriver);
begin
  Value := ADriver.ReadString;
end;

procedure TJCoreOPFADMAnsiString.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteString(Value);
end;

{ TJCoreOPFADMObject }

function TJCoreOPFADMObject.GetValue: TObject;
begin
  if Assigned(AttrAddr) then
    Result := TObject(AttrAddr^)
  else
    Result := UseGetter;
end;

procedure TJCoreOPFADMObject.SetValue(const AValue: TObject);
begin
  if Assigned(AttrAddr) then
    TObject(AttrAddr^) := AValue
  else
    UseSetter(AValue);
end;

function TJCoreOPFADMObject.UseGetter: TObject;
begin
  Result := GetObjectProp(PID.Entity, AttrPropInfo, nil);
end;

procedure TJCoreOPFADMObject.UseSetter(const AValue: TObject);
begin
  SetObjectProp(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMObject.InternalGetter;
begin
  UseGetter;
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

procedure TJCoreOPFADMEntity.SetCompositionOID(AValue: TJCoreOPFOID);
begin
  if FCompositionOID <> AValue then
  begin
    FreeAndNil(FCompositionOID);
    FCompositionOID := AValue;
  end;
end;

function TJCoreOPFADMEntity.AcquirePID: TJCoreOPFPID;
var
  VObject: TObject;
begin
  VObject := Value;
  if Assigned(VObject) then
    Result := PID.PIDManager.AcquirePID(VObject)
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

procedure TJCoreOPFADMEntity.InternalLoad;
begin
  // No OID means nil reference or non persistent entity. So no loading.
  if Assigned(CompositionOID) then
    PID.PIDManager.LoadEntity(PID, Self);
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
  FreeAndNil(FCompositionOID);
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

procedure TJCoreOPFADMEntity.AssignComposition(const AComposite: TObject);
begin
  SetObjectProp(PID.Entity, Metadata.PropInfo, AComposite);
end;

class function TJCoreOPFADMEntity.AttributeType: TJCoreOPFAttributeType;
begin
  Result := jatEntity;
end;

procedure TJCoreOPFADMEntity.ReadFromDriver(const ADriver: TJCoreOPFDriver);
begin
  if not ADriver.ReadNull then
    CompositionOID := Metadata.CompositionMetadata.OIDClass.CreateFromDriver(ADriver)
  else
    CompositionOID := nil;
end;

procedure TJCoreOPFADMEntity.WriteToDriver(const ADriver: TJCoreOPFDriver);
var
  VPID: TJCoreOPFPID;
begin
  VPID := AcquirePID;
  if Assigned(VPID) then
  begin
    if not Assigned(VPID.OID) then
      VPID.PIDManager.StorePID(VPID);
    VPID.OID.WriteToDriver(ADriver)
  end else
    Metadata.CompositionMetadata.OIDClass.WriteNull(ADriver);
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
    FPIDArray := PID.PIDManager.CreatePIDArray(ItemsArray);
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
  I: Integer;
begin
  for I := Low(OIDCache) to High(OIDCache) do
  begin
    VOID := OIDCache[I];
    OIDCache[I] := nil;
    VOID.Free;
  end;
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

procedure TJCoreOPFADMCollection.InternalCommit;
begin
  inherited;
  FChangesUpdated := False;
  FItemsArrayUpdated := False;
  FPIDArrayUpdated := False;
end;

function TJCoreOPFADMCollection.InternalIsDirty: Boolean;
var
  VItems: TJCoreObjectArray;
  VPIDArray: TJCoreOPFPIDArray;
begin
  VItems := ItemsArray;
  Result := ArraySizeIsDirty(VItems);
  if not Result then
  begin
    VPIDArray := PIDArray;
    Result := ArrayOrderIsDirty(VPIDArray) or ArrayContentIsDirty(VPIDArray);
  end;
end;

procedure TJCoreOPFADMCollection.InternalLoad;
begin
  PID.PIDManager.LoadCollection(PID, Self);
end;

procedure TJCoreOPFADMCollection.InternalUpdateCache;
var
  VPIDArray: TJCoreOPFPIDArray;
  VOIDCache: TJCoreOPFOIDArray;
  VOID: TJCoreOPFOID;
  I, J: Integer;
begin
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

procedure TJCoreOPFADMCollection.AssignArray(const AArray: TJCoreObjectArray);
begin
  InternalAssignArray(AArray);
end;

class function TJCoreOPFADMCollection.AttributeType: TJCoreOPFAttributeType;
begin
  Result := jatCollection;
end;

procedure TJCoreOPFADMCollection.ReadFromDriver(const ADriver: TJCoreOPFDriver);
begin
  if not Metadata.HasLazyload then
    Load;
end;

procedure TJCoreOPFADMCollection.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  // Mapping does the job using OIDRemoved and PIDAdded properties
end;

{ TJCoreOPFADMFPSListCollection }

function TJCoreOPFADMFPSListCollection.GetList: TFPSList;
var
  VObject: TObject;
begin
  VObject := Value;
  if VObject is TFPSList then
    Result := TFPSList(VObject)
  else
    Result := nil;
end;

procedure TJCoreOPFADMFPSListCollection.InternalAssignArray(const AArray: TJCoreObjectArray);
var
  VItems: TFPSList;
  VItem: TObject;
begin
  VItems := GetList;
  if not Assigned(VItems) then
  begin
    VItems := TJCoreObjectList.Create(True);
    Value := VItems;
  end;
  VItems.Clear;
  for VItem in AArray do
    VItems.Add(@VItem);
end;

function TJCoreOPFADMFPSListCollection.InternalCreateItemsArray: TJCoreObjectArray;
var
  VItems: TFPSList;
  I: Integer;
begin
  VItems := GetList;
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

{ TJCoreOPFADMMapping }

function TJCoreOPFADMMapping.BuildChangedADMArray(
  const AAttributeType: TJCoreOPFAttributeTypeSet): TJCoreOPFADMArray;
var
  VADM: TJCoreOPFADM;
  VCount, I: Integer;
begin
  SetLength(Result, Count);
  VCount := 0;
  for I := 0 to Pred(Count) do
  begin
    VADM := Data[I];
    if (VADM.AttributeType in AAttributeType) and VADM.IsDirty then
    begin
      Result[VCount] := VADM;
      Inc(VCount);
    end;
  end;
  SetLength(Result, VCount);
end;

function TJCoreOPFADMMapping.GetADM(const AIndex: Integer): TJCoreOPFADM;
begin
  Result := Data[AIndex];
end;

function TJCoreOPFADMMapping.GetADMAttributeChanged: TJCoreOPFADMArray;
begin
  if not Assigned(FADMAttributeChanged) then
    FADMAttributeChanged := BuildChangedADMArray([jatSimple, jatEntity]);
  Result := FADMAttributeChanged;
end;

function TJCoreOPFADMMapping.GetADMByName(const AAttributeName: string): TJCoreOPFADM;
var
  VIndex: Integer;
begin
  VIndex := IndexOf(AAttributeName);
  if VIndex = -1 then
    raise EJCoreAttributeNotFound.Create(PID.Entity.ClassName, AAttributeName);
  Result := Data[VIndex];
end;

function TJCoreOPFADMMapping.GetADMCollectionChanged: TJCoreOPFADMArray;
begin
  if not Assigned(FADMCollectionChanged) then
    FADMCollectionChanged := BuildChangedADMArray([jatCollection]);
  Result := FADMCollectionChanged;
end;

function TJCoreOPFADMMapping.InternalIsDirty(const AIncludeCollections: Boolean): Boolean;
var
  VADM: TJCoreOPFADM;
  I: Integer;
begin
  Result := True;
  for I := 0 to Pred(Count) do
  begin
    VADM := Data[I];
    if VADM.Metadata.AttributeType = jatCollection then
    begin
      if AIncludeCollections and VADM.IsDirty then
        Exit;
    end else if VADM.IsDirty then
      Exit;
  end;
  Result := False;
end;

constructor TJCoreOPFADMMapping.Create(const APID: TJCoreOPFPID);
begin
  inherited Create;
  // do not own ADMs; PID.ADMMap will do.
  FPID := APID;
end;

function TJCoreOPFADMMapping.AcquireADM(const AAttributeName: string): TJCoreOPFADM;
begin
  Result := ADMByName[AAttributeName];
end;

procedure TJCoreOPFADMMapping.Commit;
begin
  SetLength(FADMAttributeChanged, 0);
  SetLength(FADMCollectionChanged, 0);
end;

function TJCoreOPFADMMapping.IsAttributesDirty: Boolean;
begin
  Result := InternalIsDirty(False);
end;

function TJCoreOPFADMMapping.IsDirty: Boolean;
begin
  Result := InternalIsDirty(True);
end;

{ TJCoreOPFPID }

function TJCoreOPFPID.CreateADM(const AAttrMetadata: TJCoreOPFAttrMetadata): TJCoreOPFADM;
var
  VADM: TJCoreOPFADM;
begin
  VADM := AAttrMetadata.CreateADM(Self);
  ADMMap.Add(AAttrMetadata.Name, VADM);
  if AAttrMetadata.HasLazyload then
  begin
    try
      VADM.UpdateAttrAddr(@FAttrAddrRef);
    finally
      FAttrAddrRef := nil;
    end;
  end;
  Result := VADM;
end;

procedure TJCoreOPFPID.CreateADMs(const AMaps: TJCoreOPFMaps);
var
  VAttrMetadata: TJCoreOPFAttrMetadata;
  VADMMapping: TJCoreOPFADMMapping;
  VMap: TJCoreOPFMap;
begin
  for VMap in AMaps do
  begin
    VADMMapping := TJCoreOPFADMMapping.Create(Self);
    ADMMappingMap.Add(VMap, VADMMapping);
    for VAttrMetadata in VMap do
      VADMMapping.Add(VAttrMetadata.Name, CreateADM(VAttrMetadata));
  end;
end;

function TJCoreOPFPID.GetADMMapping(const AIndex: Integer): TJCoreOPFADMMapping;
begin
  Result := ADMMappingMap.Data[AIndex];
end;

function TJCoreOPFPID.IGetEntity: TObject;
begin
  Result := FEntity;
end;

function TJCoreOPFPID.IGetIsPersistent: Boolean;
begin
  Result := FIsPersistent;
end;

function TJCoreOPFPID.IGetOID: IJCoreOID;
begin
  Result := FOID;
end;

function TJCoreOPFPID.IGetOwner: IJCorePID;
begin
  Result := FOwner;
end;

function TJCoreOPFPID.AcquireADMByAttrAddr(const AAttrAddr: Pointer): TJCoreOPFADM;
var
  I: Integer;
begin
  for I := 0 to Pred(ADMMap.Count) do
  begin
    Result := ADMMap.Data[I];
    if Result.AttrAddr = AAttrAddr then
      Exit;
  end;
  raise EJCoreAttributeNotFound.Create(Entity.ClassName, IntToStr(PtrUInt(AAttrAddr)));
end;

constructor TJCoreOPFPID.Create(const APIDManager: IJCoreOPFPIDManager;
  const AEntity: TObject; const AMetadata: TJCoreOPFClassMetadata);
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FPIDManager := APIDManager;
  FEntity := AEntity;
  FMetadata := AMetadata;
  FIsPersistent := False;
  FADMMap := TJCoreOPFADMMap.Create;
  FADMMappingMap := TJCoreOPFADMMappingMap.Create;
  CreateADMs(AMetadata.Maps);
end;

destructor TJCoreOPFPID.Destroy;
var
  I: Integer;
begin
  for I := 0 to Pred(FADMMap.Count) do
    FADMMap.Data[I].Free;
  for I := 0 to Pred(FADMMappingMap.Count) do
    FADMMappingMap.Data[I].Free;
  FreeAndNil(FADMMappingMap);
  FreeAndNil(FADMMap);
  FreeAndNil(FOID);
  inherited Destroy;
end;

function TJCoreOPFPID.AcquireADM(const AAttributeName: string): TJCoreOPFADM;
var
  VIndex: Integer;
begin
  VIndex := ADMMap.IndexOf(AAttributeName);
  if VIndex = -1 then
    raise EJCoreAttributeNotFound.Create(Entity.ClassName, AAttributeName);
  Result := ADMMap.Data[VIndex];
end;

function TJCoreOPFPID.ADMByName(const AAttributeName: string): IJCoreADM;
begin
  Result := AcquireADM(AAttributeName) as IJCoreADM;
end;

function TJCoreOPFPID.ADMMappingCount: Integer;
begin
  Result := ADMMappingMap.Count;
end;

procedure TJCoreOPFPID.AssignOID(const AOID: TJCoreOPFOID);
begin
  if IsPersistent and Assigned(AOID) then
    raise EJCoreOPFCannotAssignOIDPersistent.Create;
  if FOID <> AOID then
  begin
    FreeAndNil(FOID);
    FOID := AOID;
    if Assigned(FOID) then
      FOID.AddRef;
  end;
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
  I: Integer;
begin
  FIsPersistent := Assigned(FOID);
  for I := 0 to Pred(ADMMap.Count) do
    ADMMap.Data[I].Commit;
  for I := 0 to Pred(ADMMappingMap.Count) do
    ADMMappingMap.Data[I].Commit;
end;

function TJCoreOPFPID.IsDirty: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Pred(ADMMappingMap.Count) do
    if ADMMappingMap.Data[I].IsDirty then
      Exit;
  Result := False;
end;

function TJCoreOPFPID.Lazyload(const AAttrAddr: Pointer): Boolean;
begin
  // Return False only if an attribute *should* but *couldn't* be loaded
  // This will also return True if a composition references nil. The entity
  // should use this result and decide if an instance should be created
  if not Assigned(FAttrAddrRef) then
  begin
    // Normal execution
    Result := Assigned(AAttrAddr) and Assigned(TObject(AAttrAddr^));
    if not Result then
    begin
      AcquireADMByAttrAddr(AAttrAddr).Load;
      Result := Assigned(TObject(AAttrAddr^));
    end;
  end else
  begin
    // PID initialization, saving attribute address
    FAttrAddrRef^ := AAttrAddr;
    Result := True;
  end;
end;

{ TJCoreOPFAttrMetadata }

function TJCoreOPFAttrMetadata.GetCompositionMetadata: TJCoreOPFClassMetadata;
begin
  Result := inherited CompositionMetadata as TJCoreOPFClassMetadata;
end;

function TJCoreOPFAttrMetadata.GetHasExternalLink: Boolean;
begin
  Result := CompositionType = jctAggregation;
end;

function TJCoreOPFAttrMetadata.GetModel: TJCoreOPFModel;
begin
  Result := inherited Model as TJCoreOPFModel;
end;

function TJCoreOPFAttrMetadata.ReadComposition(const ATypeInfo: PTypeInfo): TClass;
var
  VClass: TClass;
begin
  VClass := GetTypeData(ATypeInfo)^.ClassType;
  if Model.IsEntityClass(VClass) then
    Result := VClass
  else
    Result := Model.FindSpecializedClass(VClass);
  if not Assigned(Result) then
    raise EJCoreOPFUnsupportedAttributeType.Create(ATypeInfo^.Name);
end;

constructor TJCoreOPFAttrMetadata.Create(const AModel: TJCoreModel; const AOwner: TJCoreClassMetadata;
  const APropInfo: PPropInfo);
var
  VOwner: TJCoreOPFClassMetadata;
begin
  inherited Create(AModel, AOwner, APropInfo);
  FADMClass := Model.AcquireADMClass(PropInfo^.PropType);
  FAttributeType := ADMClass.AttributeType;
  FPersistentFieldName := UpperCase(Name);
  if IsClass then
  begin
    CompositionClass := ReadComposition(APropInfo^.PropType);
    VOwner := Owner as TJCoreOPFClassMetadata;
    FExternalLinkTableName := VOwner.TableName + '_' + PersistentFieldName;
    FExternalLinkLeftFieldName := VOwner.TableName;
    if VOwner <> CompositionMetadata then
      FExternalLinkRightFieldName := CompositionMetadata.TableName
    else
      FExternalLinkRightFieldName := ExternalLinkTableName;
  end else
    CompositionClass := nil;
  FHasLazyload := True; // which also means "I dont know yet, try it"
end;

function TJCoreOPFAttrMetadata.CreateADM(const APID: TJCoreOPFPID): TJCoreOPFADM;
begin
  Result := ADMClass.Create(APID, Self);
end;

function TJCoreOPFAttrMetadata.IsCollection: Boolean;
begin
  Result := AttributeType = jatCollection;
end;

procedure TJCoreOPFAttrMetadata.NoLazyload;
begin
  FHasLazyload := False;
end;

{ TJCoreOPFMap }

constructor TJCoreOPFMap.Create(const AMetadata: TJCoreOPFClassMetadata);
var
  VOwner: TJCoreOPFClassMetadata;
  VTableName: string;
  VOIDName: TJCoreStringArray;
  I: Integer;
begin
  { TODO : Proper metadata -> map conversion }
  inherited Create(False);
  FMetadata := AMetadata;
  FOIDClass := Metadata.OIDClass;
  FOIDName := Metadata.OIDName;
  FTableName := Metadata.TableName;
  VOwner := Metadata.OwnerMetadata as TJCoreOPFClassMetadata;
  if Assigned(VOwner) then
  begin
    VTableName := VOwner.TableName;
    VOIDName := VOwner.OIDName;
    SetLength(FOwnerOIDName, Length(VOIDName));
    if Length(VOIDName) = 1 then
      FOwnerOIDName[0] := VTableName
    else
      for I := 0 to Pred(Length(VOIDName)) do
        FOwnerOIDName[I] := VTableName + '_' + VOIDName[I];
  end else
    SetLength(FOwnerOIDName, 0);
  FGeneratorName := Metadata.GeneratorName;
  FSubMaps := Metadata.SubMaps;
  SetLength(FSubClasses, FSubMaps.Count);
  for I := Low(FSubClasses) to High(FSubClasses) do
    FSubClasses[I] := FSubMaps[I].Metadata.TheClass;
end;

{ TJCoreOPFClassMetadata }

function TJCoreOPFClassMetadata.GetAttributes(const AIndex: Integer): TJCoreOPFAttrMetadata;
begin
  Result := inherited Attributes[AIndex] as TJCoreOPFAttrMetadata;
end;

function TJCoreOPFClassMetadata.GetMaps: TJCoreOPFMaps;
begin
  if not Assigned(FMaps) then
    FMaps := Model.CreateMaps(Self);
  Result := FMaps;
end;

function TJCoreOPFClassMetadata.GetModel: TJCoreOPFModel;
begin
  Result := inherited Model as TJCoreOPFModel;
end;

function TJCoreOPFClassMetadata.GetParent: TJCoreOPFClassMetadata;
begin
  Result := inherited Parent as TJCoreOPFClassMetadata;
end;

function TJCoreOPFClassMetadata.GetSubMaps: TJCoreOPFMaps;
begin
  if not Assigned(FSubMaps) then
    FSubMaps := Model.CreateSubMaps(Self);
  Result := FSubMaps;
end;

constructor TJCoreOPFClassMetadata.Create(const AModel: TJCoreModel; const AClass: TClass;
  const AParent: TJCoreClassMetadata);
begin
  inherited Create(AModel, AClass, AParent);
  FTableName := UpperCase(Copy(TheClass.ClassName, 2, MaxInt));
end;

destructor TJCoreOPFClassMetadata.Destroy;
begin
  FreeAndNil(FMaps);
  FreeAndNil(FSubMaps);
  inherited Destroy;
end;

function TJCoreOPFClassMetadata.AttributeByName(
  const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  Result := inherited AttributeByName(AAttributeName) as TJCoreOPFAttrMetadata;
end;

{ TJCoreOPFModel }

procedure TJCoreOPFModel.AcquireMaps(const AMetadata: TJCoreOPFClassMetadata; const AMaps: TJCoreOPFMaps);
begin
  if Assigned(AMetadata) then
  begin
    AcquireMaps(AMetadata.Parent, AMaps);
    if AMetadata.AttributeCount > 0 then
      AMaps.Add(AcquireMap(AMetadata));
  end;
end;

procedure TJCoreOPFModel.AcquireSubMaps(const AMetadata: TJCoreOPFClassMetadata; const AMaps: TJCoreOPFMaps);
var
  VClass: TClass;
  VOPFMetadata: TJCoreOPFClassMetadata;
  I: Integer;
begin
  for I := 0 to Pred(ClassMap.Count) do
  begin
    VClass := ClassMap.Data[I];
    if VClass.ClassParent = AMetadata.TheClass then
    begin
      VOPFMetadata := AcquireMetadata(VClass);
      AMaps.Add(AcquireMap(VOPFMetadata));
      AcquireSubMaps(VOPFMetadata, AMaps);
    end;
  end;
end;

procedure TJCoreOPFModel.AddADMClass(const AADMClassArray: array of TJCoreOPFADMClass);
var
  VADMClass: TJCoreOPFADMClass;
begin
  for VADMClass in AADMClassArray do
    ADMClassList.Add(VADMClass);
end;

function TJCoreOPFModel.AcquireMap(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMap;
var
  VName: string;
  VMap: TJCoreOPFMap;
  VIndex: Integer;
  I: Integer;
begin
  VName := AMetadata.TheClass.ClassName;
  VIndex := MapMap.IndexOf(VName);
  if VIndex = -1 then
  begin
    VMap := TJCoreOPFMap.Create(AMetadata);
    VIndex := MapMap.Add(VName, VMap);
    for I := 0 to Pred(AMetadata.AttributeCount) do
      VMap.Add(AMetadata[I]);
  end;
  Result := MapMap.Data[VIndex];
end;

function TJCoreOPFModel.AttributeMetadataClass: TJCoreAttrMetadataClass;
begin
  Result := TJCoreOPFAttrMetadata;
end;

function TJCoreOPFModel.ClassMetadataClass: TJCoreClassMetadataClass;
begin
  Result := TJCoreOPFClassMetadata;
end;

function TJCoreOPFModel.CreateAttrMetadata(const AMetadata: TJCoreClassMetadata;
  const APropInfo: PPropInfo): TJCoreAttrMetadata;
var
  VAttrMetadata: TJCoreOPFAttrMetadata;
begin
  VAttrMetadata := inherited CreateAttrMetadata(AMetadata, APropInfo) as TJCoreOPFAttrMetadata;
  VAttrMetadata.Changed;
  Result := VAttrMetadata;
end;

procedure TJCoreOPFModel.Finit;
var
  I: Integer;
begin
  FreeAndNil(FADMClassList);
  for I := 0 to Pred(FMapMap.Count) do
    FMapMap.Data[I].Free;
  FreeAndNil(FMapMap);
  inherited Finit;
end;

procedure TJCoreOPFModel.InitRegistry;
begin
  inherited InitRegistry;
  AddADMClass([TJCoreOPFADMType32, TJCoreOPFADMType64, TJCoreOPFADMFloat,
   TJCoreOPFADMAnsiString, TJCoreOPFADMEntity, TJCoreOPFADMFPSListCollection]);
end;

function TJCoreOPFModel.IsReservedAttr(const AAttrName: ShortString): Boolean;
begin
  Result := SameText(SPID, AAttrName) or inherited IsReservedAttr(AAttrName);
end;

procedure TJCoreOPFModel.RefineClassMetadata(const AClassMetadata: TJCoreClassMetadata);
var
  VMetadata: TJCoreOPFClassMetadata;
  VOIDName: TJCoreStringArray;
begin
  inherited RefineClassMetadata(AClassMetadata);
  VMetadata := AClassMetadata as TJCoreOPFClassMetadata;
  SetLength(VOIDName, 1);
  VOIDName[0] := 'ID';
  VMetadata.FOIDName := VOIDName; // friend class
  VMetadata.FOIDClass := OIDClass; // friend class
  VMetadata.GeneratorName := GeneratorName; // friend class
end;

constructor TJCoreOPFModel.Create;
begin
  FADMClassList := TJCoreOPFADMClassList.Create;
  inherited Create;
  FMapMap := TJCoreOPFMapMap.Create;
  FOIDClass := TJCoreOPFOIDString;
end;

function TJCoreOPFModel.AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
begin
  for Result in ADMClassList do
    if Result.Apply(Self, AAttrTypeInfo) then
      Exit;
  raise EJCoreOPFUnsupportedAttributeType.Create(AAttrTypeInfo);
end;

function TJCoreOPFModel.AcquireAttrMetadata(const AClass: TClass;
  const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  Result := AcquireMetadata(AClass).AttributeByName(AAttributeName);
end;

function TJCoreOPFModel.AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
begin
  Result := inherited AcquireMetadata(AClass) as TJCoreOPFClassMetadata;
end;

function TJCoreOPFModel.CreateMaps(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMaps;
begin
  Result := TJCoreOPFMaps.Create(False);
  try
    AcquireMaps(AMetadata, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJCoreOPFModel.CreateSubMaps(const AMetadata: TJCoreOPFClassMetadata): TJCoreOPFMaps;
begin
  Result := TJCoreOPFMaps.Create(False);
  try
    AcquireSubMaps(AMetadata, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

end.

