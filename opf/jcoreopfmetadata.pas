(*
  JCore, OPF Metadata and Attribute-Driver Mediator Classes
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
  JCoreMetadata;

type

  TJCoreOPFADM = class;
  TJCoreOPFADMClass = class of TJCoreOPFADM;
  TJCoreOPFADMClassList = specialize TFPGList<TJCoreOPFADMClass>;
  TJCoreOPFADMMap = specialize TFPGMap<string, TJCoreOPFADM>;

  TJCoreOPFAttrMetadata = class;

  { TJCoreOPFADM }

  TJCoreOPFADM = class(TObject)
  private
    FAttrPropInfo: PPropInfo;
    FCacheUpdated: Boolean;
    FEntity: TObject;
    FMetadata: TJCoreOPFAttrMetadata;
  protected
    function InternalIsDirty: Boolean; virtual; abstract;
    procedure InternalUpdateCache; virtual; abstract;
    property AttrPropInfo: PPropInfo read FAttrPropInfo;
    property Entity: TObject read FEntity;
  public
    constructor Create(const AEntity: TObject; const AMetadata: TJCoreOPFAttrMetadata); virtual;
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; virtual; abstract;
    function IsDirty: Boolean;
    procedure UpdateCache;
    property Metadata: TJCoreOPFAttrMetadata read FMetadata;
  end;

  { TJCoreOPFADMType32 }

  TJCoreOPFADMType32 = class(TJCoreOPFADM)
  private
    FCache: Longint;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMType64 }

  TJCoreOPFADMType64 = class(TJCoreOPFADM)
  private
    FCache: Int64;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMFloat }

  TJCoreOPFADMFloat = class(TJCoreOPFADM)
  private
    FCache: Extended;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMAnsiString }

  TJCoreOPFADMAnsiString = class(TJCoreOPFADM)
  private
    FCache: AnsiString;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFADMCollection }

  TJCoreOPFADMCollection = class(TJCoreOPFADM)
  private
    FListSizeCache: Integer;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    function CreateArray: TJCoreObjectArray; virtual; abstract;
  end;

  { TJCoreOPFADMFPSListCollection }

  TJCoreOPFADMFPSListCollection = class(TJCoreOPFADMCollection)
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
    function CreateArray: TJCoreObjectArray; override;
  end;

  TJCoreOPFModel = class;
  TJCoreOPFMetadataCompositionLinkType = (jcltEmbedded, jcltExternal);

  { TJCoreOPFAttrMetadata }

  TJCoreOPFAttrMetadata = class(TJCoreAttrMetadata)
  private
    FADMClass: TJCoreOPFADMClass;
    FCompositionLinkType: TJCoreOPFMetadataCompositionLinkType;
    FModel: TJCoreOPFModel;
  protected
    property Model: TJCoreOPFModel read FModel;
  public
    constructor Create(const AModel: TJCoreOPFModel; const APropInfo: PPropInfo);
    function CreateADM(const AEntity: TObject): TJCoreOPFADM;
    property CompositionLinkType: TJCoreOPFMetadataCompositionLinkType read FCompositionLinkType write FCompositionLinkType;
  end;

  { TJCoreOPFClassMetadata }

  TJCoreOPFClassMetadata = class(TJCoreClassMetadata)
  public
    { TODO : Generics? }
    function AttributeByName(const AAttributeName: string): TJCoreOPFAttrMetadata;
  end;

  { TJCoreOPFModel }

  TJCoreOPFModel = class(TJCoreModel)
  private
    FADMList: TJCoreOPFADMClassList;
  protected
    function CreateAttribute(const APropInfo: PPropInfo): TJCoreAttrMetadata; override;
    function CreateMetadata(const AClass: TClass): TJCoreClassMetadata; override;
    procedure Finit; override;
    procedure InitRegistry; override;
    function IsReservedAttr(const AAttrName: ShortString): Boolean; override;
    property ADMList: TJCoreOPFADMClassList read FADMList;
  public
    constructor Create; override;
    function AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
    function AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
    class function AcquireModel: TJCoreOPFModel;
  end;

implementation

uses
  sysutils,
  JCoreOPFConsts,
  JCoreOPFException;

{ TJCoreOPFADM }

constructor TJCoreOPFADM.Create(const AEntity: TObject;
  const AMetadata: TJCoreOPFAttrMetadata);
begin
  inherited Create;
  FEntity := AEntity;
  FAttrPropInfo := AMetadata.PropInfo;
  FCacheUpdated := False;
  FMetadata := AMetadata;
end;

function TJCoreOPFADM.IsDirty: Boolean;
begin
  Result := not FCacheUpdated or InternalIsDirty;
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

class function TJCoreOPFADMType32.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
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

class function TJCoreOPFADMType64.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
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

class function TJCoreOPFADMFloat.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
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

class function TJCoreOPFADMAnsiString.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkAString;
end;

{ TJCoreOPFADMCollection }

function TJCoreOPFADMCollection.InternalIsDirty: Boolean;
var
  VItems: TJCoreObjectArray;
begin
  { TODO : evaluate after lazy loading implementation }
  VItems := CreateArray;
  { TODO : implement same qty added/removed check }
  { TODO : implement item dirty check }
  Result := Length(VItems) <> FListSizeCache;
end;

procedure TJCoreOPFADMCollection.InternalUpdateCache;
var
  VItems: TJCoreObjectArray;
begin
  { TODO : evaluate after lazy loading implementation }
  VItems := CreateArray;
  FListSizeCache := Length(VItems);
end;

{ TJCoreOPFADMFPSListCollection }

class function TJCoreOPFADMFPSListCollection.Apply(
  const AAttrTypeInfo: PTypeInfo): Boolean;
var
  VTypeData: PTypeData;
begin
  if AAttrTypeInfo^.Kind = tkClass then
  begin
    VTypeData := GetTypeData(AAttrTypeInfo);
    Result := Assigned(VTypeData) and VTypeData^.ClassType.InheritsFrom(TFPSList);
  end else
    Result := False;
end;

function TJCoreOPFADMFPSListCollection.CreateArray: TJCoreObjectArray;
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

{ TJCoreOPFAttrMetadata }

constructor TJCoreOPFAttrMetadata.Create(const AModel: TJCoreOPFModel;
  const APropInfo: PPropInfo);
begin
  inherited Create(APropInfo);
  FModel := AModel;
  FCompositionLinkType := jcltEmbedded;
end;

function TJCoreOPFAttrMetadata.CreateADM(const AEntity: TObject): TJCoreOPFADM;
begin
  if not Assigned(FADMClass) then
    FADMClass := Model.AcquireADMClass(PropInfo^.PropType);
  Result := FADMClass.Create(AEntity, Self);
end;

{ TJCoreOPFClassMetadata }

function TJCoreOPFClassMetadata.AttributeByName(
  const AAttributeName: string): TJCoreOPFAttrMetadata;
begin
  Result := inherited AttributeByName(AAttributeName) as TJCoreOPFAttrMetadata;
end;

{ TJCoreOPFModel }

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
  FreeAndNil(FADMList);
  inherited Finit;
end;

procedure TJCoreOPFModel.InitRegistry;
begin
  inherited InitRegistry;
  ADMList.Add(TJCoreOPFADMType32);
  ADMList.Add(TJCoreOPFADMType64);
  ADMList.Add(TJCoreOPFADMFloat);
  ADMList.Add(TJCoreOPFADMAnsiString);
  ADMList.Add(TJCoreOPFADMFPSListCollection);
end;

function TJCoreOPFModel.IsReservedAttr(const AAttrName: ShortString): Boolean;
begin
  Result := SameText(SPID, AAttrName) or inherited IsReservedAttr(AAttrName);
end;

constructor TJCoreOPFModel.Create;
begin
  FADMList := TJCoreOPFADMClassList.Create;
  inherited Create;
end;

function TJCoreOPFModel.AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
begin
  for Result in ADMList do
    if Result.Apply(AAttrTypeInfo) then
      Exit;
  raise EJCoreOPFUnsupportedAttributeType.Create(AAttrTypeInfo);
end;

function TJCoreOPFModel.AcquireMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
begin
  Result := inherited AcquireMetadata(AClass) as TJCoreOPFClassMetadata;
end;

class function TJCoreOPFModel.AcquireModel: TJCoreOPFModel;
begin
  Result := inherited AcquireModel as TJCoreOPFModel;
end;

end.

