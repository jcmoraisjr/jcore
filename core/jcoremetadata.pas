(*
  JCore, Metadata Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreMetadata;

{$I jcore.inc}

interface

uses
  typinfo,
  fgl,
  JCoreClasses,
  JCoreEntity;

type

  TJCoreMetadataCompositionType = (jctNone, jctComposition, jctAggregation);

  TJCoreClassMetadata = class;
  TJCoreClassMetadataClass = class of TJCoreClassMetadata;

  TJCoreModel = class;

  { TJCoreAttrMetadata }

  TJCoreAttrMetadata = class(TObject)
  private
    FCompositionClass: TClass;
    FCompositionMetadata: TJCoreClassMetadata;
    FCompositionType: TJCoreMetadataCompositionType;
    FModel: TJCoreModel;
    FName: string;
    FOwner: TJCoreClassMetadata;
    FPropInfo: PPropInfo;
    FSize: Integer;
    function GetCompositionMetadata: TJCoreClassMetadata;
    procedure ReadPropertyInfo;
    procedure SetCompositionClass(AValue: TClass);
    procedure SetCompositionType(AValue: TJCoreMetadataCompositionType);
  protected
    procedure Changed;
    function IsClass: Boolean;
    function IsString: Boolean;
    property Model: TJCoreModel read FModel;
  public
    constructor Create(const AModel: TJCoreModel; const AOwner: TJCoreClassMetadata; const APropInfo: PPropInfo); virtual;
    function IsCollection: Boolean; virtual; abstract;
    property CompositionClass: TClass read FCompositionClass write SetCompositionClass;
    property CompositionMetadata: TJCoreClassMetadata read GetCompositionMetadata;
    property CompositionType: TJCoreMetadataCompositionType read FCompositionType write SetCompositionType;
    property Name: string read FName;
    property Owner: TJCoreClassMetadata read FOwner;
    property PropInfo: PPropInfo read FPropInfo;
    property Size: Integer read FSize write FSize;
  end;

  TJCoreAttrMetadataClass = class of TJCoreAttrMetadata;
  TJCoreAttrMetadataList = specialize TFPGObjectList<TJCoreAttrMetadata>;

  { TJCoreClassMetadata }

  TJCoreClassMetadata = class(TObject)
  private
    FAttrList: TJCoreAttrMetadataList;
    FClass: TClass;
    FModel: TJCoreModel;
    FOwnerMetadata: TJCoreClassMetadata;
    FParent: TJCoreClassMetadata;
    function GetAttributes(const AIndex: Integer): TJCoreAttrMetadata;
    procedure SetOwnerMetadata(AValue: TJCoreClassMetadata);
  protected
    property AttrList: TJCoreAttrMetadataList read FAttrList;
    property Model: TJCoreModel read FModel;
  public
    constructor Create(const AModel: TJCoreModel; const AClass: TClass; const AParent: TJCoreClassMetadata); virtual;
    destructor Destroy; override;
    procedure AddAttribute(const AAttribute: TJCoreAttrMetadata);
    function AttributeByName(const AAttributeName: string): TJCoreAttrMetadata;
    function AttributeCount: Integer;
    property Attributes[const AIndex: Integer]: TJCoreAttrMetadata read GetAttributes; default;
    property OwnerMetadata: TJCoreClassMetadata read FOwnerMetadata write SetOwnerMetadata;
    property Parent: TJCoreClassMetadata read FParent;
    property TheClass: TClass read FClass;
  end;

  TJCoreClassMetadataMap = specialize TFPGMap<Pointer, TJCoreClassMetadata>;
  TJCoreGenericsMap = specialize TFPGMap<Pointer, TClass>;

  { TJCoreModel }

  TJCoreModel = class(TJCoreManagedObject, IJCoreModel)
  private
    FClassMap: TJCoreClassMap;
    FGenericsMap: TJCoreGenericsMap;
    FMetadataMap: TJCoreClassMetadataMap;
  protected
    function AttributeMetadataClass: TJCoreAttrMetadataClass; virtual;
    procedure BuildClassMetadata(const AMetadata: TJCoreClassMetadata);
    function ClassMetadataClass: TJCoreClassMetadataClass; virtual;
    function CreateAttrMetadata(const AMetadata: TJCoreClassMetadata; const APropInfo: PPropInfo): TJCoreAttrMetadata;
    function CreateClassMetadata(const AClass: TClass): TJCoreClassMetadata;
    procedure Finit; override;
    procedure InitRegistry; virtual;
    function IsReservedAttr(const AAttrName: ShortString): Boolean; virtual;
    procedure RefineClassMetadata(const AClassMetadata: TJCoreClassMetadata); virtual;
    property ClassMap: TJCoreClassMap read FClassMap;
    property GenericsMap: TJCoreGenericsMap read FGenericsMap;
    property MetadataMap: TJCoreClassMetadataMap read FMetadataMap;
  public
    constructor Create; virtual;
    function AcquireMetadata(const AClass: TClass): TJCoreClassMetadata;
    procedure AddClass(const AClassArray: array of TClass);
    procedure AddGenerics(const ASpecializedClass, ASpecializationClass: TClass);
    function FindClass(const AClassName: string): TClass;
    function FindSpecializedClass(const ASpecializationClass: TClass): TClass;
    procedure InitEntity(const AEntity: TObject); virtual; abstract;
    function IsEntityClass(const AClass: TClass): Boolean;
  end;

implementation

uses
  sysutils;

{ TJCoreAttrMetadata }

function TJCoreAttrMetadata.GetCompositionMetadata: TJCoreClassMetadata;
begin
  if not Assigned(FCompositionMetadata) and Assigned(CompositionClass) then
    FCompositionMetadata := Model.AcquireMetadata(CompositionClass);
  Result := FCompositionMetadata;
end;

procedure TJCoreAttrMetadata.ReadPropertyInfo;
const
  CCompositionType : array[Boolean] of TJCoreMetadataCompositionType = (jctAggregation, jctComposition);
var
  VTypeData: PTypeData;
  VInstance: TObject;
begin
  if IsString then
  begin
    FSize := PropInfo^.Index;
  end else if IsClass then
  begin
    VTypeData := GetTypeData(PropInfo^.PropType);
    VInstance := VTypeData^.ClassType.NewInstance;
    try
      FCompositionType := CCompositionType[IsStoredProp(VInstance, PropInfo)];
    finally
      VInstance.FreeInstance;
    end;
  end;
end;

procedure TJCoreAttrMetadata.SetCompositionClass(AValue: TClass);
begin
  if FCompositionClass <> AValue then
  begin
    FCompositionClass := AValue;
    FCompositionMetadata := nil;
  end;
end;

procedure TJCoreAttrMetadata.SetCompositionType(AValue: TJCoreMetadataCompositionType);
begin
  if FCompositionType <> AValue then
  begin
    FCompositionType := AValue;
    Changed;
  end;
end;

procedure TJCoreAttrMetadata.Changed;
var
  VCompositionMetadata: TJCoreClassMetadata;
begin
  VCompositionMetadata := CompositionMetadata;
  if Assigned(VCompositionMetadata) and IsCollection then
  begin
    if CompositionType = jctComposition then
      VCompositionMetadata.OwnerMetadata := Owner
    else // jctAggregation
      VCompositionMetadata.OwnerMetadata := nil;
  end;
end;

function TJCoreAttrMetadata.IsClass: Boolean;
begin
  Result := PropInfo^.PropType^.Kind = tkClass;
end;

function TJCoreAttrMetadata.IsString: Boolean;
begin
  Result := PropInfo^.PropType^.Kind in [tkSString, tkAString, tkLString, tkWString, tkUString];
end;

constructor TJCoreAttrMetadata.Create(const AModel: TJCoreModel; const AOwner: TJCoreClassMetadata;
  const APropInfo: PPropInfo);
begin
  inherited Create;
  FModel := AModel;
  FOwner := AOwner;
  FPropInfo := APropInfo;
  FName := APropInfo^.Name;
  FCompositionType := jctNone;
  ReadPropertyInfo;
end;

{ TJCoreClassMetadata }

function TJCoreClassMetadata.GetAttributes(
  const AIndex: Integer): TJCoreAttrMetadata;
begin
  Result := AttrList[AIndex];
end;

procedure TJCoreClassMetadata.SetOwnerMetadata(AValue: TJCoreClassMetadata);
begin
  if not Assigned(FOwnerMetadata) or not Assigned(AValue) then
    FOwnerMetadata := AValue
  else if FOwnerMetadata <> AValue then
    raise EJCoreMetadataAlreadyOwned.Create;
end;

constructor TJCoreClassMetadata.Create(const AModel: TJCoreModel; const AClass: TClass;
  const AParent: TJCoreClassMetadata);
begin
  inherited Create;
  FModel := AModel;
  FClass := AClass;
  FParent := AParent;
  FAttrList := TJCoreAttrMetadataList.Create(True);
end;

destructor TJCoreClassMetadata.Destroy;
begin
  FreeAndNil(FAttrList);
  inherited Destroy;
end;

procedure TJCoreClassMetadata.AddAttribute(const AAttribute: TJCoreAttrMetadata);
begin
  AttrList.Add(AAttribute);
end;

function TJCoreClassMetadata.AttributeByName(
  const AAttributeName: string): TJCoreAttrMetadata;
begin
  for Result in AttrList do
    if SameText(AAttributeName, Result.Name) then
      Exit;
  raise EJCoreAttributeNotFound.Create(TheClass.ClassName, AAttributeName);
end;

function TJCoreClassMetadata.AttributeCount: Integer;
begin
  Result := AttrList.Count;
end;

{ TJCoreModel }

function TJCoreModel.AttributeMetadataClass: TJCoreAttrMetadataClass;
begin
  Result := TJCoreAttrMetadata;
end;

procedure TJCoreModel.BuildClassMetadata(const AMetadata: TJCoreClassMetadata);
var
  VClass: TClass;
  VPropList: PPropList;
  VPropListCount: Integer;
  VParentPropCount: Integer;
  I: Integer;
begin
  VClass := AMetadata.TheClass;
  if VClass = TObject then
    Exit;
  VPropListCount := GetPropList(VClass, VPropList);
  RefineClassMetadata(AMetadata);
  if Assigned(VPropList) then
  begin
    try
      VParentPropCount :=
       GetTypeData(PTypeInfo(VClass.ClassParent.ClassInfo))^.PropCount;
      for I := VParentPropCount to Pred(VPropListCount) do
        if not IsReservedAttr(VPropList^[I]^.Name) then
          AMetadata.AddAttribute(CreateAttrMetadata(AMetadata, VPropList^[I]));
    finally
      Freemem(VPropList);
    end;
  end;
end;

function TJCoreModel.ClassMetadataClass: TJCoreClassMetadataClass;
begin
  Result := TJCoreClassMetadata;
end;

function TJCoreModel.CreateAttrMetadata(const AMetadata: TJCoreClassMetadata;
  const APropInfo: PPropInfo): TJCoreAttrMetadata;
begin
  Result := AttributeMetadataClass.Create(Self, AMetadata, APropInfo);
end;

function TJCoreModel.CreateClassMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VParent: TClass;
  VParentMetadata: TJCoreClassMetadata;
begin
  VParent := AClass.ClassParent;
  if VParent <> TObject then
    VParentMetadata := AcquireMetadata(VParent)
  else
    VParentMetadata := nil;
  Result := ClassMetadataClass.Create(Self, AClass, VParentMetadata);
end;

procedure TJCoreModel.Finit;
var
  I: Integer;
begin
  FreeAndNil(FClassMap);
  FreeAndNil(FGenericsMap);
  { TODO : Fix AV on all map.free if an exception raises freeing an item.
           Need to assign nil or remove the item from the map }
  for I := 0 to Pred(FMetadataMap.Count) do
    FMetadataMap.Data[I].Free;
  FreeAndNil(FMetadataMap);
  inherited Finit;
end;

procedure TJCoreModel.InitRegistry;
begin
end;

function TJCoreModel.IsReservedAttr(const AAttrName: ShortString): Boolean;
begin
  Result := False;
end;

procedure TJCoreModel.RefineClassMetadata(const AClassMetadata: TJCoreClassMetadata);
begin
end;

constructor TJCoreModel.Create;
begin
  inherited Create;
  FClassMap := TJCoreClassMap.Create;
  FGenericsMap := TJCoreGenericsMap.Create;
  FMetadataMap := TJCoreClassMetadataMap.Create;
  InitRegistry;
  if not Assigned(JCoreDefaultModel) then
    JCoreDefaultModel := Self;
end;

function TJCoreModel.AcquireMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VMetadata: TJCoreClassMetadata;
  VIndex: Integer;
begin
  { TODO : Thread safe }
  VIndex := MetadataMap.IndexOf(AClass);
  if VIndex = -1 then
  begin
    VMetadata := CreateClassMetadata(AClass);
    VIndex := MetadataMap.Add(AClass, VMetadata);
    try
      BuildClassMetadata(VMetadata);
    except
      MetadataMap.Delete(VIndex);
      FreeAndNil(VMetadata);
      raise;
    end;
  end;
  Result := MetadataMap.Data[VIndex];
end;

procedure TJCoreModel.AddClass(const AClassArray: array of TClass);
var
  VClass: TClass;
begin
  for VClass in AClassArray do
    ClassMap.Add(VClass.ClassName, VClass);
end;

procedure TJCoreModel.AddGenerics(const ASpecializedClass, ASpecializationClass: TClass);
begin
  GenericsMap.Add(ASpecializedClass, ASpecializationClass);
end;

function TJCoreModel.FindClass(const AClassName: string): TClass;
var
  VIndex: Integer;
begin
  VIndex := ClassMap.IndexOf(AClassName);
  if VIndex >= 0 then
    Result := ClassMap.Data[VIndex]
  else
    Result := nil;
end;

function TJCoreModel.FindSpecializedClass(const ASpecializationClass: TClass): TClass;
var
  VIndex: Integer;
begin
  VIndex := GenericsMap.IndexOf(ASpecializationClass);
  if VIndex >= 0 then
    Result := GenericsMap.Data[VIndex]
  else
    Result := nil;
end;

function TJCoreModel.IsEntityClass(const AClass: TClass): Boolean;
begin
  Result := ClassMap.IndexOfData(AClass) >= 0;
end;

end.

