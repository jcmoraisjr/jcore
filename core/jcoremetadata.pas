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
  JCoreClasses;

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
    FPropInfo: PPropInfo;
    function GetIsClass: Boolean;
    procedure SetCompositionMetadata(AValue: TJCoreClassMetadata);
  protected
    property Model: TJCoreModel read FModel;
  public
    constructor Create(const AModel: TJCoreModel; const APropInfo: PPropInfo); virtual;
    property CompositionClass: TClass read FCompositionClass;
    property CompositionMetadata: TJCoreClassMetadata read FCompositionMetadata write SetCompositionMetadata;
    property CompositionType: TJCoreMetadataCompositionType read FCompositionType write FCompositionType;
    property IsClass: Boolean read GetIsClass;
    property Name: string read FName;
    property PropInfo: PPropInfo read FPropInfo;
  end;

  TJCoreAttrMetadataClass = class of TJCoreAttrMetadata;
  TJCoreAttrMetadataList = specialize TFPGObjectList<TJCoreAttrMetadata>;

  { TJCoreClassMetadata }

  TJCoreClassMetadata = class(TObject)
  private
    FAttrList: TJCoreAttrMetadataList;
    FClass: TClass;
    FParent: TJCoreClassMetadata;
    function GetAttributes(const AIndex: Integer): TJCoreAttrMetadata;
  protected
    property AttrList: TJCoreAttrMetadataList read FAttrList;
  public
    constructor Create(const AClass: TClass; const AParent: TJCoreClassMetadata); virtual;
    destructor Destroy; override;
    procedure AddAttribute(const AAttribute: TJCoreAttrMetadata);
    function AttributeByName(const AAttributeName: string): TJCoreAttrMetadata;
    function AttributeCount: Integer;
    property Attributes[const AIndex: Integer]: TJCoreAttrMetadata read GetAttributes; default;
    property Parent: TJCoreClassMetadata read FParent;
    property TheClass: TClass read FClass;
  end;

  TJCoreClassMetadataMap = specialize TFPGMap<Pointer, TJCoreClassMetadata>;

  { TJCoreModel }

  TJCoreModel = class(TJCoreManagedObject)
  private
    FClassMap: TJCoreClassMap;
    FMetadataMap: TJCoreClassMetadataMap;
  protected
    procedure AddClass(const AClass: TClass);
    function AttributeMetadataClass: TJCoreAttrMetadataClass; virtual;
    function BuildMetadata(const AClass: TClass): TJCoreClassMetadata; virtual;
    function ClassMetadataClass: TJCoreClassMetadataClass; virtual;
    function CreateAttribute(const APropInfo: PPropInfo): TJCoreAttrMetadata;
    function CreateMetadata(const AClass: TClass): TJCoreClassMetadata; virtual;
    procedure Finit; override;
    procedure InitRegistry; virtual;
    function IsReservedAttr(const AAttrName: ShortString): Boolean; virtual;
    property ClassMap: TJCoreClassMap read FClassMap;
  public
    constructor Create; virtual;
    function AcquireMetadata(const AClass: TClass): TJCoreClassMetadata;
    function FindClass(const AClassName: string): TClass;
    function IsEntityClass(const AClass: TClass): Boolean;
  end;

implementation

uses
  sysutils;

{ TJCoreAttrMetadata }

function TJCoreAttrMetadata.GetIsClass: Boolean;
begin
  Result := PropInfo^.PropType^.Kind = tkClass;
end;

procedure TJCoreAttrMetadata.SetCompositionMetadata(AValue: TJCoreClassMetadata);
begin
  if FCompositionMetadata <> AValue then
  begin
    FCompositionMetadata := AValue;
    if Assigned(FCompositionMetadata) then
      FCompositionClass := FCompositionMetadata.TheClass
    else
      FCompositionClass := nil;
  end;
end;

constructor TJCoreAttrMetadata.Create(const AModel: TJCoreModel;
  const APropInfo: PPropInfo);
begin
  inherited Create;
  FModel := AModel;
  FPropInfo := APropInfo;
  FName := APropInfo^.Name;
  if IsClass then
    FCompositionType := jctComposition
  else
    FCompositionType := jctNone;
end;

{ TJCoreClassMetadata }

function TJCoreClassMetadata.GetAttributes(
  const AIndex: Integer): TJCoreAttrMetadata;
begin
  Result := AttrList[AIndex];
end;

constructor TJCoreClassMetadata.Create(const AClass: TClass;
  const AParent: TJCoreClassMetadata);
begin
  inherited Create;
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

procedure TJCoreModel.AddClass(const AClass: TClass);
begin
  ClassMap.Add(AClass.ClassName, AClass);
end;

function TJCoreModel.AttributeMetadataClass: TJCoreAttrMetadataClass;
begin
  Result := TJCoreAttrMetadata;
end;

function TJCoreModel.BuildMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VPropList: PPropList;
  VPropListCount: Integer;
  VParentPropCount: Integer;
  I: Integer;
begin
  Result := CreateMetadata(AClass);
  if AClass = TObject then
    Exit;
  try
    VPropListCount := GetPropList(AClass, VPropList);
    if Assigned(VPropList) then
    begin
      try
        VParentPropCount :=
         GetTypeData(PTypeInfo(AClass.ClassParent.ClassInfo))^.PropCount;
        for I := VParentPropCount to Pred(VPropListCount) do
          if not IsReservedAttr(VPropList^[I]^.Name) then
            Result.AddAttribute(CreateAttribute(VPropList^[I]));
      finally
        Freemem(VPropList);
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJCoreModel.ClassMetadataClass: TJCoreClassMetadataClass;
begin
  Result := TJCoreClassMetadata;
end;

function TJCoreModel.CreateAttribute(
  const APropInfo: PPropInfo): TJCoreAttrMetadata;
begin
  Result := AttributeMetadataClass.Create(Self, APropInfo);
end;

function TJCoreModel.CreateMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VParent: TClass;
  VParentMetadata: TJCoreClassMetadata;
begin
  VParent := AClass.ClassParent;
  if VParent <> TObject then
    VParentMetadata := AcquireMetadata(VParent)
  else
    VParentMetadata := nil;
  Result := ClassMetadataClass.Create(AClass, VParentMetadata);
end;

procedure TJCoreModel.Finit;
var
  I: Integer;
begin
  FreeAndNil(FClassMap);
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

constructor TJCoreModel.Create;
begin
  inherited Create;
  FClassMap := TJCoreClassMap.Create;
  FMetadataMap := TJCoreClassMetadataMap.Create;
  InitRegistry;
end;

function TJCoreModel.AcquireMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VIndex: Integer;
begin
  { TODO : Thread safe }
  VIndex := FMetadataMap.IndexOf(AClass);
  if VIndex = -1 then
    VIndex := FMetadataMap.Add(AClass, BuildMetadata(AClass));
  Result := FMetadataMap.Data[VIndex];
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

function TJCoreModel.IsEntityClass(const AClass: TClass): Boolean;
begin
  Result := ClassMap.IndexOfData(AClass) >= 0;
end;

end.

