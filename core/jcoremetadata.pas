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

  TJCoreMetadataCompositionType = (jctComposition, jctAggregation);

  { TJCoreAttrMetadata }

  TJCoreAttrMetadata = class(TObject)
  private
    FCompositionClass: TClass;
    FCompositionType: TJCoreMetadataCompositionType;
    FName: string;
    FPropInfo: PPropInfo;
  public
    constructor Create(const APropInfo: PPropInfo);
    property CompositionClass: TClass read FCompositionClass;
    property CompositionType: TJCoreMetadataCompositionType read FCompositionType;
    property Name: string read FName;
    property PropInfo: PPropInfo read FPropInfo;
  end;

  TJCoreAttrMetadataList = specialize TFPGObjectList<TJCoreAttrMetadata>;

  { TJCoreClassMetadata }

  TJCoreClassMetadata = class(TObject)
  private
    FAttrList: TJCoreAttrMetadataList;
    FClass: TClass;
    function GetAttributes(const AIndex: Integer): TJCoreAttrMetadata;
  protected
    property AttrList: TJCoreAttrMetadataList read FAttrList;
    property TheClass: TClass read FClass;
  public
    constructor Create(const AClass: TClass);
    destructor Destroy; override;
    procedure AddAttribute(const AAttribute: TJCoreAttrMetadata);
    function AttributeByName(const AAttributeName: string): TJCoreAttrMetadata;
    function AttributeCount: Integer;
    property Attributes[const AIndex: Integer]: TJCoreAttrMetadata read GetAttributes; default;
  end;

  TJCoreClassMetadataMap = specialize TFPGMap<Pointer, TJCoreClassMetadata>;

  { TJCoreModel }

  TJCoreModel = class(TJCoreManagedObject)
  private
    class var FInstance: TJCoreModel;
  private
    FMetadataMap: TJCoreClassMetadataMap;
  protected
    function BuildMetadata(const AClass: TClass): TJCoreClassMetadata; virtual;
    function CreateAttribute(const APropInfo: PPropInfo): TJCoreAttrMetadata; virtual;
    function CreateMetadata(const AClass: TClass): TJCoreClassMetadata; virtual;
    procedure Finit; override;
    procedure InitRegistry; virtual;
    function IsReservedAttr(const AAttrName: ShortString): Boolean; virtual;
  public
    constructor Create; virtual;
    function AcquireMetadata(const AClass: TClass): TJCoreClassMetadata;
    class function AcquireModel: TJCoreModel;
  end;

implementation

uses
  sysutils;

{ TJCoreAttrMetadata }

constructor TJCoreAttrMetadata.Create(const APropInfo: PPropInfo);
begin
  inherited Create;
  FPropInfo := APropInfo;
  FName := APropInfo^.Name;
end;

{ TJCoreClassMetadata }

function TJCoreClassMetadata.GetAttributes(
  const AIndex: Integer): TJCoreAttrMetadata;
begin
  Result := AttrList[AIndex];
end;

constructor TJCoreClassMetadata.Create(const AClass: TClass);
begin
  inherited Create;
  FClass := AClass;
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

function TJCoreModel.CreateAttribute(
  const APropInfo: PPropInfo): TJCoreAttrMetadata;
begin
  Result := TJCoreAttrMetadata.Create(APropInfo);
end;

function TJCoreModel.CreateMetadata(const AClass: TClass): TJCoreClassMetadata;
begin
  Result := TJCoreClassMetadata.Create(AClass);
end;

procedure TJCoreModel.Finit;
var
  I: Integer;
begin
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

class function TJCoreModel.AcquireModel: TJCoreModel;
begin
  Result := FInstance;
  if not Assigned(Result) then
    Result := Create
  else
    Result.AddRef;
end;

end.

