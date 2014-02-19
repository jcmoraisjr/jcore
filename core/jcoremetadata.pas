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

  { TJCoreAttrMetadata }

  TJCoreAttrMetadata = class(TObject)
  private
    FName: string;
    FPropInfo: PPropInfo;
  protected
    property PropInfo: PPropInfo read FPropInfo;
  public
    constructor Create(const APropInfo: PPropInfo);
    property Name: string read FName;
  end;

  TJCoreAttrMetadataClass = class of TJCoreAttrMetadata;

  { TJCoreAttrMetadataList }

  TJCoreAttrMetadataList = class(specialize TFPGObjectList<TJCoreAttrMetadata>)
  private
    FAttrMetadataClass: TJCoreAttrMetadataClass;
  public
    constructor Create(const AAttrMetadataClass: TJCoreAttrMetadataClass);
    function Add(const APropInfo: PPropInfo): TJCoreAttrMetadata; overload;
  end;

  { TJCoreClassMetadata }

  TJCoreClassMetadata = class(TObject)
  private
    FAttrList: TJCoreAttrMetadataList;
    FClass: TClass;
    function GetAttributes(const AIndex: Integer): TJCoreAttrMetadata;
  protected
    function InternalAttributeMetadataClass: TJCoreAttrMetadataClass; virtual;
    property AttrList: TJCoreAttrMetadataList read FAttrList;
    property TheClass: TClass read FClass;
  public
    constructor Create(const AClass: TClass);
    destructor Destroy; override;
    function AttributeByName(const AAttributeName: string): TJCoreAttrMetadata;
    function AttributeCount: Integer;
    procedure AutoBuild;
    property Attributes[const AIndex: Integer]: TJCoreAttrMetadata read GetAttributes; default;
  end;

  TJCoreClassMetadataClass = class of TJCoreClassMetadata;

  TJCoreClassMetadataMap = specialize TFPGMap<Pointer, TJCoreClassMetadata>;

  { TJCoreModel }

  TJCoreModel = class(TJCoreManagedObject)
  private
    class var FInstance: TJCoreModel;
  private
    FMetadataMap: TJCoreClassMetadataMap;
  protected
    function CreateMetadata(const AClass: TClass): TJCoreClassMetadata; virtual;
    function InternalMetadataClass: TJCoreClassMetadataClass; virtual;
    procedure Finit; override;
  public
    constructor Create;
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

{ TJCoreAttrMetadataList }

constructor TJCoreAttrMetadataList.Create(
  const AAttrMetadataClass: TJCoreAttrMetadataClass);
begin
  inherited Create(True);
  FAttrMetadataClass := AAttrMetadataClass;
end;

function TJCoreAttrMetadataList.Add(const APropInfo: PPropInfo): TJCoreAttrMetadata;
begin
  Result := FAttrMetadataClass.Create(APropInfo);
  try
    inherited Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TJCoreClassMetadata }

function TJCoreClassMetadata.GetAttributes(
  const AIndex: Integer): TJCoreAttrMetadata;
begin
  Result := AttrList[AIndex];
end;

function TJCoreClassMetadata.InternalAttributeMetadataClass: TJCoreAttrMetadataClass;
begin
  Result := TJCoreAttrMetadata;
end;

constructor TJCoreClassMetadata.Create(const AClass: TClass);
begin
  inherited Create;
  FClass := AClass;
  FAttrList := TJCoreAttrMetadataList.Create(InternalAttributeMetadataClass);
end;

destructor TJCoreClassMetadata.Destroy;
begin
  FreeAndNil(FAttrList);
  inherited Destroy;
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

procedure TJCoreClassMetadata.AutoBuild;
var
  VPropList: PPropList;
  VPropListCount: Integer;
  VParentPropCount: Integer;
  I: Integer;
begin
  AttrList.Clear;
  if TheClass = TObject then
    Exit;
  VPropListCount := GetPropList(TheClass, VPropList);
  if Assigned(VPropList) then
    try
      VParentPropCount :=
       GetTypeData(PTypeInfo(TheClass.ClassParent.ClassInfo))^.PropCount;
      for I := VParentPropCount to Pred(VPropListCount) do
        AttrList.Add(VPropList^[I]);
    finally
      Freemem(VPropList);
    end;
end;

{ TJCoreModel }

function TJCoreModel.CreateMetadata(const AClass: TClass): TJCoreClassMetadata;
begin
  Result := InternalMetadataClass.Create(AClass);
  try
    Result.AutoBuild;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJCoreModel.InternalMetadataClass: TJCoreClassMetadataClass;
begin
  Result := TJCoreClassMetadata;
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

constructor TJCoreModel.Create;
begin
  inherited Create;
  FMetadataMap := TJCoreClassMetadataMap.Create;
end;

function TJCoreModel.AcquireMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VIndex: Integer;
begin
  { TODO : Thread safe }
  VIndex := FMetadataMap.IndexOf(AClass);
  if VIndex = -1 then
    VIndex := FMetadataMap.Add(AClass, CreateMetadata(AClass));
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

