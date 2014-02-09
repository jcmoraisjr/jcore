(*
  JCore, OPF Metadata Classes
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
  JCoreClasses;

type

  { TJCoreOPFAttrMetadata }

  TJCoreOPFAttrMetadata = class(TObject)
  private
    FName: string;
    FPropInfo: PPropInfo;
  public
    constructor Create(const APropInfo: PPropInfo);
    property Name: string read FName;
  end;

  { TJCoreOPFAttrMetadataList }

  TJCoreOPFAttrMetadataList = class(specialize TFPGObjectList<TJCoreOPFAttrMetadata>)
  public
    function Add(const APropInfo: PPropInfo): TJCoreOPFAttrMetadata; overload;
  end;

  { TJCoreOPFClassMetadata }

  TJCoreOPFClassMetadata = class(TObject)
  private
    FAttrList: TJCoreOPFAttrMetadataList;
    FClass: TClass;
    function GetAttributes(const AIndex: Integer): TJCoreOPFAttrMetadata;
  protected
    property AttrList: TJCoreOPFAttrMetadataList read FAttrList;
    property TheClass: TClass read FClass;
  public
    constructor Create(const AClass: TClass);
    destructor Destroy; override;
    function AttributeCount: Integer;
    procedure AutoBuild;
    property Attributes[const AIndex: Integer]: TJCoreOPFAttrMetadata read GetAttributes; default;
  end;

  TJCoreOPFClassMetadataMap = specialize TFPGMap<Pointer, TJCoreOPFClassMetadata>;

  IJCoreOPFMetadataBuilder = interface
    function CreateMetadata(const AClass: TClass): TJCoreOPFClassMetadata;
  end;

  { TJCoreOPFModel }

  TJCoreOPFModel = class(TJCoreManagedObject)
  private
    class var FInstance: TJCoreOPFModel;
  private
    FMetadataMap: TJCoreOPFClassMetadataMap;
  protected
    procedure Finit; override;
  public
    constructor Create;
    function AcquireMetadata(const AClass: TClass; const AMetadataBuilder: IJCoreOPFMetadataBuilder = nil): TJCoreOPFClassMetadata;
    class function AcquireModel: TJCoreOPFModel;
  end;

implementation

uses
  sysutils,
  JCoreOPFConsts,
  JCoreOPFException;

{ TJCoreOPFAttrMetadata }

constructor TJCoreOPFAttrMetadata.Create(const APropInfo: PPropInfo);
begin
  inherited Create;
  FPropInfo := APropInfo;
  FName := APropInfo^.Name;
end;

{ TJCoreOPFAttrMetadataList }

function TJCoreOPFAttrMetadataList.Add(const APropInfo: PPropInfo): TJCoreOPFAttrMetadata;
begin
  Result := TJCoreOPFAttrMetadata.Create(APropInfo);
  try
    inherited Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TJCoreOPFClassMetadata }

function TJCoreOPFClassMetadata.GetAttributes(
  const AIndex: Integer): TJCoreOPFAttrMetadata;
begin
  Result := AttrList[AIndex];
end;

constructor TJCoreOPFClassMetadata.Create(const AClass: TClass);
begin
  inherited Create;
  FClass := AClass;
  FAttrList := TJCoreOPFAttrMetadataList.Create(True);
end;

destructor TJCoreOPFClassMetadata.Destroy;
begin
  FreeAndNil(FAttrList);
  inherited Destroy;
end;

function TJCoreOPFClassMetadata.AttributeCount: Integer;
begin
  Result := AttrList.Count;
end;

procedure TJCoreOPFClassMetadata.AutoBuild;
var
  VPropList: PPropList;
  VPropListCount: Integer;
  I: Integer;
begin
  AttrList.Clear;
  VPropListCount := GetPropList(TheClass, VPropList);
  if Assigned(VPropList) then
    try
      for I := 0 to Pred(VPropListCount) do
        if not SameText(SPID, VPropList^[I]^.Name) then
          AttrList.Add(VPropList^[I]);
    finally
      Freemem(VPropList);
    end;
end;

{ TJCoreOPFModel }

procedure TJCoreOPFModel.Finit;
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

constructor TJCoreOPFModel.Create;
begin
  inherited Create;
  FMetadataMap := TJCoreOPFClassMetadataMap.Create;
end;

function TJCoreOPFModel.AcquireMetadata(const AClass: TClass;
  const AMetadataBuilder: IJCoreOPFMetadataBuilder = nil): TJCoreOPFClassMetadata;
var
  VIndex: Integer;
begin
  { TODO : Thread safe }
  VIndex := FMetadataMap.IndexOf(AClass);
  if VIndex = -1 then
  begin
    if not Assigned(AMetadataBuilder) then
      raise EJCoreOPFMetadataNotFound.Create(AClass);
    VIndex := FMetadataMap.Add(AClass, AMetadataBuilder.CreateMetadata(AClass));
  end;
  Result := FMetadataMap.Data[VIndex];
end;

class function TJCoreOPFModel.AcquireModel: TJCoreOPFModel;
begin
  Result := FInstance;
  if not Assigned(Result) then
    Result := Create
  else
    Result.AddRef;
end;

end.

