(*
  JCore, OPF Attribute-Driver Mediator Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFADM;

{$I jcore.inc}

interface

uses
  typinfo,
  fgl;

type

  TJCoreOPFADM = class;
  TJCoreOPFADMClass = class of TJCoreOPFADM;
  TJCoreOPFADMClassList = specialize TFPGList<TJCoreOPFADMClass>;
  TJCoreOPFADMMap = specialize TFPGMap<string, TJCoreOPFADM>;

  { TJCoreOPFADM }

  TJCoreOPFADM = class(TObject)
  private
    { TODO : abstract registry/acquire implementation }
    class var FADMList: TJCoreOPFADMClassList;
  private
    FAttrPropInfo: PPropInfo;
    FCacheUpdated: Boolean;
    FEntity: TObject;
  protected
    function InternalIsDirty: Boolean; virtual; abstract;
    procedure InternalUpdateCache; virtual; abstract;
    property AttrPropInfo: PPropInfo read FAttrPropInfo;
    property Entity: TObject read FEntity;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(const AEntity: TObject; const AAttrPropInfo: PPropInfo); virtual;
    class function AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; virtual; abstract;
    function IsDirty: Boolean;
    class procedure RegisterADM;
    procedure UpdateCache;
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
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

implementation

uses
  sysutils,
  JCoreOPFException;

{ TJCoreOPFADM }

class constructor TJCoreOPFADM.Create;
begin
  FADMList := TJCoreOPFADMClassList.Create;
end;

class destructor TJCoreOPFADM.Destroy;
begin
  FreeAndNil(FADMList);
end;

constructor TJCoreOPFADM.Create(const AEntity: TObject;
  const AAttrPropInfo: PPropInfo);
begin
  inherited Create;
  FEntity := AEntity;
  FAttrPropInfo := AAttrPropInfo;
  FCacheUpdated := False;
end;

class function TJCoreOPFADM.AcquireADMClass(
  const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
begin
  for Result in FADMList do
    if Result.Apply(AAttrTypeInfo) then
      Exit;
  raise EJCoreOPFUnsupportedAttributeType.Create(AAttrTypeInfo);
end;

function TJCoreOPFADM.IsDirty: Boolean;
begin
  Result := not FCacheUpdated or InternalIsDirty;
end;

class procedure TJCoreOPFADM.RegisterADM;
begin
  FADMList.Add(Self);
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
  VItems: TFPSList;
  VItemCount: Integer;
begin
  { TODO : evaluate after lazy loading implementation }
  VItems := TFPSList(GetObjectProp(Entity, AttrPropInfo, TFPSList));
  Result := True;
  if Assigned(VItems) then
    VItemCount := VItems.Count
  else
    VItemCount := 0;
  if VItemCount <> FListSizeCache then
    Exit;
  { TODO : implement same qty added/removed check }
  { TODO : implement item dirty check }
  Result := False;
end;

procedure TJCoreOPFADMCollection.InternalUpdateCache;
var
  VItems: TFPSList;
begin
  { TODO : evaluate after lazy loading implementation }
  VItems := TFPSList(GetObjectProp(Entity, AttrPropInfo, TFPSList));
  if Assigned(VItems) then
    FListSizeCache := VItems.Count
  else
    FListSizeCache := 0;
end;

class function TJCoreOPFADMCollection.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
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

initialization
  TJCoreOPFADMType32.RegisterADM;
  TJCoreOPFADMType64.RegisterADM;
  TJCoreOPFADMFloat.RegisterADM;
  TJCoreOPFADMAnsiString.RegisterADM;
  TJCoreOPFADMCollection.RegisterADM;

end.

