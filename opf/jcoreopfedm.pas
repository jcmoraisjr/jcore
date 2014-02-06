(*
  JCore, OPF Entity Driver Mediator Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFEDM;

{$I jcore.inc}

interface

uses
  typinfo,
  fgl;

type

  { TJCoreOPFEDM }

  TJCoreOPFEDM = class(TObject)
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
    constructor Create(const AEntity: TObject; const AAttrPropInfo: PPropInfo); virtual;
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; virtual; abstract;
    function IsDirty: Boolean;
    procedure UpdateCache;
  end;

  TJCoreOPFEDMClass = class of TJCoreOPFEDM;

  TJCoreOPFEDMClassArray = array of TJCoreOPFEDMClass;

  TJCoreOPFEDMMap = specialize TFPGMap<string, TJCoreOPFEDM>;

  { TJCoreOPFEDMType32 }

  TJCoreOPFEDMType32 = class(TJCoreOPFEDM)
  private
    FCache: Longint;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFEDMType64 }

  TJCoreOPFEDMType64 = class(TJCoreOPFEDM)
  private
    FCache: Int64;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFEDMFloat }

  TJCoreOPFEDMFloat = class(TJCoreOPFEDM)
  private
    FCache: Extended;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFEDMAnsiString }

  TJCoreOPFEDMAnsiString = class(TJCoreOPFEDM)
  private
    FCache: AnsiString;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AAttrTypeInfo: PTypeInfo): Boolean; override;
  end;

  { TJCoreOPFEDMCollection }

  TJCoreOPFEDMCollection = class(TJCoreOPFEDM)
  private
    FListSizeCache: Integer;
  protected
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const ATypeInfo: PTypeInfo): Boolean; override;
  end;

implementation

{ TJCoreOPFEDM }

constructor TJCoreOPFEDM.Create(const AEntity: TObject;
  const AAttrPropInfo: PPropInfo);
begin
  inherited Create;
  FEntity := AEntity;
  FAttrPropInfo := AAttrPropInfo;
  FCacheUpdated := False;
end;

function TJCoreOPFEDM.IsDirty: Boolean;
begin
  Result := not FCacheUpdated or InternalIsDirty;
end;

procedure TJCoreOPFEDM.UpdateCache;
begin
  InternalUpdateCache;
  FCacheUpdated := True;
end;

{ TJCoreOPFEDMType32 }

function TJCoreOPFEDMType32.InternalIsDirty: Boolean;
begin
  Result := GetOrdProp(Entity, AttrPropInfo) <> FCache;
end;

procedure TJCoreOPFEDMType32.InternalUpdateCache;
begin
  FCache := GetOrdProp(Entity, AttrPropInfo);
end;

class function TJCoreOPFEDMType32.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind in [tkInteger, tkChar, tkEnumeration, tkBool];
end;

{ TJCoreOPFEDMType64 }

function TJCoreOPFEDMType64.InternalIsDirty: Boolean;
begin
  Result := GetInt64Prop(Entity, AttrPropInfo) <> FCache;
end;

procedure TJCoreOPFEDMType64.InternalUpdateCache;
begin
  FCache := GetInt64Prop(Entity, AttrPropInfo);
end;

class function TJCoreOPFEDMType64.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind in [tkInt64, tkQWord];
end;

{ TJCoreOPFEDMFloat }

function TJCoreOPFEDMFloat.InternalIsDirty: Boolean;
begin
  Result := GetFloatProp(Entity, AttrPropInfo) <> FCache;
end;

procedure TJCoreOPFEDMFloat.InternalUpdateCache;
begin
  FCache := GetFloatProp(Entity, AttrPropInfo);
end;

class function TJCoreOPFEDMFloat.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkFloat;
end;

{ TJCoreOPFEDMAnsiString }

function TJCoreOPFEDMAnsiString.InternalIsDirty: Boolean;
begin
  { TODO : use hash for long strings }
  Result := GetStrProp(Entity, AttrPropInfo) <> FCache;
end;

procedure TJCoreOPFEDMAnsiString.InternalUpdateCache;
begin
  { TODO : use hash for long strings }
  FCache := GetStrProp(Entity, AttrPropInfo);
end;

class function TJCoreOPFEDMAnsiString.Apply(const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkAString;
end;

{ TJCoreOPFEDMCollection }

function TJCoreOPFEDMCollection.InternalIsDirty: Boolean;
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

procedure TJCoreOPFEDMCollection.InternalUpdateCache;
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

class function TJCoreOPFEDMCollection.Apply(const ATypeInfo: PTypeInfo): Boolean;
var
  VTypeData: PTypeData;
begin
  if ATypeInfo^.Kind = tkClass then
  begin
    VTypeData := GetTypeData(ATypeInfo);
    Result := Assigned(VTypeData) and VTypeData^.ClassType.InheritsFrom(TFPSList);
  end else
    Result := False;
end;

end.

