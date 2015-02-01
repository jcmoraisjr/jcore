(*
  JCore, OPF Attribute Data Mediator (ADM) Implementation Classes
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
  fgl,
  JCoreClasses,
  JCoreEntity,
  JCoreMetadata,
  JCoreOPFMetadata,
  JCoreOPFDriver;

type

  { TJCoreOPFADMSimple }

  TJCoreOPFADMSimple = class(TJCoreOPFADMNative)
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
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
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
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
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
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
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

  { TJCoreOPFSimpleType }

  TJCoreOPFSimpleType = class(TJCoreOPFADMValueType, IJCoreSimpleType)
  private
    FIsNull: Boolean;
  protected
    function IGetIsNull: Boolean;
    procedure InternalClear; virtual; abstract;
    procedure SetNotNull;
    function IJCoreSimpleType.IsNull = IGetIsNull;
  public
    class function AttributeType: TJCoreOPFAttributeType; override;
    procedure Clear;
    property IsNull: Boolean read FIsNull;
  end;

  { TJCoreOPFIntegerType }

  TJCoreOPFIntegerType = class(TJCoreOPFSimpleType, IJCoreIntegerType)
  private
    FOldValue: Integer;
    FValue: Integer;
    function IGetValue: Integer;
    function IOldValue: Integer;
    procedure SetValue(const AValue: Integer);
    function IJCoreIntegerType.GetValue = IGetValue;
    function IJCoreIntegerType.IsNull = IGetIsNull;
    function IJCoreIntegerType.OldValue = IOldValue;
  protected
    procedure InternalClear; override;
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    property OldValue: Integer read FOldValue;
    property Value: Integer read FValue write SetValue;
  end;

implementation

uses
  JCoreOPFException;

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

procedure TJCoreOPFADMType32.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Value := AResultSet.ReadInt32;
end;

procedure TJCoreOPFADMType32.WriteToParams(const AParams: IJCoreOPFParams);
begin
  AParams.WriteInt32(Value);
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

procedure TJCoreOPFADMType64.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Value := AResultSet.ReadInt64;
end;

procedure TJCoreOPFADMType64.WriteToParams(const AParams: IJCoreOPFParams);
begin
  AParams.WriteInt64(Value);
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

procedure TJCoreOPFADMAnsiString.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Value := AResultSet.ReadString;
end;

procedure TJCoreOPFADMAnsiString.WriteToParams(const AParams: IJCoreOPFParams);
begin
  AParams.WriteString(Value);
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
  Result := Implements(AAttrTypeInfo, TFPSList);
end;

{ TJCoreOPFSimpleType }

function TJCoreOPFSimpleType.IGetIsNull: Boolean;
begin
  Result := FIsNull;
end;

procedure TJCoreOPFSimpleType.SetNotNull;
begin
  FIsNull := False;
end;

class function TJCoreOPFSimpleType.AttributeType: TJCoreOPFAttributeType;
begin
  Result := jatSimple;
end;

procedure TJCoreOPFSimpleType.Clear;
begin
  InternalClear;
  FIsNull := True;
end;

{ TJCoreOPFIntegerType }

function TJCoreOPFIntegerType.IGetValue: Integer;
begin
  Result := FValue;
end;

function TJCoreOPFIntegerType.IOldValue: Integer;
begin
  Result := FOldValue;
end;

procedure TJCoreOPFIntegerType.SetValue(const AValue: Integer);
begin
  FValue := AValue;
  SetNotNull;
end;

procedure TJCoreOPFIntegerType.InternalClear;
begin
  FValue := 0;
end;

function TJCoreOPFIntegerType.InternalIsDirty: Boolean;
begin
  Result := FValue <> FOldValue;
end;

procedure TJCoreOPFIntegerType.InternalUpdateCache;
begin
  FOldValue := FValue;
end;

class function TJCoreOPFIntegerType.Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := Implements(AAttrTypeInfo, IJCoreIntegerType) or inherited Apply(AModel, AAttrTypeInfo);
end;

procedure TJCoreOPFIntegerType.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  if AResultSet.ReadNull then
    Clear
  else
    Value := AResultSet.ReadInt32;
end;

procedure TJCoreOPFIntegerType.WriteToParams(const AParams: IJCoreOPFParams);
begin
  if IsNull then
    AParams.WriteNull
  else
    AParams.WriteInt32(Value);
end;

end.

