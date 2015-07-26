(*
  JCore, OPF Attribute Data Mediator (ADM) Implementation Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{
  TJCoreOPFADM
  |    Abstract ADM class
  |
  +--TJCoreOPFADMController
  |  |    Controller ADM instances represents all ADMs that doesn't hold the property's value, ie, the
  |  |    ADM is just a controller, the data itself is referenced by the entity's property. Because of
  |  |    that there is no need to initialize or instantiate the ADM, the JCore's OPF Session will do
  |  |    it during the storage or retrieving of the instance.
  |  |    There are two types of controller ADM:
  |  |
  |  +--TJCoreOPFADM<type>NativeCtl
  |  |       Implements ADM of Pascal's native types. If a property is of type Integer,
  |  |       JCore will instantiate a JCoreOPFADMInt32NativeCtl as its controller.
  |  |
  |  +--TJCoreOPFADM<type>IntfCtl
  |          Implements ADM of object as value. If a property is of type IJCoreInteger, JCore will
  |          instantiate a JCoreOPFADMIntegerIntfCtl as its controller.
  |
  +--TJCoreOPFADMValueType
     |    Value types are, at the same time, an internal ADM and the Entity's property. All value type
     |    ADMs should be created with the instance itself using Model.InitEntity(<entity>);
     |
     +--TJCoreOPF<type>Type
             <type> should be AnsiString, Integer, Float, Date, etc
}

unit JCoreOPFADM;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  typinfo,
  fgl,
  JCoreClasses,
  JCoreTypes,
  JCoreEntity,
  JCoreMetadata,
  JCoreOPFMetadata,
  JCoreOPFDriver;

type

  { TJCoreOPFADMSimpleCtl }

  TJCoreOPFADMSimpleCtl = class(TJCoreOPFADMController)
  public
    class function AttributeType: TJCoreOPFAttributeType; override;
  end;

  { TJCoreOPFADMType32NativeCtl }

  TJCoreOPFADMType32NativeCtl = class(TJCoreOPFADMSimpleCtl)
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

  { TJCoreOPFADMType64NativeCtl }

  TJCoreOPFADMType64NativeCtl = class(TJCoreOPFADMSimpleCtl)
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

  { TJCoreOPFADMFloatNativeCtl }

  TJCoreOPFADMFloatNativeCtl = class(TJCoreOPFADMSimpleCtl)
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

  { TJCoreOPFADMAnsiStringNativeCtl }

  TJCoreOPFADMAnsiStringNativeCtl = class(TJCoreOPFADMSimpleCtl)
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

  { TJCoreOPFADMIntegerIntfCtl }

  TJCoreOPFADMIntegerIntfCtl = class(TJCoreOPFADMSimpleCtl)
  private
    FCache: IJCoreInteger;
    function GetValue: IJCoreInteger;
    procedure SetValue(AValue: IJCoreInteger);
    function UseGetter: IJCoreInteger;
    procedure UseSetter(const AValue: IJCoreInteger);
  protected
    procedure InternalGetter; override;
    function InternalIsDirty: Boolean; override;
    procedure InternalUpdateCache; override;
    property Value: IJCoreInteger read GetValue write SetValue;
  public
    class function Apply(const AModel: TJCoreModel; const AAttrTypeInfo: PTypeInfo): Boolean; override;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
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
  JCoreConsts;

{ TJCoreOPFADMSimpleCtl }

class function TJCoreOPFADMSimpleCtl.AttributeType: TJCoreOPFAttributeType;
begin
  Result := jatSimple;
end;

{ TJCoreOPFADMType32NativeCtl }

function TJCoreOPFADMType32NativeCtl.GetValue: Longint;
begin
  if Assigned(AttrAddr) then
    Result := Longint(AttrAddr^)
  else
    Result := UseGetter;
end;

procedure TJCoreOPFADMType32NativeCtl.SetValue(AValue: Longint);
begin
  if Assigned(AttrAddr) then
    Longint(AttrAddr^) := AValue
  else
    UseSetter(AValue);
end;

function TJCoreOPFADMType32NativeCtl.UseGetter: Longint;
begin
  Result := GetOrdProp(PID.Entity, AttrPropInfo);
end;

procedure TJCoreOPFADMType32NativeCtl.UseSetter(const AValue: Longint);
begin
  SetOrdProp(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMType32NativeCtl.InternalGetter;
begin
  UseGetter;
end;

function TJCoreOPFADMType32NativeCtl.InternalIsDirty: Boolean;
begin
  Result := Value <> FCache;
end;

procedure TJCoreOPFADMType32NativeCtl.InternalUpdateCache;
begin
  FCache := Value;
end;

class function TJCoreOPFADMType32NativeCtl.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := (AAttrTypeInfo^.Kind in [tkInteger, tkChar, tkEnumeration, tkBool]);
end;

procedure TJCoreOPFADMType32NativeCtl.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Value := AResultSet.ReadInt32;
end;

procedure TJCoreOPFADMType32NativeCtl.WriteToParams(const AParams: IJCoreOPFParams);
begin
  AParams.WriteInt32(Value);
end;

{ TJCoreOPFADMType64NativeCtl }

function TJCoreOPFADMType64NativeCtl.GetValue: Int64;
begin
  if Assigned(AttrAddr) then
  begin
    case AttrPropInfo^.PropType^.Kind of
      tkInt64: Result := PInt64(AttrAddr)^;
      tkQWord: Result := PQWord(AttrAddr)^;
      else raise EJCoreOPF.Create(2121, S2121_UnsupportedAttributeType, [AttrPropInfo^.PropType^.Name]);
    end;
  end else
    Result := UseGetter;
end;

procedure TJCoreOPFADMType64NativeCtl.SetValue(const AValue: Int64);
begin
  UseSetter(AValue);
end;

function TJCoreOPFADMType64NativeCtl.UseGetter: Int64;
begin
  Result := GetInt64Prop(PID.Entity, AttrPropInfo);
end;

procedure TJCoreOPFADMType64NativeCtl.UseSetter(const AValue: Int64);
begin
  SetInt64Prop(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMType64NativeCtl.InternalGetter;
begin
  UseGetter;
end;

function TJCoreOPFADMType64NativeCtl.InternalIsDirty: Boolean;
begin
  Result := Value <> FCache;
end;

procedure TJCoreOPFADMType64NativeCtl.InternalUpdateCache;
begin
  FCache := Value;
end;

class function TJCoreOPFADMType64NativeCtl.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind in [tkInt64, tkQWord];
end;

procedure TJCoreOPFADMType64NativeCtl.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Value := AResultSet.ReadInt64;
end;

procedure TJCoreOPFADMType64NativeCtl.WriteToParams(const AParams: IJCoreOPFParams);
begin
  AParams.WriteInt64(Value);
end;

{ TJCoreOPFADMFloatNativeCtl }

function TJCoreOPFADMFloatNativeCtl.GetGetter: Extended;
begin
  Result := GetFloatProp(PID.Entity, AttrPropInfo);
end;

function TJCoreOPFADMFloatNativeCtl.GetValue: Extended;
begin
  if Assigned(AttrAddr) then
  begin
    case GetTypeData(AttrPropInfo^.PropType)^.FloatType of
      ftSingle: Result := PSingle(AttrAddr)^;
      ftDouble: Result := PDouble(AttrAddr)^;
      ftExtended: Result := PExtended(AttrAddr)^;
      ftComp: Result := PComp(AttrAddr)^;
      ftCurr: Result := PCurrency(AttrAddr)^;
      else raise EJCoreOPF.Create(2121, S2121_UnsupportedAttributeType, [AttrPropInfo^.PropType^.Name]);
    end;
  end else
    Result := GetGetter;
end;

procedure TJCoreOPFADMFloatNativeCtl.InternalGetter;
begin
  GetGetter;
end;

function TJCoreOPFADMFloatNativeCtl.InternalIsDirty: Boolean;
begin
  Result := Value <> FCache;
end;

procedure TJCoreOPFADMFloatNativeCtl.InternalUpdateCache;
begin
  FCache := Value;
end;

class function TJCoreOPFADMFloatNativeCtl.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkFloat;
end;

{ TJCoreOPFADMAnsiStringNativeCtl }

function TJCoreOPFADMAnsiStringNativeCtl.GetValue: AnsiString;
begin
  if Assigned(AttrAddr) then
    Result := PAnsiString(AttrAddr)^
  else
    Result := UseGetter;
end;

procedure TJCoreOPFADMAnsiStringNativeCtl.SetValue(AValue: AnsiString);
begin
  if Assigned(AttrAddr) then
    PAnsiString(AttrAddr)^ := AValue
  else
    UseSetter(AValue);
end;

function TJCoreOPFADMAnsiStringNativeCtl.UseGetter: AnsiString;
begin
  Result := GetStrProp(PID.Entity, AttrPropInfo);
end;

procedure TJCoreOPFADMAnsiStringNativeCtl.UseSetter(const AValue: AnsiString);
begin
  SetStrProp(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMAnsiStringNativeCtl.InternalGetter;
begin
  UseGetter;
end;

function TJCoreOPFADMAnsiStringNativeCtl.InternalIsDirty: Boolean;
begin
  { TODO : use hash for long strings }
  Result := Value <> FCache;
end;

procedure TJCoreOPFADMAnsiStringNativeCtl.InternalUpdateCache;
begin
  { TODO : use hash for long strings }
  FCache := Value;
end;

class function TJCoreOPFADMAnsiStringNativeCtl.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := AAttrTypeInfo^.Kind = tkAString;
end;

procedure TJCoreOPFADMAnsiStringNativeCtl.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Value := AResultSet.ReadString;
end;

procedure TJCoreOPFADMAnsiStringNativeCtl.WriteToParams(const AParams: IJCoreOPFParams);
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

{ TJCoreOPFADMIntegerIntfCtl }

function TJCoreOPFADMIntegerIntfCtl.GetValue: IJCoreInteger;
begin
  if Assigned(AttrAddr) then
    Result := IJCoreInteger(AttrAddr^)
  else
    Result := UseGetter;
end;

procedure TJCoreOPFADMIntegerIntfCtl.SetValue(AValue: IJCoreInteger);
begin
  if Assigned(AttrAddr) then
    IJCoreInteger(AttrAddr^) := AValue
  else
    UseSetter(AValue);
end;

function TJCoreOPFADMIntegerIntfCtl.UseGetter: IJCoreInteger;
begin
  Result := GetInterfaceProp(PID.Entity, AttrPropInfo) as IJCoreInteger;
end;

procedure TJCoreOPFADMIntegerIntfCtl.UseSetter(const AValue: IJCoreInteger);
begin
  SetInterfaceProp(PID.Entity, AttrPropInfo, AValue);
end;

procedure TJCoreOPFADMIntegerIntfCtl.InternalGetter;
begin
  UseGetter;
end;

function TJCoreOPFADMIntegerIntfCtl.InternalIsDirty: Boolean;
var
  VValue: IJCoreInteger;
begin
  VValue := Value;
  if Assigned(FCache) then
    Result := not FCache.EqualsTo(VValue)
  else
    Result := Assigned(VValue);
end;

procedure TJCoreOPFADMIntegerIntfCtl.InternalUpdateCache;
begin
  FCache := Value;
end;

class function TJCoreOPFADMIntegerIntfCtl.Apply(const AModel: TJCoreModel;
  const AAttrTypeInfo: PTypeInfo): Boolean;
begin
  Result := Implements(AAttrTypeInfo, IJCoreInteger);
end;

procedure TJCoreOPFADMIntegerIntfCtl.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  if AResultSet.ReadNull then
    Value := nil
  else
    Value := TJCoreInteger.ValueOf(AResultSet.ReadInt32);
end;

procedure TJCoreOPFADMIntegerIntfCtl.WriteToParams(const AParams: IJCoreOPFParams);
var
  VValue: IJCoreInteger;
begin
  VValue := Value;
  if Assigned(VValue) then
    AParams.WriteInt32(Value.AsInteger)
  else
    AParams.WriteNull;
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

