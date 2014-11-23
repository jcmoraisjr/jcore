(*
  JCore, Lists and Containers Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreList;

{$I jcore.inc}

interface

uses
  typinfo,
  contnrs;

type

  { TJCoreNativeTypeOrderedList }

  TJCoreNativeTypeOrderedList = class(TObject)
  private
    FCount: Integer;
    FCurrentType: PTypeInfo;
    FOrderedList: TOrderedList;
    FTypeInfoInt64: PTypeInfo;
    FTypeInfoInteger: PTypeInfo;
    FTypeInfoString: PTypeInfo;
    procedure ClearOrderedList;
    procedure PopValue(const AType: TTypeKind);
    procedure ValidateType(const AExpectedType: PTypeInfo);
  protected
    function CreateOrderedList: TOrderedList; virtual; abstract;
    procedure PushTypedValue(const AValue: Pointer; const AType: PTypeInfo); virtual; abstract;
    property OrderedList: TOrderedList read FOrderedList;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read FCount;
    function PopInt32: Integer;
    function PopInt64: Int64;
    function PopString: string;
    function PopType: TTypeKind;
    procedure PushInt32(const AValue: Integer);
    procedure PushInt64(const AValue: Int64);
    procedure PushNull;
    procedure PushString(const AValue: string);
  end;

  { TJCoreNativeTypeStack }

  TJCoreNativeTypeStack = class(TJCoreNativeTypeOrderedList)
  protected
    function CreateOrderedList: TOrderedList; override;
    procedure PushTypedValue(const AValue: Pointer; const AType: PTypeInfo); override;
  end;

  { TJCoreNativeTypeQueue }

  TJCoreNativeTypeQueue = class(TJCoreNativeTypeOrderedList)
  protected
    function CreateOrderedList: TOrderedList; override;
    procedure PushTypedValue(const AValue: Pointer; const AType: PTypeInfo); override;
  end;

implementation

uses
  sysutils,
  JCoreClasses;

{ TJCoreNativeTypeOrderedList }

procedure TJCoreNativeTypeOrderedList.ClearOrderedList;
begin
  if Assigned(FCurrentType) then
    // there is a poptype without a popvalue
    PopValue(FCurrentType^.Kind);
  while Count > 0 do
    PopValue(PopType);
end;

procedure TJCoreNativeTypeOrderedList.PopValue(const AType: TTypeKind);
begin
  case AType of
    tkInteger: PopInt32;
    tkInt64: PopInt64;
    tkAString: PopString;
    else { TODO : Implement unsupported };
  end;
end;

procedure TJCoreNativeTypeOrderedList.ValidateType(const AExpectedType: PTypeInfo);
begin
  if AExpectedType <> FCurrentType then
    raise EJCoreListTypeExpected.Create(FCurrentType);
end;

constructor TJCoreNativeTypeOrderedList.Create;
begin
  inherited Create;
  { TODO : Allocate a buffer (manage one block) instead of use tstack (tlist+several small allocations) }
  FOrderedList := CreateOrderedList;
  FTypeInfoInt64 := PTypeInfo(TypeInfo(Int64));
  FTypeInfoInteger := PTypeInfo(TypeInfo(Integer));
  FTypeInfoString := PTypeInfo(TypeInfo(string));
end;

destructor TJCoreNativeTypeOrderedList.Destroy;
begin
  ClearOrderedList;
  FreeAndNil(FOrderedList);
  inherited Destroy;
end;

function TJCoreNativeTypeOrderedList.PopInt32: Integer;
var
  VValue: PInteger;
begin
  ValidateType(FTypeInfoInteger);
  VValue := PInteger(OrderedList.Pop);
  Result := VValue^;
  Dec(FCount);
  FCurrentType := nil;
  Dispose(VValue);
end;

function TJCoreNativeTypeOrderedList.PopInt64: Int64;
var
  VValue: PInt64;
begin
  ValidateType(FTypeInfoInt64);
  VValue := PInt64(OrderedList.Pop);
  Result := VValue^;
  Dec(FCount);
  FCurrentType := nil;
  Dispose(VValue);
end;

function TJCoreNativeTypeOrderedList.PopString: string;
var
  VValue: PAnsiString;
begin
  ValidateType(FTypeInfoString);
  VValue := PAnsiString(OrderedList.Pop);
  Result := VValue^;
  Dec(FCount);
  FCurrentType := nil;
  Dispose(VValue);
end;

function TJCoreNativeTypeOrderedList.PopType: TTypeKind;
begin
  if Count < 1 then
    raise EJCoreListIsEmpty.Create;
  ValidateType(nil);
  FCurrentType := PTypeInfo(OrderedList.Pop);
  if Assigned(FCurrentType) then
    Result := FCurrentType^.Kind
  else
  begin
    Dec(FCount);
    Result := tkUnknown; // PopNull
  end;
end;

procedure TJCoreNativeTypeOrderedList.PushInt32(const AValue: Integer);
var
  VValue: PInteger;
begin
  ValidateType(nil);
  New(VValue);
  VValue^ := AValue;
  PushTypedValue(VValue, FTypeInfoInteger);
  Inc(FCount);
end;

procedure TJCoreNativeTypeOrderedList.PushInt64(const AValue: Int64);
var
  VValue: PInt64;
begin
  ValidateType(nil);
  New(VValue);
  VValue^ := AValue;
  PushTypedValue(VValue, FTypeInfoInt64);
  Inc(FCount);
end;

procedure TJCoreNativeTypeOrderedList.PushNull;
begin
  ValidateType(nil);
  OrderedList.Push(nil);
  Inc(FCount);
end;

procedure TJCoreNativeTypeOrderedList.PushString(const AValue: string);
var
  VValue: PAnsiString;
begin
  ValidateType(nil);
  New(VValue);
  VValue^ := AValue;
  PushTypedValue(VValue, FTypeInfoString);
  Inc(FCount);
end;

{ TJCoreNativeTypeStack }

function TJCoreNativeTypeStack.CreateOrderedList: TOrderedList;
begin
  Result := TStack.Create;
end;

procedure TJCoreNativeTypeStack.PushTypedValue(const AValue: Pointer; const AType: PTypeInfo);
begin
  OrderedList.Push(AValue);
  OrderedList.Push(AType);
end;

{ TJCoreNativeTypeQueue }

function TJCoreNativeTypeQueue.CreateOrderedList: TOrderedList;
begin
  Result := TQueue.Create;
end;

procedure TJCoreNativeTypeQueue.PushTypedValue(const AValue: Pointer; const AType: PTypeInfo);
begin
  OrderedList.Push(AType);
  OrderedList.Push(AValue);
end;

end.

