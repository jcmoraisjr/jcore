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

  { TJCoreNativeTypeStack }

  TJCoreNativeTypeStack = class(TObject)
  private
    FCount: Integer;
    FCurrentType: PTypeInfo;
    FStack: TStack;
    FTypeInfoInt64: PTypeInfo;
    FTypeInfoInteger: PTypeInfo;
    FTypeInfoString: PTypeInfo;
    procedure ClearStack;
    procedure PopValue(const AType: TTypeKind);
    procedure ValidateType(const AExpectedType: PTypeInfo);
  protected
    property Stack: TStack read FStack;
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

implementation

uses
  sysutils,
  JCoreClasses;

{ TJCoreNativeTypeStack }

procedure TJCoreNativeTypeStack.ClearStack;
begin
  if Assigned(FCurrentType) then
    // there is a poptype without a popvalue
    PopValue(FCurrentType^.Kind);
  while Count > 0 do
    PopValue(PopType);
end;

procedure TJCoreNativeTypeStack.PopValue(const AType: TTypeKind);
begin
  case AType of
    tkInteger: PopInt32;
    tkInt64: PopInt64;
    tkAString: PopString;
    else { TODO : Implement unsupported };
  end;
end;

procedure TJCoreNativeTypeStack.ValidateType(const AExpectedType: PTypeInfo);
begin
  if AExpectedType <> FCurrentType then
    raise EJCoreListTypeExpected.Create(FCurrentType);
end;

constructor TJCoreNativeTypeStack.Create;
begin
  inherited Create;
  { TODO : Allocate a buffer (manage one block) instead of use tstack (tlist+several small allocations) }
  FStack := TStack.Create;
  FTypeInfoInt64 := PTypeInfo(TypeInfo(Int64));
  FTypeInfoInteger := PTypeInfo(TypeInfo(Integer));
  FTypeInfoString := PTypeInfo(TypeInfo(string));
end;

destructor TJCoreNativeTypeStack.Destroy;
begin
  ClearStack;
  FreeAndNil(FStack);
  inherited Destroy;
end;

function TJCoreNativeTypeStack.PopInt32: Integer;
var
  VValue: PInteger;
begin
  ValidateType(FTypeInfoInteger);
  VValue := PInteger(Stack.Pop);
  Result := VValue^;
  Dec(FCount);
  FCurrentType := nil;
  Dispose(VValue);
end;

function TJCoreNativeTypeStack.PopInt64: Int64;
var
  VValue: PInt64;
begin
  ValidateType(FTypeInfoInt64);
  VValue := PInt64(Stack.Pop);
  Result := VValue^;
  Dec(FCount);
  FCurrentType := nil;
  Dispose(VValue);
end;

function TJCoreNativeTypeStack.PopString: string;
var
  VValue: PAnsiString;
begin
  ValidateType(FTypeInfoString);
  VValue := PAnsiString(Stack.Pop);
  Result := VValue^;
  Dec(FCount);
  FCurrentType := nil;
  Dispose(VValue);
end;

function TJCoreNativeTypeStack.PopType: TTypeKind;
begin
  if Count < 1 then
    raise EJCoreListIsEmpty.Create;
  ValidateType(nil);
  FCurrentType := PTypeInfo(Stack.Pop);
  if Assigned(FCurrentType) then
    Result := FCurrentType^.Kind
  else
  begin
    Dec(FCount);
    Result := tkUnknown; // PopNull
  end;
end;

procedure TJCoreNativeTypeStack.PushInt32(const AValue: Integer);
var
  VValue: PInteger;
begin
  ValidateType(nil);
  New(VValue);
  VValue^ := AValue;
  Stack.Push(VValue);
  Stack.Push(FTypeInfoInteger);
  Inc(FCount);
end;

procedure TJCoreNativeTypeStack.PushInt64(const AValue: Int64);
var
  VValue: PInt64;
begin
  ValidateType(nil);
  New(VValue);
  VValue^ := AValue;
  Stack.Push(VValue);
  Stack.Push(FTypeInfoInt64);
  Inc(FCount);
end;

procedure TJCoreNativeTypeStack.PushNull;
begin
  ValidateType(nil);
  Stack.Push(nil);
  Inc(FCount);
end;

procedure TJCoreNativeTypeStack.PushString(const AValue: string);
var
  VValue: PAnsiString;
begin
  ValidateType(nil);
  New(VValue);
  VValue^ := AValue;
  Stack.Push(VValue);
  Stack.Push(FTypeInfoString);
  Inc(FCount);
end;

end.

