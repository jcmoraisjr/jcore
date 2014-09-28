unit TestList;

{$mode objfpc}{$H+}

interface

uses
  fpcunit,
  typinfo,
  JCoreList;

type

  { TNativeTypeStackTest }

  TNativeTypeStackTest = class(TTestCase)
  private
    FStack: TJCoreNativeTypeStack;
  protected
    function TypeToName(const AType: TTypeKind): string;
    procedure SetUp; override;
    procedure TearDown; override;
    property Stack: TJCoreNativeTypeStack read FStack;
  published
    procedure PushPopInt32;
    procedure PushPopInt64;
    procedure PushPopString;
    procedure PushPopNull;
    procedure PushAllCount;
    procedure PushAllPopAll;
    procedure PushLessPopMore;
    procedure PopWrongType;
    procedure PopValueWithoutPopType;
    procedure PopTypePushValue;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreClasses;

{ TNativeTypeStackTest }

function TNativeTypeStackTest.TypeToName(const AType: TTypeKind): string;
begin
  Result := GetEnumName(TypeInfo(TTypeKind), Ord(AType));
end;

procedure TNativeTypeStackTest.SetUp;
begin
  inherited SetUp;
  FStack := TJCoreNativeTypeStack.Create;
end;

procedure TNativeTypeStackTest.TearDown;
begin
  FreeAndNil(FStack);
  inherited TearDown;
end;

procedure TNativeTypeStackTest.PushPopInt32;
var
  VType: TTypeKind;
  VValue: Integer;
begin
  Stack.PushInt32(10);
  AssertEquals('stack.count', 1, Stack.Count);
  VType := Stack.PopType;
  AssertEquals('stack.poptype', 'tkInteger', TypeToName(VType));
  VValue := Stack.PopInt32;
  AssertEquals('stack.popint32', 10, VValue);
  AssertEquals('stack.count', 0, Stack.Count);
end;

procedure TNativeTypeStackTest.PushPopInt64;
var
  VType: TTypeKind;
  VValue: Int64;
begin
  Stack.PushInt64(18);
  AssertEquals('stack.count', 1, Stack.Count);
  VType := Stack.PopType;
  AssertEquals('stack.poptype', 'tkInt64', TypeToName(VType));
  VValue := Stack.PopInt64;
  AssertEquals('stack.popint32', 18, VValue);
  AssertEquals('stack.count', 0, Stack.Count);
end;

procedure TNativeTypeStackTest.PushPopString;
var
  VType: TTypeKind;
  VValue: string;
begin
  Stack.PushString('data');
  AssertEquals('stack.count', 1, Stack.Count);
  VType := Stack.PopType;
  AssertEquals('stack.poptype', 'tkAString', TypeToName(VType));
  VValue := Stack.PopString;
  AssertEquals('stack.popint32', 'data', VValue);
  AssertEquals('stack.count', 0, Stack.Count);
end;

procedure TNativeTypeStackTest.PushPopNull;
var
  VType: TTypeKind;
begin
  Stack.PushNull;
  AssertEquals('stack.count', 1, Stack.Count);
  VType := Stack.PopType;
  AssertEquals('stack.poptype', 'tkUnknown', TypeToName(VType));
  AssertEquals('stack.count', 0, Stack.Count);
end;

procedure TNativeTypeStackTest.PushAllCount;
begin
  Stack.PushInt32(10);
  Stack.PushInt64(18);
  Stack.PushString('data');
  Stack.PushNull;
  AssertEquals('stack.count', 4, Stack.Count);
  Stack.PushInt32(1010);
  Stack.PushInt64(1018);
  Stack.PushString('test data');
  Stack.PushNull;
  AssertEquals('stack.count', 8, Stack.Count);
end;

procedure TNativeTypeStackTest.PushAllPopAll;
var
  VType: TTypeKind;
begin
  Stack.PushInt32(10);
  Stack.PushInt64(18);
  Stack.PushString('data');
  Stack.PushNull;
  Stack.PushInt32(1010);
  Stack.PushInt64(1018);
  Stack.PushString('test data');
  Stack.PushNull;
  VType := Stack.PopType;
  AssertEquals('stack.poptype[7]', 'tkUnknown', TypeToName(VType));
  VType := Stack.PopType;
  AssertEquals('stack.poptype[6]', 'tkAString', TypeToName(VType));
  AssertEquals('stack.popvalue[6]', 'test data', Stack.PopString);
  VType := Stack.PopType;
  AssertEquals('stack.poptype[5]', 'tkInt64', TypeToName(VType));
  AssertEquals('stack.popvalue[5]', 1018, Stack.PopInt64);
  VType := Stack.PopType;
  AssertEquals('stack.poptype[4]', 'tkInteger', TypeToName(VType));
  AssertEquals('stack.popvalue[4]', 1010, Stack.PopInt32);
  VType := Stack.PopType;
  AssertEquals('stack.poptype[3]', 'tkUnknown', TypeToName(VType));
  VType := Stack.PopType;
  AssertEquals('stack.poptype[2]', 'tkAString', TypeToName(VType));
  AssertEquals('stack.popvalue[2]', 'data', Stack.PopString);
  VType := Stack.PopType;
  AssertEquals('stack.poptype[1]', 'tkInt64', TypeToName(VType));
  AssertEquals('stack.popvalue[1]', 18, Stack.PopInt64);
  VType := Stack.PopType;
  AssertEquals('stack.poptype[0]', 'tkInteger', TypeToName(VType));
  AssertEquals('stack.popvalue[0]', 10, Stack.PopInt32);
  AssertEquals('stack.count', 0, Stack.Count);
end;

procedure TNativeTypeStackTest.PushLessPopMore;
begin
  Stack.PushInt32(1010);
  Stack.PushString('data');
  Stack.PopType;
  Stack.PopString;
  Stack.PopType;
  Stack.PopInt32;
  try
    Stack.PopType;
  except
    on E: EJCoreListIsEmpty do
      Exit;
  end;
  Fail('EJCoreListIsEmpty expected');
end;

procedure TNativeTypeStackTest.PopWrongType;
begin
  Stack.PushString('data');
  Stack.PopType;
  try
    Stack.PopInt32;
  except
    on E: EJCoreListTypeExpected do
    begin
      AssertEquals('expected type', 'tkAString', TypeToName(E.ExpectedType^.Kind));
      Exit;
    end;
  end;
  Fail('EJCoreListTypeExpected expected');
end;

procedure TNativeTypeStackTest.PopValueWithoutPopType;
begin
  Stack.PushInt32(18);
  try
    Stack.PopInt32;
  except
    on E: EJCoreListTypeExpected do
      Exit;
  end;
  Fail('EJCoreListTypeExpected expected');
end;

procedure TNativeTypeStackTest.PopTypePushValue;
begin
  Stack.PushInt32(10);
  Stack.PopType;
  try
    Stack.PushString('data');
  except
    on E: EJCoreListTypeExpected do
      Exit;
  end;
  Fail('EJCoreListTypeExpected expected');
end;

initialization
  RegisterTest('jcore.core.list', TNativeTypeStackTest);

end.

