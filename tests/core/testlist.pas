unit TestList;

{$mode objfpc}{$H+}

interface

uses
  fpcunit,
  typinfo,
  JCoreList;

type

  { TNativeTypeOrderedListTest }

  TNativeTypeOrderedListTest = class(TTestCase)
  private
    FQueue: TJCoreNativeTypeQueue;
    FStack: TJCoreNativeTypeStack;
    function GetQueue: TJCoreNativeTypeQueue;
    function GetStack: TJCoreNativeTypeStack;
  protected
    function TypeToName(const AType: TTypeKind): string;
    procedure TearDown; override;
    property Queue: TJCoreNativeTypeQueue read GetQueue;
    property Stack: TJCoreNativeTypeStack read GetStack;
  published
    procedure PushPopInt32;
    procedure PushPopInt64;
    procedure PushPopString;
    procedure PushPopNull;
    procedure PushAllCount;
    procedure PushAllPopAllQueue;
    procedure PushAllPopAllStack;
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

{ TNativeTypeOrderedListTest }

function TNativeTypeOrderedListTest.GetQueue: TJCoreNativeTypeQueue;
begin
  if not Assigned(FQueue) then
    FQueue := TJCoreNativeTypeQueue.Create;
  Result := FQueue;
end;

function TNativeTypeOrderedListTest.GetStack: TJCoreNativeTypeStack;
begin
  if not Assigned(FStack) then
    FStack := TJCoreNativeTypeStack.Create;
  Result := FStack;
end;

function TNativeTypeOrderedListTest.TypeToName(const AType: TTypeKind): string;
begin
  Result := GetEnumName(TypeInfo(TTypeKind), Ord(AType));
end;

procedure TNativeTypeOrderedListTest.TearDown;
begin
  FreeAndNil(FQueue);
  FreeAndNil(FStack);
  inherited TearDown;
end;

procedure TNativeTypeOrderedListTest.PushPopInt32;
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

procedure TNativeTypeOrderedListTest.PushPopInt64;
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

procedure TNativeTypeOrderedListTest.PushPopString;
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

procedure TNativeTypeOrderedListTest.PushPopNull;
var
  VType: TTypeKind;
begin
  Stack.PushNull;
  AssertEquals('stack.count', 1, Stack.Count);
  VType := Stack.PopType;
  AssertEquals('stack.poptype', 'tkUnknown', TypeToName(VType));
  AssertEquals('stack.count', 0, Stack.Count);
end;

procedure TNativeTypeOrderedListTest.PushAllCount;
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

procedure TNativeTypeOrderedListTest.PushAllPopAllQueue;
var
  VType: TTypeKind;
begin
  Queue.PushInt32(10);
  Queue.PushInt64(18);
  Queue.PushString('data');
  Queue.PushNull;
  Queue.PushInt32(1010);
  Queue.PushInt64(1018);
  Queue.PushString('test data');
  Queue.PushNull;
  VType := Queue.PopType;
  AssertEquals('queue.poptype[0]', 'tkInteger', TypeToName(VType));
  AssertEquals('queue.popvalue[0]', 10, Queue.PopInt32);
  VType := Queue.PopType;
  AssertEquals('queue.poptype[1]', 'tkInt64', TypeToName(VType));
  AssertEquals('queue.popvalue[1]', 18, Queue.PopInt64);
  VType := Queue.PopType;
  AssertEquals('queue.poptype[2]', 'tkAString', TypeToName(VType));
  AssertEquals('queue.popvalue[2]', 'data', Queue.PopString);
  VType := Queue.PopType;
  AssertEquals('queue.poptype[3]', 'tkUnknown', TypeToName(VType));
  VType := Queue.PopType;
  AssertEquals('queue.poptype[4]', 'tkInteger', TypeToName(VType));
  AssertEquals('queue.popvalue[4]', 1010, Queue.PopInt32);
  VType := Queue.PopType;
  AssertEquals('queue.poptype[5]', 'tkInt64', TypeToName(VType));
  AssertEquals('queue.popvalue[5]', 1018, Queue.PopInt64);
  VType := Queue.PopType;
  AssertEquals('queue.poptype[6]', 'tkAString', TypeToName(VType));
  AssertEquals('queue.popvalue[6]', 'test data', Queue.PopString);
  VType := Queue.PopType;
  AssertEquals('queue.poptype[7]', 'tkUnknown', TypeToName(VType));
  AssertEquals('queue.count', 0, Queue.Count);
end;

procedure TNativeTypeOrderedListTest.PushAllPopAllStack;
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

procedure TNativeTypeOrderedListTest.PushLessPopMore;
begin
  Stack.PushInt32(1010);
  Stack.PushString('data');
  Stack.PopType;
  Stack.PopString;
  Stack.PopType;
  Stack.PopInt32;
  try
    Stack.PopType;
    Fail('EJCoreClasses(0203) expected');
  except
    on E: EJCoreClasses do
      if E.Code <> 203 then
        raise;
  end;
end;

procedure TNativeTypeOrderedListTest.PopWrongType;
begin
  Queue.PushString('data');
  Queue.PopType;
  try
    Queue.PopInt32;
    Fail('EJCoreClasses(0202) expected');
  except
    on E: EJCoreClasses do
      if E.Code <> 202 then
        raise;
  end;
end;

procedure TNativeTypeOrderedListTest.PopValueWithoutPopType;
begin
  Stack.PushInt32(18);
  try
    Stack.PopInt32;
    Fail('EJCoreClasses(0202) expected');
  except
    on E: EJCoreClasses do
      if E.Code <> 202 then
        raise;
  end;
end;

procedure TNativeTypeOrderedListTest.PopTypePushValue;
begin
  Queue.PushInt32(10);
  Queue.PopType;
  try
    Queue.PushString('data');
    Fail('EJCoreClasses(0202) expected');
  except
    on E: EJCoreClasses do
      if E.Code <> 202 then
        raise;
  end;
end;

initialization
  RegisterTest('jcore.core.list', TNativeTypeOrderedListTest);

end.

