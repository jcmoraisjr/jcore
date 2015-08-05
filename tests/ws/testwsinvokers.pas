unit TestWSInvokers;

{$mode objfpc}{$H+}

interface

uses
  typinfo,
  fpcunit,
  JCoreWSInvokers;

type

  { TTestWSMethodData }

  TTestWSMethodData = class(TTestCase)
  private
    FMethodData: TJCoreWSMethodData;
    procedure P01;
    procedure P02(AObject: TObject);
    procedure P03(var AObject: TObject);
    procedure P04(const AObject: TObject);
    procedure P05(const AObject: TInterfacedObject);
    procedure P06(const AObject: TObject; const AMsg: string);
    procedure P07(const AObject: TObject); cdecl;
    function F01: TObject;
    function F02: TInterfacedObject;
    function F03: string;
    function F04(const AObject: TObject): string;
    function F05(const AMsg: string): TObject;
    function F06(const AMsg1, AMsg2: string): TObject;
    function F07(const AMsg: string): TObject; cdecl;
  protected
    procedure TearDown; override;
    function CreateMethodData(const AMethodTypeInfo: PTypeInfo): TJCoreWSMethodData;
  published
    procedure ValidP01;
    procedure ValidP02;
    procedure ValidP03;
    procedure ValidP04;
    procedure ValidP05;
    procedure ValidP06;
    procedure ValidP07;
    procedure ValidF01;
    procedure ValidF02;
    procedure ValidF03;
    procedure ValidF04;
    procedure ValidF05;
    procedure ValidF06;
    procedure ValidF07;
    procedure InvalidTypeInfo;
    procedure IsProcedureConstObject;
    procedure IsFunctionAsObject;
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreClasses;

{ TTestWSMethodData }

procedure TTestWSMethodData.P01;
begin
end;

procedure TTestWSMethodData.P02(AObject: TObject);
begin
end;

procedure TTestWSMethodData.P03(var AObject: TObject);
begin
end;

procedure TTestWSMethodData.P04(const AObject: TObject);
begin
end;

procedure TTestWSMethodData.P05(const AObject: TInterfacedObject);
begin
end;

procedure TTestWSMethodData.P06(const AObject: TObject; const AMsg: string);
begin
end;

procedure TTestWSMethodData.P07(const AObject: TObject); cdecl;
begin
end;

function TTestWSMethodData.F01: TObject;
begin
  Result := nil;
end;

function TTestWSMethodData.F02: TInterfacedObject;
begin
  Result := nil;
end;

function TTestWSMethodData.F03: string;
begin
  Result := '';
end;

function TTestWSMethodData.F04(const AObject: TObject): string;
begin
  Result := '';
end;

function TTestWSMethodData.F05(const AMsg: string): TObject;
begin
  Result := nil;
end;

function TTestWSMethodData.F06(const AMsg1, AMsg2: string): TObject;
begin
  Result := nil;
end;

function TTestWSMethodData.F07(const AMsg: string): TObject; cdecl;
begin
  Result := nil;
end;

procedure TTestWSMethodData.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FMethodData);
end;

function TTestWSMethodData.CreateMethodData(const AMethodTypeInfo: PTypeInfo): TJCoreWSMethodData;
begin
  FreeAndNil(FMethodData);
  FMethodData := TJCoreWSMethodData.Create(AMethodTypeInfo);
  Result := FMethodData;
end;

procedure TTestWSMethodData.ValidP01;
var
  VMethod: TJCoreWSMethodData;
begin
  //procedure P01;
  VMethod := CreateMethodData(TypeInfo(@P01));
  AssertEquals('type', Ord(mkProcedure), Ord(VMethod.MethodKind));
  AssertEquals('param count', 0, Length(VMethod.Params));
  AssertEquals('call conv', Ord(ccReg), Ord(VMethod.CConv));
end;

procedure TTestWSMethodData.ValidP02;
var
  VMethod: TJCoreWSMethodData;
begin
  //procedure P02(AObject: TObject);
  VMethod := CreateMethodData(TypeInfo(@P02));
  AssertEquals('type', Ord(mkProcedure), Ord(VMethod.MethodKind));
  AssertEquals('param count', 1, Length(VMethod.Params));
  AssertFalse('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertFalse('param[0] flag var', pfVar in VMethod.Params[0].Flags);
  AssertEquals('param[0] name', 'AObject', VMethod.Params[0].ParamName);
  AssertEquals('param[0] kind', Ord(tkClass), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('param[0] type', TObject, GetTypeData(VMethod.Params[0].ParamTypeInfo)^.ClassType);
  AssertEquals('call conv', Ord(ccReg), Ord(VMethod.CConv));
end;

procedure TTestWSMethodData.ValidP03;
var
  VMethod: TJCoreWSMethodData;
begin
  //procedure P03(var AObject: TObject);
  VMethod := CreateMethodData(TypeInfo(@P03));
  AssertEquals('type', Ord(mkProcedure), Ord(VMethod.MethodKind));
  AssertEquals('param count', 1, Length(VMethod.Params));
  AssertTrue('param[0] flag var', pfVar in VMethod.Params[0].Flags);
  AssertEquals('param[0] kind', Ord(tkClass), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('param[0] type', TObject, GetTypeData(VMethod.Params[0].ParamTypeInfo)^.ClassType);
  AssertEquals('call conv', Ord(ccReg), Ord(VMethod.CConv));
end;

procedure TTestWSMethodData.ValidP04;
var
  VMethod: TJCoreWSMethodData;
begin
  //procedure P04(const AObject: TObject);
  VMethod := CreateMethodData(TypeInfo(@P04));
  AssertEquals('type', Ord(mkProcedure), Ord(VMethod.MethodKind));
  AssertEquals('param count', 1, Length(VMethod.Params));
  AssertTrue('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertEquals('param[0] kind', Ord(tkClass), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('param[0] type', TObject, GetTypeData(VMethod.Params[0].ParamTypeInfo)^.ClassType);
end;

procedure TTestWSMethodData.ValidP05;
var
  VMethod: TJCoreWSMethodData;
begin
  //procedure P05(const AObject: TInterfacedObject);
  VMethod := CreateMethodData(TypeInfo(@P05));
  AssertEquals('param count', 1, Length(VMethod.Params));
  AssertTrue('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertEquals('param[0] kind', Ord(tkClass), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('param[0] type', TInterfacedObject, GetTypeData(VMethod.Params[0].ParamTypeInfo)^.ClassType);
end;

procedure TTestWSMethodData.ValidP06;
var
  VMethod: TJCoreWSMethodData;
begin
  //procedure P06(const AObject: TObject; const AMsg: string);
  VMethod := CreateMethodData(TypeInfo(@P06));
  AssertEquals('param count', 2, Length(VMethod.Params));
  AssertTrue('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertEquals('param[0] kind', Ord(tkClass), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('param[0] type', TObject, GetTypeData(VMethod.Params[0].ParamTypeInfo)^.ClassType);
  AssertTrue('param[1] flag const', pfConst in VMethod.Params[1].Flags);
  AssertEquals('param[1] name', 'AMsg', VMethod.Params[1].ParamName);
  AssertEquals('param[1] kind', Ord(tkAString), Ord(VMethod.Params[1].ParamTypeInfo^.Kind));
end;

procedure TTestWSMethodData.ValidP07;
var
  VMethod: TJCoreWSMethodData;
begin
  //procedure P07(const AObject: TObject); cdecl;
  VMethod := CreateMethodData(TypeInfo(@P07));
  AssertEquals('type', Ord(mkProcedure), Ord(VMethod.MethodKind));
  AssertEquals('param count', 1, Length(VMethod.Params));
  AssertTrue('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertEquals('param[0] kind', Ord(tkClass), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('param[0] type', TObject, GetTypeData(VMethod.Params[0].ParamTypeInfo)^.ClassType);
  AssertEquals('call conv', Ord(ccCdecl), Ord(VMethod.CConv));
end;

procedure TTestWSMethodData.ValidF01;
var
  VMethod: TJCoreWSMethodData;
begin
  //function F01: TObject;
  VMethod := CreateMethodData(TypeInfo(@F01));
  AssertEquals('type', Ord(mkFunction), Ord(VMethod.MethodKind));
  AssertEquals('param count', 0, Length(VMethod.Params));
  AssertEquals('result kind', Ord(tkClass), Ord(VMethod.ResultType^.Kind));
  AssertEquals('result type', TObject, GetTypeData(VMethod.ResultType)^.ClassType);
  AssertEquals('call conv', Ord(ccReg), Ord(VMethod.CConv));
end;

procedure TTestWSMethodData.ValidF02;
var
  VMethod: TJCoreWSMethodData;
begin
  //function F02: TInterfacedObject;
  VMethod := CreateMethodData(TypeInfo(@F02));
  AssertEquals('type', Ord(mkFunction), Ord(VMethod.MethodKind));
  AssertEquals('param count', 0, Length(VMethod.Params));
  AssertEquals('result kind', Ord(tkClass), Ord(VMethod.ResultType^.Kind));
  AssertEquals('result type', TInterfacedObject, GetTypeData(VMethod.ResultType)^.ClassType);
end;

procedure TTestWSMethodData.ValidF03;
var
  VMethod: TJCoreWSMethodData;
begin
  //function F03: string;
  VMethod := CreateMethodData(TypeInfo(@F03));
  AssertEquals('type', Ord(mkFunction), Ord(VMethod.MethodKind));
  AssertEquals('param count', 0, Length(VMethod.Params));
  AssertEquals('result kind', Ord(tkAString), Ord(VMethod.ResultType^.Kind));
end;

procedure TTestWSMethodData.ValidF04;
var
  VMethod: TJCoreWSMethodData;
begin
  //function F04(const AObject: TObject): string;
  VMethod := CreateMethodData(TypeInfo(@F04));
  AssertEquals('type', Ord(mkFunction), Ord(VMethod.MethodKind));
  AssertEquals('param count', 1, Length(VMethod.Params));
  AssertTrue('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertEquals('param[0] name', 'AObject', VMethod.Params[0].ParamName);
  AssertEquals('param[0] kind', Ord(tkClass), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('param[0] type', TObject, GetTypeData(VMethod.Params[0].ParamTypeInfo)^.ClassType);
  AssertEquals('result kind', Ord(tkAString), Ord(VMethod.ResultType^.Kind));
end;

procedure TTestWSMethodData.ValidF05;
var
  VMethod: TJCoreWSMethodData;
begin
  //function F05(const AMsg: string): TObject;
  VMethod := CreateMethodData(TypeInfo(@F05));
  AssertEquals('type', Ord(mkFunction), Ord(VMethod.MethodKind));
  AssertEquals('param count', 1, Length(VMethod.Params));
  AssertTrue('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertEquals('param[0] name', 'AMsg', VMethod.Params[0].ParamName);
  AssertEquals('param[0] kind', Ord(tkAString), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('result kind', Ord(tkClass), Ord(VMethod.ResultType^.Kind));
  AssertEquals('result type', TObject, GetTypeData(VMethod.ResultType)^.ClassType);
end;

procedure TTestWSMethodData.ValidF06;
var
  VMethod: TJCoreWSMethodData;
begin
  //function F06(const AMsg1, AMsg2: string): TObject;
  VMethod := CreateMethodData(TypeInfo(@F06));
  AssertEquals('type', Ord(mkFunction), Ord(VMethod.MethodKind));
  AssertEquals('param count', 2, Length(VMethod.Params));
  AssertTrue('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertEquals('param[0] name', 'AMsg1', VMethod.Params[0].ParamName);
  AssertEquals('param[0] kind', Ord(tkAString), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertTrue('param[1] flag const', pfConst in VMethod.Params[1].Flags);
  AssertEquals('param[1] name', 'AMsg2', VMethod.Params[1].ParamName);
  AssertEquals('param[1] kind', Ord(tkAString), Ord(VMethod.Params[1].ParamTypeInfo^.Kind));
  AssertEquals('result kind', Ord(tkClass), Ord(VMethod.ResultType^.Kind));
  AssertEquals('result type', TObject, GetTypeData(VMethod.ResultType)^.ClassType);
end;

procedure TTestWSMethodData.ValidF07;
var
  VMethod: TJCoreWSMethodData;
begin
  //function F07(const AMsg: string): TObject; cdecl;
  VMethod := CreateMethodData(TypeInfo(@F07));
  AssertEquals('type', Ord(mkFunction), Ord(VMethod.MethodKind));
  AssertEquals('param count', 1, Length(VMethod.Params));
  AssertTrue('param[0] flag const', pfConst in VMethod.Params[0].Flags);
  AssertEquals('param[0] kind', Ord(tkAString), Ord(VMethod.Params[0].ParamTypeInfo^.Kind));
  AssertEquals('result kind', Ord(tkClass), Ord(VMethod.ResultType^.Kind));
  AssertEquals('result type', TObject, GetTypeData(VMethod.ResultType)^.ClassType);
  AssertEquals('call conv', Ord(ccCdecl), Ord(VMethod.CConv));
end;

procedure TTestWSMethodData.InvalidTypeInfo;
begin
  try
    CreateMethodData(TypeInfo(string));
    Fail('EJCoreWS(3103) expected');
  except
    on E: EJCoreWS do
      if E.Code <> 3103 then
        raise;
  end;
end;

procedure TTestWSMethodData.IsProcedureConstObject;
var
  VMethod: TJCoreWSMethodData;
begin
  //procedure P04(const AObject: TObject);
  VMethod := CreateMethodData(TypeInfo(@P04));
  AssertTrue('is procedure with object', VMethod.IsProcedureConstObject);
end;

procedure TTestWSMethodData.IsFunctionAsObject;
var
  VMethod: TJCoreWSMethodData;
begin
  //function F01: TObject;
  VMethod := CreateMethodData(TypeInfo(@F01));
  AssertTrue('is function as object', VMethod.IsFunctionAsObject);
end;

initialization
  RegisterTest('jcore.ws.invokers', TTestWSMethodData);

end.

