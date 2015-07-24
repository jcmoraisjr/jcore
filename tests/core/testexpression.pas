unit TestExpression;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  fpcunit,
  JCoreClasses,
  JCoreExpression;

type

  ETestExpressionError = class(EJCoreException);

  { TTestExpression }

  TTestExpression = class(TTestCase)
  private
    FExpression: TJCoreExpression;
    procedure AssertExpressionException(const AExpression: string; const AException: TJCoreExceptionClass; const ACode: Integer; const ACalcExpression: Boolean = True);
    procedure AssertExpressionNumber(const AExpression: string; AExpected: Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Expression1;
    procedure Expression2;
    procedure Expression3;
    procedure Expression4;
    procedure Expression5;
    procedure Expression6;
    procedure ExpressionWithInvalidNumber;
    procedure ExpressionWithoutCloseParenthese;
    procedure ExpressionWithoutOpenParenthese;
    procedure FunctionOneParam;
    procedure FunctionTwoParams;
    procedure FunctionWrongParamCount;
    procedure FunctionOptionalParams;
    procedure OperationIntMult;
    procedure OperationIntDiv;
    procedure OperationIntExp;
    procedure OperationIntExpNeg;
    procedure OperationIntSub;
    procedure OperationIntSum;
    procedure OperationFracSum;
    procedure OperationFracSub;
    procedure OperationFracMult;
    procedure OperationFracDiv;
    procedure OperationFracExp;
    procedure UserDefinedFunction;
    procedure UserDefinedOperation;
    procedure Variables;
    procedure CreatingParsing;
    procedure ParsingExecuting;
  end;

  { TTestError }

  TTestError = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  { TTestOptionalFunction }

  TTestOptionalFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  { TTestSinFunction }

  TTestSinFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  { TTestCosFunction }

  TTestCosFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  { TTestTanFunction }

  TTestTanFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  { TTestModOperation }

  TTestModOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

implementation

uses
  testregistry,
  Math,
  JCoreExpressionLib;

{ TTestError }

function TTestError.MaxParams: Integer;
begin
  Result := 0;
end;

function TTestError.MinParams: Integer;
begin
  Result := 0;
end;

class function TTestError.Name: string;
begin
  Result := 'error';
end;

procedure TTestError.VarCalc;
begin
  raise ETestExpressionError.Create(0, 'I am the TTestError class!', []);
end;

{ TTestExpression }

procedure TTestExpression.AssertExpressionException(const AExpression: string;
  const AException: TJCoreExceptionClass; const ACode: Integer; const ACalcExpression: Boolean);
var
  VExceptionName: string;
  VExceptionClass: string;
begin
  try
    FExpression.ParseExpression(AExpression);
    if ACalcExpression then
      FExpression.VarValue;
  except
    on E: Exception do
    begin
      if (E.ClassType = AException) and (EJCoreException(E).Code = ACode) then
        Exit;
      if Assigned(AException) then
        VExceptionName := Format('%s(%d)', [AException.ClassName, ACode])
      else
        VExceptionName := 'No exception';
      VExceptionClass := E.ClassName;
      if E is EJCoreException then
        VExceptionClass := Format('%s(%.4d)', [VExceptionClass, EJCoreException(E).Code]);
      Fail(VExceptionName + ' was expected but ' + VExceptionClass + ' was raised.');
    end;
  end;
  if Assigned(AException) then
    Fail(AException.ClassName + ' was expected but no exception was raised.');
end;

procedure TTestExpression.AssertExpressionNumber(
  const AExpression: string; AExpected: Double);
begin
  FExpression.ParseExpression(AExpression);
  AssertEquals(AExpected, FExpression.VarValue, 0.00000001);
end;

procedure TTestExpression.SetUp;
begin
  FExpression := TJCoreExpression.Create;
end;

procedure TTestExpression.TearDown;
begin
  FreeAndNil(FExpression);
end;

procedure TTestExpression.Expression1;
begin
  AssertExpressionNumber('(23+3*2*2)/5+(9.5-20)*2/3', 0);
end;

procedure TTestExpression.Expression2;
begin
  AssertExpressionNumber('15+9*4^3', 591);
end;

procedure TTestExpression.Expression3;
begin
  AssertExpressionNumber('(15+9)*4^3', 1536);
end;

procedure TTestExpression.Expression4;
begin
  AssertExpressionNumber('15+(9*4)^3', 46671);
end;

procedure TTestExpression.Expression5;
begin
  AssertExpressionNumber('(15+9*4)^3', 132651);
end;

procedure TTestExpression.Expression6;
begin
  AssertExpressionNumber('(15+9)*(4^3)', 1536);
end;

procedure TTestExpression.ExpressionWithInvalidNumber;
begin
  AssertExpressionException('10.5+10.25.5', EJCoreParser, 402);
end;

procedure TTestExpression.ExpressionWithoutCloseParenthese;
begin
  AssertExpressionException('(15+9)*(4^3', EJCoreParser, 402);
end;

procedure TTestExpression.ExpressionWithoutOpenParenthese;
begin
  AssertExpressionException('-3)^2', EJCoreParser, 402);
end;

procedure TTestExpression.FunctionOneParam;
begin
  AssertExpressionNumber('sqrt(49)', 7);
end;

procedure TTestExpression.FunctionTwoParams;
begin
  AssertExpressionNumber('min(10,20)', 10);
end;

procedure TTestExpression.FunctionWrongParamCount;
begin
  AssertExpressionException('min(1,2,3)', EJCoreParser, 402);
end;

procedure TTestExpression.FunctionOptionalParams;
begin
  JCoreExpressionLibrary.RegisterFunctions([TTestOptionalFunction]);
  try
    AssertExpressionNumber('optional(1,1)', 1);
    AssertExpressionNumber('optional(2,1,2)', 3);
    AssertExpressionNumber('optional(3,1,2,3)', 6);
    //AssertExpressionNumber('optional(3,1,2,3,4)', 0);
    AssertExpressionException('optional(3,1,2,3,4)', ETestExpressionError, 0);
  finally
    JCoreExpressionLibrary.UnregisterFunctions([TTestOptionalFunction]);
    AssertExpressionException('optional(2,1,2)', EJCoreParser, 402);
  end;
end;

procedure TTestExpression.OperationIntSum;
begin
  AssertExpressionNumber('101+11', 112);
end;

procedure TTestExpression.OperationIntSub;
begin
  AssertExpressionNumber('22-31', -9);
end;

procedure TTestExpression.OperationIntMult;
begin
  AssertExpressionNumber('3*7', 21);
end;

procedure TTestExpression.OperationIntDiv;
begin
  AssertExpressionNumber('25/7', 3.57142857);
end;

procedure TTestExpression.OperationIntExp;
begin
  AssertExpressionNumber('5^3', 125);
end;

procedure TTestExpression.OperationFracSum;
begin
  AssertExpressionNumber('1.125+3.625', 4.75);
end;

procedure TTestExpression.OperationFracSub;
begin
  AssertExpressionNumber('2.2-0.15', 2.05);
end;

procedure TTestExpression.OperationFracMult;
begin
  AssertExpressionNumber('3.25*7.5', 24.375);
end;

procedure TTestExpression.OperationFracDiv;
begin
  AssertExpressionNumber('2.5/0.75', 3.33333333);
end;

procedure TTestExpression.OperationFracExp;
begin
  AssertExpressionNumber('16^0.5', 4);
end;

procedure TTestExpression.OperationIntExpNeg;
begin
  AssertExpressionNumber('-3^2', 9);
end;

procedure TTestExpression.UserDefinedFunction;
begin
  JCoreExpressionLibrary.RegisterFunctions([TTestSinFunction,
   TTestCosFunction, TTestTanFunction]);
  try
    AssertExpressionNumber('tan(0.7854)-sin(0.7854)/cos(0.7854)', 0);
  finally
    JCoreExpressionLibrary.UnregisterFunctions([TTestSinFunction,
     TTestCosFunction, TTestTanFunction]);
    AssertExpressionException('tan(0.7854)', EJCoreParser, 402);
    AssertExpressionException('sin(0.7854)', EJCoreParser, 402);
    AssertExpressionException('cos(0.7854)', EJCoreParser, 402);
  end;
end;

procedure TTestExpression.UserDefinedOperation;
begin
  JCoreExpressionLibrary.RegisterOperations([TTestModOperation]);
  try
    AssertExpressionNumber('10 mod 3', 1);
  finally
    JCoreExpressionLibrary.UnregisterOperations([TTestModOperation]);
    AssertExpressionException('10 mod 3', EJCoreParser, 402);
  end;
end;

procedure TTestExpression.Variables;
begin
  FExpression.Vars.Variable['x'] := -3;
  FExpression.Vars.Variable['y'] := -6;
  AssertExpressionNumber('x^2-3*y-1', 26);
end;

procedure TTestExpression.CreatingParsing;
var
  VExpression: TJCoreExpression;
begin
  JCoreExpressionLibrary.RegisterFunctions([TTestError]);
  try
    try
      VExpression := TJCoreExpression.Create('errorinexistent()');
      FreeAndNil(VExpression);
      Fail('EJCoreParser(0402) expected');
    except
      on E: EJCoreParser do
        if E.Code <> 402 then
          raise;
    end;
  finally
    JCoreExpressionLibrary.UnregisterFunctions([TTestError]);
    AssertExpressionException('error()', EJCoreParser, 402, False);
  end;
end;

procedure TTestExpression.ParsingExecuting;
begin
  JCoreExpressionLibrary.RegisterFunctions([TTestError]);
  try
    AssertExpressionException('errorinexistent()', EJCoreParser, 402, False);
    AssertExpressionException('error()', nil, 0, False);
    AssertExpressionException('error()', ETestExpressionError, 0, True);
  finally
    JCoreExpressionLibrary.UnregisterFunctions([TTestError]);
    AssertExpressionException('error()', EJCoreParser, 402, False);
  end;
end;

{ TTestOptionalFunction }

function TTestOptionalFunction.MaxParams: Integer;
begin
  Result := 5;
end;

function TTestOptionalFunction.MinParams: Integer;
begin
  Result := 1;
end;

class function TTestOptionalFunction.Name: string;
begin
  Result := 'optional';
end;

procedure TTestOptionalFunction.VarCalc;
var
  I: Integer;
begin
  // First param is the number of params declared beyond the first one
  if Length(Params) <> (Params[0]^ + 1) then
    raise ETestExpressionError.Create(0, 'Wrong number of params', []);
  Res^ := 0;
  for I := 1 to Pred(Length(Params)) do
    Res^ := Res^ + Params[I]^;
end;

{ TTestSinFunction }

function TTestSinFunction.MaxParams: Integer;
begin
  Result := 1;
end;

function TTestSinFunction.MinParams: Integer;
begin
  Result := 1;
end;

class function TTestSinFunction.Name: string;
begin
  Result := 'sin';
end;

procedure TTestSinFunction.VarCalc;
begin
  Res^ := Sin(Params[0]^);
end;

{ TTestCosFunction }

function TTestCosFunction.MaxParams: Integer;
begin
  Result := 1;
end;

function TTestCosFunction.MinParams: Integer;
begin
  Result := 1;
end;

class function TTestCosFunction.Name: string;
begin
  Result := 'cos';
end;

procedure TTestCosFunction.VarCalc;
begin
  Res^ := Cos(Params[0]^);
end;

{ TTestTanFunction }

function TTestTanFunction.MaxParams: Integer;
begin
  Result := 1;
end;

function TTestTanFunction.MinParams: Integer;
begin
  Result := 1;
end;

class function TTestTanFunction.Name: string;
begin
  Result := 'tan';
end;

procedure TTestTanFunction.VarCalc;
begin
  Res^ := Tan(Params[0]^);
end;

{ TTestModOperation }

class function TTestModOperation.InternalOperatorToken: string;
begin
  Result := 'mod';
end;

function TTestModOperation.Priority: Byte;
begin
  Result := 15;
end;

procedure TTestModOperation.VarCalc;
begin
  Res^ := Val1^ mod Val2^;
end;

initialization
  RegisterTest('jcore.core.expression', TTestExpression);

end.

