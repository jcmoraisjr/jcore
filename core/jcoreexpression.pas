(*
  JCore, Expression Parser Classes
  Copyright (C) 2013 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreExpression;

{$I jcore.inc}

interface

uses
  Variants,
  Contnrs,
  fgl,
  JCoreClasses,
  JCoreParser;

type
  TJCoreExpressionValue = Variant;
  PJCoreExpressionValue = ^TJCoreExpressionValue;
  PJCoreExpressionValueArray = array of PJCoreExpressionValue;

  TJCoreExpressionReader = class(TJCoreParserReader)
  protected
    function InternalCreateBigSymbolsArray: TJCoreStringArray; override;
  end;

  TJCoreExpressionObject = class(TJCoreParserObject)
  end;

  TJCoreExpressionVar = class;
  TJCoreExpressionVarList = specialize TFPGObjectList<TJCoreExpressionVar>;
  TJCoreExpressionVars = class;
  TJCoreExpressionItem = class;
  TJCoreExpressionItemClass = class of TJCoreExpressionItem;
  TJCoreExpressionOperation = class;

  { TJCoreExpression }

  TJCoreExpression = class(TJCoreExpressionObject)
  private
    FCalc: TObjectList;
    FRes: PJCoreExpressionValue;
    FVars: TJCoreExpressionVars;
    function DoParseExpression(Reader: TJCoreExpressionReader): PJCoreExpressionValue;
    function GetVars: TJCoreExpressionVars;
    function GetVarValue: Variant;
    function ParseFunction(Reader: TJCoreParserReader): TJCoreExpressionItem;
    function ParseItem(Reader: TJCoreParserReader): TJCoreExpressionItem;
    function ParseRightOperands(Reader: TJCoreParserReader; var ALeftItem: TJCoreExpressionItem; ADepth: Integer): PJCoreExpressionValue;
    function ParseVar(Reader: TJCoreParserReader): TJCoreExpressionItem;
  protected
    function InternalParseOperand(Reader: TJCoreParserReader): TJCoreExpressionItem; virtual;
    function InternalParseOperation(Reader: TJCoreParserReader): TJCoreExpressionOperation; virtual;
    procedure InternalRead(Reader: TJCoreParserReader); override;
  public
    constructor Create;
    constructor Create(AExpression: string);
    destructor Destroy; override;
    function ParseExpression(AExpression: string): PJCoreExpressionValue;
    function ParseExpression(Reader: TJCoreExpressionReader): PJCoreExpressionValue;
    property Calc: TObjectList read FCalc;
    property Res: PJCoreExpressionValue read FRes;
    property Vars: TJCoreExpressionVars read GetVars;
    property VarValue: Variant read GetVarValue;
  end;

  TJCoreExpressionVar = class(TObject)
  private
    FName: string;
    FValue: TJCoreExpressionValue;
    function GetValuePtr: PJCoreExpressionValue;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
    property Value: TJCoreExpressionValue read FValue write FValue;
    property ValuePtr: PJCoreExpressionValue read GetValuePtr;
  end;

  TJCoreExpressionVars = class(TJCoreExpressionVarList)
  private
    function GetVariable(const AVarName: string): TJCoreExpressionValue;
    procedure SetVariable(const AVarName: string; Value: TJCoreExpressionValue);
  public
    function IndexOf(const AVarName: string): Integer;
    function FindVar(const AVarName: string): TJCoreExpressionVar;
    function VarByName(const AVarName: string): TJCoreExpressionVar;
    property Variable[const AVarName: string]: TJCoreExpressionValue read GetVariable write SetVariable; default;
  end;

  TJCoreExpressionItem = class(TJCoreExpressionObject)
  private
    FNextOperation: TJCoreExpressionOperation;
    FRes: PJCoreExpressionValue;
  protected
    procedure InternalRead(Reader: TJCoreParserReader); override;
  public
    property NextOperation: TJCoreExpressionOperation read FNextOperation;
    property Res: PJCoreExpressionValue read FRes write FRes;
  end;

  TJCoreExpressionBracketItem = class(TJCoreExpressionItem)
  protected
    class function InternalApply(Reader: TJCoreParserReader): Boolean; override;
    procedure InternalRead(Reader: TJCoreParserReader); override;
  end;

  TJCoreExpressionLiteralItem = class(TJCoreExpressionItem)
  private
    FLiteral: TJCoreExpressionValue;
  protected
    class function InternalApply(Reader: TJCoreParserReader): Boolean; override;
    procedure InternalRead(Reader: TJCoreParserReader); override;
  end;

  TJCoreExpressionFunction = class;

  TJCoreExpressionFunctionItem = class(TJCoreExpressionItem)
  private
    FFnc: TJCoreExpressionFunction;
  protected
    procedure InternalRead(Reader: TJCoreParserReader); override;
  public
    property Fnc: TJCoreExpressionFunction read FFnc write FFnc;
  end;

  TJCoreExpressionVarItem = class(TJCoreExpressionItem)
  private
    FVariable: TJCoreExpressionVar;
  protected
    procedure InternalRead(Reader: TJCoreParserReader); override;
  public
    property Variable: TJCoreExpressionVar read FVariable write FVariable;
  end;

  TJCoreExpressionCalc = class(TJCoreExpressionObject)
  public
    procedure VarCalc; virtual; abstract;
  end;

  TJCoreExpressionOperationClass = class of TJCoreExpressionOperation;

  TJCoreExpressionOperation = class(TJCoreExpressionCalc)
  private
    FRes: TJCoreExpressionValue;
    FVal1: PJCoreExpressionValue;
    FVal2: PJCoreExpressionValue;
    function GetRes: PJCoreExpressionValue;
  protected
    class function InternalOperatorToken: string; virtual; abstract;
    procedure InternalRead(Reader: TJCoreParserReader); override;
  public
    function Priority: Byte; virtual; abstract;
    class function Token: string;
    property Res: PJCoreExpressionValue read GetRes;
    property Val1: PJCoreExpressionValue read FVal1 write FVal1;
    property Val2: PJCoreExpressionValue read FVal2 write FVal2;
  end;

  TJCoreExpressionFunctionClass = class of TJCoreExpressionFunction;

  TJCoreExpressionFunction = class(TJCoreExpressionCalc)
  private
    FParams: PJCoreExpressionValueArray;
    FRes: TJCoreExpressionValue;
    function GetRes: PJCoreExpressionValue;
  protected
    procedure InternalRead(Reader: TJCoreParserReader); override;
  public
    function MaxParams: Integer; virtual;
    function MinParams: Integer; virtual; abstract;
    class function Name: string; virtual; abstract;
    property Params: PJCoreExpressionValueArray read FParams write FParams;
    property Res: PJCoreExpressionValue read GetRes;
  end;

implementation

uses
  SysUtils,
  JCoreConsts,
  JCoreExpressionLib;

{ TJCoreExpressionReader }

function TJCoreExpressionReader.InternalCreateBigSymbolsArray: TJCoreStringArray;
begin
  SetLength(Result, 3);
  Result[0] := '<=';
  Result[1] := '>=';
  Result[2] := '<>';
end;

{ TJCoreExpression }

destructor TJCoreExpression.Destroy;
begin
  FreeAndNil(FVars);
  FreeAndNil(FCalc);
  inherited;
end;

function TJCoreExpression.DoParseExpression(Reader: TJCoreExpressionReader
  ): PJCoreExpressionValue;
var
  VItem: TJCoreExpressionItem;
begin
  VItem := ParseItem(Reader);
  FRes := ParseRightOperands(Reader, VItem, 0);
  Result := FRes;
end;

function TJCoreExpression.GetVars: TJCoreExpressionVars;
begin
  if not Assigned(FVars) then
    FVars := TJCoreExpressionVars.Create(True);
  Result := FVars;
end;

function TJCoreExpression.GetVarValue: Variant;
var
  I: Integer;
begin
  if Assigned(FRes) then
  begin
    if Assigned(FCalc) then
      for I := 0 to Pred(FCalc.Count) do
        TJCoreExpressionCalc(FCalc[I]).VarCalc;
    Result := FRes^;
  end else
    Result := varEmpty;
end;

function TJCoreExpression.InternalParseOperand(
  Reader: TJCoreParserReader): TJCoreExpressionItem;
begin
  Result := TJCoreExpressionItem(Parse(Reader, [
   TJCoreExpressionBracketItem, TJCoreExpressionLiteralItem]));
  if not Assigned(Result) then
  begin
    Result := ParseFunction(Reader);
    if not Assigned(Result) then
      Result := ParseVar(Reader);
  end;
end;

function TJCoreExpression.InternalParseOperation(
  Reader: TJCoreParserReader): TJCoreExpressionOperation;
var
  VOperationClass: TJCoreExpressionOperationClass;
begin
  VOperationClass :=
   JCoreExpressionLibrary.FindOperationClass(Reader.ReadNextToken);
  if Assigned(VOperationClass) then
    Result := TJCoreExpressionOperation(Parse(Reader, [VOperationClass]))
  else
    Result := nil;
end;

procedure TJCoreExpression.InternalRead(Reader: TJCoreParserReader);
begin
  inherited;
  DoParseExpression(Reader as TJCoreExpressionReader);
  Reader.ReadMatchEof;
end;

constructor TJCoreExpression.Create;
begin
  inherited Create(nil);
  FCalc := TObjectList.Create(False);
end;

constructor TJCoreExpression.Create(AExpression: string);
begin
  Create;
  ParseExpression(AExpression);
end;

function TJCoreExpression.ParseExpression(
  AExpression: string): PJCoreExpressionValue;
var
  VReader: TJCoreExpressionReader;
begin
  VReader := TJCoreExpressionReader.Create(AExpression);
  try
    Result := ParseExpression(VReader);
  finally
    FreeAndNil(VReader);
  end;
end;

function TJCoreExpression.ParseExpression(
  Reader: TJCoreExpressionReader): PJCoreExpressionValue;
begin
  Result := DoParseExpression(Reader);
  Reader.ReadMatchEof;
end;

function TJCoreExpression.ParseFunction(
  Reader: TJCoreParserReader): TJCoreExpressionItem;
var
  VFunctionParser: TJCoreExpressionFunctionItem;
  VFunctionClass: TJCoreExpressionFunctionClass;
  VFunction: TJCoreExpressionFunction;
  VPosition: TJCoreTextPos;
  VIsFunction: Boolean;
  Token: string;
begin
  Result := nil;
  VPosition := Reader.Position;
  Token := Reader.ReadToken;
  VIsFunction :=
   IsValidIdent(Token) and not Reader.Eof and (Reader.ReadChar = '(');
  Reader.Position := VPosition;
  if VIsFunction then
  begin
    VFunctionClass := JCoreExpressionLibrary.FindFunctionClass(Token);
    if not Assigned(VFunctionClass) then
      Exit;
    VFunctionParser := TJCoreExpressionFunctionItem.Create(Self);
    VFunction := VFunctionClass.Create(Self);
    VFunctionParser.Fnc := VFunction;
    VFunctionParser.Read(Reader);
    Calc.Add(VFunction);
    Result := VFunctionParser;
  end;
end;

function TJCoreExpression.ParseItem(
  Reader: TJCoreParserReader): TJCoreExpressionItem;
begin
  Result := InternalParseOperand(Reader);
  if not Assigned(Result) then
    Reader.ErrorExpected(SJCoreIdentifierMsg, Reader.ReadToken);
  Result.FNextOperation := InternalParseOperation(Reader);
end;

function TJCoreExpression.ParseRightOperands(Reader: TJCoreParserReader;
  var ALeftItem: TJCoreExpressionItem; ADepth: Integer): PJCoreExpressionValue;
var
  VRightItem: TJCoreExpressionItem;
  VCurrentOp, VNextOp: TJCoreExpressionOperation;
  VLeftOperand: PJCoreExpressionValue;
begin
  VLeftOperand := ALeftItem.Res;
  VCurrentOp := ALeftItem.NextOperation;
  while Assigned(VCurrentOp) do
  begin
    VRightItem := ParseItem(Reader);
    VNextOp := VRightItem.NextOperation;
    VCurrentOp.Val1 := VLeftOperand;
    if Assigned(VNextOp) and (VCurrentOp.Priority < VNextOp.Priority) then
      VCurrentOp.Val2 := ParseRightOperands(Reader, VRightItem, Succ(ADepth))
    else
      VCurrentOp.Val2 := VRightItem.Res;
    VLeftOperand := VCurrentOp.Res;
    Calc.Add(VCurrentOp);
    ALeftItem := VRightItem;
    if (ADepth = 0) or (Assigned(ALeftItem.NextOperation) and
     (VCurrentOp.Priority = ALeftItem.NextOperation.Priority)) then
      VCurrentOp := ALeftItem.NextOperation
    else
      VCurrentOp := nil;
  end;
  Result := VLeftOperand;
end;

function TJCoreExpression.ParseVar(
  Reader: TJCoreParserReader): TJCoreExpressionItem;
var
  VVar: TJCoreExpressionVar;
  VVarParser: TJCoreExpressionVarItem;
begin
  Result := nil;
  VVar := Vars.FindVar(Reader.ReadNextToken);
  if not Assigned(VVar) then
    Exit;
  VVarParser := TJCoreExpressionVarItem.Create(Self);
  VVarParser.Variable := VVar;
  VVarParser.Read(Reader);
  Result := VVarParser;
end;

{ TJCoreExpressionVar }

constructor TJCoreExpressionVar.Create(const AName: string);
begin
  if not IsValidIdent(AName) then
    raise EJCoreException.CreateFmt(SJCoreInvalidIdentifier, [AName]);
  inherited Create;
  FName := AName;
end;

function TJCoreExpressionVar.GetValuePtr: PJCoreExpressionValue;
begin
  Result := @FValue;
end;

{ TJCoreExpressionVars }

function TJCoreExpressionVars.FindVar(
  const AVarName: string): TJCoreExpressionVar;
var
  VIndex: Integer;
begin
  VIndex := IndexOf(AVarName);
  if VIndex >= 0 then
    Result := Items[VIndex]
  else
    Result := nil;
end;

function TJCoreExpressionVars.GetVariable(
  const AVarName: string): TJCoreExpressionValue;
begin
  Result := VarByName(AVarName).Value;
end;

function TJCoreExpressionVars.IndexOf(const AVarName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(AVarName, Items[Result].Name) then
      Exit;
  Result := -1;
end;

procedure TJCoreExpressionVars.SetVariable(
  const AVarName: string; Value: TJCoreExpressionValue);
begin
  VarByName(AVarName).Value := Value;
end;

function TJCoreExpressionVars.VarByName(
  const AVarName: string): TJCoreExpressionVar;
begin
  Result := FindVar(AVarName);
  if not Assigned(Result) then
    Result := Items[inherited Add(TJCoreExpressionVar.Create(AVarName))];
end;

{ TJCoreExpressionItem }

procedure TJCoreExpressionItem.InternalRead(Reader: TJCoreParserReader);
begin
  inherited;
end;

{ TJCoreExpressionBracketItem }

class function TJCoreExpressionBracketItem.InternalApply(
  Reader: TJCoreParserReader): Boolean;
begin
  Result := Reader.ReadToken = '(';
end;

procedure TJCoreExpressionBracketItem.InternalRead(
  Reader: TJCoreParserReader);
begin
  inherited;
  Reader.ReadMatch('(');
  Res := (Owner as TJCoreExpression).DoParseExpression(Reader as TJCoreExpressionReader);
  Reader.ReadMatch(')');
end;

{ TJCoreExpressionLiteralItem }

class function TJCoreExpressionLiteralItem.InternalApply(
  Reader: TJCoreParserReader): Boolean;
var
  Token: string;
begin
  Token := Reader.ReadToken;
  Result := (Token <> '') and (Token[1] in ['''', '"', '+', '-', '0'..'9']);
end;

procedure TJCoreExpressionLiteralItem.InternalRead(
  Reader: TJCoreParserReader);

  function AsFloat(const AStr: string): Double;
  var
    VErr: Integer;
  begin
    Val(AStr, Result, VErr);
    if VErr <> 0 then
      Reader.ErrorExpected(SJCoreNumberValueMsg, AStr);
  end;

var
  Token: string;
begin
  inherited;
  Token := Reader.ReadNextToken;
  if (Token <> '') and (Token[1] in ['''', '"']) then
    FLiteral := Reader.ReadUnquotedString
  else if Pos('.', Token) > 0 then
    FLiteral := AsFloat(Reader.ReadNumber)
  else
    FLiteral := Reader.ReadInteger;
  Res := @FLiteral;
end;

{ TJCoreExpressionFunctionItem }

procedure TJCoreExpressionFunctionItem.InternalRead(
  Reader: TJCoreParserReader);
const
  CDelta = 4;
var
  VParams: PJCoreExpressionValueArray;
  VMin, VMax, I: Integer;
begin
  inherited;
  Reader.ReadMatchText(Fnc.Name);
  Reader.ReadMatch('(');
  VMin := Fnc.MinParams;
  VMax := Fnc.MaxParams;
  I := 0;
  SetLength(VParams, CDelta);
  while (I <> VMax) and ((I < VMin) or (Reader.ReadNextToken <> ')')) do
  begin
    if I > 0 then
      Reader.ReadMatch(',');
    if Length(VParams) = I then
      SetLength(VParams, I + CDelta);
    VParams[I] := (Owner as TJCoreExpression).DoParseExpression(
     Reader as TJCoreExpressionReader);
    Inc(I);
  end;
  Reader.ReadMatch(')');
  SetLength(VParams, I);
  Fnc.Params := VParams;
  Res := Fnc.Res;
end;

{ TJCoreExpressionVarItem }

procedure TJCoreExpressionVarItem.InternalRead(Reader: TJCoreParserReader);
begin
  inherited;
  Reader.ReadMatchText(Variable.Name);
  Res := Variable.ValuePtr;
end;

{ TJCoreExpressionOperation }

function TJCoreExpressionOperation.GetRes: PJCoreExpressionValue;
begin
  Result := @FRes;
end;

procedure TJCoreExpressionOperation.InternalRead(Reader: TJCoreParserReader);
begin
  inherited;
  Reader.ReadMatchText(InternalOperatorToken);
end;

class function TJCoreExpressionOperation.Token: string;
begin
  Result := InternalOperatorToken;
end;

{ TJCoreExpressionFunction }

function TJCoreExpressionFunction.GetRes: PJCoreExpressionValue;
begin
  Result := @FRes;
end;

procedure TJCoreExpressionFunction.InternalRead(
  Reader: TJCoreParserReader);
begin
  inherited;
end;

function TJCoreExpressionFunction.MaxParams: Integer;
begin
  Result := -1;
end;

end.
