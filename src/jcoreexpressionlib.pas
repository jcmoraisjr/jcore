(*
  JCore, Expression Library Classes
  Copyright (C) 2012 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreExpressionLib;

{$I jcore.inc}

interface

uses
  Classes,
  JCoreExpression;

type

  { TJCoreExpressionLibrary }

  TJCoreExpressionLibrary = class(TObject)
  private
    FFunctions: TStringList;
    FOperations: TStringList;
  public
    destructor Destroy; override;
    function FindFunctionClass(const AFunctionToken: string): TJCoreExpressionFunctionClass;
    function FindOperationClass(const AOperationToken: string): TJCoreExpressionOperationClass;
    procedure RegisterFunctions(AFunctions: array of TJCoreExpressionFunctionClass);
    procedure RegisterOperations(AOperations: array of TJCoreExpressionOperationClass);
    procedure UnregisterFunctions(AFunctions: array of TJCoreExpressionFunctionClass);
    procedure UnregisterOperations(AOperations: array of TJCoreExpressionOperationClass);
  end;

  TJCoreExpressionAddOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionSubtractOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionMultiplyOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionDivideOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionIntDivOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionPowerOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionGreaterThanOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionLesserThanOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionGreaterThanOrEqualOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionLesserThanOrEqualOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionEqualOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionDiffOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionAndOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionOrOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionXorOperation = class(TJCoreExpressionOperation)
  protected
    class function InternalOperatorToken: string; override;
  public
    function Priority: Byte; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionMinFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionMaxFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionSqrtFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionPowerFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionIntPowerFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

  TJCoreExpressionIfFunction = class(TJCoreExpressionFunction)
  public
    function MaxParams: Integer; override;
    function MinParams: Integer; override;
    class function Name: string; override;
    procedure VarCalc; override;
  end;

function JCoreExpressionLibrary: TJCoreExpressionLibrary;

implementation

uses
  Math;

var
  _ExpressionLibrary: TJCoreExpressionLibrary;

function JCoreExpressionLibrary: TJCoreExpressionLibrary;
begin
  if not Assigned(_ExpressionLibrary) then
    _ExpressionLibrary := TJCoreExpressionLibrary.Create;
  Result := _ExpressionLibrary;
end;

{ TJCoreExpressionLibrary }

destructor TJCoreExpressionLibrary.Destroy;
begin
  FFunctions.Free;
  FOperations.Free;
  inherited;
end;

function TJCoreExpressionLibrary.FindFunctionClass(
  const AFunctionToken: string): TJCoreExpressionFunctionClass;
var
  VIndex: Integer;
begin
  if Assigned(FFunctions) and FFunctions.Find(AFunctionToken, VIndex) then
    Result := TJCoreExpressionFunctionClass(FFunctions.Objects[VIndex])
  else
    Result := nil;
end;

function TJCoreExpressionLibrary.FindOperationClass(
  const AOperationToken: string): TJCoreExpressionOperationClass;
var
  VIndex: Integer;
begin
  if Assigned(FOperations) and FOperations.Find(AOperationToken, VIndex) then
    Result := TJCoreExpressionOperationClass(FOperations.Objects[VIndex])
  else
    Result := nil;
end;

procedure TJCoreExpressionLibrary.RegisterFunctions(
  AFunctions: array of TJCoreExpressionFunctionClass);
var
  I: Integer;
begin
  if not Assigned(FFunctions) then
  begin
    FFunctions := TStringList.Create;
    FFunctions.Sorted := True;
  end;
  for I := 0 to Pred(Length(AFunctions)) do
    FFunctions.AddObject(AFunctions[I].Name, TObject(AFunctions[I]));
end;

procedure TJCoreExpressionLibrary.RegisterOperations(
  AOperations: array of TJCoreExpressionOperationClass);
var
  I: Integer;
begin
  if not Assigned(FOperations) then
  begin
    FOperations := TStringList.Create;
    FOperations.Sorted := True;
  end;
  for I := 0 to Pred(Length(AOperations)) do
    FOperations.AddObject(AOperations[I].Token, TObject(AOperations[I]));
end;

procedure TJCoreExpressionLibrary.UnregisterFunctions(
  AFunctions: array of TJCoreExpressionFunctionClass);
var
  VIndex, I: Integer;
begin
  if not Assigned(FFunctions) then
    Exit;
  for I := 0 to Pred(Length(AFunctions)) do
  begin
    VIndex := FFunctions.IndexOfObject(TObject(AFunctions[I]));
    if VIndex >= 0 then
      FFunctions.Delete(VIndex);
  end;
end;

procedure TJCoreExpressionLibrary.UnregisterOperations(
  AOperations: array of TJCoreExpressionOperationClass);
var
  VIndex, I: Integer;
begin
  if not Assigned(FOperations) then
    Exit;
  for I := 0 to Pred(Length(AOperations)) do
  begin
    VIndex := FOperations.IndexOfObject(TObject(AOperations[I]));
    if VIndex >= 0 then
      FOperations.Delete(VIndex);
  end;
end;

{ TJCoreExpressionAddOperation }

class function TJCoreExpressionAddOperation.InternalOperatorToken: string;
begin
  Result := '+';
end;

function TJCoreExpressionAddOperation.Priority: Byte;
begin
  Result := 10;
end;

procedure TJCoreExpressionAddOperation.VarCalc;
begin
  Res^ := Val1^ + Val2^;
end;

{ TJCoreExpressionSubtractOperation }

class function TJCoreExpressionSubtractOperation.InternalOperatorToken: string;
begin
  Result := '-';
end;

function TJCoreExpressionSubtractOperation.Priority: Byte;
begin
  Result := 10;
end;

procedure TJCoreExpressionSubtractOperation.VarCalc;
begin
  Res^ := Val1^ - Val2^;
end;

{ TJCoreExpressionMultiplyOperation }

class function TJCoreExpressionMultiplyOperation.InternalOperatorToken: string;
begin
  Result := '*';
end;

function TJCoreExpressionMultiplyOperation.Priority: Byte;
begin
  Result := 15;
end;

procedure TJCoreExpressionMultiplyOperation.VarCalc;
begin
  Res^ := Val1^ * Val2^;
end;

{ TJCoreExpressionDivideOperation }

class function TJCoreExpressionDivideOperation.InternalOperatorToken: string;
begin
  Result := '/';
end;

function TJCoreExpressionDivideOperation.Priority: Byte;
begin
  Result := 15;
end;

procedure TJCoreExpressionDivideOperation.VarCalc;
begin
  Res^ := Val1^ / Val2^;
end;

{ TJCoreExpressionIntDivOperation }

class function TJCoreExpressionIntDivOperation.InternalOperatorToken: string;
begin
  Result := 'div';
end;

function TJCoreExpressionIntDivOperation.Priority: Byte;
begin
  Result := 15;
end;

procedure TJCoreExpressionIntDivOperation.VarCalc;
begin
  Res^ := Val1^ div Val2^;
end;

{ TJCoreExpressionPowerOperation }

class function TJCoreExpressionPowerOperation.InternalOperatorToken: string;
begin
  Result := '^';
end;

function TJCoreExpressionPowerOperation.Priority: Byte;
begin
  Result := 20;
end;

procedure TJCoreExpressionPowerOperation.VarCalc;
begin
  Res^ := Power(Val1^, Val2^);
end;

{ TJCoreExpressionGreaterThanOperation }

class function TJCoreExpressionGreaterThanOperation.InternalOperatorToken: string;
begin
  Result := '>';
end;

function TJCoreExpressionGreaterThanOperation.Priority: Byte;
begin
  Result := 5;
end;

procedure TJCoreExpressionGreaterThanOperation.VarCalc;
begin
  Res^ := Val1^ > Val2^;
end;

{ TJCoreExpressionLesserThanOperation }

class function TJCoreExpressionLesserThanOperation.InternalOperatorToken: string;
begin
  Result := '<';
end;

function TJCoreExpressionLesserThanOperation.Priority: Byte;
begin
  Result := 5;
end;

procedure TJCoreExpressionLesserThanOperation.VarCalc;
begin
  Res^ := Val1^ < Val2^;
end;

{ TJCoreExpressionGreaterThanOrEqualOperation }

class function TJCoreExpressionGreaterThanOrEqualOperation.InternalOperatorToken: string;
begin
  Result := '>=';
end;

function TJCoreExpressionGreaterThanOrEqualOperation.Priority: Byte;
begin
  Result := 5;
end;

procedure TJCoreExpressionGreaterThanOrEqualOperation.VarCalc;
begin
  Res^ := Val1^ >= Val2^;
end;

{ TJCoreExpressionLesserThanOrEqualOperation }

class function TJCoreExpressionLesserThanOrEqualOperation.InternalOperatorToken: string;
begin
  Result := '<=';
end;

function TJCoreExpressionLesserThanOrEqualOperation.Priority: Byte;
begin
  Result := 5;
end;

procedure TJCoreExpressionLesserThanOrEqualOperation.VarCalc;
begin
  Res^ := Val1^ <= Val2^;
end;

{ TJCoreExpressionEqualOperation }

class function TJCoreExpressionEqualOperation.InternalOperatorToken: string;
begin
  Result := '=';
end;

function TJCoreExpressionEqualOperation.Priority: Byte;
begin
  Result := 5;
end;

procedure TJCoreExpressionEqualOperation.VarCalc;
begin
  Res^ := Val1^ = Val2^;
end;

{ TJCoreExpressionDiffOperation }

class function TJCoreExpressionDiffOperation.InternalOperatorToken: string;
begin
  Result := '<>';
end;

function TJCoreExpressionDiffOperation.Priority: Byte;
begin
  Result := 5;
end;

procedure TJCoreExpressionDiffOperation.VarCalc;
begin
  Res^ := Val1^ <> Val2^;
end;

{ TJCoreExpressionAndOperation }

class function TJCoreExpressionAndOperation.InternalOperatorToken: string;
begin
  Result := 'and';
end;

function TJCoreExpressionAndOperation.Priority: Byte;
begin
  Result := 15;
end;

procedure TJCoreExpressionAndOperation.VarCalc;
begin
  Res^ := Val1^ and Val2^;
end;

{ TJCoreExpressionOrOperation }

class function TJCoreExpressionOrOperation.InternalOperatorToken: string;
begin
  Result := 'or';
end;

function TJCoreExpressionOrOperation.Priority: Byte;
begin
  Result := 10;
end;

procedure TJCoreExpressionOrOperation.VarCalc;
begin
  Res^ := Val1^ or Val2^;
end;

{ TJCoreExpressionXorOperation }

class function TJCoreExpressionXorOperation.InternalOperatorToken: string;
begin
  Result := 'xor';
end;

function TJCoreExpressionXorOperation.Priority: Byte;
begin
  Result := 10;
end;

procedure TJCoreExpressionXorOperation.VarCalc;
begin
  Res^ := Val1^ xor Val2^;
end;

{ TJCoreExpressionMinFunction }

function TJCoreExpressionMinFunction.MaxParams: Integer;
begin
  Result := 2;
end;

function TJCoreExpressionMinFunction.MinParams: Integer;
begin
  Result := 2;
end;

class function TJCoreExpressionMinFunction.Name: string;
begin
  Result := 'Min';
end;

procedure TJCoreExpressionMinFunction.VarCalc;
begin
  if Params[0]^ < Params[1]^ then
    Res^ := Params[0]^
  else
    Res^ := Params[1]^;
end;

{ TJCoreExpressionMaxFunction }

function TJCoreExpressionMaxFunction.MaxParams: Integer;
begin
  Result := 2;
end;

function TJCoreExpressionMaxFunction.MinParams: Integer;
begin
  Result := 2;
end;

class function TJCoreExpressionMaxFunction.Name: string;
begin
  Result := 'Max';
end;

procedure TJCoreExpressionMaxFunction.VarCalc;
begin
  if Params[0]^ > Params[1]^ then
    Res^ := Params[0]^
  else
    Res^ := Params[1]^;
end;

{ TJCoreExpressionSqrtFunction }

function TJCoreExpressionSqrtFunction.MaxParams: Integer;
begin
  Result := 1;
end;

function TJCoreExpressionSqrtFunction.MinParams: Integer;
begin
  Result := 1;
end;

class function TJCoreExpressionSqrtFunction.Name: string;
begin
  Result := 'Sqrt';
end;

procedure TJCoreExpressionSqrtFunction.VarCalc;
begin
  Res^ := Sqrt(Params[0]^);
end;

{ TJCoreExpressionPowerFunction }

function TJCoreExpressionPowerFunction.MaxParams: Integer;
begin
  Result := 2;
end;

function TJCoreExpressionPowerFunction.MinParams: Integer;
begin
  Result := 2;
end;

class function TJCoreExpressionPowerFunction.Name: string;
begin
  Result := 'Power';
end;

procedure TJCoreExpressionPowerFunction.VarCalc;
begin
  Res^ := Power(Params[0]^, Params[1]^);
end;

{ TJCoreExpressionIntPowerFunction }

function TJCoreExpressionIntPowerFunction.MaxParams: Integer;
begin
  Result := 2;
end;

function TJCoreExpressionIntPowerFunction.MinParams: Integer;
begin
  Result := 2;
end;

class function TJCoreExpressionIntPowerFunction.Name: string;
begin
  Result := 'IntPower';
end;

procedure TJCoreExpressionIntPowerFunction.VarCalc;
begin
  Res^ := IntPower(Params[0]^, Params[1]^);
end;

{ TJCoreExpressionIfFunction }

function TJCoreExpressionIfFunction.MaxParams: Integer;
begin
  Result := 3;
end;

function TJCoreExpressionIfFunction.MinParams: Integer;
begin
  Result := 3;
end;

class function TJCoreExpressionIfFunction.Name: string;
begin
  Result := 'If';
end;

procedure TJCoreExpressionIfFunction.VarCalc;
begin
  if Params[0]^ then
    Res^ := Params[1]^
  else
    Res^ := Params[2]^;
end;

initialization
  JCoreExpressionLibrary.RegisterOperations([
   TJCoreExpressionAddOperation,
   TJCoreExpressionSubtractOperation,
   TJCoreExpressionMultiplyOperation,
   TJCoreExpressionDivideOperation,
   TJCoreExpressionIntDivOperation,
   TJCoreExpressionPowerOperation,
   TJCoreExpressionGreaterThanOperation,
   TJCoreExpressionGreaterThanOrEqualOperation,
   TJCoreExpressionLesserThanOperation,
   TJCoreExpressionLesserThanOrEqualOperation,
   TJCoreExpressionEqualOperation,
   TJCoreExpressionDiffOperation,
   TJCoreExpressionAndOperation,
   TJCoreExpressionOrOperation,
   TJCoreExpressionXorOperation]);
  JCoreExpressionLibrary.RegisterFunctions([
   TJCoreExpressionMinFunction,
   TJCoreExpressionMaxFunction,
   TJCoreExpressionSqrtFunction,
   TJCoreExpressionPowerFunction,
   TJCoreExpressionIntPowerFunction,
   TJCoreExpressionIfFunction]);

finalization
  _ExpressionLibrary.Free;

end.
