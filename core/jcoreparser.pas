(*
  JCore, Base Parser Classes
  Copyright (C) 2013 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreParser;

{$I jcore.inc}

interface

uses
  fgl,
  JCoreClasses;

type
  TJCoreParserReader = class(TJCoreTextReader)
  private
    FBigSymbols: TJCoreStringArray;
    procedure CheckEof(const AErrorMsg: string);
  protected
    procedure InitReader; override;
    procedure InternalCheckComment({%H-}var AToken: string); virtual;
    function InternalReadIdentifier: string;
    function InternalReadNumber: string;
    function InternalReadString: string;
    function InternalReadSymbol: string;
    function InternalReadToken: string; override;
    function InternalCreateBigSymbolsArray: TJCoreStringArray; virtual;
    function IsIdentifierChar(Ch: Char; First: Boolean): Boolean;
    function IsNumericChar(Ch: Char; First: Boolean): Boolean;
    function IsStringDelimiter(Ch: Char): Boolean;
  public
    function ReadBoolean: Boolean;
    function ReadIdentifier: string;
    function ReadInteger: Integer;
    function ReadNext(const AToken: string; AInclude: Boolean): string; overload;
    function ReadNext(const ATokens: array of string; AInclude: Boolean): string; overload;
    function ReadNumber: string;
    function ReadPath: string;
    function ReadString: string;
    function ReadUnquotedString: string;
  end;

  TJCoreParserObject = class;
  PJCoreParserObject = ^TJCoreParserObject;
  TJCoreParserClass = class of TJCoreParserObject;
  TJCoreParserList = specialize TFPGObjectList<TJCoreParserObject>;

  { TJCoreParserObject }

  TJCoreParserObject = class(TObject)
  private
    FItemList: TJCoreParserList;
    FOwner: TJCoreParserObject;
    FParent: TJCoreParserObject;
    function GetItemList: TJCoreParserList;
    function GetItems(AIndex: Integer): TJCoreParserObject;
  protected
    function FindRule(Reader: TJCoreParserReader; AClasses: array of TJCoreParserClass): TJCoreParserClass;
    class function InternalApply({%H-}Reader: TJCoreParserReader): Boolean; virtual;
    function InternalCreateObject(AClass: TJCoreParserClass): TJCoreParserObject; virtual;
    procedure InternalRead({%H-}Reader: TJCoreParserReader); virtual;
    function Parse(Reader: TJCoreParserReader; AParserClasses: array of TJCoreParserClass; AParent: TJCoreParserObject = nil; AMandatory: Boolean = False; const AErrorExpectedMsg: string = ''): TJCoreParserObject;
    property ItemList: TJCoreParserList read GetItemList;
  public
    constructor Create(AOwner: TJCoreParserObject); virtual;
    destructor Destroy; override;
    class function Apply(Reader: TJCoreParserReader): Boolean;
    procedure ExtractItem(AItem: TJCoreParserObject);
    function ItemCount: Integer;
    procedure Read(Reader: TJCoreParserReader);
    property Items[AIndex: Integer]: TJCoreParserObject read GetItems; default;
    property Owner: TJCoreParserObject read FOwner;
    property Parent: TJCoreParserObject read FParent write FParent;
  end;

implementation

uses
  SysUtils,
  JCoreConsts;

{ TJCoreParserReader }

procedure TJCoreParserReader.CheckEof(const AErrorMsg: string);
begin
  SkipSpaces;
  if Eof then
    ErrorExpected(AErrorMsg, '');
end;

procedure TJCoreParserReader.InitReader;
begin
  inherited;
  FBigSymbols := InternalCreateBigSymbolsArray;
end;

procedure TJCoreParserReader.InternalCheckComment(var AToken: string);
begin
end;

function TJCoreParserReader.InternalCreateBigSymbolsArray: TJCoreStringArray;
begin
  SetLength(Result, 0);
end;

function TJCoreParserReader.InternalReadIdentifier: string;
var
  Ch: Char;
  VIdent: ShortString;
  VLen: Byte;
begin
  VLen := 0;
  Ch := ReadChar;
  repeat
    Inc(VLen);
    if VLen = Pred(High(VLen)) then
      ErrorMsg(SJCoreTokenLengthOutOfBounds);
    VIdent[VLen] := Ch;
    if Eof then
    begin
      VIdent[0] := Char(VLen);
      Result := VIdent;
      Exit;
    end;
    Ch := ReadChar;
  until not IsIdentifierChar(Ch, False);
  UnreadChar;
  VIdent[0] := Char(VLen);
  Result := VIdent;
end;

function TJCoreParserReader.InternalReadNumber: string;
var
  Ch: Char;
  VNumStr: ShortString;
  VLen: Byte;
begin
  { TODO : 1.5.6 will result as a Number }
  VLen := 0;
  Ch := ReadChar;
  repeat
    Inc(VLen);
    if VLen = Pred(High(VLen)) then
      ErrorMsg(SJCoreTokenLengthOutOfBounds);
    VNumStr[VLen] := Ch;
    if Eof then
    begin
      VNumStr[0] := Char(VLen);
      Result := VNumStr;
      Exit;
    end;
    Ch := ReadChar;
  until not IsNumericChar(Ch, False);
  UnreadChar;
  VNumStr[0] := Char(VLen);
  Result := VNumStr;
end;

function TJCoreParserReader.InternalReadString: string;

  procedure SafeInc(var AVar: Byte);
  begin
    Inc(AVar);
    if AVar = Pred(High(AVar)) then
      ErrorMsg(SJCoreStringLengthOutOfBounds);
  end;

var
  Ch, Delimiter: Char;
  VStr: ShortString;
  VLen: Byte;
begin
  Delimiter := ReadChar;
  VStr[1] := Delimiter;
  VLen := 1;
  while True do
  begin
    Ch := ReadChar;
    if Ch in [#10, #13] then
      ErrorExpected(SJCoreStringDelimiterMsg, SJCoreLineBreakMsg);
    SafeInc(VLen);
    VStr[VLen] := Ch;
    if Ch = Delimiter then
    begin
      if Eof then
        Break;
      Ch := ReadChar;
      if Ch <> Delimiter then
      begin
        UnreadChar;
        Break;
      end else
      begin
        SafeInc(VLen);
        VStr[VLen] := Ch;
      end;
    end;
  end;
  VStr[0] := Char(VLen);
  Result := VStr;
end;

function TJCoreParserReader.InternalReadSymbol: string;

  function IsSymbol(const ASymbol: string): Boolean;
  var
    VLen: Integer;
    I: Integer;
  begin
    Result := True;
    VLen := Length(ASymbol);
    for I := Low(FBigSymbols) to High(FBigSymbols) do
      if Copy(FBigSymbols[I], 1, VLen) = ASymbol then
        Exit;
    Result := False;
  end;

var
  Ch: Char;
begin
  Result := '';
  Ch := ReadChar;
  repeat
    Result := Result + Ch;
    Ch := ReadChar;
  until not IsSymbol(Result + Ch);
  UnreadChar;
end;

function TJCoreParserReader.InternalReadToken: string;
var
  Ch: Char;
begin
  Result := '';
  Ch := ReadChar;
  if Eof then
    Result := Ch
  else
  begin
    UnreadChar;
    if IsStringDelimiter(Ch) then
      Result := InternalReadString
    else if IsNumericChar(Ch, True) then
      Result := InternalReadNumber
    else if IsIdentifierChar(Ch, True) then
      Result := InternalReadIdentifier
    else
      Result := InternalReadSymbol;
    InternalCheckComment(Result);
  end;
end;

function TJCoreParserReader.IsIdentifierChar(Ch: Char; First: Boolean): Boolean;
begin
  Result :=
   (Ch in ['A'..'Z', 'a'..'z', '_']) or (not First and (Ch in ['0'..'9']));
end;

function TJCoreParserReader.IsNumericChar(Ch: Char; First: Boolean): Boolean;
begin
  Result := (Ch in ['0'..'9']) or (not First and (Ch in ['.']));
end;

function TJCoreParserReader.IsStringDelimiter(Ch: Char): Boolean;
begin
  Result := Ch in ['''', '"'];
end;

function TJCoreParserReader.ReadBoolean: Boolean;
var
  Token: string;
begin
  Token := ReadToken;
  Result := False;
  if SameText(Token, SJCoreTrueString) then
    Result := True
  else if not SameText(Token, SJCoreFalseString) then
    ErrorExpected(SJCoreBooleanValueMsg, Token);
end;

function TJCoreParserReader.ReadIdentifier: string;
begin
  CheckEof(SJCoreIdentifierMsg);
  Result := ReadToken;
  if not IsIdentifierChar(Result[1], True) then
    ErrorExpected(SJCoreIdentifierMsg, Result);
end;

function TJCoreParserReader.ReadInteger: Integer;
var
  Token: string;
  VErr: Integer;
begin
  CheckEof(SJCoreIntegerValueMsg);
  Token := ReadNumber;
  Val(Token, Result, VErr);
  if VErr <> 0 then
    ErrorExpected(SJCoreIntegerValueMsg, Token);
end;

function TJCoreParserReader.ReadNext(const ATokens: array of string;
  AInclude: Boolean): string;

  function MatchToken(const AToken: string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := Low(ATokens) to High(ATokens) do
      if SameText(ATokens[I], AToken) then
        Exit;
    Result := False;
  end;

var
  Token: string;
  VOldPos, VNewPos: TJCoreTextPos;
begin
  VOldPos := Position;
  repeat
    Token := ReadToken;
    if Token = '' then
    begin
      Position := VOldPos;
      ErrorFmt(SJCoreUnexpectedEof, []);
    end;
  until MatchToken(Token);
  if not AInclude then
    UnreadToken;
  VNewPos := Position;
  Position := VOldPos;
  Result := ReadChars(VNewPos.Position - VOldPos.Position);
end;

function TJCoreParserReader.ReadNext(const AToken: string;
  AInclude: Boolean): string;
begin
  Result := ReadNext([AToken], AInclude);
end;

function TJCoreParserReader.ReadNumber: string;

  function IsSignedNumber(const AStr: string): Boolean;
  begin
    Result := (AStr <> '') and (AStr[1] in ['+', '-']) and
     not Eof and IsNumericChar(NextChar, True);
  end;

begin
  CheckEof(SJCoreNumberValueMsg);
  Result := ReadToken;
  if IsSignedNumber(Result) then
    Result := Result + InternalReadNumber;
  if (Result = '') or not (Result[1] in ['0'..'9', '+', '-']) then
    ErrorExpected(SJCoreNumberValueMsg, Result);
end;

function TJCoreParserReader.ReadPath: string;
var
  VPos: TJCoreTextPos;
  Ch: Char;
begin
  Result := ReadIdentifier;
  VPos := Position;
  Ch := ReadChar;
  while (Ch = '.') and IsIdentifierChar(NextChar, True) do
  begin
    Result := Result + '.' + ReadIdentifier;
    VPos := Position;
    Ch := ReadChar;
  end;
  Position := VPos;
end;

function TJCoreParserReader.ReadString: string;
begin
  CheckEof(SJCoreStringValueMsg);
  Result := ReadToken;
  if (Length(Result) < 2) or (Result[1] <> Result[Length(Result)]) or
   not IsStringDelimiter(Result[1]) then
    ErrorExpected(SJCoreStringValueMsg, Result);
end;

function TJCoreParserReader.ReadUnquotedString: string;
var
  VStr: string;
  VPStr: PChar;
begin
  VStr := ReadString;
  VPStr := PChar(VStr);
  Result := AnsiExtractQuotedStr(VPStr, VStr[1]);
end;

{ TJCoreParserObject }

class function TJCoreParserObject.Apply(Reader: TJCoreParserReader): Boolean;
var
  VPosition: TJCoreTextPos;
begin
  VPosition := Reader.Position;
  try
    Result := InternalApply(Reader);
  finally
    Reader.Position := VPosition;
  end;
end;

constructor TJCoreParserObject.Create(AOwner: TJCoreParserObject);
begin
  inherited Create;
  FOwner := AOwner;
  if Assigned(FOwner) then
    FOwner.ItemList.Add(Self);
end;

destructor TJCoreParserObject.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.ExtractItem(Self);
  FreeAndNil(FItemList);
  inherited;
end;

procedure TJCoreParserObject.ExtractItem(AItem: TJCoreParserObject);
begin
  if Assigned(FItemList) then
    FItemList.Extract(AItem);
end;

function TJCoreParserObject.FindRule(Reader: TJCoreParserReader;
  AClasses: array of TJCoreParserClass): TJCoreParserClass;
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
  begin
    Result := AClasses[I];
    if Assigned(Result) and Result.Apply(Reader) then
      Exit;
  end;
  Result := nil;
end;

function TJCoreParserObject.GetItemList: TJCoreParserList;
begin
  if not Assigned(FItemList) then
    FItemList := TJCoreParserList.Create(True);
  Result := FItemList;
end;

function TJCoreParserObject.GetItems(AIndex: Integer): TJCoreParserObject;
begin
  Result := ItemList[AIndex];
end;

class function TJCoreParserObject.InternalApply(
  Reader: TJCoreParserReader): Boolean;
begin
  Result := True;
end;

function TJCoreParserObject.InternalCreateObject(
  AClass: TJCoreParserClass): TJCoreParserObject;
begin
  Result := AClass.Create(Self);
end;

procedure TJCoreParserObject.InternalRead(Reader: TJCoreParserReader);
begin
end;

function TJCoreParserObject.ItemCount: Integer;
begin
  if Assigned(FItemList) then
    Result := FItemList.Count
  else
    Result := 0;
end;

function TJCoreParserObject.Parse(
  Reader: TJCoreParserReader;
  AParserClasses: array of TJCoreParserClass;
  AParent: TJCoreParserObject;
  AMandatory: Boolean;
  const AErrorExpectedMsg: string): TJCoreParserObject;
var
  VRule: TJCoreParserClass;
begin
  VRule := FindRule(Reader, AParserClasses);
  if Assigned(VRule) then
  begin
    if not Assigned(AParent) then
      AParent := Self;
    Result := InternalCreateObject(VRule);
    Result.Parent := AParent;
    Result.Read(Reader);
  end else
  begin
    Result := nil;
    if AMandatory then
      Reader.ErrorExpected(AErrorExpectedMsg, Reader.ReadToken);
  end;
end;

procedure TJCoreParserObject.Read(Reader: TJCoreParserReader);
begin
  InternalRead(Reader);
end;

end.
