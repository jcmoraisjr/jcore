(*
  JCore, Base Classes
  Copyright (C) 2013 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreClasses;

{$I jcore.inc}

interface

uses
  fgl,
  SysUtils,
  typinfo,
  Classes,
  variants;

type

  TJCoreExceptionClass = class of EJCoreException;

  { EJCoreException }

  EJCoreException = class(Exception)
  private
    FCode: Integer;
    FMsg: string;
  public
    constructor Create(const ACode: Integer; const AMsg: string; const AArgs: array of const);
    property Code: Integer read FCode;
    property Msg: string read FMsg;
  end;

  { EJCoreNilPointer }

  EJCoreNilPointer = class(EJCoreException)      // 0100
  public
    constructor Create;
  end;

  EJCoreClasses = class(EJCoreException);        // 0200
  EJCoreDIC = class(EJCoreException);            // 0300
  EJCoreParser = class;                          // 0400
  EJCoreMetadata = class(EJCoreException);       // 0500
  EJCoreOPF = class(EJCoreException);            // 2100
  EJCoreWS = class(EJCoreException);             // 3100

  TJCoreTextPos = record
    Line, Column: Integer;
    Position: Integer;
  end;

  { EJCoreParser }

  EJCoreParser = class(EJCoreException)
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(const APosition: TJCoreTextPos; const ACode: Integer; const AMsg: string; const AArgs: array of const);
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

  //PComponent = ^TComponent;

  //TChars = set of Char;
  TJCoreStringArray = array of string;
  TJCoreClassArray = array of TClass;
  TJCoreVariantArray = array of Variant;

  TJCoreClassMap = specialize TFPGMap<String, TClass>;
  TJCoreInt64List = specialize TFPGList<Int64>;
  TJCoreObjectList = specialize TFPGObjectList<TObject>;
  TJCoreObjectArray = array of TObject;

  IJCoreInterface = interface(IInterface)
  ['{CB83C7A1-B07E-40C4-B70B-964C14A8F94E}']
    function SupportsIntf(const IID: TGUID): Boolean;
  end;

  { TJCoreManagedObject }

  TJCoreManagedObject = class(TPersistent, IInterface, IJCoreInterface)
  private
    FRefCount: Integer;
  protected
    procedure Finit; virtual;
    function QueryInterface({$ifdef fpc_has_constref}constref{$else}const{$endif} IID: TGUID; out Obj): HResult; {$ifndef windows}cdecl{$else}stdcall{$endif};
    function SupportsIntf(const IID: TGUID): Boolean;
    function _AddRef: Integer; {$ifndef windows}cdecl{$else}stdcall{$endif};
    function _Release: Integer; {$ifndef windows}cdecl{$else}stdcall{$endif};
  public
    destructor Destroy; reintroduce;
    function AddRef: Integer; virtual;
    procedure FreeInstance; override;
    class function NewInstance: TObject; override;
    function Release: Integer; virtual;
    property RefCount: Integer read FRefCount;
  end;

  TJCoreManagedIObject = class(TJCoreManagedObject)
  public
    procedure AfterConstruction; override;
  end;

  { TJCoreFactory }

  generic TJCoreFactory<T> = class(TObject)
  protected
    function Choose(const AType1, AType2: T): T;
    function InternalSelect(const AType1, AType2: T): T; virtual;
  end;

  TJCoreTextReader = class(TObject)
  { TODO : Refactor class -- move Token implementation to the ParserReader }
  private
    FBuffer: string;
    FBufferBasePos: Integer;
    FCurrentPos: TJCoreTextPos;
    FCurrentToken: string;
    FSize: Integer;
    FTokenPos: TJCoreTextPos;
    function GetEof: Boolean;
  protected
    procedure InitReader; virtual;
    function InternalReadToken: string; virtual; abstract;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); overload;
    constructor Create(const AString: string); overload;
    procedure ErrorExpected(const AExpectedToken, AToken: string);
    procedure Error(const ACode: Integer; const AMsg: string; const AArgs: array of const);
    function NextChar: Char;
    function ReadChar(GoForward: Boolean = True): Char;
    function ReadChars(ACount: Integer): string;
    procedure ReadMatch(const AToken: string);
    procedure ReadMatchEof;
    procedure ReadMatchText(const AToken: string);
    function ReadNextEol: string;
    function ReadNextToken: string;
    function ReadToken: string;
    procedure SkipSpaces;
    procedure UnreadChar;
    procedure UnreadToken;
    property Eof: Boolean read GetEof;
    property Position: TJCoreTextPos read FCurrentPos write FCurrentPos;
    property TokenPos: TJCoreTextPos read FTokenPos;
  end;

implementation

uses
  Math,
  JCoreUtils,
  JCoreConsts;

{ EJCoreException }

constructor EJCoreException.Create(const ACode: Integer; const AMsg: string; const AArgs: array of const);
begin
  FCode := ACode;
  FMsg := Format(AMsg, AArgs);
  inherited Create(JCoreFormatMessage(FCode, FMsg));
end;

{ EJCoreNilPointer }

constructor EJCoreNilPointer.Create;
begin
  inherited Create(101, S0101_NilPointer, []);
end;

{ EJCoreParser }

constructor EJCoreParser.Create(const APosition: TJCoreTextPos; const ACode: Integer; const AMsg: string;
  const AArgs: array of const);
begin
  inherited Create(ACode, AMsg, AArgs);
  FLine := APosition.Line;
  FColumn := APosition.Column;
end;

{ TJCoreManagedObject }

function TJCoreManagedObject.AddRef: Integer;
begin
  Result := InterLockedIncrement(FRefCount);
end;

destructor TJCoreManagedObject.Destroy;
begin
end;

procedure TJCoreManagedObject.FreeInstance;
begin
  Release;
  if FRefCount = 0 then
    try
      Finit;
    finally
      inherited;
    end;
end;

class function TJCoreManagedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  with TJCoreManagedObject(Result) do
    FRefCount := 1;
end;

procedure TJCoreManagedObject.Finit;
begin
end;

function TJCoreManagedObject.QueryInterface(
  {$ifdef fpc_has_constref}constref{$else}const{$endif} IID: TGUID; out Obj): HResult; {$ifndef windows}cdecl{$else}stdcall{$endif};
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TJCoreManagedObject.Release: Integer;
begin
  Result := InterLockedDecrement(FRefCount);
  if FRefCount < 0 then
    raise EJCoreClasses.Create(204, S0204_CannotReleaseInstance, [ClassName]);
end;

function TJCoreManagedObject.SupportsIntf(const IID: TGUID): Boolean;
begin
  Result := Assigned(GetInterfaceEntry(IID));
end;

function TJCoreManagedObject._AddRef: Integer; {$ifndef windows}cdecl{$else}stdcall{$endif};
begin
  Result := AddRef;
end;

function TJCoreManagedObject._Release: Integer; {$ifndef windows}cdecl{$else}stdcall{$endif};
begin
  Result := Release;
  if Result = 0 then
    try
      Finit;
    finally
      inherited FreeInstance;
    end;
end;

{ TJCoreManagedIObject }

procedure TJCoreManagedIObject.AfterConstruction;
begin
  inherited;
  InterLockedDecrement(FRefCount);  // friend class
end;

{ TJCoreFactory }

function TJCoreFactory.Choose(const AType1, AType2: T): T;
begin
  if (AType1 = nil) and (AType2 = nil) then
    raise EJCoreNilPointer.Create
  else if (AType2 = nil) or AType1.InheritsFrom(AType2) then
    Result := AType1
  else if (AType1 = nil) or AType2.InheritsFrom(AType1) then
    Result := AType2
  else
    Result := InternalSelect(AType1, AType2);
  if (Result = nil) then
    raise EJCoreClasses.Create(201, S0201_AmbiguousImplementation, [AType1.ClassName, AType2.ClassName]);
end;

function TJCoreFactory.InternalSelect(const AType1, AType2: T): T;
begin
  Result := nil;
end;

{ TJCoreTextReader }

constructor TJCoreTextReader.Create(AStream: TStream; AOwnsStream: Boolean);
var
  VBuffer: string;
  VSize: Integer;
begin
  VSize := AStream.Size;
  SetLength(VBuffer, VSize);
  AStream.Read(VBuffer[1], VSize);
  if AOwnsStream then
    FreeAndNil(AStream);
  Create(VBuffer);
end;

constructor TJCoreTextReader.Create(const AString: string);
begin
  inherited Create;
  InitReader;
  FBufferBasePos := 1;
  FBuffer := AString;
  FSize := Length(FBuffer);
  FCurrentPos.Line := 1;
  FCurrentPos.Column := 1;
  FCurrentPos.Position := FBufferBasePos;
  FTokenPos := FCurrentPos;
end;

procedure TJCoreTextReader.ErrorExpected(const AExpectedToken, AToken: string);
var
  VToken: string;
begin
  if AToken = '' then
    VToken := SJCoreEofMsg
  else
    VToken := AToken;
  raise EJCoreParser.Create(TokenPos, 402, S0402_TokenExpected, [AExpectedToken, VToken]);
end;

procedure TJCoreTextReader.Error(const ACode: Integer; const AMsg: string; const AArgs: array of const);
begin
  raise EJCoreParser.Create(TokenPos, ACode, AMsg, AArgs);
end;

function TJCoreTextReader.GetEof: Boolean;
begin
  Result := FCurrentPos.Position >= FSize + FBufferBasePos;
end;

procedure TJCoreTextReader.InitReader;
begin
end;

function TJCoreTextReader.NextChar: Char;
begin
  Result := ReadChar(False);
end;

function TJCoreTextReader.ReadChar(GoForward: Boolean): Char;
begin
  if Eof then
    Error(404, S0404_UnexpectedEof, []);
  Result := FBuffer[FCurrentPos.Position];
  if GoForward then
  begin
    if Result = #10 then
    begin
      Inc(FCurrentPos.Line);
      FCurrentPos.Column := 1;
    end else if Result <> #13 then
      Inc(FCurrentPos.Column);
    Inc(FCurrentPos.Position);
  end;
end;

function TJCoreTextReader.ReadChars(ACount: Integer): string;
var
  VCount, I: Integer;
begin
  VCount := Min(ACount, FSize - FCurrentPos.Position + FBufferBasePos);
  SetLength(Result, VCount);
  for I := 1 to VCount do
    Result[I] := ReadChar;
end;

procedure TJCoreTextReader.ReadMatch(const AToken: string);
var
  Token: string;
begin
  Token := ReadToken;
  if Token <> AToken then
    ErrorExpected(AToken, Token);
end;

procedure TJCoreTextReader.ReadMatchEof;
begin
  SkipSpaces;
  if ReadNextToken <> '' then
    ErrorExpected(SJCoreEofMsg, ReadNextToken);
end;

procedure TJCoreTextReader.ReadMatchText(const AToken: string);
var
  Token: string;
begin
  Token := ReadToken;
  if not SameText(Token, AToken) then
    ErrorExpected(AToken, Token);
end;

function TJCoreTextReader.ReadNextEol: string;
var
  VOldPos, VNewPos: TJCoreTextPos;
begin
  VOldPos := Position;
  repeat until ReadChar in [#10, #13];
  VNewPos := Position;
  Position := VOldPos;
  { TODO : Verify if last line without #10 }
  Result := ReadChars(VNewPos.Position - VOldPos.Position - 1);
  SkipSpaces;
end;

function TJCoreTextReader.ReadNextToken: string;
begin
  Result := ReadToken;
  UnreadToken;
end;

function TJCoreTextReader.ReadToken: string;
begin
  SkipSpaces;
  if (FCurrentToken <> '') and (FTokenPos.Position = FCurrentPos.Position) then
  begin
    Result := FCurrentToken;

    { TODO : Improve }
    Inc(FCurrentPos.Position, Length(FCurrentToken));
    Inc(FCurrentPos.Column, Length(FCurrentToken));

    FCurrentToken := '';
  end else
  begin
    FTokenPos := Position;
    if not Eof then
      FCurrentToken := InternalReadToken
    else
      FCurrentToken := '';
    Result := FCurrentToken;
  end;
end;

procedure TJCoreTextReader.SkipSpaces;
begin
  if not Eof then
  begin
    while ReadChar in [' ', #0, #9, #10, #13] do
      if Eof then
        Exit;
    UnreadChar;
  end;
end;

procedure TJCoreTextReader.UnreadChar;
var
  VPos: TJCoreTextPos;
begin
  VPos := Position;
  Dec(VPos.Position);
  if VPos.Column > 1 then
    Dec(VPos.Column);
  Position := VPos;
end;

procedure TJCoreTextReader.UnreadToken;
begin
  Position := TokenPos;
end;

end.
