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
  Classes;

type
  { EJCoreException }

  EJCoreException = class(Exception)
  public
    constructor Create(const AMsg: string);
    constructor CreateFmt(const AMsg: string; const AArgs: array of const);
  end;

  EJCoreError = class(EJCoreException);

  { EJCoreNilPointerException }

  EJCoreNilPointerException = class(EJCoreException)
  public
    constructor Create;
  end;

  { EJCoreAmbiguousImplementation }

  EJCoreAmbiguousImplementation = class(EJCoreException)
  public
    constructor Create(const AClassName1, AClassName2: string);
  end;

  { EJCoreUnsupportedIntfException }

  EJCoreUnsupportedIntfException = class(EJCoreException)
  strict private
    FClass: TClass;
    FGUID: TGuid;
  public
    constructor Create(AClass: TClass; AGUID: TGuid);
    property TheClass: TClass read FClass;
    property GUID: TGuid read FGUID;
  end;

  { EJCoreAttributeNotFound }

  EJCoreAttributeNotFound = class(EJCoreException)
  public
    constructor Create(const AClassName, AAttributeName: string);
  end;

  { EJCoreListTypeExpected }

  EJCoreListTypeExpected = class(EJCoreException)
  private
    FExpectedType: PTypeInfo;
  public
    constructor Create(const AExpectedType: PTypeInfo);
    property ExpectedType: PTypeInfo read FExpectedType;
  end;

  { EJCoreListIsEmpty }

  EJCoreListIsEmpty = class(EJCoreException)
  public
    constructor Create;
  end;

  { EJCoreMetadataAlreadyOwned }

  EJCoreMetadataAlreadyOwned = class(EJCoreException)
  public
    constructor Create;
  end;

  EJCoreConversionError = class(EJCoreError);

  TJCoreTextPos = record
    Line, Column: Integer;
    Position: Integer;
  end;

  EJCoreReadError = class(EJCoreError)
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(APosition: TJCoreTextPos; const AMsg: string);
    constructor CreateFmt(APosition: TJCoreTextPos; const AMsg: string; const AParams: array of const);
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

  //PComponent = ^TComponent;

  //TChars = set of Char;
  TJCoreStringArray = array of string;
  TJCoreClassArray = array of TClass;

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
    procedure ErrorFmt(const AMsg: string; const AParams: array of const);
    procedure ErrorMsg(const AMsg: string);
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
  JCoreConsts;

{ EJCoreException }

constructor EJCoreException.Create(const AMsg: string);
begin
  inherited Create(AMsg);
end;

constructor EJCoreException.CreateFmt(const AMsg: string;
  const AArgs: array of const);
begin
  inherited CreateFmt(AMsg, AArgs);
end;

{ EJCoreNilPointerException }

constructor EJCoreNilPointerException.Create;
begin
  inherited Create(SJCoreNilPointer);
end;

{ EJCoreAmbiguousImplementation }

constructor EJCoreAmbiguousImplementation.Create(const AClassName1,
  AClassName2: string);
begin
end;

{ EJCoreUnsupportedIntfException }

constructor EJCoreUnsupportedIntfException.Create(AClass: TClass; AGUID: TGuid);
begin
  CreateFmt(SJCoreUnsupportedInterface, [AClass.ClassName, GUIDToString(AGUID)]);
  FClass := AClass;
  FGUID := AGUID;
end;

{ EJCoreAttributeNotFound }

constructor EJCoreAttributeNotFound.Create(const AClassName,
  AAttributeName: string);
begin
end;

{ EJCoreListTypeExpected }

constructor EJCoreListTypeExpected.Create(const AExpectedType: PTypeInfo);
begin
  FExpectedType := AExpectedType;
end;

{ EJCoreListIsEmpty }

constructor EJCoreListIsEmpty.Create;
begin
end;

{ EJCoreMetadataAlreadyOwned }

constructor EJCoreMetadataAlreadyOwned.Create;
begin
end;

{ EJCoreReadError }

constructor EJCoreReadError.Create(
  APosition: TJCoreTextPos; const AMsg: string);
begin
  inherited Create(AMsg);
  FLine := APosition.Line;
  FColumn := APosition.Column;
end;

constructor EJCoreReadError.CreateFmt(APosition: TJCoreTextPos;
  const AMsg: string; const AParams: array of const);
begin
  inherited CreateFmt(AMsg, AParams);
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
    raise EJCoreException.CreateFmt(SJCoreCannotReleaseInstance, [ClassName]);
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
    raise EJCoreNilPointerException.Create
  else if (AType2 = nil) or AType1.InheritsFrom(AType2) then
    Result := AType1
  else if (AType1 = nil) or AType2.InheritsFrom(AType1) then
    Result := AType2
  else
    Result := InternalSelect(AType1, AType2);
  if (Result = nil) then
    raise EJCoreAmbiguousImplementation.Create(AType1.ClassName, AType2.ClassName);
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
  raise EJCoreReadError.CreateFmt(TokenPos, SJCoreTokenExpected,
   [AExpectedToken, VToken]);
end;

procedure TJCoreTextReader.ErrorFmt(
  const AMsg: string; const AParams: array of const);
begin
  raise EJCoreReadError.CreateFmt(TokenPos, AMsg, AParams);
end;

procedure TJCoreTextReader.ErrorMsg(const AMsg: string);
begin
  ErrorFmt(AMsg, []);
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
    ErrorMsg(SJCoreUnexpectedEof);
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
