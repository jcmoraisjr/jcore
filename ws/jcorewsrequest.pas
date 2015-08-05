(*
  JCore WebServices, Request Handler Classes
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreWSRequest;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  Classes,
  fgl,
  HTTPDefs,
  JCoreLogger,
  JCoreWSIntf,
  JCoreWSController;

type

  { TJCoreWSRequest }

  TJCoreWSRequest = class helper for TRequest
  public
    function MatchNextPathInfoToken(const APattern: string): Boolean;
  end;

  { TJCoreWSResponse }

  TJCoreWSResponse = class helper for TResponse
  protected
    function StatusCode(const ACode: Integer): string;
  public
    procedure Send(const ACode: Integer; const AContent: string = ''; const AContentType: string = 'text/plain');
  end;

  { TJCoreWSRequestHandlerItem }

  TJCoreWSRequestHandlerItem = class(TObject)
  private
    FRequestHandler: IJCoreWSRequestHandler;
    FPattern: string;
  public
    constructor Create(const ARequestHandler: IJCoreWSRequestHandler; const APattern: string);
    property RequestHandler: IJCoreWSRequestHandler read FRequestHandler;
    property Pattern: string read FPattern;
  end;

  TJCoreWSRequestHandlerList = specialize TFPGObjectList<TJCoreWSRequestHandlerItem>;

  { TJCoreWSAbstractRequestRouter }

  TJCoreWSAbstractRequestRouter = class(TInterfacedObject, IJCoreWSRequestRouter)
  private
    class var FLogger: IJCoreLogger;
    FHandlerList: TJCoreWSRequestHandlerList;
  protected
    function PatternMatch(const ARequest: TRequest; APattern: string): Boolean; virtual; abstract;
    class property Logger: IJCoreLogger read FLogger;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AddRequestHandler(const ARequestHandler: IJCoreWSRequestHandler; const APattern: string);
    procedure RouteRequest(const ARequest: TRequest; const AResponse: TResponse);
  end;

  { TJCoreWSSimpleMatchRequestRouter }

  TJCoreWSSimpleMatchRequestRouter = class(TJCoreWSAbstractRequestRouter)
  protected
    function PatternMatch(const ARequest: TRequest; APattern: string): Boolean; override;
  end;

  { TJCoreWSRESTRequestRouter }

  TJCoreWSRESTRequestRouter = class(TJCoreWSAbstractRequestRouter)
  protected
    function PatternMatch(const ARequest: TRequest; APattern: string): Boolean; override;
  end;

  { TJCoreWSRESTRequestHandler }

  TJCoreWSRESTRequestHandler = class(TInterfacedObject, IJCoreWSRequestHandler)
  private
    class var FDefaultInvokersRegistered: Boolean;
    FControllers: TJCoreWSControllerClassList;
  protected
    function FindMethod(const AControllerURLFrag, AMethodURLFrag: string): TJCoreWSControllerMethod;
    procedure HandleRequest(const ARequest: TRequest; const AResponse: TResponse);
    procedure InternalRegisterDefaultInvokers; virtual;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function AddController(const AControllerClass: TClass; const AControllerName: string = ''): TJCoreWSControllerClass;
  end;

implementation

uses
  sysutils,
  JCoreUtils,
  JCoreConsts,
  JCoreDIC,
  JCoreWSInvokers;

{ TJCoreWSRequest }

function TJCoreWSRequest.MatchNextPathInfoToken(const APattern: string): Boolean;

  function ReadToken(var ARef: Integer; const AExpression: string): string;
  var
    VStarting: Integer;
  begin
    if ARef <= Length(AExpression) then
    begin
      while (ARef <= Length(AExpression)) and (AExpression[ARef] = '/') do
        Inc(ARef);
      VStarting := ARef;
      while (ARef <= Length(AExpression)) and (AExpression[ARef] <> '/') do
        Inc(ARef);
      Result := Copy(AExpression, VStarting, ARef - VStarting);
    end else
      Result := '';
  end;

var
  VPatternRef, VPathInfoRef, I: Integer;
  VPatternToken, VPathInfoToken, VPathInfo: string;
  VQuery: TStrings;
begin
  VPathInfo := PathInfo;
  VPatternRef := 1;
  VPathInfoRef := Length(ReturnedPathInfo) + 1;
  VPatternToken := ReadToken(VPatternRef, APattern);
  if VPatternToken <> '' then
    VPathInfoToken := ReadToken(VPathInfoRef, VPathInfo)
  else
    VPathInfoToken := '';
  VQuery := nil;
  Result := False;
  try
    while (VPatternToken <> '') and (VPathInfoToken <> '') do
    begin
      if VPatternToken[1] = ':' then
      begin
        if not Assigned(VQuery) then
          VQuery := TStringList.Create;
        // adding two strings: key and value pair
        VQuery.AddStrings([Copy(VPatternToken, 2, Length(VPatternToken)), VPathInfoToken]);
      end else if VPatternToken <> VPathInfoToken then
        Exit;
      VPatternToken := ReadToken(VPatternRef, APattern);
      if VPatternToken <> '' then
        VPathInfoToken := ReadToken(VPathInfoRef, VPathInfo);
    end;
    Result := VPatternToken = '';
    // only change the request object if the pattern matches
    if Result then
    begin
      if Assigned(VQuery) then
        for I := 0 to Pred(VQuery.Count div 2) do
          QueryFields.Values[VQuery[2*I]] := VQuery[2*I+1];
      ReturnedPathInfo := Copy(VPathInfo, 1, VPathInfoRef - 1);
    end;
  finally
    FreeAndNil(VQuery);
  end;
end;

{ TJCoreWSResponse }

function TJCoreWSResponse.StatusCode(const ACode: Integer): string;
begin
  case ACode of
    100: Result := 'Continue';
    101: Result := 'Switching Protocols';
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoritative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    300: Result := 'Multiple Choices';
    301: Result := 'Moved Permanently';
    302: Result := 'Found';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    305: Result := 'Use Proxy';
    307: Result := 'Temporary Redirect';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Time-out';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Request Entity Too Large';
    414: Result := 'Request-URI Too Large';
    415: Result := 'Unsupported Media Type';
    416: Result := 'Requested range not satisfiable';
    417: Result := 'Expectation Failed';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Time-out';
    505: Result := 'HTTP Version not supported';
  else
    Result := Format(JCoreFormatMessage(3202, S3202_UnknownStatus), [ACode]);
  end;
end;

procedure TJCoreWSResponse.Send(const ACode: Integer; const AContent: string; const AContentType: string);
begin
  Code := ACode;
  CodeText := StatusCode(ACode);
  Content := AContent;
  ContentType := AContentType;
end;

{ TJCoreWSRequestHandlerItem }

constructor TJCoreWSRequestHandlerItem.Create(const ARequestHandler: IJCoreWSRequestHandler;
  const APattern: string);
begin
  inherited Create;
  FRequestHandler := ARequestHandler;
  FPattern := APattern;
end;

{ TJCoreWSAbstractRequestRouter }

destructor TJCoreWSAbstractRequestRouter.Destroy;
begin
  FreeAndNil(FHandlerList);
  inherited Destroy;
end;

procedure TJCoreWSAbstractRequestRouter.AfterConstruction;
var
  VLogFactory: IJCoreLogFactory;
begin
  inherited AfterConstruction;
  if not Assigned(FLogger) then
  begin
    TJCoreDIC.Locate(IJCoreLogFactory, VLogFactory);
    FLogger := VLogFactory.GetLogger('jcore.ws.request.router');
  end;
end;

procedure TJCoreWSAbstractRequestRouter.AddRequestHandler(const ARequestHandler: IJCoreWSRequestHandler;
  const APattern: string);
begin
  if not Assigned(FHandlerList) then
    FHandlerList := TJCoreWSRequestHandlerList.Create(True);
  FHandlerList.Add(TJCoreWSRequestHandlerItem.Create(ARequestHandler, APattern));
end;

procedure TJCoreWSAbstractRequestRouter.RouteRequest(const ARequest: TRequest; const AResponse: TResponse);
var
  I: Integer;
begin
  try
    if Assigned(FHandlerList) then
      for I := 0 to Pred(FHandlerList.Count) do
        if PatternMatch(ARequest, FHandlerList[I].Pattern) then
        begin
          FHandlerList[I].RequestHandler.HandleRequest(ARequest, AResponse);
          Exit;
        end;
    AResponse.Send(404);
  except
    on E: Exception do
    begin
      Logger.Error('Internal server error', E);
      AResponse.Send(500, JCoreFormatMessage(3201, S3201_InternalServerError500));
    end;
  end;
end;

{ TJCoreWSSimpleMatchRequestRouter }

function TJCoreWSSimpleMatchRequestRouter.PatternMatch(const ARequest: TRequest; APattern: string): Boolean;
var
  VPatternLength, VPathLength, VPos, I: Integer;
  VPathInfo: string;
begin
  VPathInfo := ARequest.PathInfo;
  if (VPathInfo <> '') and (VPathInfo[1] <> '/') and (APattern <> '') and (APattern[1] = '/') then
    VPathInfo := '/' + VPathInfo;

  Result := APattern = VPathInfo;
  if Result then
    Exit;

  // Path+'*' should be greater than or equal Pattern
  VPatternLength := Length(APattern);
  VPathLength := Length(VPathInfo);
  Result := VPathLength + 1 >= VPatternLength;
  if not Result then
    Exit;

  // No '*', no match
  VPos := Pos('*', APattern);
  Result := VPos <> 0;
  if not Result then
    Exit;

  // Matching before and after '*'
  Result := False;
  for I := 1 to Pred(VPos) do
    if APattern[I] <> VPathInfo[I] then
      Exit;
  for I := VPatternLength downto Succ(VPos) do
    if APattern[I] <> VPathInfo[VPathLength + I - VPatternLength] then
      Exit;

  Result := True;
end;

{ TJCoreWSRESTRequestRouter }

function TJCoreWSRESTRequestRouter.PatternMatch(const ARequest: TRequest; APattern: string): Boolean;
begin
  Result := ARequest.MatchNextPathInfoToken(APattern);
end;

{ TJCoreWSRESTRequestHandler }

function TJCoreWSRESTRequestHandler.FindMethod(
  const AControllerURLFrag, AMethodURLFrag: string): TJCoreWSControllerMethod;
var
  I: Integer;
begin
  for I := 0 to Pred(FControllers.Count) do
    if FControllers[I].ControllerURLFrag = AControllerURLFrag then
    begin
      Result := FControllers[I].FindMethod(AMethodURLFrag);
      Exit;
    end;
  Result := nil;
end;

procedure TJCoreWSRESTRequestHandler.HandleRequest(const ARequest: TRequest;
  const AResponse: TResponse);
var
  VMethod: TJCoreWSControllerMethod;
  VControllerURLFrag: string;
  VMethodURLFrag: string;
begin
  if ARequest.MatchNextPathInfoToken(':' + SJCoreController) then
  begin
    VControllerURLFrag := ARequest.QueryFields.Values[SJCoreController];
    if ARequest.MatchNextPathInfoToken(':' + SJCoreMethod) then
      VMethodURLFrag := ARequest.QueryFields.Values[SJCoreMethod]
    else
      VMethodURLFrag := LowerCase(ARequest.Method);
    VMethod := FindMethod(VControllerURLFrag, VMethodURLFrag);
  end else
    VMethod := nil;
  if Assigned(VMethod) and ARequest.MatchNextPathInfoToken(VMethod.MethodPattern) and
   (ARequest.PathInfo = ARequest.ReturnedPathInfo) then
  begin
    if VMethod.AcceptRequestMethod(ARequest.Method) then
      VMethod.HandleRequest(ARequest, AResponse)
    else
      AResponse.Send(405,
       Format(JCoreFormatMessage(3204, S3204_URLNotAllowed), [ARequest.PathInfo, ARequest.Method]));
  end else
    AResponse.Send(404, Format(JCoreFormatMessage(3203, S3203_URLNotFound), [ARequest.PathInfo]));
end;

procedure TJCoreWSRESTRequestHandler.InternalRegisterDefaultInvokers;
var
  FMethodRegistry: IJCoreWSMethodRegistry;
begin
  TJCoreDIC.Locate(IJCoreWSMethodRegistry, FMethodRegistry);
  FMethodRegistry.AddInvoker([TJCoreWSProcObjectInvoker, TJCoreWSFncObjectInvoker]);
end;

destructor TJCoreWSRESTRequestHandler.Destroy;
begin
  FreeAndNil(FControllers);
  inherited Destroy;
end;

procedure TJCoreWSRESTRequestHandler.AfterConstruction;
begin
  inherited AfterConstruction;
  if not FDefaultInvokersRegistered then
  begin
    InternalRegisterDefaultInvokers;
    FDefaultInvokersRegistered := True;
  end;
end;

function TJCoreWSRESTRequestHandler.AddController(const AControllerClass: TClass;
  const AControllerName: string): TJCoreWSControllerClass;
begin
  if not Assigned(FControllers) then
    FControllers := TJCoreWSControllerClassList.Create(True);
  Result := TJCoreWSControllerClass.Create(AControllerClass, AControllerName);
  FControllers.Add(Result);
end;

initialization
  TJCoreDIC.LazyRegister(IJCoreWSRequestRouter, TJCoreWSRESTRequestRouter, jdsApplication);

finalization
  TJCoreDIC.Unregister(IJCoreWSRequestRouter, TJCoreWSRESTRequestRouter);

end.

