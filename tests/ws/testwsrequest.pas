unit TestWSRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  HTTPDefs,
  fpcunit,
  JCoreWSIntf,
  JCoreWSRequest;

type

  { TTestWSRequest }

  TTestWSRequest = class(TTestCase)
  private
    FRequest: TRequest;
    FResponse: TResponse;
    FRouter: IJCoreWSRequestRouter;
    function GetRequest: TRequest;
    function GetResponse: TResponse;
    function GetRouter: IJCoreWSRequestRouter;
  protected
    function CreateRouter: IJCoreWSRequestRouter; virtual; abstract;
    procedure TearDown; override;
    property Request: TRequest read GetRequest;
    property Response: TResponse read GetResponse;
    property Router: IJCoreWSRequestRouter read GetRouter;
  end;

  { TTestWSRequestSimpleRouter }

  TTestWSRequestSimpleRouter = class(TTestWSRequest)
  protected
    function CreateRouter: IJCoreWSRequestRouter; override;
  published
    procedure Simple;
    procedure NoSlash;
    procedure PatternPrefix;
    procedure PatternMiddle;
    procedure PatternSuffix;
    procedure PatternNoSlash1;
    procedure PatternNoSlash2;
    procedure FirstHandlerMatch1;
    procedure FirstHandlerMatch2;
    procedure NoHandler404;
    procedure Simple404;
    procedure NoPattern404;
    procedure PatternPrefix404;
    procedure PatternMiddle404;
    procedure PatternSuffix404;
  end;

  { TTestWSRequestRESTRouter }

  TTestWSRequestRESTRouter = class(TTestWSRequest)
  protected
    function CreateRouter: IJCoreWSRequestRouter; override;
  published
    procedure Simple;
    procedure NoSlash;
    procedure OneField;
    procedure TwoFields;
    procedure TwoFieldsNoSlash;
    procedure ThreeFields;
    procedure FirstHandlerMatch1;
    procedure FirstHandlerMatch2;
    procedure NoHandler404;
    procedure Simple404;
    procedure ShortUrl404;
  end;

  { TTestRequest }

  TTestRequest = class(TRequest)
  end;

  { TTestResponse }

  TTestResponse = class(TResponse)
  protected
    procedure DoSendContent; override;
    procedure DoSendHeaders(Headers: TStrings); override;
  end;

  { TTestSimple1RequestHandler }

  TTestSimple1RequestHandler = class(TInterfacedObject, IJCoreWSRequestHandler)
  public
    procedure HandleRequest(const ARequest: TRequest; const AResponse: TResponse);
  end;

  { TTestSimple2RequestHandler }

  TTestSimple2RequestHandler = class(TInterfacedObject, IJCoreWSRequestHandler)
  public
    procedure HandleRequest(const ARequest: TRequest; const AResponse: TResponse);
  end;

implementation

uses
  sysutils,
  testregistry;

function TTestWSRequest.GetRequest: TRequest;
begin
  if not Assigned(FRequest) then
    FRequest := TTestRequest.Create;
  Result := FRequest;
end;

function TTestWSRequest.GetResponse: TResponse;
begin
  if not Assigned(FResponse) then
  begin
    FResponse := TTestResponse.Create(Request);
    FResponse.Contents.LineBreak := '\n';
  end;
  Result := FResponse;
end;

function TTestWSRequest.GetRouter: IJCoreWSRequestRouter;
begin
  if not Assigned(FRouter) then
    FRouter := CreateRouter;
  Result := FRouter;
end;

procedure TTestWSRequest.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  FRouter := nil;
end;

{ TTestWSRequestSimpleRouter }

function TTestWSRequestSimpleRouter.CreateRouter: IJCoreWSRequestRouter;
begin
  Result := TJCoreWSSimpleMatchRequestRouter.Create;
end;

procedure TTestWSRequestSimpleRouter.Simple;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple');
  Request.PathInfo := '/simple';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.NoSlash;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple');
  Request.PathInfo := 'simple';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.PatternPrefix;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '*.page');
  Request.PathInfo := '/simple/url/some.page';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.PatternMiddle;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple/*.page');
  Request.PathInfo := '/simple/url/some.page';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.PatternSuffix;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple/*');
  Request.PathInfo := '/simple/url';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.PatternNoSlash1;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple*');
  Request.PathInfo := '/simpleurl';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.PatternNoSlash2;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple*');
  Request.PathInfo := '/simple/url';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.FirstHandlerMatch1;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple/url');
  Router.AddRequestHandler(TTestSimple2RequestHandler.Create, '/simple/*');
  Request.PathInfo := '/simple/url';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.FirstHandlerMatch2;
begin
  Router.AddRequestHandler(TTestSimple2RequestHandler.Create, '/simple/*');
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple/url');
  Request.PathInfo := '/simple/url';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'two\n', Response.Content);
end;

procedure TTestWSRequestSimpleRouter.NoHandler404;
begin
  Request.PathInfo := '/any';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

procedure TTestWSRequestSimpleRouter.Simple404;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple');
  Request.PathInfo := '/wrong/url';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

procedure TTestWSRequestSimpleRouter.NoPattern404;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple');
  Request.PathInfo := '/simple/url';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

procedure TTestWSRequestSimpleRouter.PatternPrefix404;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '*.page');
  Request.PathInfo := '/simple/url/some.pages';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

procedure TTestWSRequestSimpleRouter.PatternMiddle404;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple/*.page');
  Request.PathInfo := '/simple/url/somepage';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

procedure TTestWSRequestSimpleRouter.PatternSuffix404;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/simple/*');
  Request.PathInfo := '/simple';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

{ TTestWSRequestRESTRouter }

function TTestWSRequestRESTRouter.CreateRouter: IJCoreWSRequestRouter;
begin
  Result := TJCoreWSRESTRequestRouter.Create;
end;

procedure TTestWSRequestRESTRouter.Simple;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr');
  Request.PathInfo := '/attr';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestRESTRouter.NoSlash;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr');
  Request.PathInfo := 'attr';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestRESTRouter.OneField;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr/:master');
  Request.PathInfo := '/attr/first';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
  AssertEquals('field 1', 'first', Request.QueryFields.Values['master']);
end;

procedure TTestWSRequestRESTRouter.TwoFields;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr/:master/:next');
  Request.PathInfo := '/attr/first/second';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
  AssertEquals('field 1', 'first', Request.QueryFields.Values['master']);
  AssertEquals('field 2', 'second', Request.QueryFields.Values['next']);
end;

procedure TTestWSRequestRESTRouter.TwoFieldsNoSlash;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr/:master/:next');
  Request.PathInfo := 'attr/first/second';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
  AssertEquals('field 1', 'first', Request.QueryFields.Values['master']);
  AssertEquals('field 2', 'second', Request.QueryFields.Values['next']);
end;

procedure TTestWSRequestRESTRouter.ThreeFields;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr/:master/:next');
  Request.PathInfo := '/attr/first/second/third';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
  AssertEquals('field 1', 'first', Request.QueryFields.Values['master']);
  AssertEquals('field 2', 'second', Request.QueryFields.Values['next']);
  AssertEquals('returned path info', '/attr/first/second', Request.ReturnedPathInfo);
end;

procedure TTestWSRequestRESTRouter.FirstHandlerMatch1;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr/url');
  Router.AddRequestHandler(TTestSimple2RequestHandler.Create, '/attr');
  Request.PathInfo := '/attr/url';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'one\n', Response.Content);
end;

procedure TTestWSRequestRESTRouter.FirstHandlerMatch2;
begin
  Router.AddRequestHandler(TTestSimple2RequestHandler.Create, '/attr');
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr/url');
  Request.PathInfo := '/attr/url';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 200, Response.Code);
  AssertEquals('content', 'two\n', Response.Content);
end;

procedure TTestWSRequestRESTRouter.NoHandler404;
begin
  Request.PathInfo := '/any';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

procedure TTestWSRequestRESTRouter.Simple404;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr/:first');
  Request.PathInfo := '/other/one';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

procedure TTestWSRequestRESTRouter.ShortUrl404;
begin
  Router.AddRequestHandler(TTestSimple1RequestHandler.Create, '/attr/:first');
  Request.PathInfo := '/attr/';
  Router.RouteRequest(Request, Response);
  AssertEquals('status code', 404, Response.Code);
end;

{ TTestResponse }

procedure TTestResponse.DoSendContent;
begin
end;

procedure TTestResponse.DoSendHeaders(Headers: TStrings);
begin
end;

{ TTestSimple1RequestHandler }

procedure TTestSimple1RequestHandler.HandleRequest(const ARequest: TRequest; const AResponse: TResponse);
begin
  AResponse.Content := 'one';
end;

{ TTestSimple2RequestHandler }

procedure TTestSimple2RequestHandler.HandleRequest(const ARequest: TRequest; const AResponse: TResponse);
begin
  AResponse.Content := 'two';
end;

initialization
  RegisterTest('jcore.ws.request', TTestWSRequestSimpleRouter);
  RegisterTest('jcore.ws.request', TTestWSRequestRESTRouter);

end.

