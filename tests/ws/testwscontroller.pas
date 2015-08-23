unit TestWSController;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  typinfo,
  HTTPDefs,
  fpcunit,
  JCoreWSIntf,
  JCoreWSInvokers,
  JCoreWSRequest,
  JCoreWSController;

type

  { TTestWSMethodRegistry }

  TTestWSMethodRegistry = class(TTestCase)
  private
    procedure ProcObj(const AObject: TObject);
    function FncObj: TObject;
  private
    FMethodData: TJCoreWSMethodData;
    FRegistry: IJCoreWSMethodRegistry;
    function GetRegistry: IJCoreWSMethodRegistry;
  protected
    function CreateMethodData(const AMethodTypeInfo: PTypeInfo): TJCoreWSMethodData;
    procedure TearDown; override;
    property Registry: IJCoreWSMethodRegistry read GetRegistry;
  published
    procedure WithAmbiguity;
    procedure WithInheritance;
    procedure WithoutConflict;
    procedure EmptyList;
    procedure InvokerNotFound;
  end;

  { TTestWSControllerClass }

  TTestWSControllerClass = class(TTestCase)
  private
    FController: TJCoreWSControllerClass;
    FRegistry: IJCoreWSMethodRegistry;
  protected
    function CreateController(const AClass: TClass; const ANane: string = ''): TJCoreWSControllerClass;
    procedure RegisterInvokers(const AInvokers: array of TJCoreWSMethodInvokerClass);
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ValidControllerName;
    procedure ParamControllerName;
    procedure UnsupportedMethod;
    procedure InvokeFunctionRead;
    procedure InvokeProcedureSave;
  end;

  { TTestWSControllerRequest }

  TTestWSControllerRequest = class(TTestCase)
  private
    FControllers: TJCoreWSRESTRequestHandler;
    FRequest: TRequest;
    FResponse: TResponse;
    FRouter: IJCoreWSRequestRouter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Router: IJCoreWSRequestRouter read FRouter;
    property Controllers: TJCoreWSRESTRequestHandler read FControllers;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
  published
    procedure ImplicitFncObjInvoker;
    procedure ImplicitProcObjInvoker;
    procedure InvokeForMethodGet;
    procedure InvokeForMethodPost;
    procedure MethodPattern;
    procedure NotFoundIfPatternUnread;
    procedure NotFoundIfPathInfoUnread;
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

  { TTestInvokerProc }

  TTestInvokerProc = class(TJCoreWSMethodInvoker)
  public
    function Match(const AMethodData: TJCoreWSMethodData): Boolean; override;
    procedure Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest; const AResponse: TResponse; const AMethod: TMethod); override;
  end;

  { TTestInvokerProcAmbiguous }

  TTestInvokerProcAmbiguous = class(TJCoreWSMethodInvoker)
  public
    function Match(const AMethodData: TJCoreWSMethodData): Boolean; override;
    procedure Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest; const AResponse: TResponse; const AMethod: TMethod); override;
  end;

  { TTestInvokerProcInherited }

  TTestInvokerProcInherited = class(TTestInvokerProc)
  public
    function Match(const AMethodData: TJCoreWSMethodData): Boolean; override;
    procedure Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest; const AResponse: TResponse; const AMethod: TMethod); override;
  end;

  { TTestInvokerFnc }

  TTestInvokerFnc = class(TJCoreWSMethodInvoker)
  public
    function Match(const AMethodData: TJCoreWSMethodData): Boolean; override;
    procedure Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest; const AResponse: TResponse; const AMethod: TMethod); override;
  end;

  { TClient }

  TClient = class(TPersistent)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TClientController }

  TClientController = class(TPersistent)
  public
    class var ReadingName: string;
    class var WritingName: string;
  published
    function Get: TClient;
    procedure Post(const AClient: TClient);
    function Read: TClient;
    procedure Save(const AClient: TClient);
  end;

  { TProduct }

  TProduct = class(TPersistent)
  private
    FDescription: string;
  published
    property Description: string read FDescription write FDescription;
  end;

  { TProductController }

  TProductController = class(TPersistent)
  public
    class var Data: string;
  published
    function Read: TProduct;
    procedure Save(const AProduct: TProduct);
  end;

  { TTestClientInvokerController }

  TTestClientInvokerController = class(TJCoreWSMethodInvoker)
  public
    destructor Destroy; override;
    class var Data:  string;
    function Match(const AMethodData: TJCoreWSMethodData): Boolean; override;
    procedure Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest; const AResponse: TResponse; const AMethod: TMethod); override;
  end;

  TTestMethodRegistry = class(TJCoreWSMethodRegistry)
  end;

implementation

uses
  sysutils,
  testregistry,
  JCoreClasses,
  JCoreDIC;

{ TTestWSMethodRegistry }

procedure TTestWSMethodRegistry.ProcObj(const AObject: TObject);
begin
end;

function TTestWSMethodRegistry.FncObj: TObject;
begin
  Result := nil;
end;

function TTestWSMethodRegistry.GetRegistry: IJCoreWSMethodRegistry;
begin
  if not Assigned(FRegistry) then
    FRegistry := TJCoreWSMethodRegistry.Create;
  Result := FRegistry;
end;

function TTestWSMethodRegistry.CreateMethodData(const AMethodTypeInfo: PTypeInfo): TJCoreWSMethodData;
begin
  FreeAndNil(FMethodData);
  FMethodData := TJCoreWSMethodData.Create(AMethodTypeInfo);
  Result := FMethodData;
end;

procedure TTestWSMethodRegistry.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FMethodData);
  FRegistry := nil;
end;

procedure TTestWSMethodRegistry.WithAmbiguity;
begin
  Registry.AddInvoker([TTestInvokerProc, TTestInvokerProcAmbiguous]);
  try
    Registry.FindInvoker(CreateMethodData(TypeInfo(@ProcObj)));
    Fail('EJCoreWS(3104) expected');
  except
    on E: EJCoreWS do
      if E.Code <> 3104 then
        raise;
  end;
end;

procedure TTestWSMethodRegistry.WithInheritance;
var
  VInvoker: TJCoreWSMethodInvoker;
begin
  Registry.AddInvoker([TTestInvokerProc, TTestInvokerProcInherited]);
  VInvoker := Registry.FindInvoker(CreateMethodData(TypeInfo(@ProcObj)));
  AssertNotNull('invoker', VInvoker);
  AssertEquals('invoker class', 'TTestInvokerProcInherited', VInvoker.ClassName);
end;

procedure TTestWSMethodRegistry.WithoutConflict;
var
  VInvoker: TJCoreWSMethodInvoker;
begin
  Registry.AddInvoker([TTestInvokerProc, TTestInvokerFnc]);
  VInvoker := Registry.FindInvoker(CreateMethodData(TypeInfo(@FncObj)));
  AssertNotNull('invoker', VInvoker);
  AssertEquals('invoker class', 'TTestInvokerFnc', VInvoker.ClassName);
end;

procedure TTestWSMethodRegistry.EmptyList;
var
  VInvoker: TJCoreWSMethodInvoker;
begin
  VInvoker := Registry.FindInvoker(CreateMethodData(TypeInfo(@FncObj)));
  AssertNull('invoker', VInvoker);
end;

procedure TTestWSMethodRegistry.InvokerNotFound;
var
  VInvoker: TJCoreWSMethodInvoker;
begin
  Registry.AddInvoker([TTestInvokerProc]);
  VInvoker := Registry.FindInvoker(CreateMethodData(TypeInfo(@FncObj)));
  AssertNull('invoker', VInvoker);
end;

{ TTestWSControllerClass }

function TTestWSControllerClass.CreateController(const AClass: TClass; const ANane: string
  ): TJCoreWSControllerClass;
begin
  FreeAndNil(FController);
  FController := TJCoreWSControllerClass.Create(AClass, ANane);
  Result := FController;
end;

procedure TTestWSControllerClass.RegisterInvokers(const AInvokers: array of TJCoreWSMethodInvokerClass);
begin
  FRegistry.AddInvoker(AInvokers);
end;

procedure TTestWSControllerClass.SetUp;
begin
  inherited SetUp;
  TJCoreDIC.Register(IJCoreWSMethodRegistry, TTestMethodRegistry, jdsApplication);
  TJCoreDIC.Locate(IJCoreWSMethodRegistry, FRegistry);
  TClientController.ReadingName := '<unset>';
  TClientController.WritingName := '<unset>';
  TTestClientInvokerController.Data := '';
end;

procedure TTestWSControllerClass.TearDown;
begin
  inherited TearDown;
  FRegistry := nil;
  FreeAndNil(FController);
  TJCoreDIC.Unregister(IJCoreWSMethodRegistry, TTestMethodRegistry);
end;

procedure TTestWSControllerClass.ValidControllerName;
begin
  AssertEquals('controller name', 'client', CreateController(TClientController).ControllerURLFrag);
end;

procedure TTestWSControllerClass.ParamControllerName;
begin
  AssertEquals('controller name', 'cli', CreateController(TClientController, 'cli').ControllerURLFrag);
end;

procedure TTestWSControllerClass.UnsupportedMethod;
var
  VController: TJCoreWSControllerClass;
  VMethod: TJCoreWSControllerMethod;
begin
  VController := CreateController(TClientController);
  VController.AddMethod(@TClientController.Get, TypeInfo(@TClientController.Get));
  VMethod := VController.FindMethod('get');
  AssertNotNull('method not null', VMethod);
  try
    VMethod.HandleRequest(nil, nil);
    Fail('EJCoreWS(3101) expected');
  except
    on E: EJCoreWS do
      if E.Code <> 3101 then
        raise;
  end;
end;

procedure TTestWSControllerClass.InvokeFunctionRead;
var
  VController: TJCoreWSControllerClass;
  VMethod: TJCoreWSControllerMethod;
begin
  RegisterInvokers([TTestClientInvokerController]);
  VController := CreateController(TClientController);
  VController.AddMethod(@TClientController.Read, TypeInfo(@TClientController.Read));
  VMethod := VController.FindMethod('read');
  AssertNotNull('method not null', VMethod);
  TClientController.ReadingName := 'joe';
  VMethod.HandleRequest(nil, nil);
  AssertEquals('client name', 'joe', TTestClientInvokerController.Data);
end;

procedure TTestWSControllerClass.InvokeProcedureSave;
var
  VController: TJCoreWSControllerClass;
  VMethod: TJCoreWSControllerMethod;
begin
  RegisterInvokers([TTestClientInvokerController]);
  VController := CreateController(TClientController);
  VController.AddMethod(@TClientController.Save, TypeInfo(@TClientController.Save));
  VMethod := VController.FindMethod('save');
  AssertNotNull('method not null', VMethod);
  TClientController.ReadingName := 'jack';
  VMethod.HandleRequest(nil, nil);
  AssertEquals('client name', 'jack', TClientController.WritingName);
end;

{ TTestWSControllerRequest }

procedure TTestWSControllerRequest.SetUp;
begin
  inherited SetUp;
  FRouter := TJCoreWSRESTRequestRouter.Create;
  FControllers := TJCoreWSRESTRequestHandler.Create;
  FRouter.AddRequestHandler(FControllers, '/api');
  FRequest := TTestRequest.Create;
  FResponse := TTestResponse.Create(FRequest);
  FResponse.Contents.LineBreak := '\n';
  TClientController.ReadingName := '<unset>';
  TClientController.WritingName := '<unset>';
end;

procedure TTestWSControllerRequest.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  FRouter := nil;
end;

procedure TTestWSControllerRequest.ImplicitFncObjInvoker;
begin
  Controllers.AddController(TClientController).
    AddMethod(@TClientController.Read, TypeInfo(@TClientController.Read));
  Request.PathInfo := '/api/client/read';
  TClientController.ReadingName := 'bill';
  Router.RouteRequest(Request, Response);
  AssertEquals('response content', '{"Name":"bill"}\n', Response.Content);
  AssertEquals('response code', 200, Response.Code);
end;

procedure TTestWSControllerRequest.ImplicitProcObjInvoker;
begin
  Controllers.AddController(TClientController).
    AddMethod(@TClientController.Save, TypeInfo(@TClientController.Save));
  Request.PathInfo := '/api/client/save';
  Request.Content:='{ "Name": "jack" }';
  Router.RouteRequest(Request, Response);
  AssertEquals('client name', 'jack', TClientController.WritingName);
  AssertEquals('response code', 200, Response.Code);
end;

procedure TTestWSControllerRequest.InvokeForMethodGet;
begin
  Controllers.AddController(TClientController).
    AddMethod(@TClientController.Get, TypeInfo(@TClientController.Get));
  Request.Method := 'GET';
  Request.PathInfo := '/api/client';
  TClientController.ReadingName := 'john';
  Router.RouteRequest(Request, Response);
  AssertEquals('response content', '{"Name":"john"}\n', Response.Content);
  AssertEquals('response code', 200, Response.Code);
end;

procedure TTestWSControllerRequest.InvokeForMethodPost;
begin
  Controllers.AddController(TClientController).
    AddMethod(@TClientController.Post, TypeInfo(@TClientController.Post));
  Request.Method := 'POST';
  Request.PathInfo := '/api/client';
  Request.Content:='{ "Name": "Jack" }';
  Router.RouteRequest(Request, Response);
  AssertEquals('client name', 'Jack', TClientController.WritingName);
  AssertEquals('response code', 200, Response.Code);
end;

procedure TTestWSControllerRequest.MethodPattern;
begin
  Controllers.AddController(TClientController).
    AddMethod(@TClientController.Read, TypeInfo(@TClientController.Read), ':id');
  Request.PathInfo := '/api/client/read/14';
  TClientController.ReadingName := 'jimmy';
  Router.RouteRequest(Request, Response);
  AssertEquals('id', '14', Request.QueryFields.Values['id']);
  AssertEquals('client name', '{"Name":"jimmy"}\n', Response.Content);
  AssertEquals('response code', 200, Response.Code);
end;

procedure TTestWSControllerRequest.NotFoundIfPatternUnread;
begin
  Controllers.AddController(TClientController).
    AddMethod(@TClientController.Read, TypeInfo(@TClientController.Read), ':id');
  Request.PathInfo := '/api/client/read';
  Router.RouteRequest(Request, Response);
  AssertEquals('response code', 404, Response.Code);
end;

procedure TTestWSControllerRequest.NotFoundIfPathInfoUnread;
begin
  Controllers.AddController(TClientController).
    AddMethod(@TClientController.Read, TypeInfo(@TClientController.Read));
  Request.PathInfo := '/api/client/read/14';
  Router.RouteRequest(Request, Response);
  AssertEquals('response code', 404, Response.Code);
end;

{ TTestResponse }

procedure TTestResponse.DoSendContent;
begin
end;

procedure TTestResponse.DoSendHeaders(Headers: TStrings);
begin
end;

{ TTestInvokerProc }

function TTestInvokerProc.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := AMethodData.MatchProcedure([TObject]);
end;

procedure TTestInvokerProc.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
begin
end;

{ TTestInvokerProcAmbiguous }

function TTestInvokerProcAmbiguous.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := AMethodData.MatchProcedure([TObject]);
end;

procedure TTestInvokerProcAmbiguous.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
begin
end;

{ TTestInvokerProcInherited }

function TTestInvokerProcInherited.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := AMethodData.MatchProcedure([TObject]);
end;

procedure TTestInvokerProcInherited.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
begin
end;

{ TTestInvokerFnc }

function TTestInvokerFnc.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := AMethodData.MatchFunction([], TObject);
end;

procedure TTestInvokerFnc.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
begin
end;

{ TClientController }

function TClientController.Get: TClient;
begin
  Result := Read;
end;

procedure TClientController.Post(const AClient: TClient);
begin
  Save(AClient);
end;

function TClientController.Read: TClient;
begin
  Result := TClient.Create;
  Result.Name := ReadingName;
end;

procedure TClientController.Save(const AClient: TClient);
begin
  WritingName := AClient.Name;
end;

{ TProductController }

function TProductController.Read: TProduct;
begin
  Result := TProduct.Create;
  Result.Description := 'sand';
end;

procedure TProductController.Save(const AProduct: TProduct);
begin
  Data := AProduct.Description;
end;

{ TTestClientInvokerController }

destructor TTestClientInvokerController.Destroy;
begin
  inherited Destroy;
end;

function TTestClientInvokerController.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := True;
end;

procedure TTestClientInvokerController.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
type
  TObjectFnc = function: TObject of object;
  TObjectProc = procedure(const AObject: TObject) of object;
var
  VClient: TClient;
begin
  case AMethodData.MethodKind of
    mkFunction: begin
      VClient := TObjectFnc(AMethod)() as TClient;
      Data := VClient.Name;
      FreeAndNil(VClient);
    end;
    mkProcedure: begin
      VClient := TClient.Create;
      try
        VClient.Name := TClientController.ReadingName;
        TObjectProc(AMethod)(VClient);
      finally
        FreeAndNil(VClient);
      end;
    end;
  end;
end;

initialization
  RegisterTest('jcore.ws.controller', TTestWSMethodRegistry);
  RegisterTest('jcore.ws.controller', TTestWSControllerClass);
  RegisterTest('jcore.ws.controller', TTestWSControllerRequest);

end.

