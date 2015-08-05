(*
  JCore WebServices, Embedded HTTP Server Application Handler Classes
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreWSHTTPServerApp;

{$I jcore.inc}

interface

uses
  Classes,
  HTTPDefs,
  custweb,
  custhttpapp,
  JCoreWSIntf,
  JCoreWSRequest;

type

  { TJCoreWSFCLHTTPHandler }

  TJCoreWSFCLHTTPHandler = class(TFPHTTPServerHandler)
  private
    FRequestRouter: IJCoreWSRequestRouter;
  public
    constructor Create(AOwner: TComponent); override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TJCoreWSFCLHTTPApp }

  TJCoreWSFCLHTTPApp = class(TCustomHTTPApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

  { TJCoreWSHTTPServerHandler }

  TJCoreWSHTTPServerHandler = class(TInterfacedObject, IJCoreWSApplicationHandler)
  private
    FHTTPApp: TJCoreWSFCLHTTPApp;
    FParams: TStrings;
  public
    destructor Destroy; override;
    function Params: TStrings;
    procedure Run;
  end;

implementation

uses
  sysutils,
  JCoreDIC;

{ TJCoreWSFCLHTTPHandler }

constructor TJCoreWSFCLHTTPHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TJCoreDIC.Locate(IJCoreWSRequestRouter, FRequestRouter);
end;

procedure TJCoreWSFCLHTTPHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  FRequestRouter.RouteRequest(ARequest, AResponse);
end;

{ TJCoreWSFCLHTTPApp }

function TJCoreWSFCLHTTPApp.InitializeWebHandler: TWebHandler;
begin
  Result := TJCoreWSFCLHTTPHandler.Create(Self);
end;

{ TJCoreWSHTTPServerHandler }

destructor TJCoreWSHTTPServerHandler.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FHTTPApp);
  inherited Destroy;
end;

function TJCoreWSHTTPServerHandler.Params: TStrings;
begin
  if not Assigned(FParams) then
  begin
    FParams := TStringList.Create;
    FParams.Values['Title'] := 'httpserver';
    FParams.Values['Port'] := '8080';
    FParams.Values['Threaded'] := 'True';
  end;
  Result := FParams;
end;

procedure TJCoreWSHTTPServerHandler.Run;
begin
  FreeAndNil(FHTTPApp);
  FHTTPApp := TJCoreWSFCLHTTPApp.Create(nil);
  FHTTPApp.Title := Params.Values['Title'];
  FHTTPApp.Port := StrToInt(Params.Values['Port']);
  FHTTPApp.Threaded := SameText(Params.Values['Threaded'], 'True');
  FHTTPApp.Initialize;
  FHTTPApp.Run;
end;

initialization
  TJCoreDIC.Register(IJCoreWSApplicationHandler, 'HTTP', TJCoreWSHTTPServerHandler, jdsApplication);

finalization
  TJCoreDIC.Unregister(IJCoreWSApplicationHandler, TJCoreWSHTTPServerHandler);

end.

