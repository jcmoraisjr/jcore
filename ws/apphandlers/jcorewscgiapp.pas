(*
  JCore WebServices, CGI Application Handler Classes
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreWSCGIApp;

{$I jcore.inc}

interface

uses
  Classes,
  HTTPDefs,
  custweb,
  custcgi,
  JCoreWSIntf,
  JCoreWSRequest;

type

  { TJCoreWSFCLCGIHandler }

  TJCoreWSFCLCGIHandler = class(TCGIHandler)
  private
    FRequestRouter: IJCoreWSRequestRouter;
  public
    constructor Create(AOwner: TComponent); override;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TJCoreWSFCLCGIApp }

  TJCoreWSFCLCGIApp = class(TCustomCGIApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

  { TJCoreWSCGIApplicationHandler }

  TJCoreWSCGIApplicationHandler = class(TInterfacedObject, IJCoreWSApplicationHandler)
  private
    FCGIApp: TJCoreWSFCLCGIApp;
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

{ TJCoreWSFCLCGIHandler }

constructor TJCoreWSFCLCGIHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TJCoreDIC.Locate(IJCoreWSRequestRouter, FRequestRouter);
end;

procedure TJCoreWSFCLCGIHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  FRequestRouter.RouteRequest(ARequest, AResponse);
end;

{ TJCoreWSFCLCGIApp }

function TJCoreWSFCLCGIApp.InitializeWebHandler: TWebHandler;
begin
  Result := TJCoreWSFCLCGIHandler.Create(Self);
end;

{ TJCoreWSCGIApplicationHandler }

destructor TJCoreWSCGIApplicationHandler.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FCGIApp);
  inherited Destroy;
end;

function TJCoreWSCGIApplicationHandler.Params: TStrings;
begin
  if not Assigned(FParams) then
    FParams := TStringList.Create;
  Result := FParams;
end;

procedure TJCoreWSCGIApplicationHandler.Run;
begin
  FreeAndNil(FCGIApp);
  FCGIApp := TJCoreWSFCLCGIApp.Create(nil);
  FCGIApp.Initialize;
  FCGIApp.Run;
end;

initialization
  TJCoreDIC.Register(IJCoreWSApplicationHandler, 'CGI', TJCoreWSCGIApplicationHandler, jdsApplication);

finalization
  TJCoreDIC.Unregister(IJCoreWSApplicationHandler, TJCoreWSCGIApplicationHandler);

end.

