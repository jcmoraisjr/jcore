(*
  Usage ( *nix ):
    get:
      curl -i http://localhost:8080/api/person
      wget --quiet --server-response -O- http://localhost:8080/api/person
      * or point your browser to http://localhost:8080/api/person
    post:
      curl -i -d '{"Name":"bill"}' http://localhost:8080/api/person
      wget --quiet --server-response --post-data='{"Name":"bill"}' -O- http://localhost:8080/api/person
      * output of post method will be displayed in the stdout
*)

program ws001;

uses
  heaptrc,
  Classes,
  JCoreDIC,
  JCoreWSIntf,
  JCoreWSRequest,
  JCoreWSCGIApp,
  JCoreWSHTTPServerApp;

type

  { TPerson }

  TPerson = class(TPersistent)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TPersonController }

  TPersonController = class(TPersistent)
  published
    function Get: TPerson;
    procedure Post(const APerson: TPerson);
  end;

{ TPersonController }

function TPersonController.Get: TPerson;
begin
  Result := TPerson.Create;
  Result.Name := 'jimmy';
end;

procedure TPersonController.Post(const APerson: TPerson);
begin
  // Comment out if using CGI-BIN application
  writeln('Person name: ', APerson.Name);
end;

var
  VRouter: IJCoreWSRequestRouter;
  VHandler: TJCoreWSRESTRequestHandler;
  VApp: IJCoreWSApplicationHandler;
begin
  TJCoreDIC.Locate(IJCoreWSRequestRouter, VRouter);
  VHandler := TJCoreWSRESTRequestHandler.Create;
  VRouter.AddRequestHandler(VHandler, '/api');
  VHandler.AddController(TPersonController).
    AddMethod(@TPersonController.Get, TypeInfo(@TPersonController.Get)).
    AddMethod(@TPersonController.Post, TypeInfo(@TPersonController.Post));

  // Valid qualifiers are HTTP (embedded server) and CGI (cgi-bin application)
  // Comment out the writeln above if using CGI
  TJCoreDIC.Locate(IJCoreWSApplicationHandler, 'HTTP', VApp);
  VApp.Params.Values['Threaded'] := 'False';
  VApp.Run;
end.

