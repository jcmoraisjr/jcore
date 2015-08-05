(*
  JCore WebServices, Public Interfaces
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreWSIntf;

{$I jcore.inc}

interface

uses
  Classes,
  HTTPDefs;

type

  { IJCoreWSRequestHandler }

  IJCoreWSRequestHandler = interface(IInterface)
  ['{9456CC12-A428-DB7F-B6D8-6BA4B4877E52}']
    procedure HandleRequest(const ARequest: TRequest; const AResponse: TResponse);
  end;

  { IJCoreWSRequestRouter }

  IJCoreWSRequestRouter = interface(IInterface)
  ['{631AEA60-951D-8E5C-784A-29ADA11D7FCD}']
    procedure AddRequestHandler(const ARequestHandler: IJCoreWSRequestHandler; const APattern: string);
    procedure RouteRequest(const ARequest: TRequest; const AResponse: TResponse);
  end;

  { IJCoreWSApplicationHandler }

  IJCoreWSApplicationHandler = interface(IInterface)
  ['{9A538879-A492-0E74-397F-2E397D1A480C}']
    function Params: TStrings;
    procedure Run;
  end;

implementation

end.

