(*
  JCore, Entity Support Interfaces
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreEntity;

{$I jcore.inc}

interface

type

  { IJCoreOID }

  IJCoreOID = interface
  ['{C4DE3860-6FBC-01A0-BA08-1990558E6AF1}']
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsString: string;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
    property AsString: string read GetAsString;
  end;

  { IJCoreADM }

  IJCoreADM = interface
  ['{BB774071-F57D-AFB8-70BC-3A2B0DD69690}']
    function IsDirty: Boolean;
    procedure UpdateCache;
  end;

  { IJCorePID }

  IJCorePID = interface(IInterface)
  ['{92E3EA6B-0D18-E9DE-53C7-ED02857C558B}']
    function ADMByName(const AAttributeName: string): IJCoreADM;
    function Entity: TObject;
    function GetIsPersistent: Boolean;
    function IsDirty: Boolean;
    function OID: IJCoreOID;
    function Owner: IJCorePID;
    property IsPersistent: Boolean read GetIsPersistent;
  end;

implementation

end.

