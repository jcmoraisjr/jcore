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
    function Lazyload(const AAttrAddr: Pointer): Boolean;
    function OID: IJCoreOID;
    function Owner: IJCorePID;
    property IsPersistent: Boolean read GetIsPersistent;
  end;

  { TJCoreEntityProxy }

  {$M+}
  TJCoreEntityProxy = class(TObject)
  private
    FPID: IJCorePID;
  public
    constructor Create(const APID: IJCorePID);
    function IsDirty: Boolean;
    function IsPersistent: Boolean;
    function Lazyload(const AAttrAddr: Pointer): Boolean;
    function OID: IJCoreOID;
    function Owner: IJCorePID;
    function PID: IJCorePID;
  end;
  {$M-}

implementation

{ TJCoreEntityProxy }

constructor TJCoreEntityProxy.Create(const APID: IJCorePID);
begin
  inherited Create;
  FPID := APID;
end;

function TJCoreEntityProxy.IsDirty: Boolean;
begin
  if Assigned(Self) and Assigned(FPID) then
    Result := FPID.IsDirty
  else
    Result := True;
end;

function TJCoreEntityProxy.IsPersistent: Boolean;
begin
  if Assigned(Self) and Assigned(FPID) then
    Result := FPID.IsPersistent
  else
    Result := False;
end;

function TJCoreEntityProxy.Lazyload(const AAttrAddr: Pointer): Boolean;
begin
  Result := Assigned(Self) and Assigned(FPID);
  if Result then
    Result := FPID.Lazyload(AAttrAddr);
end;

function TJCoreEntityProxy.OID: IJCoreOID;
begin
  if Assigned(Self) and Assigned(FPID) then
    Result := FPID.OID
  else
    Result := nil;
end;

function TJCoreEntityProxy.Owner: IJCorePID;
begin
  if Assigned(Self) and Assigned(FPID) then
    Result := FPID.Owner
  else
    Result := nil;
end;

function TJCoreEntityProxy.PID: IJCorePID;
begin
  if Assigned(Self) and Assigned(FPID) then
    Result := FPID
  else
    Result := nil;
end;

end.

