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

  IJCoreOID = interface(IInterface)
  ['{C4DE3860-6FBC-01A0-BA08-1990558E6AF1}']
    function AsString: string;
  end;

  { IJCoreADM }

  IJCoreADM = interface(IInterface)
  ['{BB774071-F57D-AFB8-70BC-3A2B0DD69690}']
    function IsDirty: Boolean;
  end;

  { IJCoreSimpleType }

  IJCoreSimpleType = interface(IJCoreADM)
  ['{5529EDF7-C6DE-BD26-D47B-F8604C56B481}']
    procedure Clear;
    function IsNull: Boolean;
  end;

  { IJCoreIntegerType }

  IJCoreIntegerType = interface(IJCoreSimpleType)
  ['{0B5AEE1C-752F-B7ED-327F-5D483A5C3D01}']
    function GetValue: Integer;
    procedure SetValue(const AValue: Integer);
    function OldValue: Integer;
    property Value: Integer read GetValue write SetValue;
  end;

  { IJCorePID }

  IJCorePID = interface(IInterface)
  ['{92E3EA6B-0D18-E9DE-53C7-ED02857C558B}']
    function ADMByName(const AAttributeName: string): IJCoreADM;
    function Entity: TObject;
    function IsDirty: Boolean;
    function IsPersistent: Boolean;
    function Lazyload(const AAttrAddr: Pointer): Boolean;
    function OID: IJCoreOID;
    function Owner: IJCorePID;
  end;

  IJCoreModel = interface(IInterface)
  ['{864C46EB-53E8-F576-445C-8573FD52E873}']
    procedure InitEntity(const AEntity: TObject);
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

  TJCoreEntity = class(TObject)
    _proxy: TJCoreEntityProxy;
  public
    destructor Destroy; override;
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
  // True = do nothing, the object(s) were already loaded or I just loaded the object(s)
  // False = I failed to load the attribute, please create it yourself
  Result := Assigned(AAttrAddr) and Assigned(TObject(AAttrAddr^)) and not Assigned(Self);
  if Result then // there is an instance and there isn't a proxy/PID, so nothing to do
    Exit;
  // without an instance: pid.lazyload should be called to retrieve the instance
  // with an instance: pid.lazyload will check if the ADM was properly initialized
  Result := Assigned(Self) and Assigned(FPID) and Assigned(AAttrAddr);
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

{ TJCoreEntity }

destructor TJCoreEntity.Destroy;
begin
  _proxy.Free;
  _proxy := nil;
  inherited Destroy;
end;

end.

