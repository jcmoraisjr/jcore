(*
  JCore, OPF ID Interfaces
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFID;

{$I jcore.inc}

interface

uses
  JCoreOPFDriver;

type

  { TJCoreOPFOID }

  // COM interface wasn't used because the amount of inc/decref calls in the
  // PID.OID property.
  // CORBA interface isn't a good option because of the lifecycle of the
  // instance in some points of the code, and we don't want to mix interfaces
  // that need and don't need to be managed.
  TJCoreOPFOID = class(TObject)
  protected
    function GetAsInt64: Int64; virtual; abstract;
    function GetAsInteger: Integer; virtual; abstract;
    function GetAsString: string; virtual; abstract;
  public
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
    property AsString: string read GetAsString;
  end;

  { IJCoreOPFPID }

  IJCoreOPFPID = interface(IInterface)
  ['{C2E47A60-B063-1FC0-D566-BAAC73195623}']
    procedure AssignOID(const AOID: TJCoreOPFOID);
    procedure Commit;
    function GetEntity: TObject;
    function GetIsPersistent: Boolean;
    function GetOID: TJCoreOPFOID;
    function GetOwner: IJCoreOPFPID;
    procedure ReleaseOID(const AOID: TJCoreOPFOID);
    procedure SetOwner(const AValue: IJCoreOPFPID);
    property Entity: TObject read GetEntity;
    property IsPersistent: Boolean read GetIsPersistent;
    property OID: TJCoreOPFOID read GetOID;
    property Owner: IJCoreOPFPID read GetOwner write SetOwner;
  end;

  TJCoreOPFPIDArray = array of IJCoreOPFPID;

implementation

end.

