(*
  JCore, OPF OID Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFOID;

{$I jcore.inc}

interface

uses
  JCoreEntity,
  JCoreOPFDriver,
  JCoreOPFOIDGen;

type

  IJCoreOPFOID = interface(IJCoreOID)
    function EqualsOID(const AOther: IJCoreOPFOID): Boolean;
    procedure WriteToParams(const AParams: IJCoreOPFParams);
  end;

  { TJCoreOPFOID }

  TJCoreOPFOID = class(TInterfacedObject, IJCoreOPFOID)
  private
    function IJCoreOPFOID.AsString = GetAsString;
  protected
    function GetAsString: string; virtual; abstract;
  public
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator); virtual; abstract;
    constructor CreateFromResultSet(const AResultSet: IJCoreOPFResultSet); virtual; abstract;
    constructor CreateFromString(const AStringOID: string); virtual; abstract;
    function EqualsOID(const AOther: IJCoreOPFOID): Boolean;
    class procedure WriteNull(const AParams: IJCoreOPFParams); virtual; abstract;
    procedure WriteToParams(const AParams: IJCoreOPFParams); virtual; abstract;
    property AsString: string read GetAsString;
  end;

  TJCoreOPFOIDClass = class of TJCoreOPFOID;
  TJCoreOPFOIDArray = array of IJCoreOPFOID;

  { TJCoreOPFOIDInt64 }

  TJCoreOPFOIDInt64 = class(TJCoreOPFOID)
  private
    FValue: Int64;
  protected
    function GetAsString: string; override;
  public
    constructor Create(const AValue: Int64);
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator); override;
    constructor CreateFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    constructor CreateFromString(const AStringOID: string); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const AParams: IJCoreOPFParams); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    property Value: Int64 read FValue;
  end;

  { TJCoreOPFOIDString }

  TJCoreOPFOIDString = class(TJCoreOPFOID)
  private
    FValue: string;
  protected
    function GetAsString: string; override;
  public
    constructor Create(const AValue: string);
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator); override;
    constructor CreateFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    constructor CreateFromString(const AStringOID: string); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const AParams: IJCoreOPFParams); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    property Value: string read FValue;
  end;

implementation

uses
  sysutils;

{ TJCoreOPFOID }

function TJCoreOPFOID.EqualsOID(const AOther: IJCoreOPFOID): Boolean;
begin
  Result := Assigned(AOther) and (AOther.AsString = AsString);
end;

{ TJCoreOPFOIDInt64 }

function TJCoreOPFOIDInt64.GetAsString: string;
begin
  Result := IntToStr(Value);
end;

constructor TJCoreOPFOIDInt64.Create(const AValue: Int64);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TJCoreOPFOIDInt64.CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator);
begin
  Create(AGenerator.ReadInt64);
end;

constructor TJCoreOPFOIDInt64.CreateFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Create(AResultSet.ReadInt64);
end;

constructor TJCoreOPFOIDInt64.CreateFromString(const AStringOID: string);
begin
  Create(StrToInt64(AStringOID));
end;

function TJCoreOPFOIDInt64.Equals(AOther: TObject): Boolean;
begin
  Result := (AOther is TJCoreOPFOIDInt64) and (TJCoreOPFOIDInt64(AOther).Value = Value);
end;

class procedure TJCoreOPFOIDInt64.WriteNull(const AParams: IJCoreOPFParams);
begin
  AParams.WriteNull;
end;

procedure TJCoreOPFOIDInt64.WriteToParams(const AParams: IJCoreOPFParams);
begin
  AParams.WriteInt64(Value);
end;

{ TJCoreOPFOIDString }

function TJCoreOPFOIDString.GetAsString: string;
begin
  Result := Value;
end;

constructor TJCoreOPFOIDString.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TJCoreOPFOIDString.CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator);
begin
  Create(AGenerator.ReadString);
end;

constructor TJCoreOPFOIDString.CreateFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Create(AResultSet.ReadString);
end;

constructor TJCoreOPFOIDString.CreateFromString(const AStringOID: string);
begin
  Create(AStringOID);
end;

function TJCoreOPFOIDString.Equals(AOther: TObject): Boolean;
begin
  Result := (AOther is TJCoreOPFOIDString) and (TJCoreOPFOIDString(AOther).Value = Value);
end;

class procedure TJCoreOPFOIDString.WriteNull(const AParams: IJCoreOPFParams);
begin
  AParams.WriteNull;
end;

procedure TJCoreOPFOIDString.WriteToParams(const AParams: IJCoreOPFParams);
begin
  AParams.WriteString(Value);
end;

end.

