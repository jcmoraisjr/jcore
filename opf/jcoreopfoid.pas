(*
  JCore, OPF Object ID Classes
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
  JCoreClasses,
  JCoreEntity,
  JCoreOPFDriver,
  JCoreOPFOIDGen;

type

  { TJCoreOPFOID }

  TJCoreOPFOID = class(TJCoreManagedObject, IJCoreOID)
  private
    function IJCoreOID.AsString = GetAsString;
  protected
    function GetAsString: string; virtual; abstract;
  public
    constructor CreateFromDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator); virtual; abstract;
    constructor CreateFromString(const AStringOID: string); virtual; abstract;
    class procedure WriteNull(const ADriver: TJCoreOPFDriver); virtual; abstract;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
    property AsString: string read GetAsString;
  end;

  TJCoreOPFOIDClass = class of TJCoreOPFOID;
  TJCoreOPFOIDArray = array of TJCoreOPFOID;

  { TJCoreOPFOIDInt64 }

  TJCoreOPFOIDInt64 = class(TJCoreOPFOID)
  private
    FValue: Int64;
  protected
    function GetAsString: string; override;
  public
    constructor Create(const AValue: Int64);
    constructor CreateFromDriver(const ADriver: TJCoreOPFDriver); override;
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator); override;
    constructor CreateFromString(const AStringOID: string); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const ADriver: TJCoreOPFDriver); override;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
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
    constructor CreateFromDriver(const ADriver: TJCoreOPFDriver); override;
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator); override;
    constructor CreateFromString(const AStringOID: string); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const ADriver: TJCoreOPFDriver); override;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
    property Value: string read FValue;
  end;

implementation

uses
  sysutils;

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

constructor TJCoreOPFOIDInt64.CreateFromDriver(const ADriver: TJCoreOPFDriver);
begin
  Create(ADriver.ReadInt64);
end;

constructor TJCoreOPFOIDInt64.CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator);
begin
  Create(AGenerator.ReadInt64);
end;

constructor TJCoreOPFOIDInt64.CreateFromString(const AStringOID: string);
begin
  Create(StrToInt64(AStringOID));
end;

function TJCoreOPFOIDInt64.Equals(AOther: TObject): Boolean;
begin
  Result := (AOther is TJCoreOPFOIDInt64) and (TJCoreOPFOIDInt64(AOther).Value = Value);
end;

class procedure TJCoreOPFOIDInt64.WriteNull(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteNull;
end;

procedure TJCoreOPFOIDInt64.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteInt64(Value);
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

constructor TJCoreOPFOIDString.CreateFromDriver(const ADriver: TJCoreOPFDriver);
begin
  Create(ADriver.ReadString);
end;

constructor TJCoreOPFOIDString.CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator);
begin
  Create(AGenerator.ReadString);
end;

constructor TJCoreOPFOIDString.CreateFromString(const AStringOID: string);
begin
  Create(AStringOID);
end;

function TJCoreOPFOIDString.Equals(AOther: TObject): Boolean;
begin
  Result := (AOther is TJCoreOPFOIDString) and (TJCoreOPFOIDString(AOther).Value = Value);
end;

class procedure TJCoreOPFOIDString.WriteNull(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteNull;
end;

procedure TJCoreOPFOIDString.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteString(Value);
end;

end.

