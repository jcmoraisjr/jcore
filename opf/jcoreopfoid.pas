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
  JCoreOPFEntity,
  JCoreOPFDriver;

type

  { TJCoreOPFOID }

  TJCoreOPFOID = class(TJCoreManagedObject, IJCoreOPFOID)
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

  TJCoreOPFOIDArray = array of TJCoreOPFOID;

  { TJCoreOPFIntegerOID }

  TJCoreOPFIntegerOID = class(TJCoreOPFOID)
  private
    FValue: Integer;
  protected
    function GetAsInt64: Int64; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
  public
    constructor Create(const AValue: Integer);
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
    property Value: Integer read FValue;
  end;

  { TJCoreOPFStringOID }

  TJCoreOPFStringOID = class(TJCoreOPFOID)
  private
    FValue: string;
  protected
    function GetAsInt64: Int64; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
  public
    constructor Create(const AValue: string);
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
    property Value: string read FValue;
  end;

implementation

uses
  sysutils;

{ TJCoreOPFIntegerOID }

function TJCoreOPFIntegerOID.GetAsInt64: Int64;
begin
  Result := Value;
end;

function TJCoreOPFIntegerOID.GetAsInteger: Integer;
begin
  Result := Value;
end;

function TJCoreOPFIntegerOID.GetAsString: string;
begin
  Result := IntToStr(Value);
end;

constructor TJCoreOPFIntegerOID.Create(const AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TJCoreOPFIntegerOID.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteInteger(Value);
end;

{ TJCoreOPFStringOID }

function TJCoreOPFStringOID.GetAsInt64: Int64;
begin
  Result := StrToInt64(Value);
end;

function TJCoreOPFStringOID.GetAsInteger: Integer;
begin
  Result := StrToInt(Value);
end;

function TJCoreOPFStringOID.GetAsString: string;
begin
  Result := Value;
end;

constructor TJCoreOPFStringOID.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TJCoreOPFStringOID.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteString(Value);
end;

end.

