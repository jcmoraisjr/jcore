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
  JCoreOPFDriver;

type

  { TJCoreOPFOID }

  TJCoreOPFOID = class(TJCoreManagedObject, IJCoreOID)
  protected
    function GetAsString: string; virtual; abstract;
  public
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
  end;

  TJCoreOPFOIDArray = array of TJCoreOPFOID;

  { TJCoreOPFIntegerOID }

  TJCoreOPFIntegerOID = class(TJCoreOPFOID)
  private
    FValue: Integer;
  protected
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

