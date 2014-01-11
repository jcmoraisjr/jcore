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
  JCoreOPFDriver;

type

  { TJCoreOPFOID }

  TJCoreOPFOID = class(TObject)
  public
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
  end;

  { TJCoreOPFIntegerOID }

  TJCoreOPFIntegerOID = class(TJCoreOPFOID)
  private
    FValue: Integer;
  public
    constructor Create(const AValue: Integer);
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
    property Value: Integer read FValue;
  end;

  { TJCoreOPFStringOID }

  TJCoreOPFStringOID = class(TJCoreOPFOID)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); override;
    property Value: string read FValue;
  end;

implementation

{ TJCoreOPFIntegerOID }

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

