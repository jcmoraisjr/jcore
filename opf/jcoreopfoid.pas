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
  JCoreOPFGenerator;

type

  { TJCoreOPFOID }

  TJCoreOPFOID = class(TJCoreManagedObject, IJCoreOID)
  protected
    function GetAsString: string; virtual; abstract;
  public
    constructor CreateFromDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFGenerator); virtual; abstract;
    constructor CreateFromString(const AStringOID: string); virtual; abstract;
    class procedure WriteNull(const ADriver: TJCoreOPFDriver); virtual; abstract;
    procedure WriteToDriver(const ADriver: TJCoreOPFDriver); virtual; abstract;
    property AsString: string read GetAsString;
  end;

  TJCoreOPFOIDClass = class of TJCoreOPFOID;
  TJCoreOPFOIDArray = array of TJCoreOPFOID;

  { TJCoreOPFIntegerOID }

  TJCoreOPFIntegerOID = class(TJCoreOPFOID)
  private
    FValue: Integer;
  protected
    function GetAsString: string; override;
  public
    constructor Create(const AValue: Integer);
    constructor CreateFromDriver(const ADriver: TJCoreOPFDriver); override;
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFGenerator); override;
    constructor CreateFromString(const AStringOID: string); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const ADriver: TJCoreOPFDriver); override;
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
    constructor CreateFromDriver(const ADriver: TJCoreOPFDriver); override;
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFGenerator); override;
    constructor CreateFromString(const AStringOID: string); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const ADriver: TJCoreOPFDriver); override;
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

constructor TJCoreOPFIntegerOID.CreateFromDriver(const ADriver: TJCoreOPFDriver);
begin
  Create(ADriver.ReadInteger);
end;

constructor TJCoreOPFIntegerOID.CreateFromGenerator(const AGenerator: IJCoreOPFGenerator);
begin
  Create(AGenerator.ReadInt64);
end;

constructor TJCoreOPFIntegerOID.CreateFromString(const AStringOID: string);
begin
  Create(StrToInt(AStringOID));
end;

function TJCoreOPFIntegerOID.Equals(AOther: TObject): Boolean;
begin
  Result := (AOther is TJCoreOPFIntegerOID) and (TJCoreOPFIntegerOID(AOther).Value = Value);
end;

class procedure TJCoreOPFIntegerOID.WriteNull(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteNull;
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

constructor TJCoreOPFStringOID.CreateFromDriver(const ADriver: TJCoreOPFDriver);
begin
  Create(ADriver.ReadString);
end;

constructor TJCoreOPFStringOID.CreateFromGenerator(const AGenerator: IJCoreOPFGenerator);
begin
  Create(AGenerator.ReadString);
end;

constructor TJCoreOPFStringOID.CreateFromString(const AStringOID: string);
begin
  Create(AStringOID);
end;

function TJCoreOPFStringOID.Equals(AOther: TObject): Boolean;
begin
  Result := (AOther is TJCoreOPFStringOID) and (TJCoreOPFStringOID(AOther).Value = Value);
end;

class procedure TJCoreOPFStringOID.WriteNull(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteNull;
end;

procedure TJCoreOPFStringOID.WriteToDriver(const ADriver: TJCoreOPFDriver);
begin
  ADriver.WriteString(Value);
end;

end.

