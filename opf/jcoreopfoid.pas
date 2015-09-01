(*
  JCore, OPF OID Storage and Generator Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFOID;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  JCoreEntity,
  JCoreOPFDriver;

type

  IJCoreOPFOID = interface(IJCoreOID)
    function EqualsOID(const AOther: IJCoreOPFOID): Boolean;
    procedure WriteToParams(const AParams: IJCoreOPFParams);
  end;

  { IJCoreOPFOIDGenerator }

  IJCoreOPFOIDGenerator = interface(IInterface)
    function ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
    function ReadString(const ADriver: TJCoreOPFDriver): string;
  end;

  { TJCoreOPFOID }

  TJCoreOPFOID = class(TInterfacedObject, IJCoreOPFOID)
  private
    function IJCoreOPFOID.AsString = GetAsString;
  protected
    function GetAsString: string; virtual; abstract;
  public
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator; const ADriver: TJCoreOPFDriver); virtual; abstract;
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
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator; const ADriver: TJCoreOPFDriver); override;
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
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator; const ADriver: TJCoreOPFDriver); override;
    constructor CreateFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    constructor CreateFromString(const AStringOID: string); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const AParams: IJCoreOPFParams); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    property Value: string read FValue;
  end;

  { TJCoreOPFOIDGeneratorSequence }

  TJCoreOPFOIDGeneratorSequence = class(TInterfacedObject, IJCoreOPFOIDGenerator)
  private
    FSequenceName: string;
  protected
    property SequenceName: string read FSequenceName;
  public
    constructor Create(const ASequenceName: string);
    function ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
    function ReadString(const ADriver: TJCoreOPFDriver): string;
  end;

  { TJCoreOPFOIDGeneratorGUID }

  TJCoreOPFOIDGeneratorGUID = class(TInterfacedObject, IJCoreOPFOIDGenerator)
  public
    function ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
    function ReadString(const ADriver: TJCoreOPFDriver): string;
  end;

implementation

uses
  sysutils,
  JCoreConsts,
  JCoreClasses;

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

constructor TJCoreOPFOIDInt64.CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator;
  const ADriver: TJCoreOPFDriver);
begin
  Create(AGenerator.ReadInt64(ADriver));
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

constructor TJCoreOPFOIDString.CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator;
  const ADriver: TJCoreOPFDriver);
begin
  Create(AGenerator.ReadString(ADriver));
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

{ TJCoreOPFOIDGeneratorSequence }

constructor TJCoreOPFOIDGeneratorSequence.Create(const ASequenceName: string);
begin
  inherited Create;
  FSequenceName := ASequenceName;
end;

function TJCoreOPFOIDGeneratorSequence.ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
var
  VResultSet: IJCoreOPFSQLResultSet;
begin
  if not (ADriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPF.Create(2109, S2109_DriverIsNotSQL, [ADriver.DriverName]);
  VResultSet := TJCoreOPFSQLDriver(ADriver).SequenceResultSet(SequenceName, 1);
  Result := VResultSet.ReadInt64;
end;

function TJCoreOPFOIDGeneratorSequence.ReadString(const ADriver: TJCoreOPFDriver): string;
begin
  Result := IntToStr(ReadInt64(ADriver));
end;

{ TJCoreOPFOIDGeneratorGUID }

{$warn 5033 off}
function TJCoreOPFOIDGeneratorGUID.ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
begin
  raise EJCoreOPF.Create(2121, S2121_UnsupportedAttributeType, [SJCoreInt64ValueMsg]);
end;
{$warn 5033 on}

function TJCoreOPFOIDGeneratorGUID.ReadString(const ADriver: TJCoreOPFDriver): string;
var
  VGUID: array[0..15] of Byte;
  I: Integer;
begin
  { TODO : Check CreateGUID result }
  CreateGUID(TGUID(VGUID));
  SetLength(Result, 32);
  for I := 0 to 15 do
    Move(IntToHex(VGUID[I], 2)[1], Result[2*I+1], 2);
end;

end.

