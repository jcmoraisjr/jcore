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
  fgl,
  JCoreClasses,
  JCoreEntity,
  JCoreOPFDriver;

type

  IJCoreOPFOID = interface(IJCoreOID)
    function EqualsOID(const AOther: IJCoreOPFOID): Boolean;
    procedure WriteToParams(const AParams: IJCoreOPFParams);
    procedure WriteToOIDProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray);
  end;

  { IJCoreOPFOIDGenerator }

  IJCoreOPFOIDGenerator = interface(IInterface)
    function IsPostInsertGenerator: Boolean;
    function ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
    function ReadString(const ADriver: TJCoreOPFDriver): string;
  end;

  { TJCoreOPFOID }

  TJCoreOPFOID = class(TInterfacedObject, IJCoreOPFOID)
  private
    function IJCoreOPFOID.AsString = GetAsString;
  protected
    class procedure CheckPropInfo(const AOIDProp: TJCorePropInfoArray);
    function GetAsString: string; virtual; abstract;
  public
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator; const ADriver: TJCoreOPFDriver); virtual; abstract;
    constructor CreateFromResultSet(const AResultSet: IJCoreOPFResultSet); virtual; abstract;
    constructor CreateFromString(const AStringOID: string); virtual; abstract;
    class function Apply(const AOIDProp: TJCorePropInfoArray): Boolean; virtual;
    class procedure ClearProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray); virtual; abstract;
    function EqualsOID(const AOther: IJCoreOPFOID): Boolean;
    class procedure WriteNull(const AParams: IJCoreOPFParams); virtual; abstract;
    procedure WriteToParams(const AParams: IJCoreOPFParams); virtual; abstract;
    procedure WriteToOIDProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray); virtual; abstract;
    property AsString: string read GetAsString;
  end;

  TJCoreOPFOIDClass = class of TJCoreOPFOID;
  TJCoreOPFOIDClassList = specialize TFPGList<TJCoreOPFOIDClass>;
  TJCoreOPFOIDArray = array of IJCoreOPFOID;

  { TJCoreOPFOIDInt32 }

  TJCoreOPFOIDInt32 = class(TJCoreOPFOID)
  private
    FValue: Integer;
  protected
    function GetAsString: string; override;
  public
    constructor Create(const AValue: Integer);
    constructor CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator; const ADriver: TJCoreOPFDriver); override;
    constructor CreateFromResultSet(const AResultSet: IJCoreOPFResultSet); override;
    constructor CreateFromString(const AStringOID: string); override;
    class function Apply(const AOIDProp: TJCorePropInfoArray): Boolean; override;
    class procedure ClearProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const AParams: IJCoreOPFParams); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    procedure WriteToOIDProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray); override;
    property Value: Integer read FValue;
  end;

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
    class function Apply(const AOIDProp: TJCorePropInfoArray): Boolean; override;
    class procedure ClearProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const AParams: IJCoreOPFParams); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    procedure WriteToOIDProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray); override;
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
    class function Apply(const AOIDProp: TJCorePropInfoArray): Boolean; override;
    class procedure ClearProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray); override;
    function Equals(AOther: TObject): Boolean; override;
    class procedure WriteNull(const AParams: IJCoreOPFParams); override;
    procedure WriteToParams(const AParams: IJCoreOPFParams); override;
    procedure WriteToOIDProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray); override;
    property Value: string read FValue;
  end;

  { TJCoreOPFOIDGeneratorSQL }

  TJCoreOPFOIDGeneratorSQL = class(TInterfacedObject)
  protected
    function DriverSQL(const ADriver: TJCoreOPFDriver): TJCoreOPFSQLDriver;
    function ReadOID(const ADriver: TJCoreOPFSQLDriver; const ASQL: string): Int64;
  end;

  { TJCoreOPFOIDGeneratorSequence }

  TJCoreOPFOIDGeneratorSequence = class(TJCoreOPFOIDGeneratorSQL, IJCoreOPFOIDGenerator)
  private
    FSequenceName: string;
  protected
    property SequenceName: string read FSequenceName;
  public
    constructor Create(const ASequenceName: string);
    function IsPostInsertGenerator: Boolean;
    function ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
    function ReadString(const ADriver: TJCoreOPFDriver): string;
  end;

  { TJCoreOPFOIDGeneratorAutoinc }

  TJCoreOPFOIDGeneratorAutoinc = class(TJCoreOPFOIDGeneratorSQL, IJCoreOPFOIDGenerator)
  public
    function IsPostInsertGenerator: Boolean;
    function ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
    function ReadString(const ADriver: TJCoreOPFDriver): string;
  end;

  { TJCoreOPFOIDGeneratorGUID }

  TJCoreOPFOIDGeneratorGUID = class(TInterfacedObject, IJCoreOPFOIDGenerator)
  public
    function IsPostInsertGenerator: Boolean;
    function ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
    function ReadString(const ADriver: TJCoreOPFDriver): string;
  end;

implementation

uses
  sysutils,
  typinfo,
  JCoreConsts;

{ TJCoreOPFOID }

class procedure TJCoreOPFOID.CheckPropInfo(const AOIDProp: TJCorePropInfoArray);
begin
  if not Apply(AOIDProp) then
    raise EJCoreOPF.Create(2134, S2134_InvalidOIDProp, [ClassName]);
end;

class function TJCoreOPFOID.Apply(const AOIDProp: TJCorePropInfoArray): Boolean;
begin
  Result := False;
end;

function TJCoreOPFOID.EqualsOID(const AOther: IJCoreOPFOID): Boolean;
begin
  Result := Assigned(AOther) and (AOther.AsString = AsString);
end;

{ TJCoreOPFOIDInt32 }

function TJCoreOPFOIDInt32.GetAsString: string;
begin
  Result := IntToStr(Value);
end;

constructor TJCoreOPFOIDInt32.Create(const AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TJCoreOPFOIDInt32.CreateFromGenerator(const AGenerator: IJCoreOPFOIDGenerator;
  const ADriver: TJCoreOPFDriver);
begin
  { TODO : check overflow }
  Create(AGenerator.ReadInt64(ADriver));
end;

constructor TJCoreOPFOIDInt32.CreateFromResultSet(const AResultSet: IJCoreOPFResultSet);
begin
  Create(AResultSet.ReadInt32);
end;

constructor TJCoreOPFOIDInt32.CreateFromString(const AStringOID: string);
begin
  Create(StrToInt(AStringOID));
end;

class function TJCoreOPFOIDInt32.Apply(const AOIDProp: TJCorePropInfoArray): Boolean;
begin
  Result := (Length(AOIDProp) = 1) and (AOIDProp[0]^.PropType^.Kind = tkInteger);
end;

class procedure TJCoreOPFOIDInt32.ClearProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray);
begin
  CheckPropInfo(AOIDProp);
  SetOrdProp(AEntity, AOIDProp[0], 0);
end;

function TJCoreOPFOIDInt32.Equals(AOther: TObject): Boolean;
begin
  Result:=inherited Equals(AOther);
end;

class procedure TJCoreOPFOIDInt32.WriteNull(const AParams: IJCoreOPFParams);
begin
  AParams.WriteNull;
end;

procedure TJCoreOPFOIDInt32.WriteToParams(const AParams: IJCoreOPFParams);
begin
  AParams.WriteInt32(Value);
end;

procedure TJCoreOPFOIDInt32.WriteToOIDProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray);
begin
  CheckPropInfo(AOIDProp);
  SetOrdProp(AEntity, AOIDProp[0], Value);
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

class function TJCoreOPFOIDInt64.Apply(const AOIDProp: TJCorePropInfoArray): Boolean;
begin
  Result := (Length(AOIDProp) = 1) and (AOIDProp[0]^.PropType^.Kind = tkInt64);
end;

class procedure TJCoreOPFOIDInt64.ClearProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray);
begin
  CheckPropInfo(AOIDProp);
  SetOrdProp(AEntity, AOIDProp[0], 0);
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

procedure TJCoreOPFOIDInt64.WriteToOIDProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray);
begin
  CheckPropInfo(AOIDProp);
  SetOrdProp(AEntity, AOIDProp[0], Value);
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

class function TJCoreOPFOIDString.Apply(const AOIDProp: TJCorePropInfoArray): Boolean;
begin
  Result := (Length(AOIDProp) = 1) and (AOIDProp[0]^.PropType^.Kind = tkAString);
end;

class procedure TJCoreOPFOIDString.ClearProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray);
begin
  CheckPropInfo(AOIDProp);
  SetStrProp(AEntity, AOIDProp[0], '');
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

procedure TJCoreOPFOIDString.WriteToOIDProp(const AEntity: TObject; const AOIDProp: TJCorePropInfoArray);
begin
  CheckPropInfo(AOIDProp);
  SetStrProp(AEntity, AOIDProp[0], Value);
end;

{ TJCoreOPFOIDGeneratorSQL }

function TJCoreOPFOIDGeneratorSQL.DriverSQL(const ADriver: TJCoreOPFDriver): TJCoreOPFSQLDriver;
begin
  if not (ADriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPF.Create(2109, S2109_DriverIsNotSQL, [ADriver.DriverName]);
  Result := TJCoreOPFSQLDriver(ADriver);
end;

function TJCoreOPFOIDGeneratorSQL.ReadOID(const ADriver: TJCoreOPFSQLDriver; const ASQL: string): Int64;
var
  VStmt: IJCoreOPFSQLStatement;
  VResultSet: IJCoreOPFSQLResultSet;
begin
  VStmt := ADriver.CreateStatement;
  VStmt.SQL := ASQL;
  VResultSet := VStmt.OpenCursor(1);
  Result := VResultSet.ReadInt64;
end;

{ TJCoreOPFOIDGeneratorSequence }

constructor TJCoreOPFOIDGeneratorSequence.Create(const ASequenceName: string);
begin
  inherited Create;
  FSequenceName := ASequenceName;
end;

function TJCoreOPFOIDGeneratorSequence.IsPostInsertGenerator: Boolean;
begin
  Result := False;
end;

function TJCoreOPFOIDGeneratorSequence.ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
var
  VDriver: TJCoreOPFSQLDriver;
begin
  VDriver := DriverSQL(ADriver);
  Result := ReadOID(VDriver, VDriver.Database.SequenceSQL(SequenceName, 1));
end;

function TJCoreOPFOIDGeneratorSequence.ReadString(const ADriver: TJCoreOPFDriver): string;
begin
  Result := IntToStr(ReadInt64(ADriver));
end;

{ TJCoreOPFOIDGeneratorAutoinc }

function TJCoreOPFOIDGeneratorAutoinc.IsPostInsertGenerator: Boolean;
begin
  Result := True;
end;

function TJCoreOPFOIDGeneratorAutoinc.ReadInt64(const ADriver: TJCoreOPFDriver): Int64;
var
  VDriver: TJCoreOPFSQLDriver;
begin
  VDriver := DriverSQL(ADriver);
  Result := ReadOID(VDriver, VDriver.Database.AutoincSQL);
end;

function TJCoreOPFOIDGeneratorAutoinc.ReadString(const ADriver: TJCoreOPFDriver): string;
begin
  Result := IntToStr(ReadInt64(ADriver));
end;

{ TJCoreOPFOIDGeneratorGUID }

function TJCoreOPFOIDGeneratorGUID.IsPostInsertGenerator: Boolean;
begin
  Result := False;
end;

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

