unit TestOPFMapping;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMapping;

type

  { TTestAbstractSQLMapping }

  TTestAbstractSQLMapping = class(TJCoreOPFSQLMapping)
  private
    class var FCurrentOID: Integer;
  protected
    function CreateOIDFromDriver(const ADriver: TJCoreOPFDriver): TJCoreOPFOID; override;
    function CreateOIDFromString(const AOID: string): TJCoreOPFOID; override;
    function GenerateOID: Integer;
  public
    class procedure ClearOID;
  end;

implementation

uses
  sysutils;

{ TTestAbstractSQLMapping }

function TTestAbstractSQLMapping.CreateOIDFromDriver(
  const ADriver: TJCoreOPFDriver): TJCoreOPFOID;
begin
  if not ADriver.ReadNull then
    Result := TJCoreOPFIntegerOID.Create(ADriver.ReadInteger)
  else
    Result := nil;
end;

function TTestAbstractSQLMapping.CreateOIDFromString(const AOID: string): TJCoreOPFOID;
var
  VOID: Integer;
begin
  if AOID = '' then
    VOID := GenerateOID
  else
    VOID := StrToInt(AOID);
  Result := TJCoreOPFIntegerOID.Create(VOID);
end;

function TTestAbstractSQLMapping.GenerateOID: Integer;
begin
  Inc(FCurrentOID);
  Result := FCurrentOID;
end;

class procedure TTestAbstractSQLMapping.ClearOID;
begin
  FCurrentOID := 0;
end;

end.

