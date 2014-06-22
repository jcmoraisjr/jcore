unit TestOPFMapping;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMappingSQL;

type

  { TTestAbstractSQLMapping }

  TTestAbstractSQLMapping = class(TJCoreOPFSQLManualMapping)
  private
    class var FCurrentOID: Integer;
  protected
    function InternalCreateOIDArray(const AOIDCount: Integer): TJCoreOPFOIDArray; override;
  public
    class procedure ClearOID;
  end;

implementation

uses
  sysutils;

{ TTestAbstractSQLMapping }

function TTestAbstractSQLMapping.InternalCreateOIDArray(const AOIDCount: Integer): TJCoreOPFOIDArray;
var
  I: Integer;
begin
  SetLength(Result, AOIDCount);
  for I := 0 to Pred(AOIDCount) do
  begin
    Inc(FCurrentOID);
    Result[I] := TJCoreOPFIntegerOID.Create(FCurrentOID);
  end;
end;

class procedure TTestAbstractSQLMapping.ClearOID;
begin
  FCurrentOID := 0;
end;

end.

