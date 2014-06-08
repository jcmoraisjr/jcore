unit TestOPFMapping;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMapping;

type

  { TTestAbstractSQLMapping }

  TTestAbstractSQLMapping = class(TJCoreOPFSQLManualMapping)
  private
    class var FCurrentOID: Integer;
  protected
    function InternalCreateOIDArray(const ACount: Integer): TJCoreOPFOIDArray; override;
  public
    class procedure ClearOID;
  end;

implementation

uses
  sysutils;

{ TTestAbstractSQLMapping }

function TTestAbstractSQLMapping.InternalCreateOIDArray(const ACount: Integer): TJCoreOPFOIDArray;
var
  I: Integer;
begin
  SetLength(Result, ACount);
  for I := 0 to Pred(ACount) do
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

