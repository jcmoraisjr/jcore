unit TestOPFDriverTest;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFDriverTest }

  TTestOPFDriverTest = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure CommitAfterCreate;
  end;

implementation

uses
  sysutils,
  testregistry,
  TestOPFModelInvoice;

{ TTestOPFDriverTest }

procedure TTestOPFDriverTest.CommitAfterCreate;
var
  VClient: TClient;
begin
  VClient := TClient.Create;
  try
    Session.Store(VClient);
    AssertSQLDriverTransaction(['commit']);
  finally
    FreeAndNil(VClient);
  end;
end;

initialization
  RegisterTest('jcore.opf.driver', TTestOPFDriverTest);

end.

