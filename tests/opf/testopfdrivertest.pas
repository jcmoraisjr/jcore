unit TestOPFDriverTest;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFSQLDriverTest }

  TTestOPFSQLDriverTest = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure CommitAfterCreate;
    procedure ExecImmediate;
    procedure ExecSQLAndFlush;
    procedure ExecImmediateBeforeFlush;
    procedure ExecSQLExpectedSizeReuseStatement;
    procedure ExecSQLUnexpectedSizeReuseStatement;
    procedure ReuseStatementBeforeFlush;
    procedure ReuseStatementAfterFlush;
    procedure ReuseStatementAfterImmediate;
    procedure ReuseStatementChangeOrder;
    procedure ReuseQueryBeforeFlush;
    procedure QueueNewStatementsAfterFlush;
    procedure DestroyStatementBeforeFlush;
    procedure OpenCursor;
    procedure FlushOnOpenCursor;
    procedure FlushOnExecOrder;
    procedure DestroyDriverBeforeFlush;
    procedure WriteReadParams;
    procedure WriteMoreReadLessParams;
    procedure WriteLessReadMoreParams;
  end;

implementation

uses
  sysutils,
  Classes,
  JCoreClasses,
  JCoreOPFDriver,
  testregistry,
  TestOPFModelInvoice;

{ TTestOPFSQLDriverTest }

procedure TTestOPFSQLDriverTest.CommitAfterCreate;
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

procedure TTestOPFSQLDriverTest.ExecImmediate;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt := Driver.CreateStatement;
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt.ExecImmediate;
  AssertSQLDriverCommands(['ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
end;

procedure TTestOPFSQLDriverTest.ExecSQLAndFlush;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt := Driver.CreateStatement;
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt.ExecSQL;
  AssertSQLDriverCommands([]);
  Driver.Commit;
  AssertSQLDriverCommands(['ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
end;

procedure TTestOPFSQLDriverTest.ExecImmediateBeforeFlush;
var
  VStmt1, VStmt2: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt1 := Driver.CreateStatement;
  VStmt2 := Driver.CreateStatement;
  VStmt1.SQL := 'DELETE FROM TABLE WHERE ID=?';
  VStmt1.ExecSQL;
  AssertSQLDriverCommands([]);
  VStmt2.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt2.ExecImmediate;
  AssertSQLDriverCommands([
   'ExecSQL DELETE FROM TABLE WHERE ID=?',
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
end;

procedure TTestOPFSQLDriverTest.ExecSQLExpectedSizeReuseStatement;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.ExpectedResultsets.Add(4);
  VStmt.WriteString('joe');
  VStmt.SQL := 'DELETE FROM TABLE WHERE NAME=?';
  VStmt.ExecSQL(2);
  VStmt.WriteString('jack');
  VStmt.ExecSQL(4);
  Driver.Commit;
end;

procedure TTestOPFSQLDriverTest.ExecSQLUnexpectedSizeReuseStatement;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.ExpectedResultsets.Add(4);
  VStmt.WriteString('joe');
  VStmt.SQL := 'DELETE FROM TABLE WHERE NAME=?';
  VStmt.ExecSQL(2);
  VStmt.WriteString('jack');
  VStmt.ExecSQL(3);
  try
    Driver.Commit;
    Fail('EJCoreOPF(2113) expected');
  except
    on E: EJCoreOPF do
      if E.Code <> 2113 then
        raise;
  end;
end;

procedure TTestOPFSQLDriverTest.ReuseStatementBeforeFlush;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt := Driver.CreateStatement;
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt.ExecSQL;
  try
    VStmt.SQL := 'DELETE FROM TABLE WHERE ID=?';
    Fail('EJCoreOPF(2112) expected');
  except
    on E: EJCoreOPF do
      if E.Code <> 2112 then
        raise;
  end;
end;

procedure TTestOPFSQLDriverTest.ReuseStatementAfterFlush;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt := Driver.CreateStatement;
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt.ExecSQL;
  Driver.Commit;
  VStmt.SQL := 'DELETE FROM TABLE WHERE ID=?';
  VStmt.ExecSQL;
  AssertSQLDriverCommands(['ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
end;

procedure TTestOPFSQLDriverTest.ReuseStatementAfterImmediate;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt := Driver.CreateStatement;
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt.ExecImmediate;
  VStmt.SQL := 'DELETE FROM TABLE WHERE ID=?';
  VStmt.ExecImmediate;
  AssertSQLDriverCommands([
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
   'ExecSQL DELETE FROM TABLE WHERE ID=?']);
end;

procedure TTestOPFSQLDriverTest.ReuseStatementChangeOrder;
var
  VStmt1, VStmt2: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt1 := Driver.CreateStatement;
  VStmt2 := Driver.CreateStatement;
  VStmt1.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt2.SQL := 'DELETE FROM TABLE WHERE ID=?';
  VStmt1.ExecSQL;
  VStmt2.ExecSQL;
  Driver.Commit;
  AssertSQLDriverCommands([
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
   'ExecSQL DELETE FROM TABLE WHERE ID=?']);
  Driver.Commands.Clear;
  VStmt2.ExecSQL;
  VStmt1.ExecSQL;
  Driver.Commit;
  AssertSQLDriverCommands([
   'ExecSQL DELETE FROM TABLE WHERE ID=?',
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
end;

procedure TTestOPFSQLDriverTest.ReuseQueryBeforeFlush;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt := Driver.CreateStatement;
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt.WriteInt32(1);
  VStmt.WriteString('name1');
  VStmt.WriteInt32(2);
  VStmt.WriteString('name2');
  VStmt.ExecSQL;
  VStmt.ExecSQL;
  Driver.Commit;
  AssertSQLDriverCommands([
   'WriteInt32 1',
   'WriteString name1',
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
   'WriteInt32 2',
   'WriteString name2',
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
end;

procedure TTestOPFSQLDriverTest.QueueNewStatementsAfterFlush;
var
  VStmt1, VStmt2: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt1 := Driver.CreateStatement;
  VStmt2 := Driver.CreateStatement;
  VStmt1.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt2.SQL := 'DELETE FROM TABLE WHERE ID=?';
  VStmt1.ExecSQL;
  VStmt2.ExecSQL;
  Driver.Commit;
  AssertSQLDriverCommands([
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
   'ExecSQL DELETE FROM TABLE WHERE ID=?']);
  Driver.Commands.Clear;
  VStmt1.SQL := 'UPDATE TABLE SET NAME=? WHERE ID=?';
  VStmt2.SQL := 'DELETE FROM TABLE WHERE NAME=?';
  VStmt1.ExecSQL;
  VStmt2.ExecSQL;
  Driver.Commit;
  AssertSQLDriverCommands([
   'ExecSQL UPDATE TABLE SET NAME=? WHERE ID=?',
   'ExecSQL DELETE FROM TABLE WHERE NAME=?']);
end;

procedure TTestOPFSQLDriverTest.DestroyStatementBeforeFlush;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt := Driver.CreateStatement;
  VStmt.SQL := 'UPDATE TABLE SET NAME=? WHERE ID=?';
  VStmt.ExecSQL;
  VStmt := nil;
  Driver.Commit;
  AssertSQLDriverCommands(['ExecSQL UPDATE TABLE SET NAME=? WHERE ID=?']);
end;

procedure TTestOPFSQLDriverTest.OpenCursor;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  VStmt.SQL := 'SELECT ID,NAME FROM TABLE';
  VStmt.OpenCursor;
  AssertSQLDriverCommands(['ExecSQL SELECT ID,NAME FROM TABLE']);
end;

procedure TTestOPFSQLDriverTest.FlushOnOpenCursor;
var
  VStmtInsert, VStmtSelect: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmtInsert := Driver.CreateStatement;
  VStmtSelect := Driver.CreateStatement;
  VStmtInsert.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmtSelect.SQL := 'SELECT ID,NAME FROM TABLE';
  VStmtInsert.ExecSQL;
  AssertSQLDriverCommands([]);
  VStmtSelect.OpenCursor;
  AssertSQLDriverCommands([
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
   'ExecSQL SELECT ID,NAME FROM TABLE']);
end;

procedure TTestOPFSQLDriverTest.FlushOnExecOrder;
var
  VStmt1, VStmt2: IJCoreOPFSQLStatement;
begin
  Driver.IgnoreParams;
  VStmt1 := Driver.CreateStatement;
  VStmt2 := Driver.CreateStatement;
  VStmt1.SQL := 'UPDATE TABLE SET NAME=? WHERE ID=?';
  VStmt2.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt2.ExecSQL;
  VStmt1.ExecSQL;
  Driver.Commit;
  AssertSQLDriverCommands([
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
   'ExecSQL UPDATE TABLE SET NAME=? WHERE ID=?']);
end;

procedure TTestOPFSQLDriverTest.DestroyDriverBeforeFlush;
var
  VDriver: TTestSQLDriver;
  VStmt: IJCoreOPFSQLStatement;
begin
  VDriver := TTestSQLDriver.Create(nil);
  VDriver.IgnoreParams;
  VStmt := VDriver.CreateStatement;
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  FreeAndNil(VDriver);
  try
    VStmt.ExecImmediate;
    Fail('EJCoreOPF(2107) expected');
  except
    on E: EJCoreOPF do
      if E.Code <> 2107 then
        raise;
  end;
end;

procedure TTestOPFSQLDriverTest.WriteReadParams;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  VStmt.WriteInt64(2);
  VStmt.WriteString('name');
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  VStmt.ExecImmediate;
  AssertSQLDriverCommands([
   'WriteInt64 2',
   'WriteString name',
   'ExecSQL INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
end;

procedure TTestOPFSQLDriverTest.WriteMoreReadLessParams;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  VStmt.WriteInt32(1);
  VStmt.WriteString('name1');
  VStmt.SQL := 'INSERT INTO TABLE (ID) VALUES (?)';
  VStmt.ExecImmediate;
  try
    Driver.Commit;
    Fail('EJCoreOPF(2111) expected');
  except
    on E: EJCoreOPF do
      if E.Code <> 2111 then
        raise;
  end;
end;

procedure TTestOPFSQLDriverTest.WriteLessReadMoreParams;
var
  VStmt: IJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  VStmt.WriteInt32(1);
  VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
  try
    VStmt.ExecImmediate;
    Fail('EJCoreClasses(0203) expected');
  except
    on E: EJCoreClasses do
      if E.Code <> 203 then
        raise;
  end;
end;

initialization
  RegisterTest('jcore.opf.driver', TTestOPFSQLDriverTest);

end.

