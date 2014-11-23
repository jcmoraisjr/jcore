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
    procedure ExecAndFlush;
    procedure ReuseStatementBeforeFlush;
    procedure ReuseStatementAfterFlush;
    procedure ReuseStatementAfterImmediate;
    procedure ReuseStatementChangeOrder;
    procedure QueueNewStatementsAfterFlush;
    procedure DestroyStatementBeforeFlush;
    procedure OpenCursor;
    procedure FlushOnOpenCursor;
    procedure FlushOnExecOrder;
    procedure DestroyDriverBeforeFlush;
    procedure WriteReadParams;
  end;

implementation

uses
  sysutils,
  Classes,
  JCoreOPFException,
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
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  try
    VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
    VStmt.ExecImmediate;
    AssertCommands(Driver.Commands, ['INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VStmt);
  end;
end;

procedure TTestOPFSQLDriverTest.ExecAndFlush;
var
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  try
    VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
    VStmt.ExecSQL;
    AssertCommands(Driver.Commands, []);
    Driver.Flush;
    AssertCommands(Driver.Commands, ['INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VStmt);
  end;
end;

procedure TTestOPFSQLDriverTest.ReuseStatementBeforeFlush;
var
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  try
    VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
    VStmt.ExecSQL;
    try
      VStmt.SQL := 'DELETE FROM TABLE WHERE ID=?';
      Fail('EJCoreOPFStatementOnQueue expected');
    except
      on E: EJCoreOPFStatementOnQueue do
        ;
    end;
  finally
    FreeAndNil(VStmt);
  end;
end;

procedure TTestOPFSQLDriverTest.ReuseStatementAfterFlush;
var
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  try
    VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
    VStmt.ExecSQL;
    Driver.Flush;
    VStmt.SQL := 'DELETE FROM TABLE WHERE ID=?';
    VStmt.ExecSQL;
    AssertCommands(Driver.Commands, ['INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VStmt);
  end;
end;

procedure TTestOPFSQLDriverTest.ReuseStatementAfterImmediate;
var
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  try
    VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
    VStmt.ExecImmediate;
    VStmt.SQL := 'DELETE FROM TABLE WHERE ID=?';
    VStmt.ExecImmediate;
    AssertCommands(Driver.Commands, [
     'INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
     'DELETE FROM TABLE WHERE ID=?']);
  finally
    FreeAndNil(VStmt);
  end;
end;

procedure TTestOPFSQLDriverTest.ReuseStatementChangeOrder;
var
  VStmt1: TJCoreOPFSQLStatement;
  VStmt2: TJCoreOPFSQLStatement;
begin
  VStmt1 := Driver.CreateStatement;
  try
    VStmt2 := Driver.CreateStatement;
    try
      VStmt1.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
      VStmt2.SQL := 'DELETE FROM TABLE WHERE ID=?';
      VStmt1.ExecSQL;
      VStmt2.ExecSQL;
      Driver.Flush;
      AssertCommands(Driver.Commands, [
       'INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
       'DELETE FROM TABLE WHERE ID=?']);
      Driver.Commands.Clear;
      VStmt2.ExecSQL;
      VStmt1.ExecSQL;
      Driver.Flush;
      AssertCommands(Driver.Commands, [
       'DELETE FROM TABLE WHERE ID=?',
       'INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
    finally
      FreeAndNil(VStmt2);
    end;
  finally
    FreeAndNil(VStmt1);
  end;
end;

procedure TTestOPFSQLDriverTest.QueueNewStatementsAfterFlush;
var
  VStmt1: TJCoreOPFSQLStatement;
  VStmt2: TJCoreOPFSQLStatement;
begin
  VStmt1 := Driver.CreateStatement;
  try
    VStmt2 := Driver.CreateStatement;
    try
      VStmt1.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
      VStmt2.SQL := 'DELETE FROM TABLE WHERE ID=?';
      VStmt1.ExecSQL;
      VStmt2.ExecSQL;
      Driver.Flush;
      AssertCommands(Driver.Commands, [
       'INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
       'DELETE FROM TABLE WHERE ID=?']);
      Driver.Commands.Clear;
      VStmt1.SQL := 'UPDATE TABLE SET NAME=? WHERE ID=?';
      VStmt2.SQL := 'DELETE FROM TABLE WHERE NAME=?';
      VStmt1.ExecSQL;
      VStmt2.ExecSQL;
      Driver.Flush;
      AssertCommands(Driver.Commands, [
       'UPDATE TABLE SET NAME=? WHERE ID=?',
       'DELETE FROM TABLE WHERE NAME=?']);
    finally
      FreeAndNil(VStmt2);
    end;
  finally
    FreeAndNil(VStmt1);
  end;
end;

procedure TTestOPFSQLDriverTest.DestroyStatementBeforeFlush;
var
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  try
    VStmt.SQL := 'UPDATE TABLE SET NAME=? WHERE ID=?';
    VStmt.ExecSQL;
    FreeAndNil(VStmt);
    Driver.Flush;
    AssertCommands(Driver.Commands, []);
  finally
    FreeAndNil(VStmt);
  end;
end;

procedure TTestOPFSQLDriverTest.OpenCursor;
var
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  try
    VStmt.SQL := 'SELECT ID,NAME FROM TABLE';
    VStmt.OpenCursor;
    AssertCommands(Driver.Commands, ['SELECT ID,NAME FROM TABLE']);
  finally
    FreeAndNil(VStmt);
  end;
end;

procedure TTestOPFSQLDriverTest.FlushOnOpenCursor;
var
  VStmtInsert: TJCoreOPFSQLStatement;
  VStmtSelect: TJCoreOPFSQLStatement;
begin
  VStmtInsert := Driver.CreateStatement;
  try
    VStmtSelect := Driver.CreateStatement;
    try
      VStmtInsert.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
      VStmtSelect.SQL := 'SELECT ID,NAME FROM TABLE';
      VStmtInsert.ExecSQL;
      AssertCommands(Driver.Commands, []);
      VStmtSelect.OpenCursor;
      AssertCommands(Driver.Commands, [
       'INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
       'SELECT ID,NAME FROM TABLE']);
    finally
      FreeAndNil(VStmtSelect);
    end;
  finally
    FreeAndNil(VStmtInsert);
  end;
end;

procedure TTestOPFSQLDriverTest.FlushOnExecOrder;
var
  VStmt1: TJCoreOPFSQLStatement;
  VStmt2: TJCoreOPFSQLStatement;
begin
  VStmt1 := Driver.CreateStatement;
  try
    VStmt2 := Driver.CreateStatement;
    try
      VStmt1.SQL := 'UPDATE TABLE SET NAME=? WHERE ID=?';
      VStmt2.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
      VStmt2.ExecSQL;
      VStmt1.ExecSQL;
      Driver.Flush;
      AssertCommands(Driver.Commands, [
       'INSERT INTO TABLE (ID,NAME) VALUES (?,?)',
       'UPDATE TABLE SET NAME=? WHERE ID=?']);
    finally
      FreeAndNil(VStmt2);
    end;
  finally
    FreeAndNil(VStmt1);
  end;
end;

procedure TTestOPFSQLDriverTest.DestroyDriverBeforeFlush;
var
  VDriver: TTestOPFSQLDriverMock;
  VStmt: TJCoreOPFSQLStatement;
begin
  VDriver := TTestOPFSQLDriverMock.Create(nil);
  try
    VStmt := VDriver.CreateStatement;
    try
      VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
      FreeAndNil(VDriver);
      try
        VStmt.ExecImmediate;
        Fail('EJCoreOPFDetachedStatement expected');
      except
        on E: EJCoreOPFDetachedStatement do
          ;
      end;
    finally
      FreeAndNil(VStmt);
    end;
  finally
    FreeAndNil(VDriver);
  end;
end;

procedure TTestOPFSQLDriverTest.WriteReadParams;
var
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := Driver.CreateStatement;
  try
    VStmt.WriteInt64(2);
    VStmt.WriteString('name');
    VStmt.SQL := 'INSERT INTO TABLE (ID,NAME) VALUES (?,?)';
    VStmt.ExecImmediate;
    AssertCommands(Driver.Commands, [
     'WriteInt64 2',
     'WriteString name',
     'INSERT INTO TABLE (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VStmt);
  end;
end;

initialization
  RegisterTest('jcore.opf.driver', TTestOPFSQLDriverTest);

end.

