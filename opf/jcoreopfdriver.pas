(*
  JCore, OPF Driver Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFDriver;

{$I jcore.inc}

interface

uses
  Classes,
  fgl,
  JCoreList,
  JCoreLogger,
  JCoreOPFOIDGen;

type

  TJCoreOPFStatement = class(TObject)
  end;

  { TJCoreOPFDriver }

  TJCoreOPFDriver = class(TObject)
  private
    FParams: TStringList;
    FStack: TJCoreNativeTypeStack;
  protected
    procedure InternalCommit; virtual; abstract;
    property Params: TStringList read FParams;
    property Stack: TJCoreNativeTypeStack read FStack;
  public
    constructor Create(const AParams: TStringList); virtual;
    destructor Destroy; override;
    procedure Commit;
    function CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator; virtual; abstract;
    function CreateStatement: TJCoreOPFStatement; virtual; abstract;
    class function DriverName: string; virtual; abstract;
    function ReadInt32: Integer; virtual; abstract;
    function ReadInt64: Int64; virtual; abstract;
    function ReadNull: Boolean; virtual; abstract;
    function ReadNullAndSkip: Boolean; virtual; abstract;
    function ReadString: string; virtual; abstract;
    procedure WriteInt32(const AValue: Integer); virtual;
    procedure WriteInt64(const AValue: Int64); virtual;
    procedure WriteNull; virtual;
    procedure WriteString(const AValue: string); virtual;
  end;

  TJCoreOPFDriverClass = class of TJCoreOPFDriver;
  TJCoreOPFDriverClassMap = specialize TFPGMap<string, TJCoreOPFDriverClass>;

  { IJCoreOPFResultSet }

  IJCoreOPFSQLResultSet = interface(IInterface)
    procedure Close;
    function IsClosed: Boolean;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadNull: Boolean;
    function ReadString: string;
    function Size: Integer;
  end;

  { IJCoreOPFSQLQuery }

  IJCoreOPFSQLQuery = interface(IInterface)
    function ExecSQL: Integer;
    function OpenCursor: IJCoreOPFSQLResultSet;
  end;

  // TFPGInterfacedObjectList is leaking memory on fpc 2.6.4
  //TJCoreOPFSQLQueryList = specialize TFPGInterfacedObjectList<IJCoreOPFSQLQuery>;
  TJCoreOPFSQLQueryList = TInterfaceList;

  TJCoreOPFSQLStatement = class;

  IJCoreOPFSQLStmtManager = interface
    function AcquireQuery(const ASQL: string; const AParams: TJCoreNativeTypeQueue): IJCoreOPFSQLQuery;
    procedure DetachStatement(const AStmt: TJCoreOPFSQLStatement);
    procedure Flush;
    procedure QueueStatement(const AStmt: TJCoreOPFSQLStatement);
    procedure ReleaseQuery(const AQuery: IJCoreOPFSQLQuery);
  end;

  { TJCoreOPFSQLStatement }

  TJCoreOPFSQLStatement = class(TJCoreOPFStatement)
  private
    FExpectedSize: Integer;
    FParams: TJCoreNativeTypeQueue;
    FQuery: IJCoreOPFSQLQuery;
    FQueued: Boolean;
    FSQL: string;
    FStmtManager: IJCoreOPFSQLStmtManager;
    function GetQuery: IJCoreOPFSQLQuery;
    function GetStmtManager: IJCoreOPFSQLStmtManager;
    procedure SetSQL(const AValue: string);
  protected
    procedure CheckQueued;
    procedure EnsureResultSetSize(const ACount, AExpectedSize: Integer);
    procedure Flush;
    procedure QueueStatement(const AExpectedSize: Integer = -1);
    procedure ReleaseQuery;
    procedure ReleaseStmtManager;
    procedure UnqueueStatement;
    property Params: TJCoreNativeTypeQueue read FParams;
    property Query: IJCoreOPFSQLQuery read GetQuery;
    property StmtManager: IJCoreOPFSQLStmtManager read GetStmtManager;
  public
    constructor Create(const AStmtManager: IJCoreOPFSQLStmtManager);
    destructor Destroy; override;
    function ExecImmediate: Integer;
    procedure ExecImmediate(const AExpectedSize: Integer);
    procedure ExecSQL;
    procedure ExecSQL(const AExpectedSize: Integer);
    function OpenCursor: IJCoreOPFSQLResultSet;
    function OpenCursor(const AExpectedSize: Integer): IJCoreOPFSQLResultSet;
    procedure WriteInt32(const AValue: Integer);
    procedure WriteInt64(const AValue: Int64);
    procedure WriteNull;
    procedure WriteString(const AValue: string);
    property SQL: string read FSQL write SetSQL;
  end;

  TJCoreOPFSQLStatementList = specialize TFPGObjectList<TJCoreOPFSQLStatement>;

  { TJCoreOPFSQLDriver }

  TJCoreOPFSQLDriver = class(TJCoreOPFDriver, IJCoreOPFSQLStmtManager)
  private
    FQueryList: TJCoreOPFSQLQueryList;
    FStmtList: TJCoreOPFSQLStatementList;
    FStmtQueue: TJCoreOPFSQLStatementList;
    class var FLOGSQL: IJCoreLogger;
    function AcquireQuery(const ASQL: string; const AParams: TJCoreNativeTypeQueue): IJCoreOPFSQLQuery;
    procedure DetachStatement(const AStmt: TJCoreOPFSQLStatement);
    procedure QueueStatement(const AStmt: TJCoreOPFSQLStatement);
    procedure ReleaseQuery(const AQuery: IJCoreOPFSQLQuery);
    procedure ReleaseStatements;
  protected
    function InternalCreateQuery(const ASQL: string; const AParams: TJCoreNativeTypeQueue): IJCoreOPFSQLQuery; virtual; abstract;
    function InternalExecSQL(const ASQL: string): Integer; virtual; abstract;
    class property LOGSQL: IJCoreLogger read FLOGSQL;
    property QueryList: TJCoreOPFSQLQueryList read FQueryList;
    property StmtList: TJCoreOPFSQLStatementList read FStmtList;
    property StmtQueue: TJCoreOPFSQLStatementList read FStmtQueue;
  public
    constructor Create(const AParams: TStringList); override;
    destructor Destroy; override;
    function CreateStatement: TJCoreOPFSQLStatement; override;
    function ExecSQL(const ASQL: string): Integer;
    procedure ExecSQL(const ASQL: string; const AExpectedSize: Integer);
    procedure Flush;
  end;

  { TJCoreOPFOIDGeneratorSQLDriver }

  TJCoreOPFOIDGeneratorSQLDriver = class(TJCoreOPFOIDGeneratorInt64)
  private
    FDriver: TJCoreOPFSQLDriver;
    FGeneratorName: string;
  protected
    property Driver: TJCoreOPFSQLDriver read FDriver;
    property GeneratorName: string read FGeneratorName;
  public
    constructor Create(const ADriver: TJCoreOPFSQLDriver; const AGeneratorName: string);
  end;

implementation

uses
  sysutils,
  JCoreOPFException;

{ TJCoreOPFDriver }

constructor TJCoreOPFDriver.Create(const AParams: TStringList);
begin
  inherited Create;
  FParams := AParams;
  FStack := TJCoreNativeTypeStack.Create;
end;

destructor TJCoreOPFDriver.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

procedure TJCoreOPFDriver.Commit;
begin
  InternalCommit;
end;

procedure TJCoreOPFDriver.WriteInt32(const AValue: Integer);
begin
  Stack.PushInt32(AValue);
end;

procedure TJCoreOPFDriver.WriteInt64(const AValue: Int64);
begin
  Stack.PushInt64(AValue);
end;

procedure TJCoreOPFDriver.WriteNull;
begin
  Stack.PushNull;
end;

procedure TJCoreOPFDriver.WriteString(const AValue: string);
begin
  Stack.PushString(AValue);
end;

{ TJCoreOPFSQLStatement }

function TJCoreOPFSQLStatement.GetQuery: IJCoreOPFSQLQuery;
begin
  if not Assigned(FQuery) then
    FQuery := StmtManager.AcquireQuery(SQL, Params);
  Result := FQuery;
end;

function TJCoreOPFSQLStatement.GetStmtManager: IJCoreOPFSQLStmtManager;
begin
  if not Assigned(FStmtManager) then
    raise EJCoreOPFDetachedStatement.Create;
  Result := FStmtManager;
end;

procedure TJCoreOPFSQLStatement.SetSQL(const AValue: string);
begin
  if FSQL <> AValue then
  begin
    CheckQueued;
    FSQL := AValue;
    ReleaseQuery;
  end;
end;

procedure TJCoreOPFSQLStatement.CheckQueued;
begin
  if FQueued or (FExpectedSize >= 0) then
    raise EJCoreOPFStatementOnQueue.Create;
end;

procedure TJCoreOPFSQLStatement.EnsureResultSetSize(const ACount, AExpectedSize: Integer);
begin
  if (AExpectedSize >= 0) and (ACount <> AExpectedSize) then
  begin
    if ACount = 0 then
      raise EJCoreOPFEmptyResultSet.Create(AExpectedSize);
    raise EJCoreOPFUnexpectedResultSetSize.Create(AExpectedSize, ACount);
  end;
end;

procedure TJCoreOPFSQLStatement.Flush;
var
  VSize: Integer;
begin
  if FQueued then
  begin
    VSize := Query.ExecSQL;
    EnsureResultSetSize(VSize, FExpectedSize);
    UnqueueStatement;
  end;
end;

procedure TJCoreOPFSQLStatement.QueueStatement(const AExpectedSize: Integer = -1);
begin
  CheckQueued;
  StmtManager.QueueStatement(Self);
  FExpectedSize := AExpectedSize;
  FQueued := True;
end;

procedure TJCoreOPFSQLStatement.ReleaseQuery;
var
  VQuery: IJCoreOPFSQLQuery;
begin
  if Assigned(FQuery) then
  begin
    VQuery := FQuery;
    FQuery := nil;
    StmtManager.ReleaseQuery(VQuery);
    UnqueueStatement;
  end;
end;

procedure TJCoreOPFSQLStatement.ReleaseStmtManager;
begin
  if Assigned(FStmtManager) then
  begin
    ReleaseQuery;
    FStmtManager.DetachStatement(Self);
    FStmtManager := nil;
  end;
end;

procedure TJCoreOPFSQLStatement.UnqueueStatement;
begin
  FExpectedSize := -1;
  FQueued := False;
end;

constructor TJCoreOPFSQLStatement.Create(const AStmtManager: IJCoreOPFSQLStmtManager);
begin
  inherited Create;
  FStmtManager := AStmtManager;
  FParams := TJCoreNativeTypeQueue.Create;
  UnqueueStatement;
end;

destructor TJCoreOPFSQLStatement.Destroy;
begin
  ReleaseStmtManager;
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TJCoreOPFSQLStatement.ExecImmediate: Integer;
begin
  CheckQueued;
  Result := Query.ExecSQL;
end;

procedure TJCoreOPFSQLStatement.ExecImmediate(const AExpectedSize: Integer);
begin
  EnsureResultSetSize(ExecImmediate, AExpectedSize);
end;

procedure TJCoreOPFSQLStatement.ExecSQL;
begin
  QueueStatement;
end;

procedure TJCoreOPFSQLStatement.ExecSQL(const AExpectedSize: Integer);
begin
  QueueStatement(AExpectedSize);
end;

function TJCoreOPFSQLStatement.OpenCursor: IJCoreOPFSQLResultSet;
begin
  CheckQueued;
  StmtManager.Flush;
  Result := Query.OpenCursor;
end;

function TJCoreOPFSQLStatement.OpenCursor(const AExpectedSize: Integer): IJCoreOPFSQLResultSet;
begin
  Result := OpenCursor();
  EnsureResultSetSize(Result.Size, AExpectedSize);
end;

procedure TJCoreOPFSQLStatement.WriteInt32(const AValue: Integer);
begin
  CheckQueued;
  Params.PushInt32(AValue);
end;

procedure TJCoreOPFSQLStatement.WriteInt64(const AValue: Int64);
begin
  CheckQueued;
  Params.PushInt64(AValue);
end;

procedure TJCoreOPFSQLStatement.WriteNull;
begin
  CheckQueued;
  Params.PushNull;
end;

procedure TJCoreOPFSQLStatement.WriteString(const AValue: string);
begin
  CheckQueued;
  Params.PushString(AValue);
end;

{ TJCoreOPFSQLDriver }

function TJCoreOPFSQLDriver.AcquireQuery(const ASQL: string;
  const AParams: TJCoreNativeTypeQueue): IJCoreOPFSQLQuery;
begin
  { TODO : Implement Query Pool }
  Result := InternalCreateQuery(ASQL, AParams);
  QueryList.Add(Result);
end;

procedure TJCoreOPFSQLDriver.DetachStatement(const AStmt: TJCoreOPFSQLStatement);
begin
  if Assigned(AStmt) then
  begin
    StmtList.Remove(AStmt);
    StmtQueue.Remove(AStmt);
  end;
end;

procedure TJCoreOPFSQLDriver.QueueStatement(const AStmt: TJCoreOPFSQLStatement);
begin
  StmtQueue.Add(AStmt);
end;

procedure TJCoreOPFSQLDriver.ReleaseQuery(const AQuery: IJCoreOPFSQLQuery);
begin
  { TODO : Implement Query Pool }
  if Assigned(AQuery) then
    QueryList.Remove(AQuery);
end;

procedure TJCoreOPFSQLDriver.ReleaseStatements;
var
  I: Integer;
begin
  for I := 0 to Pred(StmtList.Count) do
    StmtList[I].ReleaseStmtManager; // friend class
end;

constructor TJCoreOPFSQLDriver.Create(const AParams: TStringList);
begin
  inherited Create(AParams);
  if not Assigned(FLOGSQL) then
    FLOGSQL := TJCoreLogger.GetLogger('jcore.opf.driver.sql');
  FQueryList := TJCoreOPFSQLQueryList.Create;
  FStmtList := TJCoreOPFSQLStatementList.Create(False);
  FStmtQueue := TJCoreOPFSQLStatementList.Create(False);
end;

destructor TJCoreOPFSQLDriver.Destroy;
begin
  ReleaseStatements;
  FreeAndNil(FQueryList);
  FreeAndNil(FStmtList);
  FreeAndNil(FStmtQueue);
  inherited Destroy;
end;

function TJCoreOPFSQLDriver.CreateStatement: TJCoreOPFSQLStatement;
begin
  Result := TJCoreOPFSQLStatement.Create(Self);
  StmtList.Add(Result);
end;

function TJCoreOPFSQLDriver.ExecSQL(const ASQL: string): Integer;
begin
  LOGSQL.Debug(ASQL);
  Result := InternalExecSQL(ASQL);
end;

procedure TJCoreOPFSQLDriver.ExecSQL(const ASQL: string; const AExpectedSize: Integer);
var
  VSize: Integer;
begin
  VSize := ExecSQL(ASQL);
  if (VSize <> AExpectedSize) and (AExpectedSize <> -1) then
  begin
    if VSize = 0 then
      raise EJCoreOPFEmptyResultSet.Create(AExpectedSize);
    raise EJCoreOPFUnexpectedResultSetSize.Create(AExpectedSize, VSize);
  end;
end;

procedure TJCoreOPFSQLDriver.Flush;
var
  I: Integer;
begin
  for I := 0 to Pred(StmtQueue.Count) do
    StmtQueue[I].Flush;  // friend class
  StmtQueue.Clear;
end;

{ TJCoreOPFOIDGeneratorSQLDriver }

constructor TJCoreOPFOIDGeneratorSQLDriver.Create(const ADriver: TJCoreOPFSQLDriver;
  const AGeneratorName: string);
begin
  inherited Create;
  FDriver := ADriver;
  FGeneratorName := AGeneratorName;
end;

end.

