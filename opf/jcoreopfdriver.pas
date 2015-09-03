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
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  typinfo,
  Classes,
  fgl,
  JCoreClasses,
  JCoreList,
  JCoreLogger;

type

  { IJCoreOPFParams }

  IJCoreOPFParams = interface(IInterface)
    function Count: Integer;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadString: string;
    function ReadType: TTypeKind;
    procedure WriteInt32(const AValue: Integer);
    procedure WriteInt64(const AValue: Int64);
    procedure WriteNull;
    procedure WriteString(const AValue: string);
    procedure WriteVariant(const AValue: Variant);
  end;

  { IJCoreOPFResultSet }

  IJCoreOPFResultSet = interface(IInterface)
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadNull: Boolean;
    function ReadString: string;
    function Size: Integer;
    procedure SkipReading;
  end;

  { IJCoreOPFSQLResultSet }

  IJCoreOPFSQLResultSet = interface(IJCoreOPFResultSet)
    procedure Close;
    function IsClosed: Boolean;
  end;

  { IJCoreOPFSQLStatement }

  IJCoreOPFSQLStatement = interface(IInterface)
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
    function GetParams: IJCoreOPFParams;
    function GetSQL: string;
    procedure SetSQL(const AValue: string);
    property Params: IJCoreOPFParams read GetParams;
    property SQL: string read GetSQL write SetSQL;
  end;

  { TJCoreOPFParams }

  TJCoreOPFParams = class(TJCoreNativeTypeQueue, IJCoreOPFParams)
  public
    function Count: Integer;
    function IJCoreOPFParams.ReadInt32 = PopInt32;
    function IJCoreOPFParams.ReadInt64 = PopInt64;
    function IJCoreOPFParams.ReadString = PopString;
    function IJCoreOPFParams.ReadType = PopType;
    procedure IJCoreOPFParams.WriteInt32 = PushInt32;
    procedure IJCoreOPFParams.WriteInt64 = PushInt64;
    procedure IJCoreOPFParams.WriteNull = PushNull;
    procedure IJCoreOPFParams.WriteString = PushString;
    procedure IJCoreOPFParams.WriteVariant = PushVariant;
  end;

  { TJCoreOPFDriver }

  TJCoreOPFDriver = class(TObject)
  private
    FParams: TStringList;
  protected
    procedure InternalCommit; virtual;
    property Params: TStringList read FParams;
  public
    constructor Create(const AParams: TStringList); virtual;
    procedure Commit;
    class function DriverName: string; virtual; abstract;
  end;

  TJCoreOPFDriverClass = class of TJCoreOPFDriver;
  TJCoreOPFDriverClassMap = specialize TFPGMap<string, TJCoreOPFDriverClass>;

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
    function AcquireQuery(const ASQL: string; const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery;
    procedure DetachStatement(const AStmt: TJCoreOPFSQLStatement);
    procedure Flush;
    procedure QueueStatement(const AStmt: TJCoreOPFSQLStatement);
    procedure ReleaseQuery(const AQuery: IJCoreOPFSQLQuery);
  end;

  { TJCoreOPFSQLStatement }

  TJCoreOPFSQLStatement = class(TJCoreManagedIObject, IJCoreOPFSQLStatement)
  private
    FExpectedSize: TJCoreIntegerList;
    FParams: IJCoreOPFParams;
    FQuery: IJCoreOPFSQLQuery;
    FSQL: string;
    FStmtManager: IJCoreOPFSQLStmtManager;
    class var FLOG: IJCoreLogger;
    function GetQuery: IJCoreOPFSQLQuery;
    function GetStmtManager: IJCoreOPFSQLStmtManager;
    procedure SetSQL(const AValue: string);
  protected
    procedure EnsureResultSetSize(const ACount, AExpectedSize: Integer);
    function ExecQuery: Integer;
    procedure Finit; override;
    procedure QueueStatement(const AExpectedSize: Integer = -1);
    procedure ReleaseQuery;
    property Query: IJCoreOPFSQLQuery read GetQuery;
    property StmtManager: IJCoreOPFSQLStmtManager read GetStmtManager;
    class property LOG: IJCoreLogger read FLOG;
  protected
    // IJCoreOPFSQLStatement
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
    function IGetParams: IJCoreOPFParams;
    function IGetSQL: string;
    function IJCoreOPFSQLStatement.GetParams = IGetParams;
    function IJCoreOPFSQLStatement.GetSQL = IGetSQL;
    property Params: IJCoreOPFParams read FParams;
    property SQL: string read FSQL write SetSQL;
  public
    constructor Create(const AStmtManager: IJCoreOPFSQLStmtManager; const AParams: IJCoreOPFParams);
    procedure CheckCommit;
    procedure CheckQueued;
    procedure Flush;
    procedure ReleaseStmtManager;
  end;

  TJCoreOPFSQLStatementList = specialize TFPGObjectList<TJCoreOPFSQLStatement>;

  { TJCoreOPFSQLDatabase }

  TJCoreOPFSQLDatabase = class(TObject)
  public
    function AutoincSQL: string; virtual;
    class function DatabaseName: string; virtual; abstract;
    function SequenceSQL(const ASequenceName: string; const AOIDCount: Integer): string; virtual;
  end;

  TJCoreOPFSQLDatabaseClass = class of TJCoreOPFSQLDatabase;

  { TJCoreOPFSQLDriver }

  TJCoreOPFSQLDriver = class(TJCoreOPFDriver, IJCoreOPFSQLStmtManager)
  private
    FDatabase: TJCoreOPFSQLDatabase;
    FQueryList: TJCoreOPFSQLQueryList;
    FStmtList: TJCoreOPFSQLStatementList;
    FStmtQueue: TJCoreOPFSQLStatementList;
    function AcquireQuery(const ASQL: string; const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery;
    procedure DetachStatement(const AStmt: TJCoreOPFSQLStatement);
    function GetDatabase: TJCoreOPFSQLDatabase;
    procedure QueueStatement(const AStmt: TJCoreOPFSQLStatement);
    procedure ReleaseQuery(const AQuery: IJCoreOPFSQLQuery);
    procedure ReleaseStatements;
  protected
    procedure InternalCommit; override;
    function InternalDatabaseClass: TJCoreOPFSQLDatabaseClass; virtual; abstract;
    function InternalCreateQuery(const ASQL: string; const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery; virtual; abstract;
    property QueryList: TJCoreOPFSQLQueryList read FQueryList;
    property StmtList: TJCoreOPFSQLStatementList read FStmtList;
    property StmtQueue: TJCoreOPFSQLStatementList read FStmtQueue;
  public
    constructor Create(const AParams: TStringList); override;
    destructor Destroy; override;
    function CreateStatement: IJCoreOPFSQLStatement;
    function CreateStatement(const AParams: IJCoreOPFParams): IJCoreOPFSQLStatement;
    procedure Flush;
    property Database: TJCoreOPFSQLDatabase read GetDatabase;
  end;

implementation

uses
  sysutils,
  JCoreConsts;

{ TJCoreOPFParams }

function TJCoreOPFParams.Count: Integer;
begin
  Result := inherited Count;
end;

{ TJCoreOPFDriver }

procedure TJCoreOPFDriver.InternalCommit;
begin
end;

constructor TJCoreOPFDriver.Create(const AParams: TStringList);
begin
  inherited Create;
  FParams := AParams;
end;

procedure TJCoreOPFDriver.Commit;
begin
  InternalCommit;
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
    raise EJCoreOPF.Create(2107, S2107_DetachedStatement, []);
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

procedure TJCoreOPFSQLStatement.EnsureResultSetSize(const ACount, AExpectedSize: Integer);
begin
  if (AExpectedSize >= 0) and (ACount <> AExpectedSize) then
  begin
    if ACount = 0 then
      raise EJCoreOPF.Create(2110, S2110_EmptyResultSet, [AExpectedSize]);
    raise EJCoreOPF.Create(2113, S2113_UnexpectedResultSetSize, [AExpectedSize, ACount]);
  end;
end;

function TJCoreOPFSQLStatement.ExecQuery: Integer;
begin
  LOG.Debug(FSQL);
  Result := Query.ExecSQL;
end;

procedure TJCoreOPFSQLStatement.Finit;
begin
  ReleaseStmtManager;
  FreeAndNil(FExpectedSize);
  inherited Finit;
end;

procedure TJCoreOPFSQLStatement.QueueStatement(const AExpectedSize: Integer = -1);
begin
  StmtManager.QueueStatement(Self);
  FExpectedSize.Add(AExpectedSize);
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
    FExpectedSize.Clear;
  end;
end;

function TJCoreOPFSQLStatement.ExecImmediate: Integer;
begin
  StmtManager.Flush;
  Result := ExecQuery;
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
  LOG.Debug(FSQL);
  Result := Query.OpenCursor;
end;

function TJCoreOPFSQLStatement.OpenCursor(const AExpectedSize: Integer): IJCoreOPFSQLResultSet;
begin
  Result := OpenCursor(); // remove the brackets and get a warning! =)
  EnsureResultSetSize(Result.Size, AExpectedSize);
end;

procedure TJCoreOPFSQLStatement.WriteInt32(const AValue: Integer);
begin
  Params.WriteInt32(AValue);
end;

procedure TJCoreOPFSQLStatement.WriteInt64(const AValue: Int64);
begin
  Params.WriteInt64(AValue);
end;

procedure TJCoreOPFSQLStatement.WriteNull;
begin
  Params.WriteNull;
end;

procedure TJCoreOPFSQLStatement.WriteString(const AValue: string);
begin
  Params.WriteString(AValue);
end;

function TJCoreOPFSQLStatement.IGetParams: IJCoreOPFParams;
begin
  Result := FParams;
end;

function TJCoreOPFSQLStatement.IGetSQL: string;
begin
  Result := FSQL;
end;

constructor TJCoreOPFSQLStatement.Create(const AStmtManager: IJCoreOPFSQLStmtManager;
  const AParams: IJCoreOPFParams);
begin
  inherited Create;
  FStmtManager := AStmtManager;
  if Assigned(AParams) then
    FParams := AParams
  else
    FParams := TJCoreOPFParams.Create;
  if not Assigned(FLOG) then
    FLOG := TJCoreLogger.GetLogger('jcore.sql');
  FExpectedSize := TJCoreIntegerList.Create;
end;

procedure TJCoreOPFSQLStatement.CheckCommit;
begin
  if Params.Count > 0 then
    raise EJCoreOPF.Create(2111, S2111_UnassignedParams, []);
end;

procedure TJCoreOPFSQLStatement.CheckQueued;
begin
  if FExpectedSize.Count > 0 then
    raise EJCoreOPF.Create(2112, S2112_StatementOnQueue, []);
end;

procedure TJCoreOPFSQLStatement.Flush;
var
  I: Integer;
begin
  try
    for I := 0 to Pred(FExpectedSize.Count) do
      EnsureResultSetSize(ExecQuery, FExpectedSize[I]);
  finally
    FExpectedSize.Clear;
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

{ TJCoreOPFSQLDatabase }

{$warn 5033 off}
function TJCoreOPFSQLDatabase.AutoincSQL: string;
begin
  raise EJCoreOPF.Create(2131, S2131_DbAutoincUnsupported, [DatabaseName]);
end;

function TJCoreOPFSQLDatabase.SequenceSQL(const ASequenceName: string; const AOIDCount: Integer): string;
begin
  raise EJCoreOPF.Create(2130, S2130_DbSequenceUnsupported, [DatabaseName]);
end;
{$warn 5033 on}

{ TJCoreOPFSQLDriver }

function TJCoreOPFSQLDriver.AcquireQuery(const ASQL: string;
  const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery;
begin
  { TODO : Implement Query Pool }
  Result := InternalCreateQuery(ASQL, AParams);
  QueryList.Add(Result);
end;

procedure TJCoreOPFSQLDriver.DetachStatement(const AStmt: TJCoreOPFSQLStatement);
begin
  if Assigned(AStmt) then
    StmtList.Remove(AStmt);
end;

function TJCoreOPFSQLDriver.GetDatabase: TJCoreOPFSQLDatabase;
begin
  if not Assigned(FDatabase) then
    FDatabase := InternalDatabaseClass.Create;
  Result := FDatabase;
end;

procedure TJCoreOPFSQLDriver.QueueStatement(const AStmt: TJCoreOPFSQLStatement);
begin
  StmtQueue.Add(AStmt);
  AStmt.AddRef;
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
  for I := Pred(StmtList.Count) downto 0 do
    StmtList[I].ReleaseStmtManager;
end;

procedure TJCoreOPFSQLDriver.InternalCommit;
var
  I: Integer;
begin
  inherited InternalCommit;
  Flush;
  for I := 0 to Pred(StmtList.Count) do
    StmtList[I].CheckCommit;
end;

constructor TJCoreOPFSQLDriver.Create(const AParams: TStringList);
begin
  inherited Create(AParams);
  FQueryList := TJCoreOPFSQLQueryList.Create;
  FStmtList := TJCoreOPFSQLStatementList.Create(False);
  FStmtQueue := TJCoreOPFSQLStatementList.Create(True);
end;

destructor TJCoreOPFSQLDriver.Destroy;
begin
  ReleaseStatements;
  FreeAndNil(FDatabase);
  FreeAndNil(FQueryList);
  FreeAndNil(FStmtList);
  FreeAndNil(FStmtQueue);
  inherited Destroy;
end;

function TJCoreOPFSQLDriver.CreateStatement: IJCoreOPFSQLStatement;
begin
  Result := CreateStatement(nil);
end;

function TJCoreOPFSQLDriver.CreateStatement(const AParams: IJCoreOPFParams): IJCoreOPFSQLStatement;
var
  VStmt: TJCoreOPFSQLStatement;
begin
  VStmt := TJCoreOPFSQLStatement.Create(Self, AParams);
  StmtList.Add(VStmt);
  Result := VStmt;
end;

procedure TJCoreOPFSQLDriver.Flush;
var
  I: Integer;
begin
  for I := 0 to Pred(StmtQueue.Count) do
    StmtQueue[I].Flush;
  StmtQueue.Clear;
end;

end.

