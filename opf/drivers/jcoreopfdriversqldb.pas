(*
  JCore, OPF SQLdb Driver
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFDriverSQLdb;

{$I jcore.inc}

interface

uses
  Classes,
  db,
  sqldb,
  JCoreOPFOIDGen,
  JCoreOPFDriver;

type

  TJCoreOPFDriverSQLdb = class;

  { TJCoreOPFQuerySQLdb }

  TJCoreOPFQuerySQLdb = class(TInterfacedObject, IJCoreOPFSQLQuery, IJCoreOPFSQLResultSet)
  private
    FCurrentField: Integer;
    FDriver: TJCoreOPFDriverSQLdb;
    FFieldCount: Integer;
    FParams: IJCoreOPFParams;
    FQuery: TSQLQuery;
    FRecordCount: Integer;
    procedure CreateQuery(const ASQL: string);
    procedure NextField;
    procedure PopulateParams;
  protected
    property CurrentField: Integer read FCurrentField;
    property Driver: TJCoreOPFDriverSQLdb read FDriver;
    property Params: IJCoreOPFParams read FParams;
    property Query: TSQLQuery read FQuery;
  public
    constructor Create(const ADriver: TJCoreOPFDriverSQLdb; const ASQL: string; const AParams: IJCoreOPFParams);
    destructor Destroy; override;
    procedure Close;
    function ExecSQL: Integer;
    function IsClosed: Boolean;
    function OpenCursor: IJCoreOPFSQLResultSet;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadNull: Boolean;
    function ReadString: string;
    function Size: Integer;
    procedure SkipReading;
  end;

  { TJCoreOPFDriverSQLdb }

  TJCoreOPFDriverSQLdb = class(TJCoreOPFSQLDriver)
  private
    FConnection: TSQLConnection;
    FConnectionName: string;
    FTransaction: TSQLTransaction;
    procedure CreateConnection;
  protected
    procedure InternalCommit; override;
    function InternalCreateQuery(const ASQL: string; const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery; override;
  public
    constructor Create(const AParams: TStringList); override;
    destructor Destroy; override;
    function CreateGenerator(const ASequenceName: string): IJCoreOPFOIDGenerator; override;
    class function DriverName: string; override;
    property Connection: TSQLConnection read FConnection;
    property ConnectionName: string read FConnectionName;
    property Transaction: TSQLTransaction read FTransaction;
  end;

  { TJCoreOPFOIDGeneratorSQLdbPostgreSQL }

  TJCoreOPFOIDGeneratorSQLdbPostgreSQL = class(TJCoreOPFOIDGeneratorSQLDriver)
  protected
    procedure InternalGenerateOIDs(const AOIDCount: Integer); override;
  end;

implementation

uses
  typinfo,
  sysutils,
  JCoreConsts,
  JCoreClasses;

{ TJCoreOPFQuerySQLdb }

procedure TJCoreOPFQuerySQLdb.CreateQuery(const ASQL: string);
var
  VSQL: String;
  VCount, VNext, I: Integer;
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := Driver.Connection;
  VSQL := '';
  VCount := 0;
  VNext := 1;
  // Changing params to :p1, :p2, ... SQLdb doesn't like the '?' syntax
  { TODO : String literals are not supported }
  for I := 1 to Length(ASQL) do
    if ASQL[I] = '?' then
    begin
      Inc(VCount);
      VSQL := VSQL + Copy(ASQL, VNext, I - VNext) + ':p' + IntToStr(VCount);
      VNext := I + 1;
    end;
  VSQL := VSQL + Copy(ASQL, VNext, Length(ASQL));
  Query.SQL.Text := VSQL;
end;

procedure TJCoreOPFQuerySQLdb.NextField;
begin
  if FFieldCount = 0 then
    raise EJCoreOPF.Create(2103, S2103_ClosedDataset, []);
  Inc(FCurrentField);
  if FCurrentField = FFieldCount then
  begin
    FCurrentField := 0;
    Query.Next;
    if Query.EOF then
      Close;
  end;
end;

procedure TJCoreOPFQuerySQLdb.PopulateParams;
var
  VParam: TParam;
  VType: TTypeKind;
  I: Integer;
begin
  for I := 0 to Pred(Query.Params.Count) do
  begin
    VParam := Query.Params[I];
    VType := Params.ReadType;
    case VType of
      tkUnknown: VParam.Clear;
      tkAString: VParam.AsString := Params.ReadString;
      tkInteger: VParam.AsInteger := Params.ReadInt32;
      tkInt64: VParam.AsLargeInt := Params.ReadInt64;
      else
        raise EJCoreOPF.Create(2104, S2104_UnsupportedType, [GetEnumName(TypeInfo(TTypeKind), Ord(VType))]);
    end;
  end;
end;

constructor TJCoreOPFQuerySQLdb.Create(const ADriver: TJCoreOPFDriverSQLdb; const ASQL: string;
  const AParams: IJCoreOPFParams);
begin
  inherited Create;
  FDriver := ADriver;
  FParams := AParams;
  CreateQuery(ASQL);
end;

destructor TJCoreOPFQuerySQLdb.Destroy;
begin
  Close;
  FreeAndNil(FQuery);
  inherited Destroy;
end;

procedure TJCoreOPFQuerySQLdb.Close;
begin
  FFieldCount := 0;
  FCurrentField := 0;
  FRecordCount := -1;
  Query.Close;
end;

function TJCoreOPFQuerySQLdb.ExecSQL: Integer;
begin
  PopulateParams;
  Query.ExecSQL;
  Result := Query.RowsAffected;
end;

function TJCoreOPFQuerySQLdb.IsClosed: Boolean;
begin
  Result := not Query.Active;
end;

function TJCoreOPFQuerySQLdb.OpenCursor: IJCoreOPFSQLResultSet;
begin
  PopulateParams;
  Query.UniDirectional := True;
  Query.Open;
  FFieldCount := Query.FieldCount;
  FRecordCount := Query.RecordCount;
  Result := Self;
end;

function TJCoreOPFQuerySQLdb.ReadInt32: Integer;
begin
  Result := Query.Fields[CurrentField].AsInteger;
  NextField;
end;

function TJCoreOPFQuerySQLdb.ReadInt64: Int64;
begin
  Result := Query.Fields[CurrentField].AsLargeInt;
  NextField;
end;

function TJCoreOPFQuerySQLdb.ReadNull: Boolean;
begin
  Result := Query.Fields[CurrentField].IsNull;
  if Result then
    NextField;
end;

function TJCoreOPFQuerySQLdb.ReadString: string;
begin
  Result := Query.Fields[CurrentField].AsString;
  NextField;
end;

function TJCoreOPFQuerySQLdb.Size: Integer;
begin
  Result := FRecordCount;
end;

procedure TJCoreOPFQuerySQLdb.SkipReading;
begin
  NextField;
end;

{ TJCoreOPFDriverSQLdb }

procedure TJCoreOPFDriverSQLdb.CreateConnection;
var
  VConnectionDef: TConnectionDef;
begin
  VConnectionDef := GetConnectionDef(ConnectionName);
  if not Assigned(VConnectionDef) then
    raise EJCoreOPF.Create(2105, S2105_ConnectionNotFound, [ConnectionName]);
  FConnection := VConnectionDef.ConnectionClass.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FConnection.Transaction := FTransaction;
  Connection.HostName := Params.Values['hostname'];
  Connection.DatabaseName := Params.Values['database'];
  Connection.UserName := Params.Values['username'];
  Connection.Password := Params.Values['password'];
end;

procedure TJCoreOPFDriverSQLdb.InternalCommit;
begin
  inherited InternalCommit;
  Transaction.Commit;
end;

function TJCoreOPFDriverSQLdb.InternalCreateQuery(const ASQL: string;
  const AParams: IJCoreOPFParams): IJCoreOPFSQLQuery;
begin
  Result := TJCoreOPFQuerySQLdb.Create(Self, ASQL, AParams);
end;

constructor TJCoreOPFDriverSQLdb.Create(const AParams: TStringList);
begin
  inherited Create(AParams);
  FConnectionName := Params.Values['connection'];
  CreateConnection;
end;

destructor TJCoreOPFDriverSQLdb.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  inherited Destroy;
end;

function TJCoreOPFDriverSQLdb.CreateGenerator(const ASequenceName: string): IJCoreOPFOIDGenerator;
type
  TSQLdbGeneratorClass = class of TJCoreOPFOIDGeneratorSQLDriver;
var
  VSQLdbGeneratorClass: TSQLdbGeneratorClass;
begin
  if SameText(ConnectionName, 'postgresql') then
    VSQLdbGeneratorClass := TJCoreOPFOIDGeneratorSQLdbPostgreSQL
  else
    raise EJCoreOPF.Create(2106, S2106_UnsupportedConnection, [ConnectionName]);
  Result := VSQLdbGeneratorClass.Create(Self, ASequenceName)
end;

class function TJCoreOPFDriverSQLdb.DriverName: string;
begin
  Result := 'SQLdb';
end;

{ TJCoreOPFOIDGeneratorSQLdbPostgreSQL }

procedure TJCoreOPFOIDGeneratorSQLdbPostgreSQL.InternalGenerateOIDs(const AOIDCount: Integer);
var
  VStmt: IJCoreOPFSQLStatement;
  VResultSet: IJCoreOPFSQLResultSet;
  I: Integer;
begin
  VStmt := Driver.CreateStatement;
  if AOIDCount > 1 then
    VStmt.SQL := Format('SELECT nextval(''%s'') FROM generate_series(1,%d)', [SequenceName, AOIDCount])
  else
    VStmt.SQL := Format('SELECT nextval(''%s'')', [SequenceName]);
  VResultSet := VStmt.OpenCursor(AOIDCount);
  for I := 0 to Pred(VResultSet.Size) do
    OIDList.Add(VResultSet.ReadInt64);
  VResultSet.Close;
end;

end.

