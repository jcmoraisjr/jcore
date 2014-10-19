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

  { TJCoreOPFDriverSQLdb }

  TJCoreOPFDriverSQLdb = class(TJCoreOPFSQLDriver)
  private
    FConnection: TSQLConnection;
    FConnectionName: string;
    FCurrentField: Integer;
    FFieldCount: Integer;
    FQuery: TSQLQuery;
    FTransaction: TSQLTransaction;
    procedure CloseQuery;
    procedure CreateConnection;
    function IsSelectStatement(const ASQL: string): Boolean;
    procedure NextField;
    procedure PopParam(const AParam: TParam);
    property CurrentField: Integer read FCurrentField;
  protected
    procedure InternalCommit; override;
    function InternalExecSQL(const ASQL: string): Integer; override;
    property Connection: TSQLConnection read FConnection;
    property Query: TSQLQuery read FQuery;
    property Transaction: TSQLTransaction read FTransaction;
  public
    constructor Create(const AParams: TStringList); override;
    destructor Destroy; override;
    function CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator; override;
    class function DriverName: string; override;
    function ReadInt32: Integer; override;
    function ReadInt64: Int64; override;
    function ReadNull: Boolean; override;
    function ReadNullAndSkip: Boolean; override;
    function ReadString: string; override;
    property ConnectionName: string read FConnectionName;
  end;

  { TJCoreOPFGeneratorSQLdbPostgreSQL }

  TJCoreOPFGeneratorSQLdbPostgreSQL = class(TJCoreOPFOIDGeneratorSQLDriver)
  protected
    procedure InternalGenerateOIDs(const AOIDCount: Integer); override;
  end;

implementation

uses
  typinfo,
  sysutils,
  JCoreOPFException;

{ TJCoreOPFDriverSQLdb }

procedure TJCoreOPFDriverSQLdb.CloseQuery;
begin
  FFieldCount := 0;
  FCurrentField := 0;
  Query.Close;
end;

procedure TJCoreOPFDriverSQLdb.CreateConnection;
var
  VConnectionDef: TConnectionDef;
begin
  VConnectionDef := GetConnectionDef(ConnectionName);
  if not Assigned(VConnectionDef) then
    raise EJCoreOPFDriver.Create('Connection not found: %s', [ConnectionName]);
  FConnection := VConnectionDef.ConnectionClass.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  FConnection.Transaction := FTransaction;
  FQuery.DataBase := FConnection;
  Connection.HostName := Params.Values['hostname'];
  Connection.DatabaseName := Params.Values['database'];
  Connection.UserName := Params.Values['username'];
  Connection.Password := Params.Values['password'];
end;

function TJCoreOPFDriverSQLdb.IsSelectStatement(const ASQL: string): Boolean;
begin
  Result := SameText(Copy(Trim(ASQL), 1, 7), 'select ');
end;

procedure TJCoreOPFDriverSQLdb.NextField;
begin
  if FFieldCount = 0 then
    raise EJCoreOPFDriver.Create('Closed dataset', []);
  Inc(FCurrentField);
  if FCurrentField = FFieldCount then
  begin
    FCurrentField := 0;
    Query.Next;
    if Query.EOF then
      CloseQuery;
  end;
end;

procedure TJCoreOPFDriverSQLdb.PopParam(const AParam: TParam);
var
  VType: TTypeKind;
begin
  VType := Stack.PopType;
  case VType of
    tkUnknown: AParam.Clear;
    tkAString: AParam.AsString := Stack.PopString;
    tkInteger: AParam.AsInteger := Stack.PopInt32;
    tkInt64: AParam.AsLargeInt := Stack.PopInt64;
    else raise EJCoreOPFDriver.Create('Unsupported type', [GetEnumName(TypeInfo(TTypeKind), Ord(VType))]);
  end;
end;

procedure TJCoreOPFDriverSQLdb.InternalCommit;
begin
  Transaction.Commit;
end;

function TJCoreOPFDriverSQLdb.InternalExecSQL(const ASQL: string): Integer;
var
  VSQL: string;
  VCount, VNext, I: Integer;
begin
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
  for I := Pred(Query.Params.Count) downto 0 do
    PopParam(Query.Params[I]);
  if IsSelectStatement(ASQL) then
  begin
    Query.Open;
    FFieldCount := Query.FieldCount;
    Result := Query.RecordCount;
  end else
  begin
    Query.ExecSQL;
    Result := Query.RowsAffected;
  end;
end;

constructor TJCoreOPFDriverSQLdb.Create(const AParams: TStringList);
begin
  inherited Create(AParams);
  FConnectionName := Params.Values['connection'];
  CreateConnection;
end;

destructor TJCoreOPFDriverSQLdb.Destroy;
begin
  FreeAndNil(FQuery);
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  inherited Destroy;
end;

function TJCoreOPFDriverSQLdb.CreateGenerator(const AGeneratorName: string): IJCoreOPFOIDGenerator;
type
  TSQLdbGeneratorClass = class of TJCoreOPFOIDGeneratorSQLDriver;
var
  VSQLdbGeneratorClass: TSQLdbGeneratorClass;
begin
  if SameText(ConnectionName, 'postgresql') then
    VSQLdbGeneratorClass := TJCoreOPFGeneratorSQLdbPostgreSQL
  else
    raise EJCoreOPFDriver.Create('Unsupported connection: %s', [ConnectionName]);
  Result := VSQLdbGeneratorClass.Create(Self, AGeneratorName)
end;

class function TJCoreOPFDriverSQLdb.DriverName: string;
begin
  Result := 'SQLdb';
end;

function TJCoreOPFDriverSQLdb.ReadInt32: Integer;
begin
  Result := Query.Fields[CurrentField].AsInteger;
  NextField;
end;

function TJCoreOPFDriverSQLdb.ReadInt64: Int64;
begin
  Result := Query.Fields[CurrentField].AsLargeInt;
  NextField;
end;

function TJCoreOPFDriverSQLdb.ReadNull: Boolean;
begin
  Result := Query.Fields[CurrentField].IsNull;
  if Result then
    NextField;
end;

function TJCoreOPFDriverSQLdb.ReadNullAndSkip: Boolean;
begin
  Result := Query.Fields[CurrentField].IsNull;
  NextField;
end;

function TJCoreOPFDriverSQLdb.ReadString: string;
begin
  Result := Query.Fields[CurrentField].AsString;
  NextField;
end;

{ TJCoreOPFGeneratorSQLdbPostgreSQL }

procedure TJCoreOPFGeneratorSQLdbPostgreSQL.InternalGenerateOIDs(const AOIDCount: Integer);
var
  I: Integer;
begin
  Driver.ExecSQL(
   Format('SELECT nextval(''%s'') FROM generate_series(1,%d)', [GeneratorName, AOIDCount]), AOIDCount);
  for I := 0 to Pred(AOIDCount) do
    OIDList.Add(Driver.ReadInt64);
end;

end.

