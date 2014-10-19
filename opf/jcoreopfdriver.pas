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

  {
    Some lines about SQL Driver.

    * Write<Data> are added in the same order of the Query fields, eg:
    - WriteInteger 15
    - WriteString ThePersonName
    - ExecSQL INSERT INTO PERSON(ID, NAME) VALUES(?, ?)
    - WriteString TheCityName
    - WriteInteger 4
    - ExecSQL UPDATE CITY SET NAME=? WHERE ID=?

    * Create a stack with Write<Data>, ie, take the last entries if the number of
      <Data> is greater then the number of Query fields:
    - WriteInteger 15
    - WriteString ThePersonName
    - WriteInteger 4
    - WriteString TheCityName
    - ExecSQL INSERT INTO CITY(ID, NAME) VALUES(?, ?)
    - WriteInteger 4
    - ExecSQL INSERT INTO PERSON(ID, NAME, CITY) VALUES(?, ?, ?)

    * The same logic to deletes:
    - WriteInteger 15
    - ExecSQL DELETE FROM PERSON WHERE ID=?

    * Selects read params from the same stack and create a separate queue for
      future reading:
    - WriteInteger 15
    - ExecSQL SELECT NAME,CITY FROM PERSON WHERE ID=?
    - ReadString
    - ReadInteger

    * Read new selects first, old selects later:
    - ExecSQL SELECT ID,NAME,CITY,BIRTHDAY FROM PERSON WHERE...
    - ReadInteger // person.id
    - ReadString // person.name
    - ReadInteger // person.city
    - ExecSQL SELECT ID,NAME FROM CITY WHERE...
    - ReadInteger // city.id
    - ReadString // city.name
    - ReadDate // person.birthday

    * The number of resultset fields need to match the number of Read<Data> and
      the number of Query fields need to match the number of Write<Data>. On the
      commit event, if there are some pending Read<Data> or Write<Data>, raise
      an exception

    * Always return the ExecSQL function with the number of the affected rows.

    * Drivers, as well as Mappings and Sessions are NOT thread safe. A threaded
      application need to create one Session per thread, which will create its
      own Mappings and Drivers. So relax =)
  }

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

  { TJCoreOPFSQLDriver }

  TJCoreOPFSQLDriver = class(TJCoreOPFDriver)
  private
    class var FLOGSQL: IJCoreLogger;
  protected
    function InternalExecSQL(const ASQL: string): Integer; virtual; abstract;
    class property LOGSQL: IJCoreLogger read FLOGSQL;
  public
    constructor Create(const AParams: TStringList); override;
    function ExecSQL(const ASQL: string): Integer;
    procedure ExecSQL(const ASQL: string; const AExpectedSize: Integer);
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

{ TJCoreOPFSQLDriver }

constructor TJCoreOPFSQLDriver.Create(const AParams: TStringList);
begin
  inherited Create(AParams);
  if not Assigned(FLOGSQL) then
    FLOGSQL := TJCoreLogger.GetLogger('jcore.opf.driver.sql');
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

{ TJCoreOPFOIDGeneratorSQLDriver }

constructor TJCoreOPFOIDGeneratorSQLDriver.Create(const ADriver: TJCoreOPFSQLDriver;
  const AGeneratorName: string);
begin
  inherited Create;
  FDriver := ADriver;
  FGeneratorName := AGeneratorName;
end;

end.

