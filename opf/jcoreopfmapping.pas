(*
  JCore, OPF Mapping Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFMapping;

{$I jcore.inc}

interface

uses
  fgl,
  JCoreOPFOID,
  JCoreOPFPID,
  JCoreOPFDriver;

type

  IJCoreOPFMapper = interface
    procedure Store(const AEntity: TObject);
  end;

  { TJCoreOPFMapping }

  TJCoreOPFMapping = class(TObject)
  private
    FDriver: TJCoreOPFDriver;
    FMapper: IJCoreOPFMapper;
  protected
    procedure InternalStore(const APID: IJCoreOPFPID); virtual; abstract;
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver); virtual;
    class function Apply(const AClass: TClass): Boolean; virtual; abstract;
    procedure Store(const APID: IJCoreOPFPID);
  end;

  TJCoreOPFMappingList = specialize TFPGObjectList<TJCoreOPFMapping>;

  TJCoreOPFMappingClass = class of TJCoreOPFMapping;

  TJCoreOPFMappingClassList = specialize TFPGList<TJCoreOPFMappingClass>;

  { TJCoreOPFSQLMapping }

  TJCoreOPFSQLMapping = class(TJCoreOPFMapping)
  private
    { TODO : Generics? }
    FSQLDriver: TJCoreOPFSQLDriver;
  protected
    function CreateOID(const APID: IJCoreOPFPID): TJCoreOPFOID; virtual; abstract;
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    function GenerateSelectStatement(const AClass: TClass; const AOID: string): string; virtual; abstract;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    function ReadFromDriver(const AClass: TClass; const AOID: string): TObject; virtual; abstract;
    procedure WriteToDriver(const APID: IJCoreOPFPID); virtual; abstract;
  protected
    procedure InternalStore(const APID: IJCoreOPFPID); override;
    property Driver: TJCoreOPFSQLDriver read FSQLDriver;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver); override;
  end;

implementation

uses
  sysutils,
  JCoreClasses,
  JCoreOPFException;

{ TJCoreOPFMapping }

constructor TJCoreOPFMapping.Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver);
begin
  if not Assigned(ADriver) or not Assigned(AMapper) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FMapper := AMapper;
  FDriver := ADriver;
end;

procedure TJCoreOPFMapping.Store(const APID: IJCoreOPFPID);
begin
  InternalStore(APID);
end;

{ TJCoreOPFSQLMapping }

procedure TJCoreOPFSQLMapping.InternalStore(const APID: IJCoreOPFPID);
begin
  if APID.IsPersistent then
  begin
    WriteToDriver(APID);
    APID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateUpdateStatement(APID));
  end else
  begin
    APID.AssignOID(CreateOID(APID));
    APID.OID.WriteToDriver(Driver);
    WriteToDriver(APID);
    Driver.ExecSQL(GenerateInsertStatement(APID));
  end;
end;

constructor TJCoreOPFSQLMapping.Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver);
begin
  if not (ADriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPFUnsupportedDriver.Create(ADriver.ClassName);
  inherited Create(AMapper, ADriver);
  FSQLDriver := TJCoreOPFSQLDriver(ADriver);
end;

end.

