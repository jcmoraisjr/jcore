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
    procedure ExecuteInsert(const APID: IJCoreOPFPID); virtual; abstract;
    procedure ExecuteUpdate(const APID: IJCoreOPFPID); virtual; abstract;
    function GenerateOID(const APID: IJCoreOPFPID): TJCoreOPFOID; virtual; abstract;
    procedure InternalStore(const APID: IJCoreOPFPID); virtual;
    procedure StorePID(const APID: IJCoreOPFPID); virtual; abstract;
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

procedure TJCoreOPFMapping.InternalStore(const APID: IJCoreOPFPID);
begin
  if APID.IsPersistent then
  begin
    StorePID(APID);
    APID.OID.WriteToDriver(Driver);
    ExecuteUpdate(APID);
  end else
  begin
    APID.AssignOID(GenerateOID(APID));
    APID.OID.WriteToDriver(Driver);
    StorePID(APID);
    ExecuteInsert(APID);
  end;
end;

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

constructor TJCoreOPFSQLMapping.Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver);
begin
  if not (ADriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPFUnsupportedDriver.Create(ADriver.ClassName);
  inherited Create(AMapper, ADriver);
  FSQLDriver := TJCoreOPFSQLDriver(ADriver);
end;

end.

