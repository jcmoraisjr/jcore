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
  JCoreOPFDriver,
  JCoreOPFID;

type

  IJCoreOPFMapper = interface
    function AcquirePID(AEntity: TObject): IJCoreOPFPID;
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    procedure Store(const AEntity: TObject);
    procedure StorePID(const APID: IJCoreOPFPID);
  end;

  { TJCoreOPFMapping }

  TJCoreOPFMapping = class(TObject)
  private
    FDriver: TJCoreOPFDriver;
    FMapper: IJCoreOPFMapper;
  protected
    function InternalRetrieve(const AClass: TClass; const AOID: string): TObject; virtual; abstract;
    procedure InternalStore(const APID: IJCoreOPFPID); virtual; abstract;
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver); virtual;
    class function Apply(const AClass: TClass): Boolean; virtual; abstract;
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
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
    function CreateOID(const AOID: string): TJCoreOPFOID; virtual; abstract;
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    function GenerateSelectStatement(const AClass: TClass): string; virtual; abstract;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    function ReadFromDriver(const AClass: TClass; const AOID: string): TObject; virtual; abstract;
    procedure WriteToDriver(const APID: IJCoreOPFPID); virtual; abstract;
  protected
    function InternalRetrieve(const AClass: TClass; const AOID: string): TObject; override;
    procedure InternalStore(const APID: IJCoreOPFPID); override;
    procedure StoreOwnedObjectList(const AOwner: IJCoreOPFPID; const AList: TFPSList);
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

function TJCoreOPFMapping.Retrieve(const AClass: TClass; const AOID: string): TObject;
begin
  Result := InternalRetrieve(AClass, AOID);
end;

procedure TJCoreOPFMapping.Store(const APID: IJCoreOPFPID);
begin
  InternalStore(APID);
end;

{ TJCoreOPFSQLMapping }

function TJCoreOPFSQLMapping.InternalRetrieve(const AClass: TClass;
  const AOID: string): TObject;
var
  VOID: TJCoreOPFOID;
  VPID: IJCoreOPFPID;
begin
  VOID := CreateOID(AOID);
  try
    VOID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateSelectStatement(AClass), 1);
    Result := ReadFromDriver(AClass, AOID);
    VPID := Mapper.AcquirePID(Result);
    VPID.AssignOID(VOID);
  except
    FreeAndNil(VOID);
    raise;
  end;
end;

procedure TJCoreOPFSQLMapping.InternalStore(const APID: IJCoreOPFPID);
begin
  if APID.IsPersistent then
  begin
    WriteToDriver(APID);
    APID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateUpdateStatement(APID));
  end else
  begin
    APID.AssignOID(CreateOID(''));
    APID.OID.WriteToDriver(Driver);
    WriteToDriver(APID);
    Driver.ExecSQL(GenerateInsertStatement(APID));
  end;
end;

procedure TJCoreOPFSQLMapping.StoreOwnedObjectList(const AOwner: IJCoreOPFPID; const AList: TFPSList);
var
  VEntity: TObject;
  VPID: IJCoreOPFPID;
  I: Integer;
begin
  if not Assigned(AList) then
    Exit;
  for I := 0 to Pred(AList.Count) do
  begin
    VEntity := TObject(AList[I]^);
    VPID := Mapper.AcquirePID(VEntity);
    VPID.Owner := AOwner;
    Mapper.StorePID(VPID);
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

