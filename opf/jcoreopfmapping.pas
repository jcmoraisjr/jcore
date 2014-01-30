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
  JCoreClasses,
  JCoreOPFDriver,
  JCoreOPFID;

type

  { IJCoreOPFMapper }

  IJCoreOPFMapper = interface
    function AcquirePID(AEntity: TObject; const AAddToInTransactionPIDList: Boolean = True): IJCoreOPFPID;
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveListPID(const AClass: TClass; const AOwner: IJCoreOPFPID): TJCoreObjectList;
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
    procedure StorePID(const APID: IJCoreOPFPID);
    procedure StoreSharedListPID(const AListBaseClass: TClass; const APID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray);
  end;

  { TJCoreOPFMapping }

  TJCoreOPFMapping = class(TObject)
  private
    FDriver: TJCoreOPFDriver;
    FMapper: IJCoreOPFMapper;
  protected
    procedure WriteNullOIDToDriver(const AClass: TClass); virtual;
  protected
    function CreateOIDFromString(const AOID: string): TJCoreOPFOID; virtual; abstract;
    function CreateOIDFromDriver(const ADriver: TJCoreOPFDriver): TJCoreOPFOID; virtual; abstract;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; virtual; abstract;
    function InternalRetrieveList(const AClass: TClass; const AOwner: IJCoreOPFPID): TJCoreObjectList; virtual; abstract;
    procedure InternalStore(const APID: IJCoreOPFPID); virtual; abstract;
    procedure InternalStoreSharedList(const APID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); virtual; abstract;
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver); virtual;
    class function Apply(const AClass: TClass): Boolean; virtual; abstract;
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveFromString(const AClass: TClass; const AStringOID: string): TObject;
    function RetrieveList(const AClass: TClass; const AOwner: IJCoreOPFPID): TJCoreObjectList;
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
    procedure Store(const APID: IJCoreOPFPID);
    procedure StoreSharedList(const APID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray);
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
    function CreateEntity(const AClass: TClass): TObject; virtual;
    function GenerateDeleteSharedItemStatement(const AOwnerPID: IJCoreOPFPID): string; virtual;
    function GenerateInsertSharedItemStatement(const AOwnerPID, ASharedPID: IJCoreOPFPID): string; virtual;
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    function GenerateSelectListFromStatement(const AClass, AOwnerClass: TClass): string; virtual;
    function GenerateSelectStatement(const AClass: TClass): string; virtual; abstract;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    procedure ReadFromDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteExternalsToDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteInternalsToDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteSharedListItemToDriver(const AOwnerPID, ASharedPID: IJCoreOPFPID); virtual;
  protected
    function CreatePIDArray(const AList: TFPSList): TJCoreOPFPIDArray;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; override;
    function InternalRetrieveList(const AClass: TClass; const AOwner: IJCoreOPFPID): TJCoreObjectList; override;
    procedure InternalStore(const APID: IJCoreOPFPID); override;
    procedure StoreOwnedObjectList(const AOwner: IJCoreOPFPID; const AList: TFPSList);
    procedure InternalStoreSharedList(const APID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); override;
    property Driver: TJCoreOPFSQLDriver read FSQLDriver;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver); override;
  end;

implementation

uses
  sysutils,
  JCoreOPFException;

{ TJCoreOPFMapping }

procedure TJCoreOPFMapping.WriteNullOIDToDriver(const AClass: TClass);
begin
  { TODO : Evaluate after attr metadata implementation }
  Driver.WriteNull;
end;

constructor TJCoreOPFMapping.Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver);
begin
  if not Assigned(ADriver) or not Assigned(AMapper) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FMapper := AMapper;
  FDriver := ADriver;
end;

function TJCoreOPFMapping.RetrieveFromDriver(const AClass: TClass;
  const ADriverOID: TJCoreOPFDriver): TObject;
var
  VOID: TJCoreOPFOID;
begin
  VOID := CreateOIDFromDriver(ADriverOID);
  try
    Result := InternalRetrieve(AClass, VOID);
  except
    FreeAndNil(VOID);
    raise;
  end;
end;

function TJCoreOPFMapping.RetrieveFromString(const AClass: TClass;
  const AStringOID: string): TObject;
var
  VOID: TJCoreOPFOID;
begin
  VOID := CreateOIDFromString(AStringOID);
  try
    Result := InternalRetrieve(AClass, VOID);
  except
    FreeAndNil(VOID);
    raise;
  end;
end;

function TJCoreOPFMapping.RetrieveList(const AClass: TClass;
  const AOwner: IJCoreOPFPID): TJCoreObjectList;
begin
  Result := InternalRetrieveList(AClass, AOwner);
end;

procedure TJCoreOPFMapping.StoreToDriver(const AClass: TClass;
  const AEntity: TObject; const ADriver: TJCoreOPFDriver);
var
  VPID: IJCoreOPFPID;
begin
  if Assigned(AEntity) then
  begin
    VPID := Mapper.AcquirePID(AEntity);
    Store(VPID);
    VPID.OID.WriteToDriver(ADriver);
  end else
    WriteNullOIDToDriver(AClass);
end;

procedure TJCoreOPFMapping.Store(const APID: IJCoreOPFPID);
begin
  InternalStore(APID);
end;

procedure TJCoreOPFMapping.StoreSharedList(const APID: IJCoreOPFPID;
  const APIDArray: TJCoreOPFPIDArray);
begin
  InternalStoreSharedList(APID, APIDArray);
end;

{ TJCoreOPFSQLMapping }

function TJCoreOPFSQLMapping.CreateEntity(const AClass: TClass): TObject;
begin
  Result := AClass.Create;
end;

function TJCoreOPFSQLMapping.GenerateDeleteSharedItemStatement(
  const AOwnerPID: IJCoreOPFPID): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateInsertSharedItemStatement(const AOwnerPID,
  ASharedPID: IJCoreOPFPID): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateSelectListFromStatement(
  const AClass, AOwnerClass: TClass): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

procedure TJCoreOPFSQLMapping.ReadFromDriver(const APID: IJCoreOPFPID);
begin
end;

procedure TJCoreOPFSQLMapping.WriteExternalsToDriver(const APID: IJCoreOPFPID);
begin
end;

procedure TJCoreOPFSQLMapping.WriteInternalsToDriver(const APID: IJCoreOPFPID);
begin
end;

procedure TJCoreOPFSQLMapping.WriteSharedListItemToDriver(const AOwnerPID,
  ASharedPID: IJCoreOPFPID);
begin
  AOwnerPID.OID.WriteToDriver(Driver);
  ASharedPID.OID.WriteToDriver(Driver);
  Driver.ExecSQL(GenerateInsertSharedItemStatement(AOwnerPID, ASharedPID));
end;

function TJCoreOPFSQLMapping.CreatePIDArray(const AList: TFPSList): TJCoreOPFPIDArray;
var
  VEntity: TObject;
  I: Integer;
begin
  if Assigned(AList) and (AList.Count > 0) then
  begin
    SetLength(Result, AList.Count);
    for I := 0 to Pred(AList.Count) do
    begin
      VEntity := TObject(AList[I]^);
      Result[I] := Mapper.AcquirePID(VEntity);
    end;
  end else
    Result := nil;
end;

function TJCoreOPFSQLMapping.InternalRetrieve(const AClass: TClass;
  const AOID: TJCoreOPFOID): TObject;
var
  VPID: IJCoreOPFPID;
begin
  AOID.WriteToDriver(Driver);
  Driver.ExecSQL(GenerateSelectStatement(AClass), 1);
  Result := CreateEntity(AClass);
  try
    VPID := Mapper.AcquirePID(Result);
    { TODO : Two owners for the same OID if an exception raises }
    VPID.AssignOID(AOID);
    ReadFromDriver(VPID);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJCoreOPFSQLMapping.InternalRetrieveList(const AClass: TClass;
  const AOwner: IJCoreOPFPID): TJCoreObjectList;
var
  VPID: IJCoreOPFPID;
  VOID: TJCoreOPFOID;
  VCount: Integer;
  I: Integer;
begin
  AOwner.OID.WriteToDriver(Driver);
  VCount := Driver.ExecSQL(GenerateSelectListFromStatement(AClass, AOwner.Entity.ClassType));
  Result := TJCoreObjectList.Create(True);
  try
    for I := 1 to VCount do
    begin
      VOID := CreateOIDFromDriver(Driver);
      try
        Result.Add(CreateEntity(AClass));
        VPID := Mapper.AcquirePID(Result.Last);
        VPID.AssignOID(VOID);
        ReadFromDriver(VPID);
      except
        FreeAndNil(VOID);
        raise;
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TJCoreOPFSQLMapping.InternalStore(const APID: IJCoreOPFPID);
begin
  if APID.IsPersistent then
  begin
    WriteInternalsToDriver(APID);
    APID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateUpdateStatement(APID));
    WriteExternalsToDriver(APID);
  end else
  begin
    APID.AssignOID(CreateOIDFromString(''));
    APID.OID.WriteToDriver(Driver);
    WriteInternalsToDriver(APID);
    Driver.ExecSQL(GenerateInsertStatement(APID));
    WriteExternalsToDriver(APID);
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
    if Apply(VEntity.ClassType) then
      Store(VPID)
    else
      Mapper.StorePID(VPID);
  end;
end;

procedure TJCoreOPFSQLMapping.InternalStoreSharedList(const APID: IJCoreOPFPID;
  const APIDArray: TJCoreOPFPIDArray);
var
  I: Integer;
begin
  if APID.IsPersistent then
  begin
    APID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateDeleteSharedItemStatement(APID));
  end;
  for I := Low(APIDArray) to High(APIDArray) do
    if not APIDArray[I].IsPersistent then
    begin
      if Apply(APIDArray[I].Entity.ClassType) then
        Store(APIDArray[I])
      else
        Mapper.StorePID(APIDArray[I]);
    end;
  for I := Low(APIDArray) to High(APIDArray) do
    WriteSharedListItemToDriver(APID, APIDArray[I]);
end;

constructor TJCoreOPFSQLMapping.Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver);
begin
  if not (ADriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPFUnsupportedDriver.Create(ADriver.ClassName);
  inherited Create(AMapper, ADriver);
  FSQLDriver := TJCoreOPFSQLDriver(ADriver);
end;

end.

