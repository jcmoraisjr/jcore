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

  IJCoreOPFMapper = interface
    function AcquirePID(AEntity: TObject; const AAddToInTransactionPIDList: Boolean = True): IJCoreOPFPID;
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    function RetrieveOwnedListPID(const AClass: TClass; const AOwner: IJCoreOPFPID): TJCoreObjectList;
    procedure Store(const AEntity: TObject);
    procedure StorePID(const APID: IJCoreOPFPID);
    procedure StoreSharedListPID(const AListBaseClass: TClass; const APID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray);
  end;

  { TJCoreOPFMapping }

  TJCoreOPFMapping = class(TObject)
  private
    FDriver: TJCoreOPFDriver;
    FMapper: IJCoreOPFMapper;
  protected
    function InternalRetrieve(const AClass: TClass; const AOID: string): TObject; virtual; abstract;
    function InternalRetrieveOwnedList(const AClass: TClass; const AOwner: IJCoreOPFPID): TJCoreObjectList; virtual; abstract;
    procedure InternalStore(const APID: IJCoreOPFPID); virtual; abstract;
    procedure InternalStoreSharedList(const APID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); virtual; abstract;
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver); virtual;
    class function Apply(const AClass: TClass): Boolean; virtual; abstract;
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    function RetrieveOwnedList(const AClass: TClass; const AOwner: IJCoreOPFPID): TJCoreObjectList;
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
    function CreateOID(const AOID: string): TJCoreOPFOID; virtual; abstract;
    function GenerateDeleteSharedItemStatement(const AOwnerPID: IJCoreOPFPID): string; virtual;
    function GenerateInsertSharedItemStatement(const AOwnerPID, ASharedPID: IJCoreOPFPID): string; virtual;
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    function GenerateSelectOwnedListStatement(const AClass: TClass): string; virtual;
    function GenerateSelectStatement(const AClass: TClass): string; virtual; abstract;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    procedure ReadFromDriver(const APID: IJCoreOPFPID); virtual;
    function ReadOwnedOIDFromDriver: TJCoreOPFOID; virtual;
    procedure WriteExternalsToDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteInternalsToDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteSharedListItemToDriver(const AOwnerPID, ASharedPID: IJCoreOPFPID); virtual;
  protected
    function CreatePIDArray(const AList: TFPSList): TJCoreOPFPIDArray;
    function InternalRetrieve(const AClass: TClass; const AOID: string): TObject; override;
    function InternalRetrieveOwnedList(const AClass: TClass; const AOwner: IJCoreOPFPID): TJCoreObjectList; override;
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

function TJCoreOPFMapping.RetrieveOwnedList(const AClass: TClass;
  const AOwner: IJCoreOPFPID): TJCoreObjectList;
begin
  Result := InternalRetrieveOwnedList(AClass, AOwner);
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
  raise EJCoreOPFUnsupportedSharedOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateInsertSharedItemStatement(const AOwnerPID,
  ASharedPID: IJCoreOPFPID): string;
begin
  raise EJCoreOPFUnsupportedSharedOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateSelectOwnedListStatement(
  const AClass: TClass): string;
begin
  { TODO : Owned objects, like Phone list, need some special methods. Perhaps
           move these methods to a subclass? But this will cause a misuse of
           user's manual mappings that inherit from a common abstract mapping
           class. Ideas for another approach is welcome. }
  raise EJCoreOPFUnsupportedOwnedOperations.Create;
end;

procedure TJCoreOPFSQLMapping.ReadFromDriver(const APID: IJCoreOPFPID);
begin
end;

function TJCoreOPFSQLMapping.ReadOwnedOIDFromDriver: TJCoreOPFOID;
begin
  raise EJCoreOPFUnsupportedOwnedOperations.Create;
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
  const AOID: string): TObject;
var
  VOID: TJCoreOPFOID;
  VPID: IJCoreOPFPID;
begin
  VOID := CreateOID(AOID);
  try
    VOID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateSelectStatement(AClass), 1);
    Result := CreateEntity(AClass);
    try
      VPID := Mapper.AcquirePID(Result);
      VPID.AssignOID(VOID);
      ReadFromDriver(VPID);
    except
      FreeAndNil(Result);
      raise;
    end;
  except
    FreeAndNil(VOID);
    raise;
  end;
end;

function TJCoreOPFSQLMapping.InternalRetrieveOwnedList(const AClass: TClass;
  const AOwner: IJCoreOPFPID): TJCoreObjectList;
var
  VPID: IJCoreOPFPID;
  VOID: TJCoreOPFOID;
  VCount: Integer;
  I: Integer;
begin
  AOwner.OID.WriteToDriver(Driver);
  VCount := Driver.ExecSQL(GenerateSelectOwnedListStatement(AClass));
  Result := TJCoreObjectList.Create(True);
  try
    for I := 1 to VCount do
    begin
      VOID := ReadOwnedOIDFromDriver;
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
    APID.AssignOID(CreateOID(''));
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

