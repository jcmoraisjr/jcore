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

  TJCoreOPFLinkType = (jltEmbedded, jltExternal);

  { IJCoreOPFMapper }

  IJCoreOPFMapper = interface
    function AcquirePID(AEntity: TObject; const AAddToInTransactionPIDList: Boolean = True): IJCoreOPFPID;
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveListPID(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
    procedure StorePID(const APID: IJCoreOPFPID);
    procedure StoreListPID(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray; const ALinkType: TJCoreOPFLinkType);
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
    procedure InternalDispose(const AClass: TClass; const AOIDArray: array of TJCoreOPFOID); virtual; abstract;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; virtual; abstract;
    function InternalRetrieveList(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID): TJCoreObjectList; virtual; abstract;
    procedure InternalStore(const APID: IJCoreOPFPID); virtual; abstract;
    procedure InternalStoreOwnedList(const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); virtual; abstract;
    procedure InternalStoreSharedList(const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); virtual; abstract;
    property Driver: TJCoreOPFDriver read FDriver;
    property Mapper: IJCoreOPFMapper read FMapper;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver); virtual;
    class function Apply(const AClass: TClass): Boolean; virtual; abstract;
    procedure Dispose(const APID: IJCoreOPFPID);
    procedure DisposeFromString(const AClass: TClass; const AOID: string);
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveFromString(const AClass: TClass; const AStringOID: string): TObject;
    function RetrieveList(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
    procedure Store(const APID: IJCoreOPFPID);
    procedure StoreList(const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray; const ALinkType: TJCoreOPFLinkType);
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
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
    function GenerateSelectListFromStatement(const AListBaseClass: TClass): string; virtual;
    function GenerateSelectStatement(const AClass: TClass): string; virtual; abstract;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    procedure ReadFromDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteExternalsToDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteInternalsToDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteSharedListItemToDriver(const AOwnerPID, ASharedPID: IJCoreOPFPID); virtual;
  protected
    function BuildParams(const ASize: Integer): string;
    function CreatePIDArray(const AList: TFPSList): TJCoreOPFPIDArray;
    procedure InternalDispose(const AClass: TClass; const AOIDArray: array of TJCoreOPFOID); override;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; override;
    function InternalRetrieveList(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID): TJCoreObjectList; override;
    procedure InternalStore(const APID: IJCoreOPFPID); override;
    procedure InternalStoreOwnedList(const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); override;
    procedure InternalStoreSharedList(const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); override;
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

procedure TJCoreOPFMapping.Dispose(const APID: IJCoreOPFPID);
begin
  InternalDispose(APID.Entity.ClassType, [APID.OID]);
end;

procedure TJCoreOPFMapping.DisposeFromString(const AClass: TClass;
  const AOID: string);
var
  VOID: TJCoreOPFOID;
begin
  VOID := CreateOIDFromString(AOID);
  try
    InternalDispose(AClass, [VOID]);
  finally
    FreeAndNil(VOID);
  end;
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

function TJCoreOPFMapping.RetrieveList(const AListBaseClass: TClass;
  const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
begin
  Result := InternalRetrieveList(AListBaseClass, AOwnerPID);
end;

procedure TJCoreOPFMapping.Store(const APID: IJCoreOPFPID);
begin
  InternalStore(APID);
end;

procedure TJCoreOPFMapping.StoreList(const AOwnerPID: IJCoreOPFPID;
  const APIDArray: TJCoreOPFPIDArray; const ALinkType: TJCoreOPFLinkType);
begin
  case ALinkType of
    jltEmbedded: InternalStoreOwnedList(AOwnerPID, APIDArray);
    jltExternal: InternalStoreSharedList(AOwnerPID, APIDArray);
  end;
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

function TJCoreOPFSQLMapping.GenerateInsertSharedItemStatement(
  const AOwnerPID, ASharedPID: IJCoreOPFPID): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateSelectListFromStatement(
  const AListBaseClass: TClass): string;
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

function TJCoreOPFSQLMapping.BuildParams(const ASize: Integer): string;
var
  I: Integer;
begin
  if ASize > 1 then
  begin
    { TODO : allocate once, at the start }
    Result := ' IN (?';
    for I := 2 to ASize do
      Result := Result + ',?';
    Result := Result + ')';
  end else if ASize = 1 then
    Result := '=?'
  else
    Result := '';
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

procedure TJCoreOPFSQLMapping.InternalDispose(const AClass: TClass;
  const AOIDArray: array of TJCoreOPFOID);
begin
  { TODO : Implement }
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
    try
      VPID.AssignOID(AOID);
      ReadFromDriver(VPID);
    except
      VPID.ReleaseOID(AOID);
      raise;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TJCoreOPFSQLMapping.InternalRetrieveList(const AListBaseClass: TClass;
  const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
var
  VPID: IJCoreOPFPID;
  VOID: TJCoreOPFOID;
  VCount: Integer;
  I: Integer;
begin
  AOwnerPID.OID.WriteToDriver(Driver);
  VCount := Driver.ExecSQL(GenerateSelectListFromStatement(AListBaseClass));
  Result := TJCoreObjectList.Create(True);
  try
    for I := 1 to VCount do
    begin
      VOID := CreateOIDFromDriver(Driver);
      try
        Result.Add(CreateEntity(AListBaseClass));
        VPID := Mapper.AcquirePID(Result.Last);
        try
          VPID.AssignOID(VOID);
          ReadFromDriver(VPID);
        except
          VPID.ReleaseOID(VOID);
          raise;
        end;
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

procedure TJCoreOPFSQLMapping.InternalStoreOwnedList(const AOwnerPID: IJCoreOPFPID;
  const APIDArray: TJCoreOPFPIDArray);
var
  I: Integer;
begin
  for I := Low(APIDArray) to High(APIDArray) do
  begin
    APIDArray[I].Owner := AOwnerPID;
    if Apply(APIDArray[I].Entity.ClassType) then
      Store(APIDArray[I])
    else
      Mapper.StorePID(APIDArray[I]);
  end;
end;

procedure TJCoreOPFSQLMapping.InternalStoreSharedList(const AOwnerPID: IJCoreOPFPID;
  const APIDArray: TJCoreOPFPIDArray);
var
  I: Integer;
begin
  if AOwnerPID.IsPersistent then
  begin
    { TODO : Implement change analyzer }
    AOwnerPID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateDeleteSharedItemStatement(AOwnerPID));
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
    WriteSharedListItemToDriver(AOwnerPID, APIDArray[I]);
end;

constructor TJCoreOPFSQLMapping.Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver);
begin
  if not (ADriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPFUnsupportedDriver.Create(ADriver.ClassName);
  inherited Create(AMapper, ADriver);
  FSQLDriver := TJCoreOPFSQLDriver(ADriver);
end;

end.

