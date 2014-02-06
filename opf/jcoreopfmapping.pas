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
  typinfo,
  fgl,
  JCoreClasses,
  JCoreOPFDriver,
  JCoreOPFID,
  JCoreOPFPID,
  JCoreOPFADM;

type

  TJCoreOPFLinkType = (jltEmbedded, jltExternal);

  { IJCoreOPFMapper }

  IJCoreOPFMapper = interface
    procedure AddInTransactionPID(const APID: IJCoreOPFPID);
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveListPID(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
    procedure StorePID(const APID: IJCoreOPFPID);
    procedure StoreListPID(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray; const ALinkType: TJCoreOPFLinkType);
  end;

  { TJCoreOPFMapping }

  TJCoreOPFMapping = class(TObject, IJCoreOPFPIDManager)
  private
    FDriver: TJCoreOPFDriver;
    FMapper: IJCoreOPFMapper;
    FADMClassArray: TJCoreOPFADMClassArray;
  protected
    function AcquireADMClass(const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
    function CreateADMClassArray: TJCoreOPFADMClassArray; virtual;
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
    function AcquirePID(AEntity: TObject; const AAddToInTransactionPIDList: Boolean = True): IJCoreOPFPID;
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
    function GenerateDeleteSharedItemStatement(const AListBaseClass: TClass): string; virtual;
    function GenerateInsertSharedItemStatement(const AListBaseClass: TClass): string; virtual;
    function GenerateInsertStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    function GenerateSelectListFromStatement(const AListBaseClass: TClass): string; virtual;
    function GenerateSelectStatement(const AClass: TClass): string; virtual; abstract;
    function GenerateUpdateStatement(const APID: IJCoreOPFPID): string; virtual; abstract;
    procedure ReadFromDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteExternalsToDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteInternalsToDriver(const APID: IJCoreOPFPID); virtual;
    procedure WriteSharedListItemToDriver(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID; const ASharedPIDArray: TJCoreOPFPIDArray); virtual;
  protected
    function BuildParams(const ASize: Integer): string;
    function CreatePIDArray(const AList: TFPSList): TJCoreOPFPIDArray;
    procedure InternalDispose(const AClass: TClass; const AOIDArray: array of TJCoreOPFOID); override;
    function InternalRetrieve(const AClass: TClass; const AOID: TJCoreOPFOID): TObject; override;
    function InternalRetrieveList(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID): TJCoreObjectList; override;
    procedure InternalStore(const APID: IJCoreOPFPID); override;
    procedure InternalStoreOwnedList(const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); override;
    procedure InternalStoreSharedList(const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray); override;
  protected
    function RetrieveListPID(const AListBaseClass: TClass; AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
    procedure StoreListPID(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray; const ALinkType: TJCoreOPFLinkType);
    property Driver: TJCoreOPFSQLDriver read FSQLDriver;
  public
    constructor Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver); override;
  end;

implementation

uses
  sysutils,
  JCoreOPFConsts,
  JCoreOPFException;

{ TJCoreOPFMapping }

function TJCoreOPFMapping.AcquireADMClass(
  const AAttrTypeInfo: PTypeInfo): TJCoreOPFADMClass;
begin
  for Result in FADMClassArray do
    if Result.Apply(AAttrTypeInfo) then
      Exit;
  raise EJCoreOPFUnsupportedAttributeType.Create(AAttrTypeInfo);
end;

function TJCoreOPFMapping.CreateADMClassArray: TJCoreOPFADMClassArray;
begin
  { TODO : could be better }
  SetLength(Result, 5);
  Result[0] := TJCoreOPFADMType32;
  Result[1] := TJCoreOPFADMType64;
  Result[2] := TJCoreOPFADMFloat;
  Result[3] := TJCoreOPFADMAnsiString;
  Result[4] := TJCoreOPFADMCollection;
end;

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
  FADMClassArray := CreateADMClassArray;
end;

function TJCoreOPFMapping.AcquirePID(AEntity: TObject;
  const AAddToInTransactionPIDList: Boolean): IJCoreOPFPID;
var
  VPropInfo: PPropInfo;
begin
  { TODO : User defined PID class }
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  VPropInfo := GetPropInfo(AEntity, SPID);
  if not Assigned(VPropInfo) then
    raise EJCoreOPFPersistentIDFieldNotFound.Create(AEntity.ClassName);
  Result := GetInterfaceProp(AEntity, VPropInfo) as IJCoreOPFPID;
  if not Assigned(Result) then
  begin
    Result := TJCoreOPFPID.Create(Self, AEntity);
    SetInterfaceProp(AEntity, VPropInfo, Result);
  end;
  { TODO : Check duplications and avoid useless calls }
  if AAddToInTransactionPIDList then
    Mapper.AddInTransactionPID(Result);
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
    VPID := AcquirePID(AEntity);
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
  const AListBaseClass: TClass): string;
begin
  raise EJCoreOPFUnsupportedListOperations.Create;
end;

function TJCoreOPFSQLMapping.GenerateInsertSharedItemStatement(
  const AListBaseClass: TClass): string;
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

procedure TJCoreOPFSQLMapping.WriteSharedListItemToDriver(
  const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID;
  const ASharedPIDArray: TJCoreOPFPIDArray);
var
  VInsertStatement: string;
  I: Integer;
begin
  VInsertStatement := GenerateInsertSharedItemStatement(AListBaseClass);
  for I := Low(ASharedPIDArray) to High(ASharedPIDArray) do
  begin
    AOwnerPID.OID.WriteToDriver(Driver);
    ASharedPIDArray[I].OID.WriteToDriver(Driver);
    Driver.ExecSQL(VInsertStatement);
  end;
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
      Result[I] := AcquirePID(VEntity);
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
    VPID := AcquirePID(Result);
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
        VPID := AcquirePID(Result.Last);
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
  for I := Low(APIDArray) to High(APIDArray) do
    if not APIDArray[I].IsPersistent then
    begin
      if Apply(APIDArray[I].Entity.ClassType) then
        Store(APIDArray[I])
      else
        Mapper.StorePID(APIDArray[I]);
    end;
end;

function TJCoreOPFSQLMapping.RetrieveListPID(const AListBaseClass: TClass;
  AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
begin
  Result := Mapper.RetrieveListPID(AListBaseClass, AOwnerPID);
end;

procedure TJCoreOPFSQLMapping.StoreListPID(const AListBaseClass: TClass;
  const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray;
  const ALinkType: TJCoreOPFLinkType);
begin
  if (ALinkType = jltExternal) and AOwnerPID.IsPersistent then
  begin
    { TODO : Implement change analyzer }
    AOwnerPID.OID.WriteToDriver(Driver);
    Driver.ExecSQL(GenerateDeleteSharedItemStatement(AListBaseClass));
  end;
  Mapper.StoreListPID(AListBaseClass, AOwnerPID, APIDArray, ALinkType);
  if ALinkType = jltExternal then
    WriteSharedListItemToDriver(AListBaseClass, AOwnerPID, APIDArray);
end;

constructor TJCoreOPFSQLMapping.Create(const AMapper: IJCoreOPFMapper; const ADriver: TJCoreOPFDriver);
begin
  if not (ADriver is TJCoreOPFSQLDriver) then
    raise EJCoreOPFUnsupportedDriver.Create(ADriver.ClassName);
  inherited Create(AMapper, ADriver);
  FSQLDriver := TJCoreOPFSQLDriver(ADriver);
end;

end.

