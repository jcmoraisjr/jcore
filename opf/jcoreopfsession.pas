(*
  JCore, OPF Session Interface
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFSession;

{$I jcore.inc}

interface

uses
  Classes,
  JCoreClasses,
  JCoreOPFID,
  JCoreOPFDriver,
  JCoreOPFMapping;

type

  IJCoreOPFSessionManager = interface
    function GetMappingClassList: TJCoreOPFMappingClassList;
    property MappingClassList: TJCoreOPFMappingClassList read GetMappingClassList;
  end;

  IJCoreOPFSession = interface(IInterface)
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    procedure Store(const AObject: TObject);
  end;

  { TJCoreOPFSession }

  TJCoreOPFSession = class(TInterfacedObject, IJCoreOPFSession, IJCoreOPFMapper)
  private
    FDriver: TJCoreOPFDriver;
    FInTransactionPIDList: TInterfaceList;
    FMappingList: TJCoreOPFMappingList;
    FSessionManager: IJCoreOPFSessionManager;
    function CreateMapping(const AMappingClass: TJCoreOPFMappingClass): TJCoreOPFMapping;
  protected
    procedure AddInTransactionPID(const APID: IJCoreOPFPID);
    function AcquireMapping(const AClass: TClass): TJCoreOPFMapping;
    procedure Commit;
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    function RetrieveListPID(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
    procedure StorePID(const APID: IJCoreOPFPID);
    procedure StoreListPID(const AListBaseClass: TClass; const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray; const ALinkType: TJCoreOPFLinkType);
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
    property InTransactionPIDList: TInterfaceList read FInTransactionPIDList;
    property SessionManager: IJCoreOPFSessionManager read FSessionManager;
  public
    constructor Create(const ASessionManager: IJCoreOPFSessionManager; const ADriver: TJCoreOPFDriver);
    destructor Destroy; override;
    function AcquirePID(AEntity: TObject; const AAddToInTransactionPIDList: Boolean = True): IJCoreOPFPID;
    procedure Dispose(const AEntity: TObject);
    procedure Dispose(const AClass: TClass; const AOID: string);
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    procedure Store(const AEntity: TObject);
  end;

implementation

uses
  sysutils,
  typinfo,
  JCoreOPFConsts,
  JCoreOPFException,
  JCoreOPFPID;

{ TJCoreOPFSession }

function TJCoreOPFSession.CreateMapping(const AMappingClass: TJCoreOPFMappingClass): TJCoreOPFMapping;
begin
  Result := AMappingClass.Create(Self, FDriver);
  try
    FMappingList.Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TJCoreOPFSession.AddInTransactionPID(const APID: IJCoreOPFPID);
begin
  InTransactionPIDList.Add(APID);
end;

function TJCoreOPFSession.AcquireMapping(const AClass: TClass): TJCoreOPFMapping;
var
  VMappingClass: TJCoreOPFMappingClass;
begin
  for Result in FMappingList do
    if Result.Apply(AClass) then
      Exit;
  for VMappingClass in SessionManager.MappingClassList do
    if VMappingClass.Apply(AClass) then
    begin
      Result := CreateMapping(VMappingClass);
      Exit;
    end;
  raise EJCoreOPFMappingNotFound.Create(AClass.ClassName);
end;

procedure TJCoreOPFSession.Commit;
var
  I: Integer;
begin
  { TODO : Implement failure check }
  for I := 0 to Pred(InTransactionPIDList.Count) do
    IJCoreOPFPID(InTransactionPIDList[I]).Commit;
  InTransactionPIDList.Clear;
end;

function TJCoreOPFSession.RetrieveFromDriver(const AClass: TClass;
  const ADriverOID: TJCoreOPFDriver): TObject;
begin
  Result := AcquireMapping(AClass).RetrieveFromDriver(AClass, ADriverOID);
end;

function TJCoreOPFSession.RetrieveListPID(const AListBaseClass: TClass;
  const AOwnerPID: IJCoreOPFPID): TJCoreObjectList;
begin
  Result := AcquireMapping(AListBaseClass).RetrieveList(AListBaseClass, AOwnerPID);
end;

procedure TJCoreOPFSession.StorePID(const APID: IJCoreOPFPID);
begin
  AcquireMapping(APID.Entity.ClassType).Store(APID);
end;

procedure TJCoreOPFSession.StoreListPID(const AListBaseClass: TClass;
  const AOwnerPID: IJCoreOPFPID; const APIDArray: TJCoreOPFPIDArray;
  const ALinkType: TJCoreOPFLinkType);
begin
  if Assigned(APIDArray) then
    AcquireMapping(AListBaseClass).StoreList(AOwnerPID, APIDArray, ALinkType);
end;

procedure TJCoreOPFSession.StoreToDriver(const AClass: TClass;
  const AEntity: TObject; const ADriver: TJCoreOPFDriver);
var
  VMapping: TJCoreOPFMapping;
begin
  if Assigned(AEntity) then
    VMapping := AcquireMapping(AEntity.ClassType)
  else
    VMapping := AcquireMapping(AClass);
  VMapping.StoreToDriver(AClass, AEntity, ADriver);
end;

constructor TJCoreOPFSession.Create(const ASessionManager: IJCoreOPFSessionManager; const ADriver: TJCoreOPFDriver);
begin
  inherited Create;
  FSessionManager := ASessionManager;
  FDriver := ADriver;
  FMappingList := TJCoreOPFMappingList.Create(True);
  FInTransactionPIDList := TInterfaceList.Create;
end;

destructor TJCoreOPFSession.Destroy;
begin
  FreeAndNil(FDriver);
  FreeAndNil(FMappingList);
  FreeAndNil(FInTransactionPIDList);
  inherited Destroy;
end;

function TJCoreOPFSession.AcquirePID(AEntity: TObject;
  const AAddToInTransactionPIDList: Boolean): IJCoreOPFPID;
var
  VPropInfo: PPropInfo;
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  VPropInfo := GetPropInfo(AEntity, SPID);
  if not Assigned(VPropInfo) then
    raise EJCoreOPFPersistentIDFieldNotFound.Create(AEntity.ClassName);
  Result := GetInterfaceProp(AEntity, VPropInfo) as IJCoreOPFPID;
  if not Assigned(Result) then
  begin
    Result := TJCoreOPFPID.Create(AEntity);
    SetInterfaceProp(AEntity, VPropInfo, Result);
  end;
  { TODO : Check duplications and avoid useless calls }
  if AAddToInTransactionPIDList then
    AddInTransactionPID(Result);
end;

procedure TJCoreOPFSession.Dispose(const AEntity: TObject);
begin
  AcquireMapping(AEntity.ClassType).Dispose(AcquirePID(AEntity));
end;

procedure TJCoreOPFSession.Dispose(const AClass: TClass; const AOID: string);
begin
  AcquireMapping(AClass).DisposeFromString(AClass, AOID);
end;

function TJCoreOPFSession.Retrieve(const AClass: TClass; const AOID: string): TObject;
begin
  Result := AcquireMapping(AClass).RetrieveFromString(AClass, AOID);
  Commit;
end;

procedure TJCoreOPFSession.Store(const AEntity: TObject);
begin
  { TODO : User defined PID class }
  StorePID(AcquirePID(AEntity));
  Commit;
end;

end.

