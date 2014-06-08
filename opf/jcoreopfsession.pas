(*
  JCore, OPF Session Class
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
  JCoreEntity,
  JCoreOPFDriver,
  JCoreOPFMetadata,
  JCoreOPFMapping;

type

  { IJCoreOPFSessionManager }

  IJCoreOPFSessionManager = interface
    function MappingClassFactory: TJCoreOPFMappingClassFactory;
    function Model: TJCoreOPFModel;
  end;

  { IJCoreOPFSession }

  IJCoreOPFSession = interface(IInterface)
    procedure Dispose(const AEntity: TObject);
    procedure Dispose(const AClass: TClass; const AOIDArray: array of string);
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    procedure Store(const AObject: TObject);
  end;

  { TJCoreOPFSession }

  TJCoreOPFSession = class(TInterfacedObject, IJCoreOPFSession, IJCoreOPFMapper, IJCoreOPFPIDManager)
  private
    FDriver: TJCoreOPFDriver;
    FInTransactionPIDList: TJCoreOPFPIDList;
    FMappingFactory: TJCoreOPFMappingFactory;
    FMappingMap: TJCoreOPFClassMappingMap;
    FModel: TJCoreOPFModel;
    FSessionManager: IJCoreOPFSessionManager;
    function AcquirePIDFromIntfProp(const AEntity: TObject): TJCoreOPFPID;
    function AcquirePIDFromProxyField(const AEntity: TObject): TJCoreOPFPID;
    function CreateClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
  protected
    function AcquireClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
    procedure AddInTransactionPID(const APID: TJCoreOPFPID);
    procedure Commit; virtual;
    function CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
    function CreateProxy(const APID: TJCoreOPFPID): TJCoreEntityProxy; virtual;
    function IGetDriver: TJCoreOPFDriver;
    function IGetModel: TJCoreOPFModel;
    procedure LoadEntity(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMEntity);
    procedure LoadCollection(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    function PIDClass: TJCoreOPFPIDClass; virtual;
    function IJCoreOPFMapper.Driver = IGetDriver;
    function IJCoreOPFMapper.Model = IGetModel;
    property Driver: TJCoreOPFDriver read FDriver;
    property InTransactionPIDList: TJCoreOPFPIDList read FInTransactionPIDList;
    property MappingFactory: TJCoreOPFMappingFactory read FMappingFactory;
    property MappingMap: TJCoreOPFClassMappingMap read FMappingMap;
    property Model: TJCoreOPFModel read FModel;
    property SessionManager: IJCoreOPFSessionManager read FSessionManager;
  public
    constructor Create(const ASessionManager: IJCoreOPFSessionManager; const ADriver: TJCoreOPFDriver);
    destructor Destroy; override;
    function AcquirePID(const AEntity: TObject): TJCoreOPFPID;
    procedure Dispose(const AEntity: TObject);
    procedure Dispose(const AClass: TClass; const AStringOIDArray: array of string);
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    procedure Store(const AEntity: TObject);
  end;

implementation

uses
  sysutils,
  typinfo,
  JCoreOPFConsts,
  JCoreOPFException;

{ TJCoreOPFSession }

function TJCoreOPFSession.AcquirePIDFromIntfProp(const AEntity: TObject): TJCoreOPFPID;
var
  VPropInfo: PPropInfo;
begin
  VPropInfo := GetPropInfo(AEntity, SPID);
  if Assigned(VPropInfo) and (VPropInfo^.PropType^.Kind = tkInterface) then
  begin
    Result := GetInterfaceProp(AEntity, VPropInfo) as TJCoreOPFPID;
    if not Assigned(Result) then
    begin
      Result := PIDClass.Create(Self, AEntity, Model.AcquireMetadata(AEntity.ClassType));
      SetInterfaceProp(AEntity, VPropInfo, Result as IJCorePID);
    end;
  end else
    Result := nil;
end;

function TJCoreOPFSession.AcquirePIDFromProxyField(const AEntity: TObject): TJCoreOPFPID;
var
  VFieldAddr: Pointer;
  VProxy: TJCoreEntityProxy;
  VMetadata: TJCoreOPFClassMetadata;
begin
  VFieldAddr := AEntity.FieldAddress(SProxy);
  if Assigned(VFieldAddr) then
  begin
    VProxy := TObject(VFieldAddr^) as TJCoreEntityProxy;
    if not Assigned(VProxy) then
    begin
      VMetadata := Model.AcquireMetadata(AEntity.ClassType);
      Result := TJCoreOPFPID(PIDClass.NewInstance);
      VProxy := CreateProxy(Result);
      TJCoreEntityProxy(VFieldAddr^) := VProxy;
      Result.Create(Self, AEntity, VMetadata);
    end else
      Result := VProxy.PID as TJCoreOPFPID;
  end else
    Result := nil;
end;

function TJCoreOPFSession.CreateClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
var
  VMetadata: TJCoreOPFClassMetadata;
  VMappingList: TJCoreOPFMappingList;
begin
  VMetadata := Model.AcquireMetadata(AClass);
  VMappingList := MappingFactory.AcquireMappingList(AClass);
  Result := TJCoreOPFClassMapping.Create(Self, VMetadata, VMappingList);
end;

function TJCoreOPFSession.AcquireClassMapping(const AClass: TClass): TJCoreOPFClassMapping;
var
  VIndex: Integer;
begin
  VIndex := MappingMap.IndexOf(AClass);
  if VIndex = -1 then
    VIndex := MappingMap.Add(AClass, CreateClassMapping(AClass));
  Result := MappingMap.Data[VIndex];
end;

procedure TJCoreOPFSession.AddInTransactionPID(const APID: TJCoreOPFPID);
begin
  if InTransactionPIDList.IndexOf(APID) = -1 then
    InTransactionPIDList.Add(APID);
end;

procedure TJCoreOPFSession.Commit;
var
  I: Integer;
begin
  { TODO : Implement failure check }
  for I := 0 to Pred(InTransactionPIDList.Count) do
    InTransactionPIDList[I].Commit;
  InTransactionPIDList.Clear;
end;

function TJCoreOPFSession.CreatePIDArray(const AItems: TJCoreObjectArray): TJCoreOPFPIDArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AItems));
  for I := Low(Result) to High(Result) do
    Result[I] := AcquirePID(AItems[I]);
end;

function TJCoreOPFSession.CreateProxy(const APID: TJCoreOPFPID): TJCoreEntityProxy;
begin
  Result := TJCoreEntityProxy.Create(APID);
end;

function TJCoreOPFSession.IGetDriver: TJCoreOPFDriver;
begin
  Result := FDriver;
end;

function TJCoreOPFSession.IGetModel: TJCoreOPFModel;
begin
  Result := FModel;
end;

procedure TJCoreOPFSession.LoadCollection(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  AcquireClassMapping(AOwnerADM.Metadata.CompositionClass).RetrieveListInternal(AOwnerPID, AOwnerADM);
end;

procedure TJCoreOPFSession.LoadEntity(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMEntity);
begin
  AcquireClassMapping(AOwnerADM.Metadata.CompositionClass).RetrieveEntityInternal(AOwnerPID, AOwnerADM);
end;

function TJCoreOPFSession.PIDClass: TJCoreOPFPIDClass;
begin
  Result := TJCoreOPFPID;
end;

constructor TJCoreOPFSession.Create(const ASessionManager: IJCoreOPFSessionManager;
  const ADriver: TJCoreOPFDriver);
begin
  inherited Create;
  FSessionManager := ASessionManager;
  FDriver := ADriver;
  FMappingFactory := TJCoreOPFMappingFactory.Create(Self, SessionManager.MappingClassFactory, ADriver);
  FMappingMap := TJCoreOPFClassMappingMap.Create;
  FModel := SessionManager.Model;
  FInTransactionPIDList := TJCoreOPFPIDList.Create(False);
end;

destructor TJCoreOPFSession.Destroy;
var
  VData: TJCoreOPFClassMapping;
  I: Integer;
begin
  FreeAndNil(FDriver);
  FreeAndNil(FInTransactionPIDList);
  FreeAndNil(FMappingFactory);
  for I := 0 to Pred(FMappingMap.Count) do
  begin
    VData := FMappingMap.Data[I];
    FMappingMap.Data[I] := nil;
    VData.Free;
  end;
  FreeAndNil(FMappingMap);
  inherited Destroy;
end;

function TJCoreOPFSession.AcquirePID(const AEntity: TObject): TJCoreOPFPID;
begin
  if not Assigned(AEntity) then
    raise EJCoreNilPointerException.Create;
  Result := AcquirePIDFromIntfProp(AEntity);
  if not Assigned(Result) then
    Result := AcquirePIDFromProxyField(AEntity);
  if not Assigned(Result) then
    raise EJCoreOPFPersistentIDNotFound.Create(AEntity.ClassName);
end;

procedure TJCoreOPFSession.Dispose(const AEntity: TObject);
begin
  AcquireClassMapping(AEntity.ClassType).Dispose(AcquirePID(AEntity));
  Commit;
end;

procedure TJCoreOPFSession.Dispose(const AClass: TClass; const AStringOIDArray: array of string);
begin
  AcquireClassMapping(AClass).Dispose(AClass, AStringOIDArray);
  Commit;
end;

function TJCoreOPFSession.Retrieve(const AClass: TClass; const AOID: string): TObject;
begin
  Result := AcquireClassMapping(AClass).Retrieve(AOID);
  Commit;
end;

procedure TJCoreOPFSession.Store(const AEntity: TObject);
begin
  AcquireClassMapping(AEntity.ClassType).Store(AcquirePID(AEntity));
  Commit;
end;

end.

