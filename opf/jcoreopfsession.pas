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
  JCoreOPFDriver,
  JCoreOPFMetadata,
  JCoreOPFMapping;

type

  IJCoreOPFSessionManager = interface
    function FindMappingClass(const AClass: TClass): TJCoreOPFMappingClass;
  end;

  IJCoreOPFSession = interface(IInterface)
    procedure Dispose(const AEntity: TObject);
    procedure Dispose(const AClass: TClass; const AOIDArray: array of string);
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    procedure Store(const AObject: TObject);
  end;

  { TJCoreOPFSession }

  TJCoreOPFSession = class(TInterfacedObject, IJCoreOPFSession, IJCoreOPFMapper)
  private
    FDriver: TJCoreOPFDriver;
    FInTransactionPIDList: TJCoreOPFPIDList;
    FMappingListMap: TJCoreOPFMappingListMap;
    FMappingList: TJCoreOPFMappingList;
    FModel: TJCoreOPFModel;
    FSessionManager: IJCoreOPFSessionManager;
    function CreateMapping(const AMappingClass: TJCoreOPFMappingClass): TJCoreOPFMapping;
  protected
    procedure AddInTransactionPID(const APID: TJCoreOPFPID);
    function AcquireMapping(const AClass: TClass): TJCoreOPFMapping;
    function AcquireMappingList(const AClass: TClass): TJCoreOPFMappingList;
    procedure Commit; virtual;
    procedure DisposeFromDriver(const AMetadata: TJCoreOPFClassMetadata; const ADriverOID: TJCoreOPFDriver; const ACount: Integer);
    function FindMapping(const AClass: TClass): TJCoreOPFMapping;
    procedure RetrieveElement(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMEntity);
    procedure RetrieveElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    procedure RetrieveLazyFromDriver(const AClass: TClass; const ADriver: TJCoreOPFDriver; const ALazyADM: TJCoreOPFADMEntity);
    procedure StoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure StorePID(const APID: TJCoreOPFPID);
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
    property InTransactionPIDList: TJCoreOPFPIDList read FInTransactionPIDList;
    property MappingListMap: TJCoreOPFMappingListMap read FMappingListMap;
    property Model: TJCoreOPFModel read FModel;
    property SessionManager: IJCoreOPFSessionManager read FSessionManager;
  public
    constructor Create(const ASessionManager: IJCoreOPFSessionManager; const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver);
    destructor Destroy; override;
    procedure Dispose(const AEntity: TObject);
    procedure Dispose(const AClass: TClass; const AOIDArray: array of string);
    function Retrieve(const AClass: TClass; const AOID: string): TObject;
    procedure Store(const AEntity: TObject);
  end;

implementation

uses
  sysutils,
  JCoreOPFException;

{ TJCoreOPFSession }

function TJCoreOPFSession.CreateMapping(const AMappingClass: TJCoreOPFMappingClass): TJCoreOPFMapping;
begin
  Result := AMappingClass.Create(Self, Model, FDriver);
  try
    FMappingList.Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TJCoreOPFSession.AddInTransactionPID(const APID: TJCoreOPFPID);
begin
  if InTransactionPIDList.IndexOf(APID) = -1 then
    InTransactionPIDList.Add(APID);
end;

function TJCoreOPFSession.AcquireMapping(const AClass: TClass): TJCoreOPFMapping;
begin
  Result := FindMapping(AClass);
  if not Assigned(Result) then
    raise EJCoreOPFMappingNotFound.Create(AClass.ClassName);
end;

function TJCoreOPFSession.AcquireMappingList(
  const AClass: TClass): TJCoreOPFMappingList;
var
  VMappingList: TJCoreOPFMappingList;
  VMapping: TJCoreOPFMapping;
  VClass: TClass;
  VIndex: Integer;
begin
  { TODO : Cache class<->mapping classes in the Config instance }
  VIndex := MappingListMap.IndexOf(AClass);
  if VIndex = -1 then
  begin
    VMappingList := TJCoreOPFMappingList.Create(False);
    try
      VClass := AClass;
      while VClass <> TObject do
      begin
        VMapping := FindMapping(VClass);
        if Assigned(VMapping) and (VMappingList.IndexOf(VMapping) = -1) then
          VMappingList.Insert(0, VMapping);
        VClass := VClass.ClassParent;
      end;
      if VMappingList.Count = 0 then
        raise EJCoreOPFMappingNotFound.Create(AClass.ClassName);
      VIndex := MappingListMap.Add(AClass, VMappingList);
    except
      FreeAndNil(VMappingList);
      raise;
    end;
  end;
  Result := MappingListMap.Data[VIndex];
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

procedure TJCoreOPFSession.DisposeFromDriver(const AMetadata: TJCoreOPFClassMetadata;
  const ADriverOID: TJCoreOPFDriver; const ACount: Integer);
var
  VMapping: TJCoreOPFMapping;
begin
  for VMapping in AcquireMappingList(AMetadata.TheClass) do
    VMapping.DisposeFromDriver(AMetadata, ADriverOID, ACount);
end;

function TJCoreOPFSession.FindMapping(const AClass: TClass): TJCoreOPFMapping;
var
  VMappingClass: TJCoreOPFMappingClass;
begin
  { TODO : several classes per mapping instance }
  { TODO :
      ambiguity (choose the subclass)
      apply override (user mapping for one class, default mapping for other classes)
      abstract registry implementations and analyzer (dic?) }
  for Result in FMappingList do
    if Result.Apply(AClass) then
      Exit;
  VMappingClass := SessionManager.FindMappingClass(AClass);
  if Assigned(VMappingClass) then
    Result := CreateMapping(VMappingClass)
  else
    Result := nil;
end;

procedure TJCoreOPFSession.RetrieveElement(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMEntity);
var
  VMapping: TJCoreOPFMapping;
begin
  for VMapping in AcquireMappingList(AOwnerADM.Metadata.CompositionClass) do
    VMapping.RetrieveElement(AOwnerPID, AOwnerADM);
end;

procedure TJCoreOPFSession.RetrieveElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
var
  VMapping: TJCoreOPFMapping;
begin
  for VMapping in AcquireMappingList(AOwnerADM.Metadata.CompositionClass) do
    VMapping.RetrieveElements(AOwnerPID, AOwnerADM);
end;

function TJCoreOPFSession.RetrieveFromDriver(const AClass: TClass;
  const ADriverOID: TJCoreOPFDriver): TObject;
begin
  Result := AcquireMapping(AClass).RetrieveFromDriver(AClass, ADriverOID);
end;

procedure TJCoreOPFSession.RetrieveLazyFromDriver(const AClass: TClass;
  const ADriver: TJCoreOPFDriver; const ALazyADM: TJCoreOPFADMEntity);
begin
  AcquireMapping(AClass).RetrieveLazyFromDriver(AClass, ADriver, ALazyADM);
end;

procedure TJCoreOPFSession.StoreElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
var
  VMapping: TJCoreOPFMapping;
begin
  for VMapping in AcquireMappingList(AOwnerADM.Metadata.CompositionClass) do
    VMapping.StoreElements(AOwnerPID, AOwnerADM);
end;

procedure TJCoreOPFSession.StorePID(const APID: TJCoreOPFPID);
var
  VMapping: TJCoreOPFMapping;
begin
  for VMapping in AcquireMappingList(APID.Entity.ClassType) do
    VMapping.Store(APID);
end;

procedure TJCoreOPFSession.StoreToDriver(const AClass: TClass;
  const AEntity: TObject; const ADriver: TJCoreOPFDriver);
var
  VMappingList: TJCoreOPFMappingList;
  VMapping: TJCoreOPFMapping;
begin
  if Assigned(AEntity) then
    VMappingList := AcquireMappingList(AEntity.ClassType)
  else
    VMappingList := AcquireMappingList(AClass);
  for VMapping in VMappingList do
    VMapping.StoreToDriver(AClass, AEntity, ADriver);
end;

constructor TJCoreOPFSession.Create(const ASessionManager: IJCoreOPFSessionManager;
  const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver);
begin
  inherited Create;
  FSessionManager := ASessionManager;
  FModel := AModel;
  FDriver := ADriver;
  FMappingListMap := TJCoreOPFMappingListMap.Create;
  FMappingList := TJCoreOPFMappingList.Create(True);
  FInTransactionPIDList := TJCoreOPFPIDList.Create(False);
end;

destructor TJCoreOPFSession.Destroy;
var
  VData: TJCoreOPFMappingList;
  I: Integer;
begin
  FreeAndNil(FDriver);
  for I := 0 to Pred(FMappingListMap.Count) do
  begin
    VData := FMappingListMap.Data[I];
    FMappingListMap.Data[I] := nil;
    VData.Free;
  end;
  FreeAndNil(FMappingListMap);
  FreeAndNil(FMappingList);
  FreeAndNil(FInTransactionPIDList);
  inherited Destroy;
end;

procedure TJCoreOPFSession.Dispose(const AEntity: TObject);
var
  VMappingList: TJCoreOPFMappingList;
  VMapping: TJCoreOPFMapping;
  VPID: TJCoreOPFPID;
begin
  VMappingList := AcquireMappingList(AEntity.ClassType);
  VPID := VMappingList.Last.AcquirePID(AEntity);
  for VMapping in VMappingList do
    VMapping.Dispose(VPID);
end;

procedure TJCoreOPFSession.Dispose(const AClass: TClass; const AOIDArray: array of string);
var
  VMapping: TJCoreOPFMapping;
begin
  for VMapping in AcquireMappingList(AClass) do
    VMapping.DisposeFromString(AClass, AOIDArray);
end;

function TJCoreOPFSession.Retrieve(const AClass: TClass; const AOID: string): TObject;
begin
  Result := AcquireMapping(AClass).RetrieveFromString(AClass, AOID);
  Commit;
end;

procedure TJCoreOPFSession.Store(const AEntity: TObject);
var
  VMappingList: TJCoreOPFMappingList;
  VMapping: TJCoreOPFMapping;
  VPID: TJCoreOPFPID;
begin
  VMappingList := AcquireMappingList(AEntity.ClassType);
  VPID := VMappingList.Last.AcquirePID(AEntity);
  for VMapping in VMappingList do
    VMapping.Store(VPID);
  Commit;
end;

end.

