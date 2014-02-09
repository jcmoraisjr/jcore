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
  JCoreOPFMetadata,
  JCoreOPFID,
  JCoreOPFDriver,
  JCoreOPFMapping;

type

  IJCoreOPFSessionManager = interface
    function FindMappingClass(const AClass: TClass): TJCoreOPFMappingClass;
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
    FModel: TJCoreOPFModel;
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
    property Model: TJCoreOPFModel read FModel;
    property SessionManager: IJCoreOPFSessionManager read FSessionManager;
  public
    constructor Create(const ASessionManager: IJCoreOPFSessionManager; const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver);
    destructor Destroy; override;
    procedure Dispose(const AEntity: TObject);
    procedure Dispose(const AClass: TClass; const AOID: string);
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
  VMappingClass := SessionManager.FindMappingClass(AClass);
  if not Assigned(VMappingClass) then
    raise EJCoreOPFMappingNotFound.Create(AClass.ClassName);
  Result := CreateMapping(VMappingClass);
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

constructor TJCoreOPFSession.Create(const ASessionManager: IJCoreOPFSessionManager;
  const AModel: TJCoreOPFModel; const ADriver: TJCoreOPFDriver);
begin
  inherited Create;
  FSessionManager := ASessionManager;
  FModel := AModel;
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

procedure TJCoreOPFSession.Dispose(const AEntity: TObject);
var
  VMapping: TJCoreOPFMapping;
begin
  VMapping := AcquireMapping(AEntity.ClassType);
  VMapping.Dispose(VMapping.AcquirePID(AEntity));
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
var
  VMapping: TJCoreOPFMapping;
begin
  VMapping := AcquireMapping(AEntity.ClassType);
  VMapping.Store(VMapping.AcquirePID(AEntity));
  Commit;
end;

end.

