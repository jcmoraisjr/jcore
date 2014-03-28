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
    FMappingList: TJCoreOPFMappingList;
    FModel: TJCoreOPFModel;
    FSessionManager: IJCoreOPFSessionManager;
    function CreateMapping(const AMappingClass: TJCoreOPFMappingClass): TJCoreOPFMapping;
  protected
    procedure AddInTransactionPID(const APID: TJCoreOPFPID);
    function AcquireMapping(const AClass: TClass): TJCoreOPFMapping;
    procedure Commit; virtual;
    procedure DisposeFromDriver(const AMetadata: TJCoreOPFClassMetadata; const ADriverOID: TJCoreOPFDriver; const ACount: Integer);
    procedure RetrieveElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    function RetrieveFromDriver(const AClass: TClass; const ADriverOID: TJCoreOPFDriver): TObject;
    procedure StoreElements(const AOwnerPID: TJCoreOPFPID; const AOwnerADM: TJCoreOPFADMCollection);
    procedure StorePID(const APID: TJCoreOPFPID);
    procedure StoreToDriver(const AClass: TClass; const AEntity: TObject; const ADriver: TJCoreOPFDriver);
    property InTransactionPIDList: TJCoreOPFPIDList read FInTransactionPIDList;
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
var
  VMappingClass: TJCoreOPFMappingClass;
begin
  for Result in FMappingList do
    if Result.Apply(AClass) then
      Exit;
  { TODO : several classes per mapping instance }
  { TODO :
      ambiguity (choose the subclass)
      apply override (user mapping for one class, default mapping for other classes)
      abstract registry implementations and analyzer (dic?) }
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
    InTransactionPIDList[I].Commit;
  InTransactionPIDList.Clear;
end;

procedure TJCoreOPFSession.DisposeFromDriver(const AMetadata: TJCoreOPFClassMetadata;
  const ADriverOID: TJCoreOPFDriver; const ACount: Integer);
begin
  AcquireMapping(AMetadata.TheClass).DisposeFromDriver(AMetadata, ADriverOID, ACount);
end;

procedure TJCoreOPFSession.RetrieveElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  AcquireMapping(AOwnerADM.Metadata.CompositionClass).RetrieveElements(AOwnerPID, AOwnerADM);
end;

function TJCoreOPFSession.RetrieveFromDriver(const AClass: TClass;
  const ADriverOID: TJCoreOPFDriver): TObject;
begin
  Result := AcquireMapping(AClass).RetrieveFromDriver(AClass, ADriverOID);
end;

procedure TJCoreOPFSession.StoreElements(const AOwnerPID: TJCoreOPFPID;
  const AOwnerADM: TJCoreOPFADMCollection);
begin
  AcquireMapping(AOwnerADM.Metadata.CompositionClass).StoreElements(AOwnerPID, AOwnerADM);
end;

procedure TJCoreOPFSession.StorePID(const APID: TJCoreOPFPID);
begin
  AcquireMapping(APID.Entity.ClassType).Store(APID);
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
  FInTransactionPIDList := TJCoreOPFPIDList.Create(False);
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

procedure TJCoreOPFSession.Dispose(const AClass: TClass; const AOIDArray: array of string);
begin
  AcquireMapping(AClass).DisposeFromString(AClass, AOIDArray);
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

