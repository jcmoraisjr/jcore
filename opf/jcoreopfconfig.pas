(*
  JCore, OPF Configuration Interface
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFConfig;

{$I jcore.inc}

interface

uses
  JCoreOPFMetadata,
  JCoreOPFDriver,
  JCoreOPFMapping,
  JCoreOPFSession;

type

  { IJCoreOPFConfiguration }

  IJCoreOPFConfiguration = interface(IInterface)
    procedure AddDriverClass(const ADriverClass: TJCoreOPFDriverClass);
    procedure AddMappingClass(const AMappingClass: TJCoreOPFMappingClass);
    function CreateSession: IJCoreOPFSession;
    function GetDriverName: string;
    procedure SetDriverName(AValue: string);
    property DriverName: string read GetDriverName write SetDriverName;
  end;

  { TJCoreOPFConfiguration }

  TJCoreOPFConfiguration = class(TInterfacedObject, IJCoreOPFConfiguration, IJCoreOPFSessionManager)
  private
    FDriverClass: TJCoreOPFDriverClass;
    FDriverClassList: TJCoreOPFDriverClassList;
    FDriverName: string;
    FHost: string;
    FMappingClassList: TJCoreOPFMappingClassList;
    FModel: TJCoreOPFModel;
    FPassword: string;
    FUsername: string;
    function GetDriverName: string;
    function GetMappingClassList: TJCoreOPFMappingClassList;
    procedure SetDriverName(AValue: string);
  protected
    function CreateDriver: TJCoreOPFDriver;
    function InternalCreateSession(const ADriver: TJCoreOPFDriver): IJCoreOPFSession; virtual;
    property MappingClassList: TJCoreOPFMappingClassList read GetMappingClassList;
  public
    constructor Create(const AModel: TJCoreOPFModel = nil);
    destructor Destroy; override;
    procedure AddDriverClass(const ADriverClass: TJCoreOPFDriverClass);
    procedure AddMappingClass(const AMappingClass: TJCoreOPFMappingClass);
    function CreateSession: IJCoreOPFSession;
    property DriverName: string read GetDriverName write SetDriverName;
    property Host: string write FHost;
    property Model: TJCoreOPFModel read FModel;
    property Password: string write FPassword;
    property Username: string write FUsername;
  end;

implementation

uses
  sysutils,
  JCoreOPFException;

{ TJCoreOPFConfiguration }

procedure TJCoreOPFConfiguration.SetDriverName(AValue: string);
var
  VIndex: Integer;
begin
  if FDriverName <> AValue then
  begin
    { TODO : thread safe }
    VIndex := FDriverClassList.IndexOf(AValue);
    if VIndex = -1 then
      raise EJCoreOPFDriverNotFound.Create(AValue);
    FDriverClass := FDriverClassList.Data[VIndex];
    FDriverName := AValue;
  end;
end;

function TJCoreOPFConfiguration.GetDriverName: string;
begin
  Result := FDriverName;
end;

function TJCoreOPFConfiguration.GetMappingClassList: TJCoreOPFMappingClassList;
begin
  Result := FMappingClassList;
end;

function TJCoreOPFConfiguration.CreateDriver: TJCoreOPFDriver;
begin
  { TODO : thread safe }
  if not Assigned(FDriverClass) then
    raise EJCoreOPFUndefinedDriver.Create;
  Result := FDriverClass.Create;
end;

function TJCoreOPFConfiguration.InternalCreateSession(
  const ADriver: TJCoreOPFDriver): IJCoreOPFSession;
begin
  Result := TJCoreOPFSession.Create(Self, Model, ADriver);
end;

constructor TJCoreOPFConfiguration.Create(
  const AModel: TJCoreOPFModel = nil);
begin
  inherited Create;
  if Assigned(AModel) then
    FModel := TJCoreOPFModel.AcquireModel
  else
    FModel := TJCoreOPFModel.Create;
  FDriverClassList := TJCoreOPFDriverClassList.Create;
  FMappingClassList := TJCoreOPFMappingClassList.Create;
end;

destructor TJCoreOPFConfiguration.Destroy;
begin
  FreeAndNil(FModel);
  FreeAndNil(FMappingClassList);
  FreeAndNil(FDriverClassList);
  inherited Destroy;
end;

procedure TJCoreOPFConfiguration.AddDriverClass(const ADriverClass: TJCoreOPFDriverClass);
begin
  FDriverClassList.Add(ADriverClass.DriverName, ADriverClass);
end;

procedure TJCoreOPFConfiguration.AddMappingClass(
  const AMappingClass: TJCoreOPFMappingClass);
begin
  FMappingClassList.Add(AMappingClass);
end;

function TJCoreOPFConfiguration.CreateSession: IJCoreOPFSession;
var
  VDriver: TJCoreOPFDriver;
begin
  VDriver := CreateDriver;
  try
    Result := InternalCreateSession(VDriver);
  except
    FreeAndNil(VDriver);
    raise;
  end;
end;

end.

