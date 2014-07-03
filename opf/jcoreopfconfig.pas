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
  JCoreOPFDriver,
  JCoreOPFMetadata,
  JCoreOPFMapping,
  JCoreOPFSession;

type

  { IJCoreOPFConfiguration }

  IJCoreOPFConfiguration = interface(IInterface)
    procedure AddDriverClass(const ADriverClass: TJCoreOPFDriverClass);
    procedure AddMappingClass(const AMappingClassArray: array of TJCoreOPFMappingClass);
    function CreateSession: IJCoreOPFSession;
    function GetDriverName: string;
    function Model: TJCoreOPFModel;
    procedure SetDriverName(AValue: string);
    property DriverName: string read GetDriverName write SetDriverName;
  end;

  { TJCoreOPFConfiguration }

  TJCoreOPFConfiguration = class(TInterfacedObject, IJCoreOPFConfiguration, IJCoreOPFSessionManager)
  private
    FDriverClass: TJCoreOPFDriverClass;
    FDriverClassMap: TJCoreOPFDriverClassMap;
    FDriverName: string;
    FMappingClassFactory: TJCoreOPFMappingClassFactory;
    FModel: TJCoreOPFModel;
    function GetDriverName: string;
    function IGetMappingClassFactory: TJCoreOPFMappingClassFactory;
    function IGetModel: TJCoreOPFModel;
    procedure SetDriverName(AValue: string);
    function IJCoreOPFSessionManager.MappingClassFactory = IGetMappingClassFactory;
    function IJCoreOPFSessionManager.Model = IGetModel;
    function IJCoreOPFConfiguration.Model = IGetModel;
  protected
    function CreateDriver: TJCoreOPFDriver;
    function InternalCreateSession(const ADriver: TJCoreOPFDriver): IJCoreOPFSession; virtual;
    property DriverClassMap: TJCoreOPFDriverClassMap read FDriverClassMap;
    property MappingClassFactory: TJCoreOPFMappingClassFactory read FMappingClassFactory;
  public
    constructor Create(const AModel: TJCoreOPFModel = nil);
    destructor Destroy; override;
    procedure AddDriverClass(const ADriverClass: TJCoreOPFDriverClass);
    procedure AddMappingClass(const AMappingClassArray: array of TJCoreOPFMappingClass);
    function CreateSession: IJCoreOPFSession;
    property DriverName: string read GetDriverName write SetDriverName;
    property Model: TJCoreOPFModel read FModel;
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
    VIndex := DriverClassMap.IndexOf(AValue);
    if VIndex = -1 then
      raise EJCoreOPFDriverNotFound.Create(AValue);
    FDriverClass := DriverClassMap.Data[VIndex];
    FDriverName := AValue;
  end;
end;

function TJCoreOPFConfiguration.GetDriverName: string;
begin
  Result := FDriverName;
end;

function TJCoreOPFConfiguration.IGetMappingClassFactory: TJCoreOPFMappingClassFactory;
begin
  Result := FMappingClassFactory;
end;

function TJCoreOPFConfiguration.IGetModel: TJCoreOPFModel;
begin
  Result := FModel;
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
  Result := TJCoreOPFSession.Create(Self, ADriver);
end;

constructor TJCoreOPFConfiguration.Create(
  const AModel: TJCoreOPFModel = nil);
begin
  if Assigned(AModel) then
    FModel := AModel
  else
    FModel := TJCoreOPFModel.Create;
  inherited Create;
  FDriverClassMap := TJCoreOPFDriverClassMap.Create;
  FMappingClassFactory := TJCoreOPFMappingClassFactory.Create(Model);
end;

destructor TJCoreOPFConfiguration.Destroy;
begin
  FreeAndNil(FModel);
  FreeAndNil(FMappingClassFactory);
  FreeAndNil(FDriverClassMap);
  inherited Destroy;
end;

procedure TJCoreOPFConfiguration.AddDriverClass(const ADriverClass: TJCoreOPFDriverClass);
begin
  DriverClassMap.Add(ADriverClass.DriverName, ADriverClass);
end;

procedure TJCoreOPFConfiguration.AddMappingClass(
  const AMappingClassArray: array of TJCoreOPFMappingClass);
begin
  MappingClassFactory.AddMappingClass(AMappingClassArray);
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

