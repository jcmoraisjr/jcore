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
  Classes,
  JCoreOPFDriver,
  JCoreOPFMetadata,
  JCoreOPFMapping,
  JCoreOPFSession;

type

  { IJCoreOPFConfiguration }

  {**
    @abstract Interface of the global OPF configuration.

    Use @link(TJCoreOPFConfiguration) class to create a new configuration instance.

    @longcode(#
    var
      VConfig: IJCoreOPFConfiguration;
    begin
      VConfig := TJCoreOPFConfiguration.Create;
    #)

    @Member(AddDriverClass
      Add an array of drivers to the configuration. Use @link(IJCoreOPFConfiguration.DriverName
      DriverName) property to define which driver should be used with new
      @link(IJCoreOPFSession sessions).
      @param(ADriverClassArray Array of drivers.)
    )
  }
  IJCoreOPFConfiguration = interface(IInterface)
    procedure AddDriverClass(const ADriverClassArray: array of TJCoreOPFDriverClass);
    procedure AddMappingClass(const AMappingClassArray: array of TJCoreOPFMappingClass);
    function CreateSession: IJCoreOPFSession;
    function GetDriverClass: TJCoreOPFDriverClass;
    function GetDriverName: string;
    function Model: TJCoreOPFModel;
    function Params: TStringList;
    procedure SetDriverClass(const AValue: TJCoreOPFDriverClass);
    procedure SetDriverName(const AValue: string);
    property DriverClass: TJCoreOPFDriverClass read GetDriverClass write SetDriverClass;
    property DriverName: string read GetDriverName write SetDriverName;
  end;

  { TJCoreOPFConfiguration }

  TJCoreOPFConfiguration = class(TInterfacedObject, IJCoreOPFConfiguration, IJCoreOPFSessionManager)
  private
    FDriverClass: TJCoreOPFDriverClass;
    FDriverClassMap: TJCoreOPFDriverClassMap;
    FMappingClassFactory: TJCoreOPFMappingClassFactory;
    FModel: TJCoreOPFModel;
    FParams: TStringList;
    function GetDriverClass: TJCoreOPFDriverClass;
    function GetDriverName: string;
    function IGetMappingClassFactory: TJCoreOPFMappingClassFactory;
    function IGetModel: TJCoreOPFModel;
    function IGetParams: TStringList;
    procedure SetDriverClass(const AValue: TJCoreOPFDriverClass);
    procedure SetDriverName(const AValue: string);
    function IJCoreOPFSessionManager.MappingClassFactory = IGetMappingClassFactory;
    function IJCoreOPFSessionManager.Model = IGetModel;
    function IJCoreOPFConfiguration.Model = IGetModel;
    function IJCoreOPFConfiguration.Params = IGetParams;
  protected
    function CreateDriver: TJCoreOPFDriver;
    function InternalCreateSession(const ADriver: TJCoreOPFDriver): IJCoreOPFSession; virtual;
    property DriverClassMap: TJCoreOPFDriverClassMap read FDriverClassMap;
    property MappingClassFactory: TJCoreOPFMappingClassFactory read FMappingClassFactory;
    property Params: TStringList read FParams;
  public
    constructor Create(const AModel: TJCoreOPFModel = nil);
    destructor Destroy; override;
    procedure AddDriverClass(const ADriverClassArray: array of TJCoreOPFDriverClass);
    procedure AddMappingClass(const AMappingClassArray: array of TJCoreOPFMappingClass);
    function CreateSession: IJCoreOPFSession;
    property DriverClass: TJCoreOPFDriverClass read GetDriverClass write SetDriverClass;
    property DriverName: string read GetDriverName write SetDriverName;
    property Model: TJCoreOPFModel read FModel;
  end;

implementation

uses
  sysutils,
  types,
  JCoreConsts,
  JCoreClasses,
  JCoreOPFADM;

{ TJCoreOPFConfiguration }

function TJCoreOPFConfiguration.GetDriverClass: TJCoreOPFDriverClass;
begin
  Result := FDriverClass;
end;

function TJCoreOPFConfiguration.GetDriverName: string;
var
  VDriverClass: TJCoreOPFDriverClass;
begin
  VDriverClass := DriverClass;
  if Assigned(VDriverClass) then
    Result := VDriverClass.DriverName
  else
    Result := '';
end;

function TJCoreOPFConfiguration.IGetMappingClassFactory: TJCoreOPFMappingClassFactory;
begin
  Result := FMappingClassFactory;
end;

function TJCoreOPFConfiguration.IGetModel: TJCoreOPFModel;
begin
  Result := FModel;
end;

function TJCoreOPFConfiguration.IGetParams: TStringList;
begin
  Result := FParams;
end;

procedure TJCoreOPFConfiguration.SetDriverClass(const AValue: TJCoreOPFDriverClass);
begin
  AddDriverClass(AValue);
  FDriverClass := AValue;
end;

procedure TJCoreOPFConfiguration.SetDriverName(const AValue: string);
var
  VIndex: Integer;
begin
  if DriverName <> AValue then
  begin
    { TODO : thread safe }
    VIndex := DriverClassMap.IndexOf(AValue);
    if VIndex = -1 then
      raise EJCoreOPF.Create(2101, S2101_DriverNotFound, [AValue]);
    DriverClass := DriverClassMap.Data[VIndex];
  end;
end;

function TJCoreOPFConfiguration.CreateDriver: TJCoreOPFDriver;
begin
  { TODO : thread safe }
  if not Assigned(FDriverClass) then
    raise EJCoreOPF.Create(2102, S2102_UndefinedDriver, []);
  Result := FDriverClass.Create(Params);
end;

function TJCoreOPFConfiguration.InternalCreateSession(
  const ADriver: TJCoreOPFDriver): IJCoreOPFSession;
begin
  Result := TJCoreOPFSession.Create(Self, ADriver);
end;

constructor TJCoreOPFConfiguration.Create(
  const AModel: TJCoreOPFModel = nil);
begin
  if not Assigned(AModel) then
  begin
    FModel := TJCoreOPFModel.Create;
    // Native types
    FModel.AddADMClass([
     TJCoreOPFADMType32NativeCtl, TJCoreOPFADMType64NativeCtl,
     TJCoreOPFADMFloatNativeCtl, TJCoreOPFADMAnsiStringNativeCtl]);
    // Object as value types
    FModel.AddADMClass([TJCoreOPFADMIntegerIntfCtl]);
    // Value types
    FModel.AddADMClass([TJCoreOPFIntegerType]);
    // Entities and collections
    FModel.AddADMClass([TJCoreOPFADMEntity, TJCoreOPFADMFPSListCollection]);
  end else
    FModel := AModel;
  inherited Create;
  FDriverClassMap := TJCoreOPFDriverClassMap.Create;
  FDriverClassMap.Duplicates := dupIgnore;
  FMappingClassFactory := TJCoreOPFMappingClassFactory.Create(Model);
  FParams := TStringList.Create;
end;

destructor TJCoreOPFConfiguration.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FModel);
  FreeAndNil(FMappingClassFactory);
  FreeAndNil(FDriverClassMap);
  inherited Destroy;
end;

procedure TJCoreOPFConfiguration.AddDriverClass(const ADriverClassArray: array of TJCoreOPFDriverClass);
var
  VDriverClass: TJCoreOPFDriverClass;
begin
  for VDriverClass in ADriverClassArray do
    DriverClassMap.Add(VDriverClass.DriverName, VDriverClass);
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

