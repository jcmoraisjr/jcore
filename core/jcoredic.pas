(*
  JCore, Dependency Injection Container Classes
  Copyright (C) 2013 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreDIC;

{$I jcore.inc}

interface

uses
  Classes,
  fgl,
  JCoreClasses;

type

  EJCoreDICException = class(EJCoreException);

  { EJCoreDICImplNotFoundException }

  EJCoreDICImplNotFoundException = class(EJCoreDICException)
  strict private
    FGUID: TGuid;
  public
    constructor Create(const AGUID: TGuid; const AQualifier: string);
    property GUID: TGuid read FGUID;
  end;

  { EJCoreDICAmbiguousImplementationException }

  EJCoreDICAmbiguousImplementationException = class(EJCoreDICException)
  strict private
    FClass1: TClass;
    FClass2: TClass;
  public
    constructor Create(AGUID: TGUID; AClass1, AClass2: TClass);
    property Class1: TClass read FClass1;
    property Class2: TClass read FClass2;
  end;

  TJCoreDICClassClass = class of TJCoreDICClass;

  { TJCoreDICClass }

  TJCoreDICClass = class(TObject)
  strict private
    FClass: TClass;
    FOverrides: TClass;
    FIsLazy: Boolean;
  strict protected
    function GetInstance: TObject; virtual; abstract;
  public
    constructor Create(AClass, AOverrides: TClass; AIsLazy: Boolean);
    function Overrides(AOther: TJCoreDICClass): Boolean;
    property Instance: TObject read GetInstance;
    property TheClass: TClass read FClass;
  end;

  { TJCoreDICSingletonClass }

  TJCoreDICSingletonClass = class(TJCoreDICClass)
  strict private
    FCurrentIntf: IUnknown;
    FCurrentObj: TObject;
    procedure SetCurrentObj(AValue: TObject);
    property CurrentObj: TObject read FCurrentObj write SetCurrentObj;
  strict protected
    function GetInstance: TObject; override;
  end;

  { TJCoreDICInstanceClass }

  TJCoreDICInstanceClass = class(TJCoreDICClass)
  strict protected
    function GetInstance: TObject; override;
  end;

  TJCoreDICClassList = specialize TFPGObjectList<TJCoreDICClass>;

  { TJCoreDICClassFactory }

  TJCoreDICClassFactory = class(TObject)
  strict private
    FClasses: TJCoreDICClassList;
    FCurrentClass: TJCoreDICClass;
    FGUID: TGuid;
    FQualifier: string;
    function ChooseClass(AClass1, AClass2: TJCoreDICClass): TJCoreDICClass;
    function GetCurrentClass: TJCoreDICClass;
    function GetQualifier: string;
    function IndexOfClass(AClass: TClass): Integer;
    procedure RecalcCurrentClass(ANewClass: TJCoreDICClass = nil);
    property Classes: TJCoreDICClassList read FClasses;
  public
    constructor Create(const AGUID: TGuid; const AQualifier: string);
    destructor Destroy; override;
    procedure AddClass(AClass: TJCoreDICClass);
    function HasCurrentClass: Boolean;
    function RemoveClass(AClass: TClass): Boolean;
    property CurrentClass: TJCoreDICClass read GetCurrentClass;
    property GUID: TGuid read FGUID;
    property Qualifier: string read GetQualifier;
  end;

  { TJCoreDICDependencyMap }

  TJCoreDICDependencyMap = class(specialize TFPGMap<string, TJCoreDICClassFactory>)
  public
    function Add(const AGUID: TGuid; const AQualifier: string; const AClassFactory: TJCoreDICClassFactory): Integer;
    function IndexOf(const AGUID: TGuid; const AQualifier: string): Integer;
    function Unregister(const AGUID: TGuid; const AClass: TClass): Boolean;
  end;

  { TJCoreDIC }

  TJCoreDIC = class(TObject)
  strict private
    class var FContainer: TJCoreDICDependencyMap;
    class constructor Create;
    class destructor Destroy;
    class function CheckGUID(const AGUID: TGuid; const AQualifier: string): TJCoreDICClassFactory;
    class procedure DoRegister(const AGUID: TGuid; const AQualifier: string; AClass: TClass; ADICClass: TJCoreDICClassClass; AOverrides: TClass; AIsLazy: Boolean);
    class property Container: TJCoreDICDependencyMap read FContainer;
  public
    class procedure LazyRegister(const AGUID: TGuid; AClass: TClass; ADICClass: TJCoreDICClassClass = nil);
    class procedure LazyRegister(const AGUID: TGuid; const AQualifier: string; AClass: TClass; ADICClass: TJCoreDICClassClass = nil);
    class procedure Locate(const AGUID: TGuid; out AIntf);
    class procedure Locate(const AGUID: TGuid; const AQualifier: string; out AIntf);
    class procedure Register(const AGUID: TGuid; AClass: TClass; ADICClass: TJCoreDICClassClass = nil; AOverrides: TClass = nil);
    class procedure Register(const AGUID: TGuid; const AQualifier: string; AClass: TClass; ADICClass: TJCoreDICClassClass = nil; AOverrides: TClass = nil);
    class function Unregister(const AGUID: TGuid; AClass: TClass): Boolean;
  end;

implementation

uses
  sysutils,
  JCoreConsts;

{ EJCoreDICImplNotFoundException }

constructor EJCoreDICImplNotFoundException.Create(const AGUID: TGuid; const AQualifier: string);
begin
  CreateFmt(SJCoreImplementationNotFound, [GUIDToString(AGUID), AQualifier]);
  FGUID := AGUID;
end;

{ EJCoreDICAmbiguousImplementationException }

constructor EJCoreDICAmbiguousImplementationException.Create(AGUID: TGUID;
  AClass1, AClass2: TClass);
begin
  CreateFmt(SJCoreAmbiguousImplementation, [
   GUIDToString(AGUID), AClass1.ClassName, AClass2.ClassName]);
  FClass1 := AClass1;
  FClass2 := AClass2;
end;

{ TJCoreDICClass }

constructor TJCoreDICClass.Create(AClass, AOverrides: TClass; AIsLazy: Boolean);
begin
  if not Assigned(AClass) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FClass := AClass;
  FOverrides := AOverrides;
  FIsLazy := AIsLazy;
end;

function TJCoreDICClass.Overrides(AOther: TJCoreDICClass): Boolean;
begin
  Result := not Assigned(AOther) or AOther.FIsLazy or
   FClass.InheritsFrom(AOther.TheClass) or
   (Assigned(FOverrides) and FOverrides.InheritsFrom(AOther.TheClass));
end;

{ TJCoreDICSingletonClass }

procedure TJCoreDICSingletonClass.SetCurrentObj(AValue: TObject);
begin
  if FCurrentObj <> AValue then
  begin
    if Assigned(AValue) then
      AValue.GetInterface(IUnknown, FCurrentIntf)
    else
      FCurrentIntf := nil;
    FCurrentObj := AValue;
  end;
end;

function TJCoreDICSingletonClass.GetInstance: TObject;
begin
  if not Assigned(CurrentObj) then
    CurrentObj := TheClass.Create;
  Result := CurrentObj;
end;

{ TJCoreDICInstanceClass }

function TJCoreDICInstanceClass.GetInstance: TObject;
begin
  Result := TheClass.Create;
end;

{ TJCoreDICClassFactory }

function TJCoreDICClassFactory.ChooseClass(
  AClass1, AClass2: TJCoreDICClass): TJCoreDICClass;
begin
  if not Assigned(AClass1) and not Assigned(AClass2) then
    raise EJCoreNilPointerException.Create
  else if Assigned(AClass1) and AClass1.Overrides(AClass2) then
    Result := AClass1
  else if Assigned(AClass2) and AClass2.Overrides(AClass1) then
    Result := AClass2
  else
    raise EJCoreDICAmbiguousImplementationException.Create(FGUID,
     AClass1.TheClass, AClass2.TheClass);
end;

function TJCoreDICClassFactory.GetCurrentClass: TJCoreDICClass;
begin
  if not Assigned(FCurrentClass) then
    raise EJCoreDICImplNotFoundException.Create(GUID, Qualifier);
  Result := FCurrentClass;
end;

function TJCoreDICClassFactory.GetQualifier: string;
begin
  if FQualifier = '' then
    Result := SJCoreDefaultQualifier
  else
    Result := FQualifier;
end;

function TJCoreDICClassFactory.IndexOfClass(AClass: TClass): Integer;
begin
  for Result := 0 to Pred(Classes.Count) do
    if (Classes[Result].TheClass = AClass) then
      Exit;
  Result := -1;
end;

procedure TJCoreDICClassFactory.RecalcCurrentClass(ANewClass: TJCoreDICClass);
var
  VCurrent: TJCoreDICClass;
  I: Integer;
begin
  VCurrent := ANewClass;
  for I := Pred(Classes.Count) downto 0 do
    VCurrent := ChooseClass(VCurrent, Classes[I]);
  FCurrentClass := VCurrent;
end;

constructor TJCoreDICClassFactory.Create(const AGUID: TGuid;
  const AQualifier: string);
begin
  inherited Create;
  FGUID := AGUID;
  FQualifier := AQualifier;
  FClasses := TJCoreDICClassList.Create(True);
end;

destructor TJCoreDICClassFactory.Destroy;
begin
  FreeAndNil(FClasses);
  inherited Destroy;
end;

procedure TJCoreDICClassFactory.AddClass(AClass: TJCoreDICClass);
begin
  RecalcCurrentClass(AClass);
  Classes.Add(AClass);
end;

function TJCoreDICClassFactory.HasCurrentClass: Boolean;
begin
  Result := Assigned(FCurrentClass);
end;

function TJCoreDICClassFactory.RemoveClass(AClass: TClass): Boolean;
var
  VIndex: Integer;
begin
  VIndex := IndexOfClass(AClass);
  Result := VIndex >= 0;
  if Result then
  begin
    Classes.Delete(VIndex);
    RecalcCurrentClass;
  end;
end;

{ TJCoreDICDependencyMap }

function TJCoreDICDependencyMap.Add(const AGUID: TGuid; const AQualifier: string;
  const AClassFactory: TJCoreDICClassFactory): Integer;
begin
  Result := inherited Add(GUIDToString(AGUID) + AQualifier, AClassFactory);
end;

function TJCoreDICDependencyMap.IndexOf(const AGUID: TGuid;
  const AQualifier: string): Integer;
begin
  Result := inherited IndexOf(GUIDToString(AGUID) + AQualifier);
end;

function TJCoreDICDependencyMap.Unregister(const AGUID: TGuid;
  const AClass: TClass): Boolean;
var
  VClassFactory: TJCoreDICClassFactory;
  VGUID: string;
  VGUIDLen: Integer;
  I: Integer;
begin
  // There is only one single list to GUIDs and Qualifiers. The index is built
  // concatenating a string representation of GUID and the qualifier.
  // To unregister, we need to iterate the list in order to find all
  // qualifiers of the given GUID.
  VGUID := GUIDToString(AGUID);
  VGUIDLen := Length(VGUID);
  Result := False;
  for I := Pred(Count) downto 0 do
  begin
    if strlcomp(PChar(Keys[I]), PChar(VGUID), VGUIDLen) = 0 then
    begin
      VClassFactory := Data[I];
      Result := VClassFactory.RemoveClass(AClass) or Result;
      if not VClassFactory.HasCurrentClass then
      begin
        Delete(I);
        FreeAndNil(VClassFactory);
      end;
    end;
  end;
end;

{ TJCoreDIC }

class constructor TJCoreDIC.Create;
begin
  FContainer := TJCoreDICDependencyMap.Create;
  FContainer.Sorted := True;
  FContainer.Duplicates := dupError;
end;

class destructor TJCoreDIC.Destroy;
var
  I: Integer;
begin
  if Assigned(FContainer) then
    for I := 0 to Pred(FContainer.Count) do
      FContainer.Data[I].Free;
  FreeAndNil(FContainer);
end;

class function TJCoreDIC.CheckGUID(const AGUID: TGuid;
  const AQualifier: string): TJCoreDICClassFactory;
var
  VIndex: Integer;
begin
  VIndex := Container.IndexOf(AGUID, AQualifier);
  if VIndex = -1 then
    VIndex := Container.Add(AGUID, AQualifier, TJCoreDICClassFactory.Create(AGUID, AQualifier));
  Result := Container.Data[VIndex];
end;

class procedure TJCoreDIC.DoRegister(const AGUID: TGuid; const AQualifier: string;
  AClass: TClass; ADICClass: TJCoreDICClassClass; AOverrides: TClass; AIsLazy: Boolean);
var
  VDICClass: TJCoreDICClass;
begin
  if not Assigned(AClass) then
    raise EJCoreNilPointerException.Create;
  if (AClass.GetInterfaceEntry(AGUID) = nil) then
    raise EJCoreUnsupportedIntfException.Create(AClass, AGUID);
  if not Assigned(ADICClass) then
    ADICClass := TJCoreDICSingletonClass;
  VDICClass := ADICClass.Create(AClass, AOverrides, AIsLazy);
  try
    CheckGUID(AGUID, AQualifier).AddClass(VDICClass);
  except
    FreeAndNil(VDICClass);
    raise;
  end;
end;

class procedure TJCoreDIC.LazyRegister(const AGUID: TGuid; AClass: TClass;
  ADICClass: TJCoreDICClassClass);
begin
  DoRegister(AGUID, '', AClass, ADICClass, nil, True);
end;

class procedure TJCoreDIC.LazyRegister(const AGUID: TGuid;
  const AQualifier: string; AClass: TClass; ADICClass: TJCoreDICClassClass);
begin
  DoRegister(AGUID, AQualifier, AClass, ADICClass, nil, True);
end;

class procedure TJCoreDIC.Locate(const AGUID: TGuid; out AIntf);
begin
  Locate(AGUID, '', AIntf);
end;

class procedure TJCoreDIC.Locate(const AGUID: TGuid; const AQualifier: string; out AIntf);
var
  VInstance: TObject;
  VIndex: Integer;
begin
  VIndex := Container.IndexOf(AGUID, AQualifier);
  if VIndex = -1 then
    raise EJCoreDICImplNotFoundException.Create(AGUID, AQualifier);
  VInstance := Container.Data[VIndex].CurrentClass.Instance;
  if not VInstance.GetInterface(AGUID, IUnknown(AIntf)) then
    raise EJCoreUnsupportedIntfException.Create(VInstance.ClassType, AGUID);
end;

class procedure TJCoreDIC.Register(
  const AGUID: TGuid; AClass: TClass;
  ADICClass: TJCoreDICClassClass; AOverrides: TClass);
begin
  DoRegister(AGUID, '', AClass, ADICClass, AOverrides, False);
end;

class procedure TJCoreDIC.Register(const AGUID: TGuid; const AQualifier: string;
  AClass: TClass; ADICClass: TJCoreDICClassClass; AOverrides: TClass);
begin
  DoRegister(AGUID, AQualifier, AClass, ADICClass, AOverrides, False);
end;

class function TJCoreDIC.Unregister(
  const AGUID: TGuid; AClass: TClass): Boolean;
begin
  Result := Container.Unregister(AGUID, AClass);
end;

end.

