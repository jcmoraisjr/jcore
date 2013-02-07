(*
  JCore, Dependency Injection Container
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

  { EJCoreDICIntfNotFoundException }

  EJCoreDICIntfNotFoundException = class(EJCoreDICException)
  strict private
    FGUID: TGuid;
  public
    constructor Create(AGUID: TGuid);
    property GUID: TGuid read FGUID;
  end;

  { EJCoreDICUnsupportedIntfException }

  EJCoreDICUnsupportedIntfException = class(EJCoreDICException)
  strict private
    FClass: TClass;
    FGUID: TGuid;
  public
    constructor Create(AClass: TClass; AGUID: TGuid);
    property TheClass: TClass read FClass;
    property GUID: TGuid read FGUID;
  end;

  { EJCoreDICAmbiguousClassException }

  EJCoreDICAmbiguousClassException = class(EJCoreDICException)
  strict private
    FClass1: TClass;
    FClass2: TClass;
  public
    constructor Create(AClass1, AClass2: TClass);
    property Class1: TClass read FClass1;
    property Class2: TClass read FClass2;
  end;

  TJCoreDICClassClass = class of TJCoreDICClass;

  { TJCoreDICClass }

  TJCoreDICClass = class(TObject)
  strict private
    FClass: TClass;
    FOverrides: TClass;
  strict protected
    function GetInstance: TObject; virtual; abstract;
  public
    constructor Create(AClass, AOverrides: TClass);
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
    function ChooseClass(AClass1, AClass2: TJCoreDICClass): TJCoreDICClass;
    function GetCurrentClass: TJCoreDICClass;
    function IndexOfClass(AClass: TClass): Integer;
    procedure RecalcCurrentClass(ANewClass: TJCoreDICClass = nil);
    property Classes: TJCoreDICClassList read FClasses;
    property GUID: TGuid read FGUID;
  public
    constructor Create(const AGUID: TGuid);
    destructor Destroy; override;
    procedure AddClass(AClass: TJCoreDICClass);
    function HasCurrentClass: Boolean;
    function RemoveClass(AClass: TClass): Boolean;
    property CurrentClass: TJCoreDICClass read GetCurrentClass;
  end;

  TJCoreDICDependencyMap = specialize TFPGMap<String, TJCoreDICClassFactory>;

  { TJCoreDIC }

  TJCoreDIC = class(TObject)
  strict private
    class var FContainer: TJCoreDICDependencyMap;
    class constructor Create;
    class destructor Destroy;
    class function CheckGUID(const AGUID: TGuid): TJCoreDICClassFactory;
    class property Container: TJCoreDICDependencyMap read FContainer;
  public
    class procedure Locate(const AGUID: TGuid; out AIntf);
    class procedure Register(const AGUID: TGuid; AClass: TClass; ADICClass: TJCoreDICClassClass = nil; AOverrides: TClass = nil);
    class function Unregister(const AGUID: TGuid; AClass: TClass): Boolean;
  end;

implementation

uses
  sysutils,
  JCoreConsts;

{ EJCoreDICIntfNotFoundException }

constructor EJCoreDICIntfNotFoundException.Create(AGUID: TGuid);
begin
  CreateFmt(SInterfaceNotFound, [GUIDToString(AGUID)]);
  FGUID := AGUID;
end;

{ EJCoreDICUnsupportedIntfException }

constructor EJCoreDICUnsupportedIntfException.Create(AClass: TClass; AGUID: TGuid);
begin
  CreateFmt(SUnsupportedInterface, [AClass.ClassName, GUIDToString(AGUID)]);
  FClass := AClass;
  FGUID := AGUID;
end;

{ EJCoreDICAmbiguousClassException }

constructor EJCoreDICAmbiguousClassException.Create(AClass1, AClass2: TClass);
begin
  CreateFmt(SAmbiguousClass, [AClass1.ClassName, AClass2.ClassName]);
  FClass1 := AClass1;
  FClass2 := AClass2;
end;

{ TJCoreDICClass }

constructor TJCoreDICClass.Create(AClass, AOverrides: TClass);
begin
  if not Assigned(AClass) then
    raise EJCoreNilPointerException.Create;
  inherited Create;
  FClass := AClass;
  FOverrides := AOverrides;
end;

function TJCoreDICClass.Overrides(AOther: TJCoreDICClass): Boolean;
begin
  Result := not Assigned(AOther) or FClass.InheritsFrom(AOther.TheClass) or
   (Assigned(FOverrides) and FOverrides.InheritsFrom(AOther.TheClass));
end;

{ TJCoreDICInstanceClass }

function TJCoreDICInstanceClass.GetInstance: TObject;
begin
  Result := TheClass.Create;
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
    raise EJCoreDICAmbiguousClassException.Create(
     AClass1.TheClass, AClass2.TheClass);
end;

function TJCoreDICClassFactory.GetCurrentClass: TJCoreDICClass;
begin
  if not Assigned(FCurrentClass) then
    raise EJCoreDICIntfNotFoundException.Create(GUID);
  Result := FCurrentClass;
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
  for I := 0 to Pred(Classes.Count) do
    VCurrent := ChooseClass(VCurrent, Classes[I]);
  FCurrentClass := VCurrent;
end;

constructor TJCoreDICClassFactory.Create(const AGUID: TGuid);
begin
  inherited Create;
  FGUID := AGUID;
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

class function TJCoreDIC.CheckGUID(const AGUID: TGuid): TJCoreDICClassFactory;
var
  VGUIDStr: string;
  VIndex: Integer;
begin
  VGUIDStr := GUIDToString(AGUID);
  VIndex := Container.IndexOf(VGUIDStr);
  if VIndex = -1 then
    VIndex := Container.Add(VGUIDStr, TJCoreDICClassFactory.Create(AGUID));
  Result := Container.Data[VIndex];
end;

class procedure TJCoreDIC.Locate(const AGUID: TGuid; out AIntf);
var
  VGUIDStr: String;
  VInstance: TObject;
  VIndex: Integer;
begin
  VGUIDStr := GUIDToString(AGUID);
  VIndex := Container.IndexOf(VGUIDStr);
  if VIndex = -1 then
    raise EJCoreDICIntfNotFoundException.Create(AGUID);
  VInstance := Container.Data[VIndex].CurrentClass.Instance;
  if not VInstance.GetInterface(AGUID, IUnknown(AIntf)) then
    raise EJCoreDICUnsupportedIntfException.Create(VInstance.ClassType, AGUID);
end;

class procedure TJCoreDIC.Register(
  const AGUID: TGuid; AClass: TClass;
  ADICClass: TJCoreDICClassClass; AOverrides: TClass);
var
  VDICClass: TJCoreDICClass;
begin
  if not Assigned(AClass) then
    raise EJCoreNilPointerException.Create;
  if (AClass.GetInterfaceEntry(AGUID) = nil) then
    raise EJCoreDICUnsupportedIntfException.Create(AClass, AGUID);
  if not Assigned(ADICClass) then
    ADICClass := TJCoreDICSingletonClass;
  VDICClass := ADICClass.Create(AClass, AOverrides);
  try
    CheckGUID(AGUID).AddClass(VDICClass);
  except
    FreeAndNil(VDICClass);
    raise;
  end;
end;

class function TJCoreDIC.Unregister(
  const AGUID: TGuid; AClass: TClass): Boolean;
var
  VClassFactory: TJCoreDICClassFactory;
  VGUIDStr: String;
  VIndex: Integer;
begin
  VGUIDStr := GUIDToString(AGUID);
  VIndex := Container.IndexOf(VGUIDStr);
  if (VIndex >= 0) then
  begin
    VClassFactory := Container.Data[VIndex];
    Result := VClassFactory.RemoveClass(AClass);
    if not VClassFactory.HasCurrentClass then
    begin
      Container.Delete(VIndex);
      FreeAndNil(VClassFactory);
    end;
  end else
    Result := False;
end;

end.

