unit TestOPFModel;

{$mode objfpc}{$H+}

interface

uses
  fgl,
  JCoreClasses,
  JCoreEntity,
  JCoreMetadata,
  JCoreOPFMetadata;

type

  { TTestBase }

  TTestBase = class(TJCoreManagedObject)
  private
    FPID: IJCorePID;
  published
    property _PID: IJCorePID read FPID write FPID;
  end;

  { TTestSimple }

  TTestSimple = class(TObject)
  private
    FPID: IJCorePID;
    FField1: Integer;
  published
    property _PID: IJCorePID read FPID write FPID;
    property Field1: Integer read FField1 write FField1;
  end;

  { TTestAddress }

  TTestAddress = class(TTestBase)
  private
    FStreet: string;
    FZipCode: string;
  published
    property Street: string read FStreet write FStreet;
    property ZipCode: string read FZipCode write FZipCode;
  end;

  { TTestCity }

  TTestCity = class(TTestBase)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TTestPhone }

  TTestPhone = class(TTestBase)
  private
    FNumber: string;
  published
    property Number: string read FNumber write FNumber;
  end;

  TTestPhoneList = specialize TFPGObjectList<TTestPhone>;

  { TTestLanguage }

  TTestLanguage = class(TTestBase)
  private
    FName: string;
  public
    constructor Create(const AName: string);
  published
    property Name: string read FName write FName;
  end;

  TTestLanguageList = specialize TFPGObjectList<TTestLanguage>;

  { TTestPerson }

  TTestPerson = class(TTestBase)
  private
    FName: string;
    FAge: Integer;
    FPhones: TTestPhoneList;
    FAddress: TTestAddress;
    FCity: TTestCity;
    FLanguages: TTestLanguageList;
    function GetLanguages: TTestLanguageList;
    function GetPhones: TTestPhoneList;
    procedure SetAddress(AValue: TTestAddress);
    procedure SetCity(AValue: TTestCity);
    procedure SetLanguages(AValue: TTestLanguageList);
    procedure SetPhones(AValue: TTestPhoneList);
  protected
    procedure Finit; override;
  published
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Phones: TTestPhoneList read GetPhones write SetPhones;
    property Address: TTestAddress read FAddress write SetAddress;
    property City: TTestCity read FCity write SetCity;
    property Languages: TTestLanguageList read GetLanguages write SetLanguages;
  end;

  { TTestEmployee }

  TTestEmployee = class(TTestPerson)
  private
    FSalary: Currency;
  published
    property Salary: Currency read FSalary;
  end;

  { TTestOPFModel }

  TTestOPFModel = class(TJCoreOPFModel)
  protected
    function BuildMetadata(const AClass: TClass): TJCoreClassMetadata; override;
    procedure InitRegistry; override;
  end;

implementation

uses
  sysutils;

{ TTestLanguage }

constructor TTestLanguage.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TTestPerson }

function TTestPerson.GetLanguages: TTestLanguageList;
begin
  if not Assigned(FLanguages) then
    FLanguages := TTestLanguageList.Create;
  Result := FLanguages;
end;

function TTestPerson.GetPhones: TTestPhoneList;
begin
  if not Assigned(FPhones) then
    FPhones := TTestPhoneList.Create;
  Result := FPhones;
end;

procedure TTestPerson.SetAddress(AValue: TTestAddress);
begin
  FreeAndNil(FAddress);
  FAddress := AValue;
end;

procedure TTestPerson.SetCity(AValue: TTestCity);
begin
  if FCity <> AValue then
  begin
    FreeAndNil(FCity);
    FCity := AValue;
  end;
end;

procedure TTestPerson.SetLanguages(AValue: TTestLanguageList);
begin
  FreeAndNil(FLanguages);
  FLanguages := AValue;
end;

procedure TTestPerson.SetPhones(AValue: TTestPhoneList);
begin
  FreeAndNil(FPhones);
  FPhones := AValue;
end;

procedure TTestPerson.Finit;
begin
  FreeAndNil(FPhones);
  FreeAndNil(FAddress);
  FreeAndNil(FCity);
  FreeAndNil(FLanguages);
  inherited Finit;
end;

{ TTestOPFModel }

function TTestOPFModel.BuildMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := inherited BuildMetadata(AClass) as TJCoreOPFClassMetadata;
  try
    if AClass = TTestPerson then
    begin
      VMetadata.AttributeByName('Languages').CompositionType := jctAggregation;
      VMetadata.AttributeByName('Languages').CompositionLinkType := jcltExternal;
      VMetadata.AttributeByName('City').CompositionType := jctAggregation;
    end;
  except
    FreeAndNil(VMetadata);
    raise;
  end;
  Result := VMetadata;
end;

procedure TTestOPFModel.InitRegistry;
begin
  inherited InitRegistry;
  AddClass(TTestPerson);
  AddClass(TTestPhone);
  AddClass(TTestLanguage);
  AddClass(TTestAddress);
  AddClass(TTestCity);
end;

end.

