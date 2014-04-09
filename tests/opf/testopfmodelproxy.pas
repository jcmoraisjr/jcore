unit TestOPFModelProxy;

{$mode objfpc}{$H+}

interface

uses
  fgl,
  JCoreClasses,
  JCoreEntity;

type

  { TTestProxyBase }

  TTestProxyBase = class(TJCoreManagedObject)
    _Proxy: TJCoreEntityProxy;
  protected
    procedure Finit; override;
  end;

  { TTestProxyCity }

  TTestProxyCity = class(TTestProxyBase)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TTestProxyPhone }

  TTestProxyPhone = class(TTestProxyBase)
  private
    FNumber: string;
  published
    property Number: string read FNumber write FNumber;
  end;

  TTestProxyPhoneList = specialize TFPGObjectList<TTestProxyPhone>;

  { TTestProxyPerson }

  TTestProxyPerson = class(TTestProxyBase)
  private
    FName: string;
    FAge: Integer;
    FPhones: TTestProxyPhoneList;
    FCity: TTestProxyCity;
    function GetCity: TTestProxyCity;
    function GetPhones: TTestProxyPhoneList;
    procedure SetCity(AValue: TTestProxyCity);
    procedure SetPhones(AValue: TTestProxyPhoneList);
  protected
    procedure Finit; override;
  published
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property Phones: TTestProxyPhoneList read GetPhones write SetPhones;
    property City: TTestProxyCity read GetCity write SetCity;
  end;

implementation

uses
  sysutils;

{ TTestProxyBase }

procedure TTestProxyBase.Finit;
begin
  FreeAndNil(_Proxy);
  inherited Finit;
end;

{ TTestProxyPerson }

function TTestProxyPerson.GetCity: TTestProxyCity;
begin
  _Proxy.Lazyload(@FCity, 'City');
  Result := FCity;
end;

function TTestProxyPerson.GetPhones: TTestProxyPhoneList;
begin
  _Proxy.Lazyload(@FPhones, 'Phones');
  Result := FPhones;
end;

procedure TTestProxyPerson.SetCity(AValue: TTestProxyCity);
begin
  if FCity <> AValue then
  begin
    FreeAndNil(FCity);
    FCity := AValue;
  end;
end;

procedure TTestProxyPerson.SetPhones(AValue: TTestProxyPhoneList);
begin
  if FPhones <> AValue then
  begin
    FreeAndNil(FPhones);
    FPhones := AValue;
  end;
end;

procedure TTestProxyPerson.Finit;
begin
  FreeAndNil(FPhones);
  FreeAndNil(FCity);
  inherited Finit;
end;

end.

