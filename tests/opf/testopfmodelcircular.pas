unit TestOPFModelCircular;

{$mode objfpc}{$H+}

interface

uses
  fgl,
  JCoreEntity;

type
  TCircularPerson = class;
  TCircularPersonList = specialize TFPGObjectList<TCircularPerson>;

  { TCircularPerson }

  TCircularPerson = class(TJCoreEntity)
  private
    FName: string;
    FDependent: TCircularPersonList;
    function GetDependent: TCircularPersonList;
    procedure SetDependent(AValue: TCircularPersonList);
  public
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Dependent: TCircularPersonList read GetDependent write SetDependent stored False;
  end;

implementation

uses
  sysutils;

function TCircularPerson.GetDependent: TCircularPersonList;
begin
  if not _proxy.Lazyload(@FDependent) then
    FDependent := TCircularPersonList.Create(True);
  Result := FDependent;
end;

procedure TCircularPerson.SetDependent(AValue: TCircularPersonList);
begin
  if AValue <> FDependent then
  begin
    FDependent.Free;
    FDependent := AValue;
  end;
end;

destructor TCircularPerson.Destroy;
begin
  FreeAndNil(FDependent);
  inherited Destroy;
end;

end.

