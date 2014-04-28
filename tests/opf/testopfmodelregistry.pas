unit TestOPFModelRegistry;

{$mode objfpc}{$H+}

interface

uses
  JCoreMetadata,
  JCoreOPFMetadata;

type

  { TTestOPFModelIPIDContact }

  TTestOPFModelIPIDContact = class(TJCoreOPFModel)
  protected
    function BuildMetadata(const AClass: TClass): TJCoreClassMetadata; override;
    procedure InitRegistry; override;
  end;

  { TTestOPFModelProxyContact }

  TTestOPFModelProxyContact = class(TJCoreOPFModel)
  protected
    function BuildMetadata(const AClass: TClass): TJCoreClassMetadata; override;
    procedure InitRegistry; override;
  end;

  { TTestOPFModelProxyInvoice }

  TTestOPFModelProxyInvoice = class(TJCoreOPFModel)
  protected
    function BuildMetadata(const AClass: TClass): TJCoreClassMetadata; override;
    procedure InitRegistry; override;
  end;

implementation

uses
  sysutils,
  TestOPFModelContact,
  TestOPFModelInvoice;

{ TTestOPFModelIPIDContact }

function TTestOPFModelIPIDContact.BuildMetadata(const AClass: TClass): TJCoreClassMetadata;
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  VMetadata := inherited BuildMetadata(AClass) as TJCoreOPFClassMetadata;
  try
    if AClass = TTestIPIDPerson then
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

procedure TTestOPFModelIPIDContact.InitRegistry;
begin
  inherited InitRegistry;
  AddClass(TTestIPIDPerson);
  AddClass(TTestIPIDPhone);
  AddClass(TTestIPIDLanguage);
  AddClass(TTestIPIDAddress);
  AddClass(TTestIPIDCity);
end;

{ TTestOPFModelProxyContact }

function TTestOPFModelProxyContact.BuildMetadata(const AClass: TClass): TJCoreClassMetadata;
begin
  Result := inherited BuildMetadata(AClass);
end;

procedure TTestOPFModelProxyContact.InitRegistry;
begin
  inherited InitRegistry;
  AddClass(TTestProxyPhone);
  AddClass(TTestProxyCity);
  AddClass(TTestProxyPerson);
end;

{ TTestOPFModelProxyInvoice }

function TTestOPFModelProxyInvoice.BuildMetadata(const AClass: TClass): TJCoreClassMetadata;
begin
  Result:=inherited BuildMetadata(AClass);
end;

procedure TTestOPFModelProxyInvoice.InitRegistry;
begin
  inherited InitRegistry;
  AddClass(TClient);
  AddClass(TPerson);
  AddClass(TProduct);
  AddClass(TProduct);
  AddClass(TInvoiceItem);
  AddClass(TInvoiceItemProduct);
  AddClass(TInvoiceItemService);
  AddClass(TInvoice);
end;

end.

