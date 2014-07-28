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
    procedure RefineClassMetadata(const AClassMetadata: TJCoreClassMetadata); override;
    procedure InitRegistry; override;
  end;

  { TTestOPFModelProxyContact }

  TTestOPFModelProxyContact = class(TJCoreOPFModel)
  protected
    procedure InitRegistry; override;
  end;

  { TTestOPFModelProxyInvoice }

  TTestOPFModelProxyInvoice = class(TJCoreOPFModel)
  protected
    procedure InitRegistry; override;
  end;

implementation

uses
  sysutils,
  TestOPFModelContact,
  TestOPFModelInvoice;

{ TTestOPFModelIPIDContact }

procedure TTestOPFModelIPIDContact.RefineClassMetadata(const AClassMetadata: TJCoreClassMetadata);
var
  VMetadata: TJCoreOPFClassMetadata;
begin
  inherited RefineClassMetadata(AClassMetadata);
  if AClassMetadata.TheClass = TTestIPIDPerson then
  begin
    VMetadata := AClassMetadata as TJCoreOPFClassMetadata;
    VMetadata.AttributeByName('Languages').CompositionType := jctAggregation;
    VMetadata.AttributeByName('City').CompositionType := jctAggregation;
  end;
end;

procedure TTestOPFModelIPIDContact.InitRegistry;
begin
  inherited InitRegistry;
  AddClass([TTestIPIDPerson, TTestIPIDPhone, TTestIPIDLanguage, TTestIPIDAddress, TTestIPIDCity]);
end;

{ TTestOPFModelProxyContact }

procedure TTestOPFModelProxyContact.InitRegistry;
begin
  inherited InitRegistry;
  AddClass([TTestProxyPhone, TTestProxyCity, TTestProxyPerson]);
end;

{ TTestOPFModelProxyInvoice }

procedure TTestOPFModelProxyInvoice.InitRegistry;
begin
  inherited InitRegistry;
  AddClass([TClient, TPerson, TCompany, TProduct, TInvoiceItem, TInvoiceItemProduct,
   TInvoiceItemService, TInvoice]);
end;

end.

