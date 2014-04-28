unit TestOPFModelInvoice;

{$mode objfpc}{$H+}

interface

uses
  fgl,
  JCoreClasses,
  JCoreEntity;

type

  { TCustomEntity }

  TCustomEntity = class(TJCoreManagedObject)
    _proxy: TJCoreEntityProxy;
  protected
    procedure Finit; override;
  end;

  { TClient }

  TClient = class(TCustomEntity)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  { TPerson }

  TPerson = class(TClient)
  private
    FNick: string;
  published
    property Nick: string read FNick write FNick;
  end;

  { TCompany }

  TCompany = class(TClient)
  private
    FContactName: string;
  published
    property ContactName: string read FContactName write FContactName;
  end;

  { TProduct }

  TProduct = class(TCustomEntity)
  private
    FName: string;
  published
    property Name: string read FName;
  end;

  { TInvoiceItem }

  TInvoiceItem = class(TCustomEntity)
  end;

  TInvoiceItemList = specialize TFPGObjectList<TInvoiceItem>;

  { TInvoiceItemProduct }

  TInvoiceItemProduct = class(TInvoiceItem)
  private
    FQty: Integer;
    FProduct: TProduct;
    function GetProduct: TProduct;
    procedure SetProduct(AValue: TProduct);
  protected
    procedure Finit; override;
  published
    property Qty: Integer read FQty write FQty;
    property Product: TProduct read GetProduct write SetProduct;
  end;

  { TInvoiceItemService }

  TInvoiceItemService = class(TInvoiceItem)
  private
    FDescription: string;
  published
    property Description: string read FDescription write FDescription;
  end;

  { TInvoice }

  TInvoice = class(TCustomEntity)
  private
    FClient: TClient;
    FDate: TDate;
    FItems: TInvoiceItemList;
    function GetClient: TClient;
    function GetItems: TInvoiceItemList;
    procedure SetClient(AValue: TClient);
    procedure SetItems(AValue: TInvoiceItemList);
  protected
    procedure Finit; override;
  published
    property Client: TClient read GetClient write SetClient;
    property Date: TDate read FDate write FDate;
    property Items: TInvoiceItemList read GetItems write SetItems;
  end;

implementation

uses
  sysutils;

{ TCustomEntity }

procedure TCustomEntity.Finit;
begin
  FreeAndNil(_proxy);
  inherited Finit;
end;

{ TInvoiceItemProduct }

function TInvoiceItemProduct.GetProduct: TProduct;
begin
  _proxy.Lazyload(@FProduct, 'Product');
  Result := FProduct;
end;

procedure TInvoiceItemProduct.SetProduct(AValue: TProduct);
begin
  if FProduct <> AValue then
  begin
    FreeAndNil(FProduct);
    FProduct := AValue;
  end;
end;

procedure TInvoiceItemProduct.Finit;
begin
  FreeAndNil(FProduct);
  inherited Finit;
end;

{ TInvoice }

function TInvoice.GetClient: TClient;
begin
  _proxy.Lazyload(@FClient, 'Client');
  Result := FClient;
end;

function TInvoice.GetItems: TInvoiceItemList;
begin
  _proxy.Lazyload(@FItems, 'Items');
  Result := FItems;
end;

procedure TInvoice.SetClient(AValue: TClient);
begin
  if FClient <> AValue then
  begin
    FreeAndNil(FClient);
    FClient := AValue;
  end;
end;

procedure TInvoice.SetItems(AValue: TInvoiceItemList);
begin
  if FItems <> AValue then
  begin
    FreeAndNil(FItems);
    FItems := AValue;
  end;
end;

procedure TInvoice.Finit;
begin
  FreeAndNil(FClient);
  FreeAndNil(FItems);
  inherited Finit;
end;

end.

