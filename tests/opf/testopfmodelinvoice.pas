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

  { TAddress }

  TAddress = class(TCustomEntity)
  private
    FStreet: string;
  published
    property Street: string read FStreet write FStreet;
  end;

  { TClient }

  TClient = class(TCustomEntity)
  private
    FName: string;
    FAddress: TAddress;
    function GetAddress: TAddress;
    procedure SetAddress(AValue: TAddress);
  protected
    procedure Finit; override;
  published
    property Name: string read FName write FName;
    property Address: TAddress read GetAddress write SetAddress;
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
    property Name: string read FName write FName;
  end;

  { TInvoiceItem }

  TInvoiceItem = class(TCustomEntity)
  private
    FTotal: Integer;
  published
    property Total: Integer read FTotal write FTotal;
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
    property Product: TProduct read GetProduct write SetProduct stored False;
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
    FDate: string;
    FItems: TInvoiceItemList;
    function GetClient: TClient;
    function GetItems: TInvoiceItemList;
    procedure SetClient(AValue: TClient);
    procedure SetItems(AValue: TInvoiceItemList);
  protected
    procedure Finit; override;
  published
    property Client: TClient read GetClient write SetClient stored False;
    property Date: string read FDate write FDate;
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

{ TClient }

function TClient.GetAddress: TAddress;
begin
  if not _proxy.Lazyload(@FAddress) then
    FAddress := TAddress.Create;
  Result := FAddress;
end;

procedure TClient.SetAddress(AValue: TAddress);
begin
  if FAddress <> AValue then
  begin
    FreeAndNil(FAddress);
    FAddress := AValue;
  end;
end;

procedure TClient.Finit;
begin
  FreeAndNil(FAddress);
  inherited Finit;
end;

{ TInvoiceItemProduct }

function TInvoiceItemProduct.GetProduct: TProduct;
begin
  _proxy.Lazyload(@FProduct);
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
  _proxy.Lazyload(@FClient);
  Result := FClient;
end;

function TInvoice.GetItems: TInvoiceItemList;
begin
  if not _proxy.Lazyload(@FItems) then
    FItems := TInvoiceItemList.Create(True);
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

