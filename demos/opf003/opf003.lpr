program opf003;

{$mode objfpc}{$H+}

uses
  heaptrc,
  sysutils,
  fgl,
  pqconnection,
  JCoreDIC,
  JCoreLogger,
  JCoreEntity,
  JCoreOPFConfig,
  JCoreOPFSession,
  JCoreOPFMappingSQL,
  JCoreOPFDriverSQLdb;

type

  { TInvoiceItem }

  TInvoiceItem = class(TJCoreEntity)
  private
    FProduct: string;
    FQty: Integer;
  published
    property Qty: Integer read FQty write FQty;
    property Product: string read FProduct write FProduct;
  end;

  TInvoiceItemList = specialize TFPGObjectList<TInvoiceItem>;

  TInvoice = class(TJCoreEntity)
  private
    FItems: TInvoiceItemList;
    function GetItems: TInvoiceItemList;
    procedure SetItems(AValue: TInvoiceItemList);
  public
    destructor Destroy; override;
  published
    property Items: TInvoiceItemList read GetItems write SetItems;
  end;

function TInvoice.GetItems: TInvoiceItemList;
begin
  if not _proxy.Lazyload(FItems) then
    FItems := TInvoiceItemList.Create(True);
  Result := FItems;
end;

procedure TInvoice.SetItems(AValue: TInvoiceItemList);
begin
  FreeAndNil(FItems);
  FItems := AValue;
end;

destructor TInvoice.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

var
  VConfig: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VInvoice: TInvoice;
  VInvoiceItem: TInvoiceItem;

{
  create table invoiceitem (
    id varchar(32),
    invoice varchar(32),
    product varchar(255),
    qty integer
  );
  create table invoice (
    id varchar(32)
  );
  alter table invoice
    add constraint pk_invoice primary key (id);
  alter table invoiceitem
    add constraint pk_invoiceitem primary key (id);
  alter table invoiceitem
    add constraint fk_invoiceitem_invoice foreign key (invoice) references invoice(id);
}

begin
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreConsoleLogFactory, jdsApplication);
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Params.Values['connection'] := 'PostgreSQL';
  VConfig.Params.Values['hostname'] := 'localhost';
  VConfig.Params.Values['database'] := 'jcore';
  VConfig.Params.Values['username'] := 'jcore';
  VConfig.Params.Values['password'] := 'jcore';
  VConfig.DriverClass := TJCoreOPFDriverSQLdb;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.AddClass([TInvoice, TInvoiceItem]);
  VSession := VConfig.CreateSession;
  VInvoice := TInvoice.Create;
  try
    VInvoiceItem := TInvoiceItem.Create;
    VInvoice.Items.Add(VInvoiceItem);
    VInvoiceItem.Product := 'bike';
    VInvoiceItem.Qty := 3;
    VInvoiceItem := TInvoiceItem.Create;
    VInvoice.Items.Add(VInvoiceItem);
    VInvoiceItem.Product := 'phone';
    VInvoiceItem.Qty := 1;
    VSession.Store(VInvoice);
  finally
    FreeAndNil(VInvoice);
  end;
end.

