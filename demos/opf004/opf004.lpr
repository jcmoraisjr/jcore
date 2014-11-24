program opf004;

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
  JCoreOPFOID,
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
  if not _proxy.Lazyload(@FItems) then
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
  LOG: IJCoreLogger;
  VConfig: IJCoreOPFConfiguration;
  VSession: IJCoreOPFSession;
  VInvoice: TInvoice;
  I: Integer;

{
  create table invoiceitem (
    id integer,
    invoice integer,
    product varchar(255),
    qty integer
  );
  create table invoice (
    id integer
  );
  alter table invoice
    add constraint pk_invoice primary key (id);
  alter table invoiceitem
    add constraint pk_invoiceitem primary key (id);
  alter table invoiceitem
    add constraint fk_invoiceitem_invoice foreign key (invoice) references invoice(id);
  insert into invoice (id) values (1);
  insert into invoice (id) values (2);
  insert into invoiceitem (id,invoice,product,qty) values (1,1,'bike',3);
  insert into invoiceitem (id,invoice,product,qty) values (2,1,'phone',1);
  insert into invoiceitem (id,invoice,product,qty) values (3,2,'bike',5);
  insert into invoiceitem (id,invoice,product,qty) values (4,2,'phone',2);
  insert into invoiceitem (id,invoice,product,qty) values (5,2,'box',10);
}

begin
  TJCoreDIC.LazyRegister(IJCoreLogFactory, TJCoreConsoleLogFactory, jdsApplication);
  LOG := TJCoreLogger.GetLogger('demos.opf004');
  VConfig := TJCoreOPFConfiguration.Create;
  VConfig.Params.Values['connection'] := 'PostgreSQL';
  VConfig.Params.Values['hostname'] := 'localhost';
  VConfig.Params.Values['database'] := 'jcore';
  VConfig.Params.Values['username'] := 'jcore';
  VConfig.Params.Values['password'] := 'jcore';
  VConfig.DriverClass := TJCoreOPFDriverSQLdb;
  VConfig.AddMappingClass([TJCoreOPFSQLMapping]);
  VConfig.Model.AddClass([TInvoice, TInvoiceItem]);
  VConfig.Model.AddGenerics(TInvoiceItemList, TInvoiceItem);
  VConfig.Model.OIDClass := TJCoreOPFOIDInt64;
  VSession := VConfig.CreateSession;
  LOG.Info('1 ----- now retrieving main object');
  VInvoice := VSession.Retrieve(TInvoice, '2') as TInvoice;
  try
    LOG.Info('2 ----- iterating the list');
    for I := 0 to Pred(VInvoice.Items.Count) do
      LOG.Info(Format('Item %d: qty=%d; product=%s', [
       I, VInvoice.Items[I].Qty, VInvoice.Items[I].Product]));
  finally
    FreeAndNil(VInvoice);
  end;
  LOG.Info('3 ----- nice, now another object');
  VInvoice := VSession.Retrieve(TInvoice, '1') as TInvoice;
  try
    LOG.Info('4 ----- iterating it''s list');
    for I := 0 to Pred(VInvoice.Items.Count) do
      LOG.Info(Format('Item %d: qty=%d; product=%s', [
       I, VInvoice.Items[I].Qty, VInvoice.Items[I].Product]));
  finally
    FreeAndNil(VInvoice);
  end;
end.

