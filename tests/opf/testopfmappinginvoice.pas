unit TestOPFMappingInvoice;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFMetadata,
  TestOPFConfig;

type

  { TClientSQLMapping }

  TClientSQLMapping = class(TTestAbstractSQLManualMapping)
  protected
    function CreateEntityFromDriver: TObject; override;
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TPersonSQLMapping }

  TPersonSQLMapping = class(TClientSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TCompanySQLMapping }

  TCompanySQLMapping = class(TClientSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TProductSQLMapping }

  TProductSQLMapping = class(TTestAbstractSQLManualMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TInvoiceSQLMapping }

  TInvoiceSQLMapping = class(TTestAbstractSQLManualMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteCollectionsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TInvoiceItemSQLMapping }

  TInvoiceItemSQLMapping = class(TTestAbstractSQLManualMapping)
  protected
    function CreateEntityFromDriver: TObject; override;
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TInvoiceItemProductSQLMapping }

  TInvoiceItemProductSQLMapping = class(TInvoiceItemSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TInvoiceItemServiceSQLMapping }

  TInvoiceItemServiceSQLMapping = class(TInvoiceItemSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

implementation

uses
  TestOPFModelInvoice;

{ TClientSQLMapping }

function TClientSQLMapping.CreateEntityFromDriver: TObject;
begin
  if Map.Metadata.TheClass = TClient then
    Result := SelectClassFromDriver([TPerson, TCompany], TClient).Create
  else
    Result := inherited CreateEntityFromDriver;
end;

function TClientSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM CLIENT WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TClientSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO CLIENT (ID,NAME) VALUES (?,?)';
end;

function TClientSQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := 'SELECT T.ID,T_1.ID,T_2.ID,T.NAME FROM CLIENT T LEFT OUTER JOIN PERSON T_1 ON T.ID=T_1.ID LEFT OUTER JOIN COMPANY T_2 ON T.ID=T_2.ID WHERE ' + BuildOIDCondition(['T.ID'], AOIDCount);
end;

function TClientSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE CLIENT SET NAME=? WHERE ID=?';
end;

procedure TClientSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VClient: TClient;
begin
  VClient := AMapping.PID.Entity as TClient;
  VClient.Name := Driver.ReadString;
end;

procedure TClientSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VClient: TClient;
begin
  VClient := AMapping.PID.Entity as TClient;
  Driver.WriteString(VClient.Name);
end;

class function TClientSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TClient;
end;

{ TPersonSQLMapping }

function TPersonSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM PERSON WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TPersonSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO PERSON (ID,NICK) VALUES (?,?)';
end;

function TPersonSQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := 'SELECT T.ID,T_1.NAME,T.NICK FROM PERSON T INNER JOIN CLIENT T_1 ON T.ID=T_1.ID WHERE ' + BuildOIDCondition(['T.ID'], AOIDCount)
end;

function TPersonSQLMapping.GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap;
  const AOIDCount: Integer): string;
begin
  if ABaseMap.Metadata.TheClass = TClient then
    Result := 'SELECT ID,NICK FROM PERSON WHERE ' + BuildOIDCondition(['ID'], AOIDCount)
  else
    Result := inherited GenerateSelectComplementaryStatement(ABaseMap, AOIDCount);
end;

function TPersonSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE PERSON SET NICK=? WHERE ID=?';
end;

procedure TPersonSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TPerson;
begin
  VPerson := AMapping.PID.Entity as TPerson;
  VPerson.Nick := Driver.ReadString;
end;

procedure TPersonSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TPerson;
begin
  VPerson := AMapping.PID.Entity as TPerson;
  Driver.WriteString(VPerson.Nick);
end;

class function TPersonSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TPerson;
end;

{ TCompanySQLMapping }

function TCompanySQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM COMPANY WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TCompanySQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO COMPANY (ID,CONTACTNAME) VALUES (?,?)';
end;

function TCompanySQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := 'SELECT T.ID,T_1.NAME,T.CONTACTNAME FROM COMPANY T INNER JOIN CLIENT T_1 ON T.ID=T_1.ID WHERE ' + BuildOIDCondition(['T.ID'], AOIDCount)
end;

function TCompanySQLMapping.GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap;
  const AOIDCount: Integer): string;
begin
  if ABaseMap.Metadata.TheClass = TClient then
    Result := 'SELECT ID,CONTACTNAME FROM COMPANY WHERE ' + BuildOIDCondition(['ID'], AOIDCount)
  else
    Result := inherited GenerateSelectComplementaryStatement(ABaseMap, AOIDCount);
end;

function TCompanySQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE COMPANY SET CONTACTNAME=? WHERE ID=?';
end;

procedure TCompanySQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VCompany: TCompany;
begin
  VCompany := AMapping.PID.Entity as TCompany;
  VCompany.ContactName := Driver.ReadString;
end;

procedure TCompanySQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VCompany: TCompany;
begin
  VCompany := AMapping.PID.Entity as TCompany;
  Driver.WriteString(VCompany.ContactName);
end;

class function TCompanySQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TCompany;
end;

{ TProductSQLMapping }

function TProductSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM PRODUCT WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TProductSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO PRODUCT (ID,NAME) VALUES (?,?)';
end;

function TProductSQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := 'SELECT ID,NAME FROM PRODUCT WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TProductSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE PRODUCT SET NAME=? WHERE ID=?';
end;

procedure TProductSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VProduct: TProduct;
begin
  VProduct := AMapping.PID.Entity as TProduct;
  VProduct.Name := Driver.ReadString;
end;

procedure TProductSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VProduct: TProduct;
begin
  VProduct := AMapping.PID.Entity as TProduct;
  Driver.WriteString(VProduct.Name);
end;

class function TProductSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TProduct;
end;

{ TInvoiceSQLMapping }

function TInvoiceSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM INVOICE WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TInvoiceSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO INVOICE (ID,CLIENT,DATE) VALUES (?,?,?)';
end;

function TInvoiceSQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := 'SELECT ID,CLIENT,DATE FROM INVOICE WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TInvoiceSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE INVOICE SET CLIENT=?, DATE=? WHERE ID=?';
end;

procedure TInvoiceSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VInvoice: TInvoice;
begin
  VInvoice := AMapping.PID.Entity as TInvoice;
  VInvoice.Client := ReadEntity(TClient) as TClient;
  VInvoice.Date := Driver.ReadString;
end;

procedure TInvoiceSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VInvoice: TInvoice;
begin
  VInvoice := AMapping.PID.Entity as TInvoice;
  WriteEntity(TClient, VInvoice.Client, False);
  Driver.WriteString(VInvoice.Date);
end;

procedure TInvoiceSQLMapping.WriteCollectionsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  WriteCollection(AMapping['Items']);
end;

class function TInvoiceSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TInvoice;
end;

{ TInvoiceItemSQLMapping }

function TInvoiceItemSQLMapping.CreateEntityFromDriver: TObject;
begin
  if Map.Metadata.TheClass = TInvoiceItem then
    Result := SelectClassFromDriver([TInvoiceItemProduct, TInvoiceItemService], TInvoiceItem).Create
  else
    Result := inherited CreateEntityFromDriver;
end;

function TInvoiceItemSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM INVOICEITEM WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TInvoiceItemSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO INVOICEITEM (ID,TOTAL) VALUES (?,?)';
end;

function TInvoiceItemSQLMapping.GenerateSelectCollectionStatement(
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string;
begin
  if AOwnerClass.TheClass = TInvoice then
    Result := 'SELECT T.ID,T_1.ID,T_2.ID,T.TOTAL FROM INVOICEITEM T LEFT OUTER JOIN INVOICEITEMPRODUCT T_1 ON T.ID=T_1.ID LEFT OUTER JOIN INVOICEITEMSERVICE T_2 ON T.ID=T_2.ID WHERE ' + BuildOIDCondition(['T.ID'], 1)
  else
    Result := inherited GenerateSelectCollectionStatement(AOwnerClass, AOwnerAttr);
end;

function TInvoiceItemSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE INVOICEITEM SET TOTAL=? WHERE ID=?';
end;

procedure TInvoiceItemSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VInvoiceItem: TInvoiceItem;
begin
  VInvoiceItem := AMapping.PID.Entity as TInvoiceItem;
  VInvoiceItem.Total := Driver.ReadInt64;
end;

procedure TInvoiceItemSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VInvoiceItem: TInvoiceItem;
begin
  VInvoiceItem := AMapping.PID.Entity as TInvoiceItem;
  Driver.WriteInt32(VInvoiceItem.Total);
end;

class function TInvoiceItemSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TInvoiceItem;
end;

{ TInvoiceItemProductSQLMapping }

function TInvoiceItemProductSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM INVOICEITEMPRODUCT WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TInvoiceItemProductSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO INVOICEITEMPRODUCT (ID,QTY,PRODUCT) VALUES (?,?,?)';
end;

function TInvoiceItemProductSQLMapping.GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap;
  const AOIDCount: Integer): string;
begin
  if ABaseMap.Metadata.TheClass = TInvoiceItem then
    Result := 'SELECT ID,QTY,PRODUCT FROM INVOICEITEMPRODUCT WHERE ' + BuildOIDCondition(['ID'], AOIDCount)
  else
    Result := inherited GenerateSelectComplementaryStatement(ABaseMap, AOIDCount);
end;

function TInvoiceItemProductSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE INVOICEITEMPRODUCT SET QTY=?, PRODUCT=? WHERE ID=?';
end;

procedure TInvoiceItemProductSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VItemProduct: TInvoiceItemProduct;
begin
  VItemProduct := AMapping.PID.Entity as TInvoiceItemProduct;
  VItemProduct.Qty := Driver.ReadInt64;
  VItemProduct.Product := ReadEntity(TProduct) as TProduct;
end;

procedure TInvoiceItemProductSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VItemProduct: TInvoiceItemProduct;
begin
  VItemProduct := AMapping.PID.Entity as TInvoiceItemProduct;
  Driver.WriteInt32(VItemProduct.Qty);
  WriteEntity(TProduct, VItemProduct.Product, False);
end;

class function TInvoiceItemProductSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TInvoiceItemProduct;
end;

{ TInvoiceItemServiceSQLMapping }

function TInvoiceItemServiceSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM INVOICEITEMSERVICE WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TInvoiceItemServiceSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO INVOICEITEMSERVICE (ID,DESCRIPTION) VALUES (?,?)';
end;

function TInvoiceItemServiceSQLMapping.GenerateSelectComplementaryStatement(const ABaseMap: TJCoreOPFMap;
  const AOIDCount: Integer): string;
begin
  if ABaseMap.Metadata.TheClass = TInvoiceItem then
    Result := 'SELECT ID,DESCRIPTION FROM INVOICEITEMSERVICE WHERE ' + BuildOIDCondition(['ID'], AOIDCount)
  else
    Result := inherited GenerateSelectComplementaryStatement(ABaseMap, AOIDCount);
end;

function TInvoiceItemServiceSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE INVOICEITEMSERVICE SET DESCRIPTION=? WHERE ID=?';
end;

procedure TInvoiceItemServiceSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VItemService: TInvoiceItemService;
begin
  VItemService := AMapping.PID.Entity as TInvoiceItemService;
  VItemService.Description := Driver.ReadString;
end;

procedure TInvoiceItemServiceSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VItemService: TInvoiceItemService;
begin
  VItemService := AMapping.PID.Entity as TInvoiceItemService;
  Driver.WriteString(VItemService.Description);
end;

class function TInvoiceItemServiceSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TInvoiceItemService;
end;

end.

