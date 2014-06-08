unit TestOPFMappingInvoice;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFMetadata,
  TestOPFMapping;

type

  { TClientSQLMapping }

  TClientSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TPersonSQLMapping }

  TPersonSQLMapping = class(TClientSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TCompanySQLMapping }

  TCompanySQLMapping = class(TClientSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TProductSQLMapping }

  TProductSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TInvoiceSQLMapping }

  TInvoiceSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TInvoiceItemSQLMapping }

  TInvoiceItemSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TInvoiceItemProductSQLMapping }

  TInvoiceItemProductSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TInvoiceItemServiceSQLMapping }

  TInvoiceItemServiceSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

const
  CSQLINSERTINVOICECLIENT = 'INSERT INTO CLIENT (ID,NAME) VALUES (?,?)';
  CSQLINSERTINVOICEPERSON = 'INSERT INTO PERSON (ID,NICK) VALUES (?,?)';
  CSQLINSERTINVOICECOMPANY = 'INSERT INTO COMPANY (ID,CONTACT) VALUES (?,?)';
  CSQLINSERTINVOICEPRODUCT = 'INSERT INTO PRODUCT (ID,NAME) VALUES (?,?)';
  CSQLINSERTINVOICEINVOICE = 'INSERT INTO INVOICE (ID,CLIENT,DATE) VALUES (?,?,?)';
  CSQLINSERTINVOICEINVOICEITEM = 'INSERT INTO INVOICEITEM (ID,TOTAL) VALUES (?,?)';
  CSQLINSERTINVOICEINVOICEITEMPRODUCT = 'INSERT INTO INVOICEITEMPRODUCT (ID,QTY,PRODUCT) VALUES (?,?,?)';
  CSQLINSERTINVOICEINVOICEITEMSERVICE = 'INSERT INTO INVOICEITEMSERVICE (ID,DESCRIPTION) VALUES (?,?)';
  CSQLSELECTINVOICECLIENT = 'SELECT P.ID,CO.ID,CL.NAME,P.NICK,CO.CONTACT FROM CLIENT CL LEFT OUTER JOIN PERSON P ON CL.ID=P.ID LEFT OUTER JOIN COMPANY CO WHERE CL.ID=CO.ID WHERE CL.ID=?';
  CSQLSELECTINVOICEPERSON = 'SELECT CL.NAME,P.NICK FROM CLIENT CL INNER JOIN PERSON P ON CL.ID=P.ID WHERE P.ID=?';
  CSQLSELECTINVOICECOMPANY = 'SELECT CL.NAME,CO.CONTACT FROM CLIENT CL INNER JOIN COMPANY CO ON CL.ID=CO.ID WHERE CO.ID=?';
  CSQLUPDATEINVOICECLIENT = 'UPDATE CLIENT SET NAME=? WHERE ID=?';
  CSQLUPDATEINVOICEPERSON = 'UPDATE PERSON SET NICK=? WHERE ID=?';
  CSQLUPDATEINVOICECOMPANY = 'UPDATE COMPANY SET CONTACTNAME=? WHERE ID=?';
  CSQLUPDATEINVOICEPRODUCT = 'UPDATE PRODUCT SET NAME=? WHERE ID=?';
  CSQLUPDATEINVOICEINVOICE = 'UPDATE INVOICE SET CLIENT=?, DATE=? WHERE ID=?';
  CSQLUPDATEINVOICEINVOICEITEM = 'UPDATE INVOICEITEM SET TOTAL=? WHERE ID=?';
  CSQLUPDATEINVOICEINVOICEITEMPRODUCT = 'UPDATE INVOICEITEMPRODUCT SET QTY=?, PRODUCT=? WHERE ID=?';
  CSQLUPDATEINVOICEINVOICEITEMSERVICE = 'UPDATE INVOICEITEMSERVICE SET DESCRIPTION=? WHERE ID=?';
  CSQLDELETEINVOICECLIENT = 'DELETE FROM CLIENT WHERE ID';
  CSQLDELETEINVOICEPERSON = 'DELETE FROM PERSON WHERE ID';
  CSQLDELETEINVOICECOMPANY = 'DELETE FROM COMPANY WHERE ID';
  CSQLDELETEINVOICEPRODUCT = 'DELETE FROM PRODUCT WHERE ID';
  CSQLDELETEINVOICEINVOICE = 'DELETE FROM INVOICE WHERE ID';
  CSQLDELETEINVOICEINVOICEITEM = 'DELETE FROM INVOICEITEM WHERE ID';
  CSQLDELETEINVOICEINVOICEITEMPRODUCT = 'DELETE FROM INVOICEITEMPRODUCT WHERE ID';
  CSQLDELETEINVOICEINVOICEITEMSERVICE = 'DELETE FROM INVOICEITEMSERVICE WHERE ID';

implementation

uses
  TestOPFModelInvoice;

{ TClientSQLMapping }

function TClientSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICECLIENT + BuildParams(ASize);
end;

function TClientSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTINVOICECLIENT;
end;

function TClientSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTINVOICECLIENT;
end;

function TClientSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEINVOICECLIENT;
end;

procedure TClientSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VClient: TClient;
begin
  VClient := APID.Entity as TClient;
  VClient.Name := Driver.ReadString;
end;

procedure TClientSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
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

function TPersonSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEPERSON + BuildParams(ASize);
end;

function TPersonSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTINVOICEPERSON;
end;

function TPersonSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTINVOICEPERSON;
end;

function TPersonSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEINVOICEPERSON;
end;

procedure TPersonSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VPerson: TPerson;
begin
  VPerson := APID.Entity as TPerson;
  VPerson.Nick := Driver.ReadString;
end;

procedure TPersonSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
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

function TCompanySQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICECOMPANY + BuildParams(ASize);
end;

function TCompanySQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTINVOICECOMPANY;
end;

function TCompanySQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTINVOICECOMPANY;
end;

function TCompanySQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEINVOICECOMPANY;
end;

procedure TCompanySQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VCompany: TCompany;
begin
  VCompany := APID.Entity as TCompany;
  VCompany.ContactName := Driver.ReadString;
end;

procedure TCompanySQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
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

function TProductSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEPRODUCT + BuildParams(ASize);
end;

function TProductSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTINVOICEPRODUCT;
end;

function TProductSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEINVOICEPRODUCT;
end;

procedure TProductSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VProduct: TProduct;
begin
  VProduct := APID.Entity as TProduct;
  VProduct.Name := Driver.ReadString;
end;

procedure TProductSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
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

function TInvoiceSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEINVOICE + BuildParams(ASize);
end;

function TInvoiceSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTINVOICEINVOICE;
end;

function TInvoiceSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEINVOICEINVOICE;
end;

procedure TInvoiceSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VInvoice: TInvoice;
begin
  VInvoice := APID.Entity as TInvoice;
  VInvoice.Client := RetrieveEntity(TClient, Driver) as TClient;
  VInvoice.Date := Driver.ReadString;
end;

procedure TInvoiceSQLMapping.WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  StoreList(AMapping, 'Items');
end;

procedure TInvoiceSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VInvoice: TInvoice;
begin
  VInvoice := AMapping.PID.Entity as TInvoice;
  StoreEntity(TClient, VInvoice.Client, Driver);
  Driver.WriteString(VInvoice.Date);
end;

class function TInvoiceSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TInvoice;
end;

{ TInvoiceItemSQLMapping }

function TInvoiceItemSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEINVOICEITEM + BuildParams(ASize);
end;

function TInvoiceItemSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTINVOICEINVOICEITEM;
end;


function TInvoiceItemSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEINVOICEINVOICEITEM;
end;

procedure TInvoiceItemSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VInvoiceItem: TInvoiceItem;
begin
  VInvoiceItem := APID.Entity as TInvoiceItem;
  VInvoiceItem.Total := Driver.ReadInteger;
end;

procedure TInvoiceItemSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VInvoiceItem: TInvoiceItem;
begin
  VInvoiceItem := AMapping.PID.Entity as TInvoiceItem;
  Driver.WriteInteger(VInvoiceItem.Total);
end;

class function TInvoiceItemSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TInvoiceItem;
end;

{ TInvoiceItemProductSQLMapping }

function TInvoiceItemProductSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEINVOICEITEMPRODUCT + BuildParams(ASize);
end;

function TInvoiceItemProductSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTINVOICEINVOICEITEMPRODUCT;
end;

function TInvoiceItemProductSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEINVOICEINVOICEITEMPRODUCT;
end;

procedure TInvoiceItemProductSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VItemProduct: TInvoiceItemProduct;
begin
  VItemProduct := APID.Entity as TInvoiceItemProduct;
  VItemProduct.Qty := Driver.ReadInteger;
  VItemProduct.Product := RetrieveEntity(TProduct, Driver) as TProduct;
end;

procedure TInvoiceItemProductSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VItemProduct: TInvoiceItemProduct;
begin
  VItemProduct := AMapping.PID.Entity as TInvoiceItemProduct;
  Driver.WriteInteger(VItemProduct.Qty);
  StoreEntity(TProduct, VItemProduct.Product, Driver);
end;

class function TInvoiceItemProductSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TInvoiceItemProduct;
end;

{ TInvoiceItemServiceSQLMapping }

function TInvoiceItemServiceSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEINVOICEITEMSERVICE + BuildParams(ASize);
end;

function TInvoiceItemServiceSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTINVOICEINVOICEITEMSERVICE;
end;

function TInvoiceItemServiceSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEINVOICEINVOICEITEMSERVICE;
end;

procedure TInvoiceItemServiceSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VItemService: TInvoiceItemService;
begin
  VItemService := APID.Entity as TInvoiceItemService;
  VItemService.Description := Driver.ReadString;
end;

procedure TInvoiceItemServiceSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
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

