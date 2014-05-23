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
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TPersonSQLMapping }

  TPersonSQLMapping = class(TClientSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TCompanySQLMapping }

  TCompanySQLMapping = class(TClientSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TProductSQLMapping }

  TProductSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TInvoiceSQLMapping }

  TInvoiceSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteExternalsToDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TInvoiceItemSQLMapping }

  TInvoiceItemSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TInvoiceItemProductSQLMapping }

  TInvoiceItemProductSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TInvoiceItemServiceSQLMapping }

  TInvoiceItemServiceSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
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
  CSQLUPDATEINVOICECOMPANY = 'UPDATE COMPANY SET CONTACT=? WHERE ID=?';
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

function TClientSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTINVOICECLIENT;
end;

function TClientSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTINVOICECLIENT;
end;

function TClientSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
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

procedure TClientSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VClient: TClient;
begin
  VClient := APID.Entity as TClient;
  Driver.WriteString(VClient.Name);
end;

class function TClientSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TClient;
end;

{ TPersonSQLMapping }

function TPersonSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEPERSON + BuildParams(ASize);
end;

function TPersonSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTINVOICEPERSON;
end;

function TPersonSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTINVOICEPERSON;
end;

function TPersonSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
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

procedure TPersonSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VPerson: TPerson;
begin
  VPerson := APID.Entity as TPerson;
  Driver.WriteString(VPerson.Nick);
end;

class function TPersonSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TPerson;
end;

{ TCompanySQLMapping }

function TCompanySQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICECOMPANY + BuildParams(ASize);
end;

function TCompanySQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTINVOICECOMPANY;
end;

function TCompanySQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTINVOICECOMPANY;
end;

function TCompanySQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
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

procedure TCompanySQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VCompany: TCompany;
begin
  VCompany := APID.Entity as TCompany;
  Driver.WriteString(VCompany.ContactName);
end;

class function TCompanySQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TCompany;
end;

{ TProductSQLMapping }

function TProductSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEPRODUCT + BuildParams(ASize);
end;

function TProductSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTINVOICEPRODUCT;
end;

function TProductSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
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

procedure TProductSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VProduct: TProduct;
begin
  VProduct := APID.Entity as TProduct;
  Driver.WriteString(VProduct.Name);
end;

class function TProductSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TProduct;
end;

{ TInvoiceSQLMapping }

function TInvoiceSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEINVOICE + BuildParams(ASize);
end;

function TInvoiceSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTINVOICEINVOICE;
end;

function TInvoiceSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATEINVOICEINVOICE;
end;

procedure TInvoiceSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VInvoice: TInvoice;
begin
  VInvoice := APID.Entity as TInvoice;
  VInvoice.Client := Mapper.RetrieveFromDriver(TClient, Driver) as TClient;
  VInvoice.Date := Driver.ReadString;
end;

procedure TInvoiceSQLMapping.WriteExternalsToDriver(const APID: TJCoreOPFPID);
begin
  StoreList(APID, 'Items');
end;

procedure TInvoiceSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VInvoice: TInvoice;
begin
  VInvoice := APID.Entity as TInvoice;
  Mapper.StoreToDriver(TClient, VInvoice.Client, Driver);
  Driver.WriteString(VInvoice.Date);
end;

class function TInvoiceSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TInvoice;
end;

{ TInvoiceItemSQLMapping }

function TInvoiceItemSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEINVOICEITEM + BuildParams(ASize);
end;

function TInvoiceItemSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTINVOICEINVOICEITEM;
end;


function TInvoiceItemSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
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

procedure TInvoiceItemSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VInvoiceItem: TInvoiceItem;
begin
  VInvoiceItem := APID.Entity as TInvoiceItem;
  Driver.WriteInteger(VInvoiceItem.Total);
end;

class function TInvoiceItemSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TInvoiceItem;
end;

{ TInvoiceItemProductSQLMapping }

function TInvoiceItemProductSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEINVOICEITEMPRODUCT + BuildParams(ASize);
end;

function TInvoiceItemProductSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTINVOICEINVOICEITEMPRODUCT;
end;

function TInvoiceItemProductSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATEINVOICEINVOICEITEMPRODUCT;
end;

procedure TInvoiceItemProductSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VItemProduct: TInvoiceItemProduct;
begin
  VItemProduct := APID.Entity as TInvoiceItemProduct;
  VItemProduct.Qty := Driver.ReadInteger;
  VItemProduct.Product := Mapper.RetrieveFromDriver(TProduct, Driver) as TProduct;
end;

procedure TInvoiceItemProductSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VItemProduct: TInvoiceItemProduct;
begin
  VItemProduct := APID.Entity as TInvoiceItemProduct;
  Driver.WriteInteger(VItemProduct.Qty);
  Mapper.StoreToDriver(TProduct, VItemProduct.Product, Driver);
end;

class function TInvoiceItemProductSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TInvoiceItemProduct;
end;

{ TInvoiceItemServiceSQLMapping }

function TInvoiceItemServiceSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEINVOICEINVOICEITEMSERVICE + BuildParams(ASize);
end;

function TInvoiceItemServiceSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTINVOICEINVOICEITEMSERVICE;
end;

function TInvoiceItemServiceSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
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

procedure TInvoiceItemServiceSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VItemService: TInvoiceItemService;
begin
  VItemService := APID.Entity as TInvoiceItemService;
  Driver.WriteString(VItemService.Description);
end;

class function TInvoiceItemServiceSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TInvoiceItemService;
end;

end.

