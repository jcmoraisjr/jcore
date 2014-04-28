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

const
  CSQLINSERTINVOICECLIENT = 'INSERT INTO CLIENT (ID,NAME) VALUES (?,?)';
  CSQLINSERTINVOICEPERSON = 'INSERT INTO PERSON (ID,NICK) VALUES (?,?)';
  CSQLINSERTINVOICECOMPANY = 'INSERT INTO COMPANY (ID,CONTACT) VALUES (?,?)';
  CSQLSELECTINVOICECLIENT = 'SELECT P.ID,CO.ID,CL.NAME,P.NICK,CO.CONTACT FROM CLIENT CL LEFT OUTER JOIN PERSON P ON CL.ID=P.ID LEFT OUTER JOIN COMPANY CO WHERE CL.ID=CO.ID WHERE CL.ID=?';
  CSQLSELECTINVOICEPERSON = 'SELECT CL.NAME,P.NICK FROM CLIENT CL INNER JOIN PERSON P ON CL.ID=P.ID WHERE P.ID=?';
  CSQLSELECTINVOICECOMPANY = 'SELECT CL.NAME,CO.CONTACT FROM CLIENT CL INNER JOIN COMPANY CO ON CL.ID=CO.ID WHERE CO.ID=?';
  CSQLUPDATEINVOICECLIENT = 'UPDATE CLIENT SET NAME=? WHERE ID=?';
  CSQLUPDATEINVOICEPERSON = 'UPDATE PERSON SET NICK=? WHERE ID=?';
  CSQLUPDATEINVOICECOMPANY = 'UPDATE COMPANY SET CONTACT=? WHERE ID=?';
  CSQLDELETEINVOICECLIENT = 'DELETE FROM CLIENT WHERE ID';
  CSQLDELETEINVOICEPERSON = 'DELETE FROM PERSON WHERE ID';
  CSQLDELETEINVOICECOMPANY = 'DELETE FROM COMPANY WHERE ID';

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

end.

