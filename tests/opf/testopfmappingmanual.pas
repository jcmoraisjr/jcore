unit TestOPFMappingManual;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFDriver,
  JCoreOPFOID,
  JCoreOPFMetadata,
  JCoreOPFMapping;

type

  { TTestAbstractSQLMapping }

  TTestAbstractSQLMapping = class(TJCoreOPFSQLMapping)
  private
    class var FCurrentOID: Integer;
  protected
    function CreateOIDFromDriver(const ADriver: TJCoreOPFDriver): TJCoreOPFOID; override;
    function CreateOIDFromString(const AOID: string): TJCoreOPFOID; override;
    function GenerateOID: Integer;
  public
    class procedure ClearOID;
  end;

  { TTestIPIDSimpleSQLMapping }

  TTestIPIDSimpleSQLMapping = class(TTestAbstractSQLMapping)
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestIPIDPersonSQLMapping }

  TTestIPIDPersonSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string; override;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string; override;
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectCompositionsForDeleteStatement(const AClass: TClass; const ASize: Integer): string; override;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string; override;
    function GenerateSelectStatement(const AClass: TClass): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteExternalsToDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestProxyPersonSQLMapping }

  TTestProxyPersonSQLMapping = class(TTestIPIDPersonSQLMapping)
  protected
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteExternalsToDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestIPIDEmployeeSQLMapping }

  TTestIPIDEmployeeSQLMapping = class(TTestIPIDPersonSQLMapping)
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestIPIDAddressSQLMapping }

  TTestIPIDAddressSQLMapping = class(TTestAbstractSQLMapping)
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

  { TTestIPIDCitySQLMapping }

  TTestIPIDCitySQLMapping = class(TTestAbstractSQLMapping)
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

  { TTestProxyCitySQLMapping }

  TTestProxyCitySQLMapping = class(TTestIPIDCitySQLMapping)
  protected
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestIPIDPhoneSQLMapping }

  TTestIPIDPhoneSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const ASize: Integer): string; override;
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TClass; const AOwnerAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestProxyPhoneSQLMapping }

  TTestProxyPhoneSQLMapping = class(TTestIPIDPhoneSQLMapping)
  protected
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

  { TTestIPIDLanguageSQLMapping }

  TTestIPIDLanguageSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const APID: TJCoreOPFPID): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TClass; const AOwnerAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const APID: TJCoreOPFPID): string; override;
    procedure ReadFromDriver(const APID: TJCoreOPFPID); override;
    procedure WriteInternalsToDriver(const APID: TJCoreOPFPID); override;
  public
    class function Apply(const AClass: TClass): Boolean; override;
  end;

const
  CSQLINSERTADDRESS = 'INSERT INTO ADDRESS (ID,STREET,ZIPCODE) VALUES (?,?,?)';
  CSQLINSERTCITY = 'INSERT INTO CITY (ID,NAME) VALUES (?,?)';
  CSQLINSERTPERSON = 'INSERT INTO PERSON (ID,NAME,AGE,ADDRESS,CITY) VALUES (?,?,?,?,?)';
  CSQLINSERTPHONE = 'INSERT INTO PHONE (ID,PERSON,NUMBER) VALUES (?,?,?)';
  CSQLINSERTLANG = 'INSERT INTO LANG (ID,NAME) VALUES (?,?)';
  CSQLINSERTPERSON_LANG = 'INSERT INTO PERSON_LANG (ID_PERSON,ID_LANG) VALUES (?,?)';
  CSQLSELECTADDRESS = 'SELECT STREET,ZIPCODE FROM ADDRESS WHERE ID=?';
  CSQLSELECTCITY = 'SELECT NAME FROM CITY WHERE ID=?';
  CSQLSELECTPERSON = 'SELECT NAME,AGE,ADDRESS,CITY FROM PERSON WHERE ID=?';
  CSQLSELECTPERSON_PHONES = 'SELECT ID,NUMBER FROM PHONE WHERE PERSON=?';
  CSQLSELECTPERSON_LANG = 'SELECT L.ID,L.NAME FROM LANG L INNER JOIN PERSON_LANG PL ON PL.ID_LANG=L.ID WHERE PL.ID_PERSON=?';
  CSQLSELECTPERSON_FOR_DELETE = 'SELECT ADDRESS FROM PERSON WHERE ID';
  CSQLSELECTPERSON_PHONES_FOR_DELETE = 'SELECT ID FROM PHONE WHERE PERSON';
  CSQLSELECTPERSON_LANG_FOR_DELETE = 'SELECT L.ID FROM LANG L INNER JOIN PERSON_LANG PL ON PL.ID_LANG=L.ID WHERE PL.ID_PERSON';
  CSQLUPDATEADDRESS = 'UPDATE ADDRESS SET STREET=?, ZIPCODE=? WHERE ID=?';
  CSQLUPDATECITY = 'UPDATE CITY SET NAME=? WHERE ID=?';
  CSQLUPDATEPERSON = 'UPDATE PERSON SET NAME=?, AGE=?, ADDRESS=?, CITY=? WHERE ID=?';
  CSQLUPDATEPHONE = 'UPDATE PHONE SET PERSON=?, NUMBER=? WHERE ID=?';
  CSQLUPDATELANG = 'UPDATE LANG SET NAME=? WHERE ID=?';
  CSQLDELETEADDRESS = 'DELETE FROM ADDRESS WHERE ID';
  CSQLDELETECITY = 'DELETE FROM CITY WHERE ID';
  CSQLDELETEPHONE = 'DELETE FROM PHONE WHERE ID';
  CSQLDELETEPERSON = 'DELETE FROM PERSON WHERE ID';
  CSQLDELETEPERSON_LANG = 'DELETE FROM PERSON_LANG WHERE ID_PERSON';
  CSQLDELETEPERSON_LANG_IDs = 'DELETE FROM PERSON_LANG WHERE ID_PERSON=? AND ID_LANG';

implementation

uses
  sysutils,
  TestOPFModelContact;

{ TTestAbstractSQLMapping }

function TTestAbstractSQLMapping.CreateOIDFromDriver(
  const ADriver: TJCoreOPFDriver): TJCoreOPFOID;
begin
  if not ADriver.ReadNull then
    Result := TJCoreOPFIntegerOID.Create(ADriver.ReadInteger)
  else
    Result := nil;
end;

function TTestAbstractSQLMapping.CreateOIDFromString(const AOID: string): TJCoreOPFOID;
var
  VOID: Integer;
begin
  if AOID = '' then
    VOID := GenerateOID
  else
    VOID := StrToInt(AOID);
  Result := TJCoreOPFIntegerOID.Create(VOID);
end;

function TTestAbstractSQLMapping.GenerateOID: Integer;
begin
  Inc(FCurrentOID);
  Result := FCurrentOID;
end;

class procedure TTestAbstractSQLMapping.ClearOID;
begin
  FCurrentOID := 0;
end;

{ TTestIPIDSimpleSQLMapping }

class function TTestIPIDSimpleSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestIPIDSimple;
end;

{ TTestIPIDPersonSQLMapping }

function TTestIPIDPersonSQLMapping.GenerateDeleteExternalLinkIDsStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := CSQLDELETEPERSON_LANG_IDs + BuildParams(ASize)
  else
    Result := inherited GenerateDeleteExternalLinkIDsStatement(AAttrMetadata, ASize);
end;

function TTestIPIDPersonSQLMapping.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := CSQLDELETEPERSON_LANG + BuildParams(ASize)
  else
    Result := inherited GenerateDeleteExternalLinksStatement(AAttrMetadata, ASize);
end;

function TTestIPIDPersonSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEPERSON + BuildParams(ASize);
end;

function TTestIPIDPersonSQLMapping.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := CSQLINSERTPERSON_LANG
  else
    Result := inherited GenerateInsertExternalLinksStatement(AAttrMetadata);
end;

function TTestIPIDPersonSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTPERSON;
end;

function TTestIPIDPersonSQLMapping.GenerateSelectCompositionsForDeleteStatement(
  const AClass: TClass; const ASize: Integer): string;
begin
  Result := CSQLSELECTPERSON_FOR_DELETE + BuildParams(ASize);
end;

function TTestIPIDPersonSQLMapping.GenerateSelectForDeleteStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const ASize: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDPhone then
    Result := CSQLSELECTPERSON_PHONES_FOR_DELETE + BuildParams(ASize)
  else if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := CSQLSELECTPERSON_LANG_FOR_DELETE + BuildParams(ASize)
  else
    Result := inherited GenerateSelectForDeleteStatement(AAttrMetadata, ASize);
end;

function TTestIPIDPersonSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTPERSON;
end;

function TTestIPIDPersonSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATEPERSON;
end;

procedure TTestIPIDPersonSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := APID.Entity as TTestIPIDPerson;
  VPerson.Name := Driver.ReadString;
  VPerson.Age := Driver.ReadInteger;
  VPerson.Address := Mapper.RetrieveFromDriver(TTestIPIDAddress, Driver) as TTestIPIDAddress;
  VPerson.City := Mapper.RetrieveFromDriver(TTestIPIDCity, Driver) as TTestIPIDCity;
  RetrieveList(APID, 'Phones');
  RetrieveList(APID, 'Languages');
end;

procedure TTestIPIDPersonSQLMapping.WriteExternalsToDriver(const APID: TJCoreOPFPID);
begin
  StoreList(APID, 'Phones');
  StoreList(APID, 'Languages');
end;

procedure TTestIPIDPersonSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := APID.Entity as TTestIPIDPerson;
  Driver.WriteString(VPerson.Name);
  Driver.WriteInteger(VPerson.Age);
  Mapper.StoreToDriver(TTestIPIDAddress, VPerson.Address, Driver);
  Mapper.StoreToDriver(TTestIPIDCity, VPerson.City, Driver);
end;

class function TTestIPIDPersonSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestIPIDPerson;
end;

{ TTestProxyPersonSQLMapping }

procedure TTestProxyPersonSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VPerson: TTestProxyPerson;
begin
  VPerson := APID.Entity as TTestProxyPerson;
  VPerson.Name := Driver.ReadString;
  VPerson.Age := Driver.ReadInteger;
  Driver.ReadNull; // VPerson.Address := Mapper.RetrieveFromDriver(TTestIPIDAddress, Driver) as TTestIPIDAddress;
  RetrieveLazy(APID, 'City');
end;

procedure TTestProxyPersonSQLMapping.WriteExternalsToDriver(
  const APID: TJCoreOPFPID);
begin
  StoreList(APID, 'Phones');
  // StoreList(APID, 'Languages');
end;

procedure TTestProxyPersonSQLMapping.WriteInternalsToDriver(
  const APID: TJCoreOPFPID);
var
  VPerson: TTestProxyPerson;
begin
  VPerson := APID.Entity as TTestProxyPerson;
  Driver.WriteString(VPerson.Name);
  Driver.WriteInteger(VPerson.Age);
  Driver.WriteNull; // Mapper.StoreToDriver(TTestIPIDAddress, VPerson.Address, Driver);
  Mapper.StoreToDriver(TTestIPIDCity, VPerson.City, Driver);
end;

class function TTestProxyPersonSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestProxyPerson;
end;

{ TTestIPIDAddressSQLMapping }

function TTestIPIDAddressSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEADDRESS + BuildParams(ASize);
end;

function TTestIPIDAddressSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTADDRESS;
end;

function TTestIPIDAddressSQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTADDRESS;
end;

function TTestIPIDAddressSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATEADDRESS;
end;

procedure TTestIPIDAddressSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VAddress: TTestIPIDAddress;
begin
  VAddress := APID.Entity as TTestIPIDAddress;
  VAddress.Street := Driver.ReadString;
  VAddress.ZipCode := Driver.ReadString;
end;

procedure TTestIPIDAddressSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VAddress: TTestIPIDAddress;
begin
  VAddress := APID.Entity as TTestIPIDAddress;
  Driver.WriteString(VAddress.Street);
  Driver.WriteString(VAddress.ZipCode);
end;

class function TTestIPIDAddressSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestIPIDAddress;
end;

{ TTestIPIDCitySQLMapping }

function TTestIPIDCitySQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETECITY + BuildParams(ASize);
end;

function TTestIPIDCitySQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTCITY;
end;

function TTestIPIDCitySQLMapping.GenerateSelectStatement(const AClass: TClass): string;
begin
  Result := CSQLSELECTCITY;
end;

function TTestIPIDCitySQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATECITY;
end;

procedure TTestIPIDCitySQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VCity: TTestIPIDCity;
begin
  VCity := APID.Entity as TTestIPIDCity;
  VCity.Name := Driver.ReadString;
end;

procedure TTestIPIDCitySQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VCity: TTestIPIDCity;
begin
  VCity := APID.Entity as TTestIPIDCity;
  Driver.WriteString(VCity.Name);
end;

class function TTestIPIDCitySQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestIPIDCity;
end;

{ TTestProxyCitySQLMapping }

procedure TTestProxyCitySQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VCity: TTestProxyCity;
begin
  VCity := APID.Entity as TTestProxyCity;
  VCity.Name := Driver.ReadString;
end;

procedure TTestProxyCitySQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VCity: TTestProxyCity;
begin
  VCity := APID.Entity as TTestProxyCity;
  Driver.WriteString(VCity.Name);
end;

class function TTestProxyCitySQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestProxyCity;
end;

{ TTestIPIDPhoneSQLMapping }

function TTestIPIDPhoneSQLMapping.GenerateDeleteStatement(const ASize: Integer): string;
begin
  Result := CSQLDELETEPHONE + BuildParams(ASize);
end;

function TTestIPIDPhoneSQLMapping.GenerateInsertStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTPHONE;
end;

function TTestIPIDPhoneSQLMapping.GenerateSelectCollectionStatement(
  const AOwnerClass: TClass; const AOwnerAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if (AOwnerClass = TTestIPIDPerson) or (AOwnerClass = TTestProxyPerson) then
    Result := CSQLSELECTPERSON_PHONES
  else
    Result := inherited GenerateSelectCollectionStatement(AOwnerClass, AOwnerAttrMetadata);
end;

function TTestIPIDPhoneSQLMapping.GenerateUpdateStatement(const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATEPHONE;
end;

procedure TTestIPIDPhoneSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VPhone: TTestIPIDPhone;
begin
  VPhone := APID.Entity as TTestIPIDPhone;
  VPhone.Number := Driver.ReadString;
end;

procedure TTestIPIDPhoneSQLMapping.WriteInternalsToDriver(const APID: TJCoreOPFPID);
var
  VPhone: TTestIPIDPhone;
begin
  VPhone := APID.Entity as TTestIPIDPhone;
  APID.Owner.OID.WriteToDriver(Driver);
  Driver.WriteString(VPhone.Number);
end;

class function TTestIPIDPhoneSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestIPIDPhone;
end;

{ TTestProxyPhoneSQLMapping }

procedure TTestProxyPhoneSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VPhone: TTestProxyPhone;
begin
  VPhone := APID.Entity as TTestProxyPhone;
  VPhone.Number := Driver.ReadString;
end;

procedure TTestProxyPhoneSQLMapping.WriteInternalsToDriver(
  const APID: TJCoreOPFPID);
var
  VPhone: TTestProxyPhone;
begin
  VPhone := APID.Entity as TTestProxyPhone;
  APID.Owner.OID.WriteToDriver(Driver);
  Driver.WriteString(VPhone.Number);
end;

class function TTestProxyPhoneSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestProxyPhone;
end;

{ TTestIPIDLanguageSQLMapping }

function TTestIPIDLanguageSQLMapping.GenerateInsertStatement(
  const APID: TJCoreOPFPID): string;
begin
  Result := CSQLINSERTLANG;
end;

function TTestIPIDLanguageSQLMapping.GenerateSelectCollectionStatement(
  const AOwnerClass: TClass; const AOwnerAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if AOwnerClass = TTestIPIDPerson then
    Result := CSQLSELECTPERSON_LANG
  else
    Result := inherited GenerateSelectCollectionStatement(AOwnerClass, AOwnerAttrMetadata);
end;

function TTestIPIDLanguageSQLMapping.GenerateUpdateStatement(
  const APID: TJCoreOPFPID): string;
begin
  Result := CSQLUPDATELANG;
end;

procedure TTestIPIDLanguageSQLMapping.ReadFromDriver(const APID: TJCoreOPFPID);
var
  VLang: TTestIPIDLanguage;
begin
  VLang := APID.Entity as TTestIPIDLanguage;
  VLang.Name := Driver.ReadString;
end;

procedure TTestIPIDLanguageSQLMapping.WriteInternalsToDriver(
  const APID: TJCoreOPFPID);
var
  VLang: TTestIPIDLanguage;
begin
  VLang := APID.Entity as TTestIPIDLanguage;
  Driver.WriteString(VLang.Name);
end;

class function TTestIPIDLanguageSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestIPIDLanguage;
end;

{ TTestIPIDEmployeeSQLMapping }

class function TTestIPIDEmployeeSQLMapping.Apply(const AClass: TClass): Boolean;
begin
  Result := AClass = TTestIPIDEmployee;
end;

end.

