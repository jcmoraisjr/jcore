unit TestOPFMappingContact;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFMetadata,
  TestOPFMapping;

type

  { TTestIPIDSimpleSQLMapping }

  TTestIPIDSimpleSQLMapping = class(TTestAbstractSQLMapping)
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDPersonSQLMapping }

  TTestIPIDPersonSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectCompositionsForDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateSelectStatement(const ABaseMap: TJCoreOPFMap; const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestProxyPersonSQLMapping }

  TTestProxyPersonSQLMapping = class(TTestIPIDPersonSQLMapping)
  protected
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDEmployeeSQLMapping }

  TTestIPIDEmployeeSQLMapping = class(TTestIPIDPersonSQLMapping)
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDAddressSQLMapping }

  TTestIPIDAddressSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectStatement(const ABaseMap: TJCoreOPFMap; const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDCitySQLMapping }

  TTestIPIDCitySQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectStatement(const ABaseMap: TJCoreOPFMap; const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestProxyCitySQLMapping }

  TTestProxyCitySQLMapping = class(TTestIPIDCitySQLMapping)
  protected
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDPhoneSQLMapping }

  TTestIPIDPhoneSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectStatement(const ABaseMap: TJCoreOPFMap; const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestProxyPhoneSQLMapping }

  TTestProxyPhoneSQLMapping = class(TTestIPIDPhoneSQLMapping)
  protected
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDLanguageSQLMapping }

  TTestIPIDLanguageSQLMapping = class(TTestAbstractSQLMapping)
  protected
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectStatement(const ABaseMap: TJCoreOPFMap; const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

const
  CSQLINSERTADDRESS = 'INSERT INTO ADDRESS (ID,STREET,ZIPCODE) VALUES (?,?,?)';
  CSQLINSERTCITY = 'INSERT INTO CITY (ID,NAME) VALUES (?,?)';
  CSQLINSERTPERSON = 'INSERT INTO PERSON (ID,NAME,AGE,ADDRESS,CITY) VALUES (?,?,?,?,?)';
  CSQLINSERTPHONE = 'INSERT INTO PHONE (ID,PERSON,NUMBER) VALUES (?,?,?)';
  CSQLINSERTLANG = 'INSERT INTO LANG (ID,NAME) VALUES (?,?)';
  CSQLINSERTPERSON_LANG = 'INSERT INTO PERSON_LANG (ID_PERSON,ID_LANG) VALUES (?,?)';
  CSQLSELECTADDRESS = 'SELECT ID,STREET,ZIPCODE FROM ADDRESS WHERE ID';
  CSQLSELECTCITY = 'SELECT ID,NAME FROM CITY WHERE ID';
  CSQLSELECTPERSON = 'SELECT ID,NAME,AGE,ADDRESS,CITY FROM PERSON WHERE ID';
  CSQLSELECTPERSON_PHONES = 'SELECT ID,NUMBER FROM PHONE WHERE PERSON=?';
  CSQLSELECTPERSON_LANG = 'SELECT L.ID,L.NAME FROM LANG L INNER JOIN PERSON_LANG PL ON PL.ID_LANG=L.ID WHERE PL.ID_PERSON=?';
  CSQLSELECTPERSON_FOR_DELETE = 'SELECT ID,ADDRESS FROM PERSON WHERE ID';
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
  TestOPFModelContact;

{ TTestIPIDSimpleSQLMapping }

class function TTestIPIDSimpleSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDSimple;
end;

{ TTestIPIDPersonSQLMapping }

function TTestIPIDPersonSQLMapping.GenerateDeleteExternalLinkIDsStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := CSQLDELETEPERSON_LANG_IDs + BuildParams(AOIDCount)
  else
    Result := inherited GenerateDeleteExternalLinkIDsStatement(AAttrMetadata, AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := CSQLDELETEPERSON_LANG + BuildParams(AOIDCount)
  else
    Result := inherited GenerateDeleteExternalLinksStatement(AAttrMetadata, AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := CSQLDELETEPERSON + BuildParams(AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := CSQLINSERTPERSON_LANG
  else
    Result := inherited GenerateInsertExternalLinksStatement(AAttrMetadata);
end;

function TTestIPIDPersonSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTPERSON;
end;

function TTestIPIDPersonSQLMapping.GenerateSelectCompositionsForDeleteStatement(
  const AOIDCount: Integer): string;
begin
  Result := CSQLSELECTPERSON_FOR_DELETE + BuildParams(AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateSelectForDeleteStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDPhone then
    Result := CSQLSELECTPERSON_PHONES_FOR_DELETE + BuildParams(AOIDCount)
  else if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := CSQLSELECTPERSON_LANG_FOR_DELETE + BuildParams(AOIDCount)
  else
    Result := inherited GenerateSelectForDeleteStatement(AAttrMetadata, AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateSelectStatement(const ABaseMap: TJCoreOPFMap;
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
begin
  Result := CSQLSELECTPERSON + BuildParams(AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEPERSON;
end;

procedure TTestIPIDPersonSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := AMapping.PID.Entity as TTestIPIDPerson;
  VPerson.Name := Driver.ReadString;
  VPerson.Age := Driver.ReadInteger;
  VPerson.Address := ReadEntity(TTestIPIDAddress) as TTestIPIDAddress;
  VPerson.City := ReadEntity(TTestIPIDCity) as TTestIPIDCity;
  ReadCollection(AMapping.PID, 'Phones');
  ReadCollection(AMapping.PID, 'Languages');
end;

procedure TTestIPIDPersonSQLMapping.WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  WriteCollection(AMapping, 'Phones');
  WriteCollection(AMapping, 'Languages');
end;

procedure TTestIPIDPersonSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := AMapping.PID.Entity as TTestIPIDPerson;
  Driver.WriteString(VPerson.Name);
  Driver.WriteInteger(VPerson.Age);
  WriteEntity(TTestIPIDAddress, VPerson.Address);
  WriteEntity(TTestIPIDCity, VPerson.City);
end;

class function TTestIPIDPersonSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDPerson;
end;

{ TTestProxyPersonSQLMapping }

procedure TTestProxyPersonSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestProxyPerson;
begin
  VPerson := AMapping.PID.Entity as TTestProxyPerson;
  VPerson.Name := Driver.ReadString;
  VPerson.Age := Driver.ReadInteger;
  Driver.ReadNull; // VPerson.Address := Mapper.RetrieveFromDriver(TTestIPIDAddress, Driver) as TTestIPIDAddress;
  ReadLazyEntity(AMapping.PID, 'City');
end;

procedure TTestProxyPersonSQLMapping.WriteExternalsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  WriteCollection(AMapping, 'Phones');
  WriteCollection(AMapping, 'Languages');
end;

procedure TTestProxyPersonSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestProxyPerson;
begin
  VPerson := AMapping.PID.Entity as TTestProxyPerson;
  Driver.WriteString(VPerson.Name);
  Driver.WriteInteger(VPerson.Age);
  Driver.WriteNull; // StoreEntity(TTestIPIDAddress, VPerson.Address, Driver);
  WriteEntity(TTestIPIDCity, VPerson.City);
end;

class function TTestProxyPersonSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestProxyPerson;
end;

{ TTestIPIDAddressSQLMapping }

function TTestIPIDAddressSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := CSQLDELETEADDRESS + BuildParams(AOIDCount);
end;

function TTestIPIDAddressSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTADDRESS;
end;

function TTestIPIDAddressSQLMapping.GenerateSelectStatement(const ABaseMap: TJCoreOPFMap;
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
begin
  Result := CSQLSELECTADDRESS + BuildParams(AOIDCount);
end;

function TTestIPIDAddressSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEADDRESS;
end;

procedure TTestIPIDAddressSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VAddress: TTestIPIDAddress;
begin
  VAddress := AMapping.PID.Entity as TTestIPIDAddress;
  VAddress.Street := Driver.ReadString;
  VAddress.ZipCode := Driver.ReadString;
end;

procedure TTestIPIDAddressSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VAddress: TTestIPIDAddress;
begin
  VAddress := AMapping.PID.Entity as TTestIPIDAddress;
  Driver.WriteString(VAddress.Street);
  Driver.WriteString(VAddress.ZipCode);
end;

class function TTestIPIDAddressSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDAddress;
end;

{ TTestIPIDCitySQLMapping }

function TTestIPIDCitySQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := CSQLDELETECITY + BuildParams(AOIDCount);
end;

function TTestIPIDCitySQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTCITY;
end;

function TTestIPIDCitySQLMapping.GenerateSelectStatement(const ABaseMap: TJCoreOPFMap;
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
begin
  Result := CSQLSELECTCITY + BuildParams(AOIDCount);
end;

function TTestIPIDCitySQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATECITY;
end;

procedure TTestIPIDCitySQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestIPIDCity;
begin
  VCity := AMapping.PID.Entity as TTestIPIDCity;
  VCity.Name := Driver.ReadString;
end;

procedure TTestIPIDCitySQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestIPIDCity;
begin
  VCity := AMapping.PID.Entity as TTestIPIDCity;
  Driver.WriteString(VCity.Name);
end;

class function TTestIPIDCitySQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDCity;
end;

{ TTestProxyCitySQLMapping }

procedure TTestProxyCitySQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestProxyCity;
begin
  VCity := AMapping.PID.Entity as TTestProxyCity;
  VCity.Name := Driver.ReadString;
end;

procedure TTestProxyCitySQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestProxyCity;
begin
  VCity := AMapping.PID.Entity as TTestProxyCity;
  Driver.WriteString(VCity.Name);
end;

class function TTestProxyCitySQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestProxyCity;
end;

{ TTestIPIDPhoneSQLMapping }

function TTestIPIDPhoneSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := CSQLDELETEPHONE + BuildParams(AOIDCount);
end;

function TTestIPIDPhoneSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTPHONE;
end;

function TTestIPIDPhoneSQLMapping.GenerateSelectStatement(const ABaseMap: TJCoreOPFMap;
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
begin
  if (AOwnerClass.TheClass = TTestIPIDPerson) or (AOwnerClass.TheClass = TTestProxyPerson) then
    Result := CSQLSELECTPERSON_PHONES
  else
    Result := inherited GenerateSelectStatement(ABaseMap, AOwnerClass, AOwnerAttr, AOIDCount);
end;

function TTestIPIDPhoneSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATEPHONE;
end;

procedure TTestIPIDPhoneSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestIPIDPhone;
begin
  VPhone := AMapping.PID.Entity as TTestIPIDPhone;
  VPhone.Number := Driver.ReadString;
end;

procedure TTestIPIDPhoneSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestIPIDPhone;
begin
  VPhone := AMapping.PID.Entity as TTestIPIDPhone;
  WriteOwnerOIDToDriver(AMapping);
  Driver.WriteString(VPhone.Number);
end;

class function TTestIPIDPhoneSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDPhone;
end;

{ TTestProxyPhoneSQLMapping }

procedure TTestProxyPhoneSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestProxyPhone;
begin
  VPhone := AMapping.PID.Entity as TTestProxyPhone;
  VPhone.Number := Driver.ReadString;
end;

procedure TTestProxyPhoneSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestProxyPhone;
begin
  VPhone := AMapping.PID.Entity as TTestProxyPhone;
  WriteOwnerOIDToDriver(AMapping);
  Driver.WriteString(VPhone.Number);
end;

class function TTestProxyPhoneSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestProxyPhone;
end;

{ TTestIPIDLanguageSQLMapping }

function TTestIPIDLanguageSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLINSERTLANG;
end;

function TTestIPIDLanguageSQLMapping.GenerateSelectStatement(const ABaseMap: TJCoreOPFMap;
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata;
  const AOIDCount: Integer): string;
begin
  if AOwnerClass.TheClass = TTestIPIDPerson then
    Result := CSQLSELECTPERSON_LANG
  else
    Result := inherited GenerateSelectStatement(ABaseMap, AOwnerClass, AOwnerAttr, AOIDCount);
end;

function TTestIPIDLanguageSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := CSQLUPDATELANG;
end;

procedure TTestIPIDLanguageSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VLang: TTestIPIDLanguage;
begin
  VLang := AMapping.PID.Entity as TTestIPIDLanguage;
  VLang.Name := Driver.ReadString;
end;

procedure TTestIPIDLanguageSQLMapping.WriteInternalsToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VLang: TTestIPIDLanguage;
begin
  VLang := AMapping.PID.Entity as TTestIPIDLanguage;
  Driver.WriteString(VLang.Name);
end;

class function TTestIPIDLanguageSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDLanguage;
end;

{ TTestIPIDEmployeeSQLMapping }

class function TTestIPIDEmployeeSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDEmployee;
end;

end.

