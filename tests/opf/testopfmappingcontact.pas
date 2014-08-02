unit TestOPFMappingContact;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFMetadata,
  TestOPFConfig;

type

  { TTestIPIDSimpleSQLMapping }

  TTestIPIDSimpleSQLMapping = class(TTestAbstractSQLManualMapping)
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDPersonSQLMapping }

  TTestIPIDPersonSQLMapping = class(TTestAbstractSQLManualMapping)
  protected
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectCompositionsForDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteCollectionsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestProxyPersonSQLMapping }

  TTestProxyPersonSQLMapping = class(TTestIPIDPersonSQLMapping)
  protected
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteCollectionsToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDEmployeeSQLMapping }

  TTestIPIDEmployeeSQLMapping = class(TTestIPIDPersonSQLMapping)
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDAddressSQLMapping }

  TTestIPIDAddressSQLMapping = class(TTestAbstractSQLManualMapping)
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

  { TTestIPIDCitySQLMapping }

  TTestIPIDCitySQLMapping = class(TTestAbstractSQLManualMapping)
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

  { TTestProxyCitySQLMapping }

  TTestProxyCitySQLMapping = class(TTestIPIDCitySQLMapping)
  protected
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDPhoneSQLMapping }

  TTestIPIDPhoneSQLMapping = class(TTestAbstractSQLManualMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestProxyPhoneSQLMapping }

  TTestProxyPhoneSQLMapping = class(TTestIPIDPhoneSQLMapping)
  protected
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDLanguageSQLMapping }

  TTestIPIDLanguageSQLMapping = class(TTestAbstractSQLManualMapping)
  protected
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromDriver(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

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
    Result := 'DELETE FROM PERSON_LANG WHERE ID_PERSON=? AND ' + BuildOIDCondition(['ID_LANG'], AOIDCount)
  else
    Result := inherited GenerateDeleteExternalLinkIDsStatement(AAttrMetadata, AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateDeleteExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := 'DELETE FROM PERSON_LANG WHERE ' + BuildOIDCondition(['PERSON'], AOIDCount)
  else
    Result := inherited GenerateDeleteExternalLinksStatement(AAttrMetadata, AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM PERSON WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateInsertExternalLinksStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    Result := 'INSERT INTO PERSON_LANG (ID_PERSON,ID_LANG) VALUES (?,?)'
  else
    Result := inherited GenerateInsertExternalLinksStatement(AAttrMetadata);
end;

function TTestIPIDPersonSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO PERSON (ID,NAME,AGE,ADDRESS,CITY) VALUES (?,?,?,?,?)';
end;

function TTestIPIDPersonSQLMapping.GenerateSelectCompositionsForDeleteStatement(
  const AOIDCount: Integer): string;
begin
  Result := 'SELECT ADDRESS FROM PERSON WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateSelectForDeleteStatement(
  const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string;
begin
  if AAttrMetadata.CompositionClass = TTestIPIDPhone then
    Result := 'SELECT ID FROM PHONE WHERE ' + BuildOIDCondition(['PERSON'], AOIDCount)
  else if AAttrMetadata.CompositionClass = TTestIPIDLanguage then
    { TODO : unused }
    Result := 'SELECT L.ID FROM LANG L INNER JOIN PERSON_LANG PL ON PL.ID_LANG=L.ID WHERE ' + BuildOIDCondition(['PL.ID_PERSON'], AOIDCount)
  else
    Result := inherited GenerateSelectForDeleteStatement(AAttrMetadata, AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := 'SELECT ID,NAME,AGE,ADDRESS,CITY FROM PERSON WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TTestIPIDPersonSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE PERSON SET NAME=?, AGE=?, ADDRESS=?, CITY=? WHERE ID=?';
end;

procedure TTestIPIDPersonSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := AMapping.PID.Entity as TTestIPIDPerson;
  VPerson.Name := Driver.ReadString;
  VPerson.Age := Driver.ReadInt64;
  VPerson.Address := ReadEntity(TTestIPIDAddress) as TTestIPIDAddress;
  VPerson.City := ReadEntity(TTestIPIDCity) as TTestIPIDCity;
  ReadCollection(AMapping['Phones']);
  ReadCollection(AMapping['Languages']);
end;

procedure TTestIPIDPersonSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := AMapping.PID.Entity as TTestIPIDPerson;
  Driver.WriteString(VPerson.Name);
  Driver.WriteInt32(VPerson.Age);
  WriteEntity(AMapping['Address']);
  WriteEntity(TTestIPIDCity, VPerson.City, False);
end;

procedure TTestIPIDPersonSQLMapping.WriteCollectionsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  WriteCollection(AMapping['Phones']);
  WriteCollection(AMapping['Languages']);
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
  VPerson.Age := Driver.ReadInt64;
  Driver.ReadNull; // VPerson.Address := Mapper.RetrieveFromDriver(TTestIPIDAddress, Driver) as TTestIPIDAddress;
  ReadLazyEntity(AMapping['City']);
end;

procedure TTestProxyPersonSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestProxyPerson;
begin
  VPerson := AMapping.PID.Entity as TTestProxyPerson;
  Driver.WriteString(VPerson.Name);
  Driver.WriteInt32(VPerson.Age);
  Driver.WriteNull; // StoreEntity(TTestIPIDAddress, VPerson.Address, Driver);
  WriteEntity(TTestProxyCity, VPerson.City, False);
end;

procedure TTestProxyPersonSQLMapping.WriteCollectionsToDriver(const AMapping: TJCoreOPFADMMapping);
begin
  WriteCollection(AMapping['Phones']);
  WriteCollection(AMapping['Languages']);
end;

class function TTestProxyPersonSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestProxyPerson;
end;

{ TTestIPIDAddressSQLMapping }

function TTestIPIDAddressSQLMapping.GenerateDeleteStatement(const AOIDCount: Integer): string;
begin
  Result := 'DELETE FROM ADDRESS WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TTestIPIDAddressSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO ADDRESS (ID,STREET,ZIPCODE) VALUES (?,?,?)';
end;

function TTestIPIDAddressSQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := 'SELECT ID,STREET,ZIPCODE FROM ADDRESS WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TTestIPIDAddressSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  { TODO : unused }
  Result := 'UPDATE ADDRESS SET STREET=?, ZIPCODE=? WHERE ID=?';
end;

procedure TTestIPIDAddressSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VAddress: TTestIPIDAddress;
begin
  VAddress := AMapping.PID.Entity as TTestIPIDAddress;
  VAddress.Street := Driver.ReadString;
  VAddress.ZipCode := Driver.ReadString;
end;

procedure TTestIPIDAddressSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
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
  Result := 'DELETE FROM CITY WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TTestIPIDCitySQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO CITY (ID,NAME) VALUES (?,?)';
end;

function TTestIPIDCitySQLMapping.GenerateSelectBaseStatement(const AOIDCount: Integer): string;
begin
  Result := 'SELECT ID,NAME FROM CITY WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TTestIPIDCitySQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE CITY SET NAME=? WHERE ID=?';
end;

procedure TTestIPIDCitySQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestIPIDCity;
begin
  VCity := AMapping.PID.Entity as TTestIPIDCity;
  VCity.Name := Driver.ReadString;
end;

procedure TTestIPIDCitySQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
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

procedure TTestProxyCitySQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
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
  Result := 'DELETE FROM PHONE WHERE ' + BuildOIDCondition(['ID'], AOIDCount);
end;

function TTestIPIDPhoneSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO PHONE (ID,PERSON,NUMBER) VALUES (?,?,?)';
end;

function TTestIPIDPhoneSQLMapping.GenerateSelectCollectionStatement(
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string;
begin
  if (AOwnerClass.TheClass = TTestIPIDPerson) or (AOwnerClass.TheClass = TTestProxyPerson) then
    Result := 'SELECT ID,NUMBER FROM PHONE WHERE PERSON=?'
  else
    Result := inherited GenerateSelectCollectionStatement(AOwnerClass, AOwnerAttr);
end;

function TTestIPIDPhoneSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'UPDATE PHONE SET PERSON=?, NUMBER=? WHERE ID=?';
end;

procedure TTestIPIDPhoneSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestIPIDPhone;
begin
  VPhone := AMapping.PID.Entity as TTestIPIDPhone;
  VPhone.Number := Driver.ReadString;
end;

procedure TTestIPIDPhoneSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestIPIDPhone;
begin
  VPhone := AMapping.PID.Entity as TTestIPIDPhone;
  WriteOwnerOID(AMapping);
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

procedure TTestProxyPhoneSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestProxyPhone;
begin
  VPhone := AMapping.PID.Entity as TTestProxyPhone;
  WriteOwnerOID(AMapping);
  Driver.WriteString(VPhone.Number);
end;

class function TTestProxyPhoneSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestProxyPhone;
end;

{ TTestIPIDLanguageSQLMapping }

function TTestIPIDLanguageSQLMapping.GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  Result := 'INSERT INTO LANG (ID,NAME) VALUES (?,?)';
end;

function TTestIPIDLanguageSQLMapping.GenerateSelectCollectionStatement(
  const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string;
begin
  if AOwnerClass.TheClass = TTestIPIDPerson then
    Result := 'SELECT L.ID,L.NAME FROM LANG L INNER JOIN PERSON_LANG PL ON PL.ID_LANG=L.ID WHERE PL.ID_PERSON=?'
  else
    Result := inherited GenerateSelectCollectionStatement(AOwnerClass, AOwnerAttr);
end;

function TTestIPIDLanguageSQLMapping.GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string;
begin
  { TODO : unused }
  Result := 'UPDATE LANG SET NAME=? WHERE ID=?';
end;

procedure TTestIPIDLanguageSQLMapping.ReadFromDriver(const AMapping: TJCoreOPFADMMapping);
var
  VLang: TTestIPIDLanguage;
begin
  VLang := AMapping.PID.Entity as TTestIPIDLanguage;
  VLang.Name := Driver.ReadString;
end;

procedure TTestIPIDLanguageSQLMapping.WriteAttributesToDriver(const AMapping: TJCoreOPFADMMapping);
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

