unit TestOPFMappingContact;

{$mode objfpc}{$H+}

interface

uses
  JCoreOPFDriver,
  JCoreOPFMetadata,
  TestOPFConfig;

type

  { TTestIPIDSimpleSQLMapping }

  TTestIPIDSimpleSQLMapping = class(TTestSQLMapping)
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDPersonSQLMapping }

  TTestIPIDPersonSQLMapping = class(TTestSQLMapping)
  protected
    function GenerateDeleteExternalLinkIDsStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateDeleteExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertExternalLinksStatement(const AAttrMetadata: TJCoreOPFAttrMetadata): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectCompositionsForDeleteStatement(const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDCount: Integer): string; override;
    function GenerateSelectForDeleteStatement(const AAttrMetadata: TJCoreOPFAttrMetadata; const AOIDCount: Integer): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteCollections(const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteLinkAttributesToParams(const AOwnerPID, APID: TJCoreOPFPID; const AADM: TJCoreOPFADMCollection; const AParams: IJCoreOPFParams); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestProxyPersonSQLMapping }

  TTestProxyPersonSQLMapping = class(TTestIPIDPersonSQLMapping)
  protected
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteCollections(const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDEmployeeSQLMapping }

  TTestIPIDEmployeeSQLMapping = class(TTestIPIDPersonSQLMapping)
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDAddressSQLMapping }

  TTestIPIDAddressSQLMapping = class(TTestSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDCitySQLMapping }

  TTestIPIDCitySQLMapping = class(TTestSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectBaseStatement(const AOIDCount: Integer): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestProxyCitySQLMapping }

  TTestProxyCitySQLMapping = class(TTestIPIDCitySQLMapping)
  protected
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDPhoneSQLMapping }

  TTestIPIDPhoneSQLMapping = class(TTestSQLMapping)
  protected
    function GenerateDeleteStatement(const AOIDCount: Integer): string; override;
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestProxyPhoneSQLMapping }

  TTestProxyPhoneSQLMapping = class(TTestIPIDPhoneSQLMapping)
  protected
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
  public
    class function Apply(const AMap: TJCoreOPFMap): Boolean; override;
  end;

  { TTestIPIDLanguageSQLMapping }

  TTestIPIDLanguageSQLMapping = class(TTestSQLMapping)
  protected
    function GenerateInsertStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    function GenerateSelectCollectionStatement(const AOwnerClass: TJCoreOPFClassMetadata; const AOwnerAttr: TJCoreOPFAttrMetadata): string; override;
    function GenerateUpdateStatement(const AMapping: TJCoreOPFADMMapping): string; override;
    procedure ReadFromResultSet(const AResultSet: IJCoreOPFResultSet; const AMapping: TJCoreOPFADMMapping); override;
    procedure WriteAttributesToParams(const AParams: IJCoreOPFParams; const AMapping: TJCoreOPFADMMapping); override;
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
  const ACompositionMetadatas: TJCoreOPFAttrMetadataArray; const AOIDCount: Integer): string;
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

procedure TTestIPIDPersonSQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := AMapping.PID.Entity as TTestIPIDPerson;
  VPerson.Name := AResultSet.ReadString;
  VPerson.Age := AResultSet.ReadInt64;
  VPerson.Address := ReadEntity(AResultSet, TTestIPIDAddress) as TTestIPIDAddress;
  VPerson.City := ReadEntity(AResultSet, TTestIPIDCity) as TTestIPIDCity;
  ReadCollection(AMapping['Phones']);
  ReadCollection(AMapping['Languages']);
end;

procedure TTestIPIDPersonSQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := AMapping.PID.Entity as TTestIPIDPerson;
  AParams.WriteString(VPerson.Name);
  AParams.WriteInt32(VPerson.Age);
  WriteEntity(AParams, AMapping['Address']);
  WriteEntity(AParams, TTestIPIDCity, VPerson.City, False);
end;

procedure TTestIPIDPersonSQLMapping.WriteCollections(const AMapping: TJCoreOPFADMMapping);
begin
  WriteCollection(AMapping['Phones']);
  WriteCollection(AMapping['Languages']);
end;

procedure TTestIPIDPersonSQLMapping.WriteLinkAttributesToParams(const AOwnerPID, APID: TJCoreOPFPID;
  const AADM: TJCoreOPFADMCollection; const AParams: IJCoreOPFParams);
begin
  AOwnerPID.OID.WriteToParams(AParams);
  APID.OID.WriteToParams(AParams);
end;

class function TTestIPIDPersonSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDPerson;
end;

{ TTestProxyPersonSQLMapping }

procedure TTestProxyPersonSQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestProxyPerson;
begin
  VPerson := AMapping.PID.Entity as TTestProxyPerson;
  VPerson.Name := AResultSet.ReadString;
  VPerson.Age := AResultSet.ReadInt64;
  AResultSet.ReadNull;
  ReadLazyEntity(AResultSet, AMapping['City']);
end;

procedure TTestProxyPersonSQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VPerson: TTestProxyPerson;
begin
  VPerson := AMapping.PID.Entity as TTestProxyPerson;
  AParams.WriteString(VPerson.Name);
  AParams.WriteInt32(VPerson.Age);
  AParams.WriteNull;
  WriteEntity(AParams, TTestProxyCity, VPerson.City, False);
end;

procedure TTestProxyPersonSQLMapping.WriteCollections(const AMapping: TJCoreOPFADMMapping);
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

procedure TTestIPIDAddressSQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  VAddress: TTestIPIDAddress;
begin
  VAddress := AMapping.PID.Entity as TTestIPIDAddress;
  VAddress.Street := AResultSet.ReadString;
  VAddress.ZipCode := AResultSet.ReadString;
end;

procedure TTestIPIDAddressSQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VAddress: TTestIPIDAddress;
begin
  VAddress := AMapping.PID.Entity as TTestIPIDAddress;
  AParams.WriteString(VAddress.Street);
  AParams.WriteString(VAddress.ZipCode);
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

procedure TTestIPIDCitySQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestIPIDCity;
begin
  VCity := AMapping.PID.Entity as TTestIPIDCity;
  VCity.Name := AResultSet.ReadString;
end;

procedure TTestIPIDCitySQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestIPIDCity;
begin
  VCity := AMapping.PID.Entity as TTestIPIDCity;
  AParams.WriteString(VCity.Name);
end;

class function TTestIPIDCitySQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDCity;
end;

{ TTestProxyCitySQLMapping }

procedure TTestProxyCitySQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestProxyCity;
begin
  VCity := AMapping.PID.Entity as TTestProxyCity;
  VCity.Name := AResultSet.ReadString;
end;

procedure TTestProxyCitySQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VCity: TTestProxyCity;
begin
  VCity := AMapping.PID.Entity as TTestProxyCity;
  AParams.WriteString(VCity.Name);
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

procedure TTestIPIDPhoneSQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestIPIDPhone;
begin
  VPhone := AMapping.PID.Entity as TTestIPIDPhone;
  VPhone.Number := AResultSet.ReadString;
end;

procedure TTestIPIDPhoneSQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestIPIDPhone;
begin
  VPhone := AMapping.PID.Entity as TTestIPIDPhone;
  WriteOwnerOID(AParams, AMapping);
  AParams.WriteString(VPhone.Number);
end;

class function TTestIPIDPhoneSQLMapping.Apply(const AMap: TJCoreOPFMap): Boolean;
begin
  Result := AMap.Metadata.TheClass = TTestIPIDPhone;
end;

{ TTestProxyPhoneSQLMapping }

procedure TTestProxyPhoneSQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestProxyPhone;
begin
  VPhone := AMapping.PID.Entity as TTestProxyPhone;
  VPhone.Number := AResultSet.ReadString;
end;

procedure TTestProxyPhoneSQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VPhone: TTestProxyPhone;
begin
  VPhone := AMapping.PID.Entity as TTestProxyPhone;
  WriteOwnerOID(AParams, AMapping);
  AParams.WriteString(VPhone.Number);
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

procedure TTestIPIDLanguageSQLMapping.ReadFromResultSet(const AResultSet: IJCoreOPFResultSet;
  const AMapping: TJCoreOPFADMMapping);
var
  VLang: TTestIPIDLanguage;
begin
  VLang := AMapping.PID.Entity as TTestIPIDLanguage;
  VLang.Name := AResultSet.ReadString;
end;

procedure TTestIPIDLanguageSQLMapping.WriteAttributesToParams(const AParams: IJCoreOPFParams;
  const AMapping: TJCoreOPFADMMapping);
var
  VLang: TTestIPIDLanguage;
begin
  VLang := AMapping.PID.Entity as TTestIPIDLanguage;
  AParams.WriteString(VLang.Name);
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

