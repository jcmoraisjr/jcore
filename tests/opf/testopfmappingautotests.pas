unit TestOPFMappingAutoTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig;

type

  { TTestOPFInsertAutoMappingTests }

  TTestOPFInsertAutoMappingTests = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure Single;
    procedure Inheritance;
    procedure CircularModel;
    procedure CompositionNull;
    procedure CompositionSingle1;
    procedure CompositionSingle2;
    procedure CompositionCollection;
    procedure AggregationCollection;
  end;

  { TTestOPFSelectAutoMappingTests }

  TTestOPFSelectAutoMappingTests = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure Single;
    procedure SingleWithCollection;
    procedure ObjectNotFound;
    procedure Inheritance;
    procedure InheritanceFromParent;
    procedure CircularModel;
    procedure EagerCollectionAggregation;
    procedure LazyEntityComposition;
    procedure LazyEntityAggregation;
    procedure LazyCollectionComposition;
  end;

  { TTestOPFUpdateAutoMappingTests }

  TTestOPFUpdateAutoMappingTests = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure Single;
    procedure InheritanceUpdateParentMap;
    procedure InheritanceUpdateSubMap;
    procedure InheritanceUpdateBothMaps;
    procedure CompositionChangeItemParentMap;
    procedure CompositionChangeItemSubMap;
    procedure CompositionChangeItemBothMaps;
    procedure CompositionAdd;
    procedure CompositionRemove;
    procedure CompositionAddRemove;
    procedure CompositionChangeOrder;
    procedure AggregationAdd;
    procedure AggregationRemove;
    procedure AggregationAddRemove;
    procedure AggregationAddRemove2;
    procedure AggregationChangeOrder;
  end;

  { TTestOPFDeleteAutoMappingTests }

  TTestOPFDeleteAutoMappingTests = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure SingleFromEntity;
    procedure SingleFromClass;
    procedure InheritanceFromEntity;
    procedure InheritanceFromClass;
    procedure InheritanceFromParentClass;
    procedure InheritanceFromClassWithOIDArray;
    procedure EntityComposition;
    procedure CollectionComposition;
  end;

implementation

uses
  testregistry,
  sysutils,
  JCoreClasses,
  TestOPFModelContact,
  TestOPFModelInvoice,
  TestOPFModelCircular;

{ TTestOPFInsertAutoMappingTests }

procedure TTestOPFInsertAutoMappingTests.Single;
var
  VProduct: TProduct;
begin
  VProduct := TProduct.Create;
  try
    VProduct.Name := 'aprod';
    Session.Store(VProduct);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteString aprod',
     'ExecSQL INSERT INTO PRODUCT (ID,NAME) VALUES (?,?)']);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFInsertAutoMappingTests.Inheritance;
var
  VCompany: TCompany;
begin
  VCompany := TCompany.Create;
  try
    VCompany.Name := 'corp';
    VCompany.ContactName := 'james';
    Session.Store(VCompany);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteString corp',
     'WriteNull',
     'ExecSQL INSERT INTO CLIENT (ID,NAME,ADDRESS) VALUES (?,?,?)',
     'WriteInt64 1',
     'WriteString james',
     'ExecSQL INSERT INTO COMPANY (ID,CONTACTNAME) VALUES (?,?)']);
  finally
    FreeAndNil(VCompany);
  end;
end;

procedure TTestOPFInsertAutoMappingTests.CircularModel;
var
  VPerson: TCircularPerson;
  VDependent: TCircularPerson;
begin
  VPerson := TCircularPerson.Create;
  try
    VPerson.Name := 'jack';
    VDependent := TCircularPerson.Create;
    VPerson.Dependent.Add(VDependent);
    VDependent.Name := 'joe';
    VDependent := TCircularPerson.Create;
    VPerson.Dependent.Add(VDependent);
    VDependent.Name := 'jane';
    SessionCircularAuto.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteString jack',
     'ExecSQL INSERT INTO CIRCULARPERSON (ID,NAME) VALUES (?,?)',
     'WriteInt64 2',
     'WriteString joe',
     'ExecSQL INSERT INTO CIRCULARPERSON (ID,NAME) VALUES (?,?)',
     'WriteInt64 3',
     'WriteString jane',
     'ExecSQL INSERT INTO CIRCULARPERSON (ID,NAME) VALUES (?,?)',
     'WriteInt64 1',
     'WriteInt64 2',
     'WriteInt32 1',
     'ExecSQL INSERT INTO CIRCULARPERSON_DEPENDENT (CIRCULARPERSON,CIRCULARPERSON_DEPENDENT,SEQ) VALUES (?,?,?)',
     'WriteInt64 1',
     'WriteInt64 3',
     'WriteInt32 2',
     'ExecSQL INSERT INTO CIRCULARPERSON_DEPENDENT (CIRCULARPERSON,CIRCULARPERSON_DEPENDENT,SEQ) VALUES (?,?,?)']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFInsertAutoMappingTests.CompositionNull;
var
  VInvoice: TInvoice;
begin
  VInvoice := TInvoice.Create;
  try
    VInvoice.Date := '02/01';
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteNull',
     'WriteString 02/01',
     'ExecSQL INSERT INTO INVOICE (ID,CLIENT,DATE) VALUES (?,?,?)']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFInsertAutoMappingTests.CompositionSingle1;
var
  VInvoice: TInvoice;
  VClient: TClient;
begin
  VInvoice := TInvoice.Create;
  try
    VClient := TClient.Create;
    VInvoice.Client := VClient;
    VClient.Name := 'bar sa';
    Session.Store(VClient);
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt64 2',
     'WriteInt64 1',
     'WriteString ',
     'ExecSQL INSERT INTO INVOICE (ID,CLIENT,DATE) VALUES (?,?,?)']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFInsertAutoMappingTests.CompositionSingle2;
var
  VInvoice: TInvoice;
begin
  VInvoice := TInvoice.Create;
  try
    VInvoice.Client := TClient.Create;
    VInvoice.Client.Name := 'bar sa';
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt64 2',
     'WriteString bar sa',
     'WriteNull',
     'ExecSQL INSERT INTO CLIENT (ID,NAME,ADDRESS) VALUES (?,?,?)',
     'WriteInt64 1',
     'WriteInt64 2',
     'WriteString ',
     'ExecSQL INSERT INTO INVOICE (ID,CLIENT,DATE) VALUES (?,?,?)']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFInsertAutoMappingTests.CompositionCollection;
var
  VInvoice: TInvoice;
  VItemProduct: TInvoiceItemProduct;
  VItemService: TInvoiceItemService;
  VProduct: TProduct;
begin
  VInvoice := TInvoice.Create;
  try
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemService := TInvoiceItemService.Create;
    VInvoice.Items.Add(VItemService);
    VProduct := TProduct.Create;
    VItemProduct.Product := VProduct;
    VProduct.Name := 'orange';
    VItemProduct.Qty := 5;
    VItemProduct.Total := 50;
    VItemService.Description := 'transport';
    VItemService.Total := 20;
    Session.Store(VProduct);
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt64 2',
     'WriteNull',
     'WriteString ',
     'ExecSQL INSERT INTO INVOICE (ID,CLIENT,DATE) VALUES (?,?,?)',
     'WriteInt64 3',
     'WriteInt32 1',
     'WriteInt64 2',
     'WriteInt32 50',
     'ExecSQL INSERT INTO INVOICEITEM (ID,SEQ,INVOICE,TOTAL) VALUES (?,?,?,?)',
     'WriteInt64 3',
     'WriteInt32 5',
     'WriteInt64 1',
     'ExecSQL INSERT INTO INVOICEITEMPRODUCT (ID,QTY,PRODUCT) VALUES (?,?,?)',
     'WriteInt64 4',
     'WriteInt32 2',
     'WriteInt64 2',
     'WriteInt32 20',
     'ExecSQL INSERT INTO INVOICEITEM (ID,SEQ,INVOICE,TOTAL) VALUES (?,?,?,?)',
     'WriteInt64 4',
     'WriteString transport',
     'ExecSQL INSERT INTO INVOICEITEMSERVICE (ID,DESCRIPTION) VALUES (?,?)'
     ]);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFInsertAutoMappingTests.AggregationCollection;
var
  VPerson: TTestIPIDPerson;
  VLang: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VLang := TTestIPIDLanguage.Create('en');
    VPerson.Languages.Add(VLang);
    SessionIPIDContactAuto.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteString ',
     'WriteInt32 0',
     'WriteNull',
     'WriteNull',
     'ExecSQL INSERT INTO TESTIPIDPERSON (ID,NAME,AGE,ADDRESS,CITY) VALUES (?,?,?,?,?)',
     'WriteInt64 2',
     'WriteString en',
     'ExecSQL INSERT INTO TESTIPIDLANGUAGE (ID,NAME) VALUES (?,?)',
     'WriteInt64 1',
     'WriteInt64 2',
     'WriteInt32 1',
     'ExecSQL INSERT INTO TESTIPIDPERSON_LANGUAGES (TESTIPIDPERSON,TESTIPIDLANGUAGE,SEQ) VALUES (?,?,?)']);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFSelectAutoMappingTests }

procedure TTestOPFSelectAutoMappingTests.Single;
var
  VProduct: TProduct;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('6');
  TTestSQLDriver.Data.Add('theprod');
  VProduct := Session.Retrieve(TProduct, '6') as TProduct;
  try
    AssertNotNull('prod nil', VProduct);
    AssertEquals('prod id', '6', VProduct._proxy.OID.AsString);
    AssertEquals('prod name', 'theprod', VProduct.Name);
    AssertSQLDriverCommands([
     'WriteInt64 6',
     {1}'ExecSQL SELECT ID,NAME FROM PRODUCT WHERE ID=?']);
    AssertEquals('expr cnt', 0, TTestSQLDriver.ExpectedResultsets.Count);
    AssertEquals('data cnt', 0, TTestSQLDriver.Data.Count);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.SingleWithCollection;
var
  VInvoice: TInvoice;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('2');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('');
  {2(lazy)}TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('3');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('1');
  TTestSQLDriver.Data.Add('50');
  TTestSQLDriver.Data.Add('4');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('2');
  TTestSQLDriver.Data.Add('75');
  VInvoice := Session.Retrieve(TInvoice, '2') as TInvoice;
  try
    AssertSQLDriverCommands([
     'WriteInt64 2',
     {1}'ExecSQL SELECT ID,CLIENT,DATE FROM INVOICE WHERE ID=?']);
    AssertEquals('invoice.items.count', 2, VInvoice.Items.Count);
    AssertEquals('invoice.items[0].total', 50, VInvoice.Items[0].Total);
    AssertEquals('invoice.items[1].total', 75, VInvoice.Items[1].Total);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.ObjectNotFound;
var
  VCompany: TObject;
begin
  TTestSQLDriver.ExpectedResultsets.Add(0);
  VCompany := nil;
  try
    try
      VCompany := Session.Retrieve(TCompany, '1');
      Fail('EJCoreOPF(2114) expected');
    except
      on E: EJCoreOPF do
        if E.Code <> 2114 then
          raise;
    end;
  finally
    FreeAndNil(VCompany);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.Inheritance;
var
  VCompany: TCompany;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('6');
  TTestSQLDriver.Data.Add('acorp');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('jack');
  VCompany := Session.Retrieve(TCompany, '6') as TCompany;
  try
    AssertNotNull('comp nil', VCompany);
    AssertEquals('comp id', '6', VCompany._proxy.OID.AsString);
    AssertEquals('comp name', 'acorp', VCompany.Name);
    AssertEquals('comp contact', 'jack', VCompany.ContactName);
    AssertSQLDriverCommands([
     'WriteInt64 6',
     {1}'ExecSQL SELECT T_0.ID,T_0.NAME,T_0.ADDRESS,T_1.CONTACTNAME FROM CLIENT T_0 INNER JOIN COMPANY T_1 ON T_0.ID=T_1.ID WHERE T_0.ID=?']);
  finally
    FreeAndNil(VCompany);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.InheritanceFromParent;
var
  VClient: TClient;
  VCompany: TCompany;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('12');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('12');
  TTestSQLDriver.Data.Add('thecorp');
  TTestSQLDriver.Data.Add('null');
  {2}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('12');
  TTestSQLDriver.Data.Add('joe');
  VClient := Session.Retrieve(TClient, '12') as TClient;
  try
    AssertNotNull('client nil', VClient);
    AssertTrue('is client TCompany', VClient is TCompany);
    VCompany := VClient as TCompany;
    AssertEquals('comp id', '12', VCompany._proxy.OID.AsString);
    AssertEquals('comp name', 'thecorp', VCompany.Name);
    AssertEquals('comp contact', 'joe', VCompany.ContactName);
    AssertSQLDriverCommands([
     'WriteInt64 12',
     {1}'ExecSQL SELECT T_0.ID,TS_0.ID,TS_1.ID,T_0.NAME,T_0.ADDRESS FROM CLIENT T_0 LEFT OUTER JOIN PERSON TS_0 ON T_0.ID=TS_0.ID LEFT OUTER JOIN COMPANY TS_1 ON T_0.ID=TS_1.ID WHERE T_0.ID=?',
     'WriteInt64 12',
     {2}'ExecSQL SELECT ID,CONTACTNAME FROM COMPANY WHERE ID=?']);
  finally
    FreeAndNil(VClient);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.CircularModel;

  procedure ReadDependents(const APerson: TCircularPerson);
  var
    I: Integer;
  begin
    for I := 0 to Pred(APerson.Dependent.Count) do
      ReadDependents(APerson.Dependent[I]);
  end;

var
  VPerson: TCircularPerson;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('1');
  TTestSQLDriver.Data.Add('joe');
  {2}TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('2');
  TTestSQLDriver.Data.Add('jack');
  TTestSQLDriver.Data.Add('3');
  TTestSQLDriver.Data.Add('jane');
  {3}TTestSQLDriver.ExpectedResultsets.Add(0);
  {4}TTestSQLDriver.ExpectedResultsets.Add(0);
  VPerson := SessionCircularAuto.Retrieve(TCircularPerson, '1') as TCircularPerson;
  try
    ReadDependents(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     {1}'ExecSQL SELECT ID,NAME FROM CIRCULARPERSON WHERE ID=?',
     'WriteInt64 1',
     {2}'ExecSQL SELECT T_0.ID,T_0.NAME FROM CIRCULARPERSON T_0 INNER JOIN CIRCULARPERSON_DEPENDENT TL_0 ON T_0.ID=TL_0.CIRCULARPERSON_DEPENDENT WHERE TL_0.CIRCULARPERSON=?',
     'WriteInt64 2',
     {3}'ExecSQL SELECT T_0.ID,T_0.NAME FROM CIRCULARPERSON T_0 INNER JOIN CIRCULARPERSON_DEPENDENT TL_0 ON T_0.ID=TL_0.CIRCULARPERSON_DEPENDENT WHERE TL_0.CIRCULARPERSON=?',
     'WriteInt64 3',
     {4}'ExecSQL SELECT T_0.ID,T_0.NAME FROM CIRCULARPERSON T_0 INNER JOIN CIRCULARPERSON_DEPENDENT TL_0 ON T_0.ID=TL_0.CIRCULARPERSON_DEPENDENT WHERE TL_0.CIRCULARPERSON=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.EagerCollectionAggregation;
var
  VPerson: TTestIPIDPerson;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('1');
  TTestSQLDriver.Data.Add('');
  TTestSQLDriver.Data.Add('0');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  {2}TTestSQLDriver.ExpectedResultsets.Add(0);
  {3}TTestSQLDriver.ExpectedResultsets.Add(0);
  VPerson := SessionIPIDContactAuto.Retrieve(TTestIPIDPerson, '1') as TTestIPIDPerson;
  try
    AssertSQLDriverCommands([
     'WriteInt64 1',
     {1}'ExecSQL SELECT ID,NAME,AGE,ADDRESS,CITY FROM TESTIPIDPERSON WHERE ID=?',
     'WriteInt64 1',
     {2}'ExecSQL SELECT ID,SEQ,NUMBER FROM TESTIPIDPHONE WHERE TESTIPIDPERSON=?',
     'WriteInt64 1',
     {3}'ExecSQL SELECT T_0.ID,T_0.NAME FROM TESTIPIDLANGUAGE T_0 INNER JOIN TESTIPIDPERSON_LANGUAGES TL_0 ON T_0.ID=TL_0.TESTIPIDLANGUAGE WHERE TL_0.TESTIPIDPERSON=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.LazyEntityComposition;
var
  VCompany: TCompany;
  VStreet: string;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('9');
  TTestSQLDriver.Data.Add('');
  TTestSQLDriver.Data.Add('11');
  TTestSQLDriver.Data.Add('');
  VCompany := Session.Retrieve(TCompany, '9') as TCompany;
  try
    {2}TTestSQLDriver.ExpectedResultsets.Add(1);
    TTestSQLDriver.Data.Add('11');
    TTestSQLDriver.Data.Add('freeway');
    TTestSQLDriver.Commands.Clear;
    VStreet := VCompany.Address.Street;
    AssertSQLDriverCommands([
     'WriteInt64 11',
     {2}'ExecSQL SELECT ID,STREET FROM ADDRESS WHERE ID=?']);
    AssertEquals('company.address.street', 'freeway', VStreet);
  finally
    FreeAndNil(VCompany);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.LazyEntityAggregation;
var
  VInvoice: TInvoice;
  VClient: TClient;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('7');
  TTestSQLDriver.Data.Add('5');
  TTestSQLDriver.Data.Add('');
  VInvoice := Session.Retrieve(TInvoice, '7') as TInvoice;
  try
    {2}TTestSQLDriver.ExpectedResultsets.Add(1);
    TTestSQLDriver.Data.Add('5');
    TTestSQLDriver.Data.Add('5');
    TTestSQLDriver.Data.Add('null');
    TTestSQLDriver.Data.Add('joseph');
    TTestSQLDriver.Data.Add('null');
    {3}TTestSQLDriver.ExpectedResultsets.Add(1);
    TTestSQLDriver.Data.Add('5');
    TTestSQLDriver.Data.Add('joe');
    TTestSQLDriver.Commands.Clear;
    VClient := VInvoice.Client;
    AssertSQLDriverCommands([
     'WriteInt64 5',
     {2}'ExecSQL SELECT T_0.ID,TS_0.ID,TS_1.ID,T_0.NAME,T_0.ADDRESS FROM CLIENT T_0 LEFT OUTER JOIN PERSON TS_0 ON T_0.ID=TS_0.ID LEFT OUTER JOIN COMPANY TS_1 ON T_0.ID=TS_1.ID WHERE T_0.ID=?',
     'WriteInt64 5',
     {3}'ExecSQL SELECT ID,NICK FROM PERSON WHERE ID=?']);
    AssertEquals('invoice.client class', 'TPerson', VClient.ClassName);
    AssertEquals('invoice.client.name', 'joseph', VClient.Name);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFSelectAutoMappingTests.LazyCollectionComposition;
var
  VInvoice: TInvoice;
  VItems: TInvoiceItemList;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('6');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('');
  VInvoice := Session.Retrieve(TInvoice, '6') as TInvoice;
  try
    {2}TTestSQLDriver.ExpectedResultsets.Add(1);
    TTestSQLDriver.Data.Add('7');
    TTestSQLDriver.Data.Add('7');
    TTestSQLDriver.Data.Add('null');
    TTestSQLDriver.Data.Add('1');
    TTestSQLDriver.Data.Add('199');
    {3}TTestSQLDriver.ExpectedResultsets.Add(1);
    TTestSQLDriver.Data.Add('7');
    TTestSQLDriver.Data.Add('1');
    TTestSQLDriver.Data.Add('null');
    TTestSQLDriver.Commands.Clear;
    VItems := VInvoice.Items;
    AssertSQLDriverCommands([
     'WriteInt64 6',
     {2}'ExecSQL SELECT T_0.ID,TS_0.ID,TS_1.ID,T_0.SEQ,T_0.TOTAL FROM INVOICEITEM T_0 LEFT OUTER JOIN INVOICEITEMPRODUCT TS_0 ON T_0.ID=TS_0.ID LEFT OUTER JOIN INVOICEITEMSERVICE TS_1 ON T_0.ID=TS_1.ID WHERE T_0.INVOICE=?',
     'WriteInt64 7',
     {3}'ExecSQL SELECT ID,QTY,PRODUCT FROM INVOICEITEMPRODUCT WHERE ID=?']);
    AssertEquals('invoice.items count', 1, VItems.Count);
    AssertEquals('invoice.items[0].total', 199, VItems[0].Total);
  finally
    FreeAndNil(VInvoice);
  end;
end;

{ TTestOPFUpdateAutoMappingTests }

procedure TTestOPFUpdateAutoMappingTests.Single;
var
  VProduct: TProduct;
begin
  VProduct := TProduct.Create;
  try
    VProduct.Name := 'aprod';
    Session.Store(VProduct);
    TTestSQLDriver.Commands.Clear;
    VProduct.Name := 'newprod';
    Session.Store(VProduct);
    AssertSQLDriverCommands([
     'WriteString newprod',
     'WriteInt64 1',
     'ExecSQL UPDATE PRODUCT SET NAME=? WHERE ID=?']);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.InheritanceUpdateParentMap;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'james';
    VPerson.Nick := 'j';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Name := 'j.james';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString j.james',
     'WriteInt64 1',
     'ExecSQL UPDATE CLIENT SET NAME=? WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.InheritanceUpdateSubMap;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'j.james';
    VPerson.Nick := 'j';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Nick := 'jj';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString jj',
     'WriteInt64 1',
     'ExecSQL UPDATE PERSON SET NICK=? WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.InheritanceUpdateBothMaps;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'james';
    VPerson.Nick := 'j';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    VPerson.Name := 'j.james';
    VPerson.Nick := 'jj';
    Session.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteString j.james',
     'WriteInt64 1',
     'ExecSQL UPDATE CLIENT SET NAME=? WHERE ID=?',
     'WriteString jj',
     'WriteInt64 1',
     'ExecSQL UPDATE PERSON SET NICK=? WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.CompositionChangeItemParentMap;
var
  VInvoice: TInvoice;
  VItemProduct: TInvoiceItemProduct;
begin
  VInvoice := TInvoice.Create;
  try
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 2;
    VItemProduct.Total := 10;
    Session.Store(VInvoice);
    VItemProduct.Total := 20;
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt32 20',
     'WriteInt64 2',
     'ExecSQL UPDATE INVOICEITEM SET TOTAL=? WHERE ID=?']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.CompositionChangeItemSubMap;
var
  VInvoice: TInvoice;
  VItemProduct: TInvoiceItemProduct;
begin
  VInvoice := TInvoice.Create;
  try
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 5;
    VItemProduct.Total := 15;
    Session.Store(VInvoice);
    VItemProduct.Qty := 3;
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt32 3',
     'WriteInt64 2',
     'ExecSQL UPDATE INVOICEITEMPRODUCT SET QTY=? WHERE ID=?']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.CompositionChangeItemBothMaps;
var
  VInvoice: TInvoice;
  VItemProduct: TInvoiceItemProduct;
begin
  VInvoice := TInvoice.Create;
  try
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 1;
    VItemProduct.Total := 10;
    Session.Store(VInvoice);
    VItemProduct.Qty := 10;
    VItemProduct.Total := 100;
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt32 100',
     'WriteInt64 2',
     'ExecSQL UPDATE INVOICEITEM SET TOTAL=? WHERE ID=?',
     'WriteInt32 10',
     'WriteInt64 2',
     'ExecSQL UPDATE INVOICEITEMPRODUCT SET QTY=? WHERE ID=?']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.CompositionAdd;
var
  VInvoice: TInvoice;
  VItemProduct: TInvoiceItemProduct;
  VItemService: TInvoiceItemService;
begin
  VInvoice := TInvoice.Create;
  try
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 1;
    VItemProduct.Total := 12;
    Session.Store(VInvoice);
    VItemService := TInvoiceItemService.Create;
    VInvoice.Items.Add(VItemService);
    VItemService.Description := 'fix';
    VItemService.Total := 5;
    TTestSQLDriver.Commands.Clear;
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt64 3',
     'WriteInt32 2',
     'WriteInt64 1',
     'WriteInt32 5',
     'ExecSQL INSERT INTO INVOICEITEM (ID,SEQ,INVOICE,TOTAL) VALUES (?,?,?,?)',
     'WriteInt64 3',
     'WriteString fix',
     'ExecSQL INSERT INTO INVOICEITEMSERVICE (ID,DESCRIPTION) VALUES (?,?)']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.CompositionRemove;
var
  VInvoice: TInvoice;
  VItemProduct: TInvoiceItemProduct;
  VItemService: TInvoiceItemService;
begin
  VInvoice := TInvoice.Create;
  try
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 15;
    VItemProduct.Total := 150;
    VItemService := TInvoiceItemService.Create;
    VInvoice.Items.Add(VItemService);
    VItemService.Description := 'cleanup';
    VItemService.Total := 30;
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 3;
    VItemProduct.Total := 300;
    Session.Store(VInvoice);
    TTestSQLDriver.Commands.Clear;
    VInvoice.Items.Delete(2);
    VInvoice.Items.Delete(0);
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt64 2',
     'WriteInt64 4',
     'ExecSQL DELETE FROM INVOICEITEM WHERE ID IN (?,?)',
     'WriteInt64 2',
     'WriteInt64 4',
     'ExecSQL DELETE FROM INVOICEITEMPRODUCT WHERE ID IN (?,?)',
     'WriteInt64 2',
     'WriteInt64 4',
     'ExecSQL DELETE FROM INVOICEITEMSERVICE WHERE ID IN (?,?)']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.CompositionAddRemove;
var
  VInvoice: TInvoice;
  VItemProduct: TInvoiceItemProduct;
  VItemService: TInvoiceItemService;
begin
  VInvoice := TInvoice.Create;
  try
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 1;
    VItemProduct.Total := 10;
    VItemService := TInvoiceItemService.Create;
    VInvoice.Items.Add(VItemService);
    VItemService.Description := 'transport';
    VItemService.Total := 20;
    VItemProduct := TInvoiceItemProduct.Create;
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 3;
    VItemProduct.Total := 30;
    Session.Store(VInvoice);
    TTestSQLDriver.Commands.Clear;
    VInvoice.Items.Delete(1);
    VItemService := TInvoiceItemService.Create;
    VInvoice.Items.Add(VItemService);
    VItemService.Description := 'fix';
    VItemService.Total := 40;
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt64 3',
     'ExecSQL DELETE FROM INVOICEITEM WHERE ID=?',
     'WriteInt64 3',
     'ExecSQL DELETE FROM INVOICEITEMPRODUCT WHERE ID=?',
     'WriteInt64 3',
     'ExecSQL DELETE FROM INVOICEITEMSERVICE WHERE ID=?',
     'WriteInt64 5',
     'WriteInt32 4',
     'WriteInt64 1',
     'WriteInt32 40',
     'ExecSQL INSERT INTO INVOICEITEM (ID,SEQ,INVOICE,TOTAL) VALUES (?,?,?,?)',
     'WriteInt64 5',
     'WriteString fix',
     'ExecSQL INSERT INTO INVOICEITEMSERVICE (ID,DESCRIPTION) VALUES (?,?)']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.CompositionChangeOrder;
var
  VInvoice: TInvoice;
  VItemService: TInvoiceItemService;
  VItemProduct: TInvoiceItemProduct;
begin
  VInvoice := TInvoice.Create;
  try
    VItemService := TInvoiceItemService.Create; // 2
    VInvoice.Items.Add(VItemService);
    VItemService.Description := 'calc';
    VItemService.Total := 100;
    VItemProduct := TInvoiceItemProduct.Create; // 3
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 2;
    VItemProduct.Total := 15;
    VItemProduct := TInvoiceItemProduct.Create; // 4
    VInvoice.Items.Add(VItemProduct);
    VItemProduct.Qty := 1;
    VItemProduct.Total := 50;
    Session.Store(VInvoice);
    TTestSQLDriver.Commands.Clear;
    VInvoice.Items.Exchange(0, 2);
    Session.Store(VInvoice);
    AssertSQLDriverCommands([
     'WriteInt32 4',
     'WriteInt64 3',
     'ExecSQL UPDATE INVOICEITEM SET SEQ=? WHERE ID=?',
     'WriteInt32 5',
     'WriteInt64 2',
     'ExecSQL UPDATE INVOICEITEM SET SEQ=? WHERE ID=?']);
  finally
    FreeAndNil(VInvoice);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.AggregationAdd;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Languages.Add(TTestIPIDLanguage.Create('english'));
    VPerson.Languages.Add(TTestIPIDLanguage.Create('spanish'));
    SessionIPIDContactAuto.Store(VPerson);
    VPerson.Languages.Add(TTestIPIDLanguage.Create('portuguese'));
    TTestSQLDriver.Commands.Clear;
    SessionIPIDContactAuto.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 4',
     'WriteString portuguese',
     'ExecSQL INSERT INTO TESTIPIDLANGUAGE (ID,NAME) VALUES (?,?)',
     'WriteInt64 1',
     'WriteInt64 4',
     'WriteInt32 3',
     'ExecSQL INSERT INTO TESTIPIDPERSON_LANGUAGES (TESTIPIDPERSON,TESTIPIDLANGUAGE,SEQ) VALUES (?,?,?)']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.AggregationRemove;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Languages.Add(TTestIPIDLanguage.Create('english')); // 2
    VPerson.Languages.Add(TTestIPIDLanguage.Create('portuguese')); // 3
    SessionIPIDContactAuto.Store(VPerson);
    VPerson.Languages.Delete(0);
    TTestSQLDriver.Commands.Clear;
    SessionIPIDContactAuto.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteInt64 2',
     'ExecSQL DELETE FROM TESTIPIDPERSON_LANGUAGES WHERE TESTIPIDPERSON=? AND TESTIPIDLANGUAGE=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.AggregationAddRemove;
var
  VPerson: TTestIPIDPerson;
  VSpanish: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Languages.Add(TTestIPIDLanguage.Create('portuguese')); // 2
    VPerson.Languages.Add(TTestIPIDLanguage.Create('english')); // 3
    VSpanish := TTestIPIDLanguage.Create('spanish'); // 4
    SessionIPIDContactAuto.Store(VPerson);
    SessionIPIDContactAuto.Store(VSpanish);
    VPerson.Languages.Delete(0);
    VPerson.Languages.Add(VSpanish); // 4
    TTestSQLDriver.Commands.Clear;
    SessionIPIDContactAuto.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteInt64 2',
     'ExecSQL DELETE FROM TESTIPIDPERSON_LANGUAGES WHERE TESTIPIDPERSON=? AND TESTIPIDLANGUAGE=?',
     'WriteInt64 1',
     'WriteInt64 4',
     'WriteInt32 3',
     'ExecSQL INSERT INTO TESTIPIDPERSON_LANGUAGES (TESTIPIDPERSON,TESTIPIDLANGUAGE,SEQ) VALUES (?,?,?)']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.AggregationAddRemove2;
var
  VPerson: TTestIPIDPerson;
  VSpanish: TTestIPIDLanguage;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Languages.Add(TTestIPIDLanguage.Create('german')); // 2
    VPerson.Languages.Add(TTestIPIDLanguage.Create('portuguese')); // 3
    VPerson.Languages.Add(TTestIPIDLanguage.Create('french')); // 4
    VSpanish := TTestIPIDLanguage.Create('spanish'); // 5
    SessionIPIDContactAuto.Store(VPerson);
    SessionIPIDContactAuto.Store(VSpanish);
    VPerson.Languages.Delete(2);
    VPerson.Languages.Delete(0);
    VPerson.Languages.Add(VSpanish); // 4
    TTestSQLDriver.Commands.Clear;
    SessionIPIDContactAuto.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'WriteInt64 2',
     'WriteInt64 4',
     'ExecSQL DELETE FROM TESTIPIDPERSON_LANGUAGES WHERE TESTIPIDPERSON=? AND TESTIPIDLANGUAGE IN (?,?)',
     'WriteInt64 1',
     'WriteInt64 5',
     'WriteInt32 3',
     'ExecSQL INSERT INTO TESTIPIDPERSON_LANGUAGES (TESTIPIDPERSON,TESTIPIDLANGUAGE,SEQ) VALUES (?,?,?)']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFUpdateAutoMappingTests.AggregationChangeOrder;
var
  VPerson: TTestIPIDPerson;
begin
  VPerson := TTestIPIDPerson.Create;
  try
    VPerson.Languages.Add(TTestIPIDLanguage.Create('english')); // 2
    VPerson.Languages.Add(TTestIPIDLanguage.Create('spanish')); // 3
    VPerson.Languages.Add(TTestIPIDLanguage.Create('portuguese')); // 4
    SessionIPIDContactAuto.Store(VPerson);
    VPerson.Languages.Exchange(0, 2);
    TTestSQLDriver.Commands.Clear;
    SessionIPIDContactAuto.Store(VPerson);
    AssertSQLDriverCommands([
     'WriteInt32 4',
     'WriteInt64 1',
     'WriteInt64 3',
     'ExecSQL UPDATE TESTIPIDPERSON_LANGUAGES SET SEQ=? WHERE TESTIPIDPERSON=? AND TESTIPIDLANGUAGE=?',
     'WriteInt32 5',
     'WriteInt64 1',
     'WriteInt64 2',
     'ExecSQL UPDATE TESTIPIDPERSON_LANGUAGES SET SEQ=? WHERE TESTIPIDPERSON=? AND TESTIPIDLANGUAGE=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

{ TTestOPFDeleteAutoMappingTests }

procedure TTestOPFDeleteAutoMappingTests.SingleFromEntity;
var
  VProduct: TProduct;
begin
  VProduct := TProduct.Create;
  try
    VProduct.Name := 'aprod';
    Session.Store(VProduct);
    TTestSQLDriver.Commands.Clear;
    Session.Dispose(VProduct);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'ExecSQL DELETE FROM PRODUCT WHERE ID=?']);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFDeleteAutoMappingTests.SingleFromClass;
begin
  Session.Dispose(TProduct, ['7']);
  AssertSQLDriverCommands([
   'WriteInt64 7',
   'ExecSQL DELETE FROM PRODUCT WHERE ID=?']);
end;

procedure TTestOPFDeleteAutoMappingTests.InheritanceFromEntity;
var
  VPerson: TPerson;
begin
  VPerson := TPerson.Create;
  try
    VPerson.Name := 'joe';
    Session.Store(VPerson);
    TTestSQLDriver.Commands.Clear;
    {1}TTestSQLDriver.ExpectedResultsets.Add(1);
    TTestSQLDriver.Data.Add('null');
    Session.Dispose(VPerson);
    AssertSQLDriverCommands([
     'WriteInt64 1',
     'ExecSQL SELECT ADDRESS FROM CLIENT WHERE ID=?',
     'WriteInt64 1',
     'ExecSQL DELETE FROM CLIENT WHERE ID=?',
     'WriteInt64 1',
     'ExecSQL DELETE FROM PERSON WHERE ID=?']);
  finally
    FreeAndNil(VPerson);
  end;
end;

procedure TTestOPFDeleteAutoMappingTests.InheritanceFromClass;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');
  Session.Dispose(TPerson, ['5']);
  AssertSQLDriverCommands([
   'WriteInt64 5',
   'ExecSQL SELECT ADDRESS FROM CLIENT WHERE ID=?',
   'WriteInt64 5',
   'ExecSQL DELETE FROM CLIENT WHERE ID=?',
   'WriteInt64 5',
   'ExecSQL DELETE FROM PERSON WHERE ID=?']);
end;

procedure TTestOPFDeleteAutoMappingTests.InheritanceFromParentClass;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('null');
  Session.Dispose(TClient, ['2']);
  AssertSQLDriverCommands([
   'WriteInt64 2',
   'ExecSQL SELECT ADDRESS FROM CLIENT WHERE ID=?',
   'WriteInt64 2',
   'ExecSQL DELETE FROM CLIENT WHERE ID=?',
   'WriteInt64 2',
   'ExecSQL DELETE FROM PERSON WHERE ID=?',
   'WriteInt64 2',
   'ExecSQL DELETE FROM COMPANY WHERE ID=?']);
end;

procedure TTestOPFDeleteAutoMappingTests.InheritanceFromClassWithOIDArray;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(3);
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  TTestSQLDriver.Data.Add('null');
  Session.Dispose(TPerson, ['7', '9', '11']);
  AssertSQLDriverCommands([
   'WriteInt64 7',
   'WriteInt64 9',
   'WriteInt64 11',
   'ExecSQL SELECT ADDRESS FROM CLIENT WHERE ID IN (?,?,?)',
   'WriteInt64 7',
   'WriteInt64 9',
   'WriteInt64 11',
   'ExecSQL DELETE FROM CLIENT WHERE ID IN (?,?,?)',
   'WriteInt64 7',
   'WriteInt64 9',
   'WriteInt64 11',
   'ExecSQL DELETE FROM PERSON WHERE ID IN (?,?,?)']);
end;

procedure TTestOPFDeleteAutoMappingTests.EntityComposition;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('7');
  TTestSQLDriver.Data.Add('8');
  Session.Dispose(TPerson, ['1', '2']);
  AssertSQLDriverCommands([
   'WriteInt64 1',
   'WriteInt64 2',
   'ExecSQL SELECT ADDRESS FROM CLIENT WHERE ID IN (?,?)',
   'WriteInt64 7',
   'WriteInt64 8',
   'ExecSQL DELETE FROM ADDRESS WHERE ID IN (?,?)',
   'WriteInt64 1',
   'WriteInt64 2',
   'ExecSQL DELETE FROM CLIENT WHERE ID IN (?,?)',
   'WriteInt64 1',
   'WriteInt64 2',
   'ExecSQL DELETE FROM PERSON WHERE ID IN (?,?)']);
end;

procedure TTestOPFDeleteAutoMappingTests.CollectionComposition;
begin
  {1}TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('9');
  Session.Dispose(TInvoice, ['18']);
  AssertSQLDriverCommands([
   'WriteInt64 18',
   {1}'ExecSQL SELECT ID FROM INVOICEITEM WHERE INVOICE=?',
   'WriteInt64 9',
   'ExecSQL DELETE FROM INVOICEITEM WHERE ID=?',
   'WriteInt64 9',
   'ExecSQL DELETE FROM INVOICEITEMPRODUCT WHERE ID=?',
   'WriteInt64 9',
   'ExecSQL DELETE FROM INVOICEITEMSERVICE WHERE ID=?',
   'WriteInt64 18',
   'ExecSQL DELETE FROM INVOICE WHERE ID=?']);
end;

initialization
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFInsertAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFSelectAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFUpdateAutoMappingTests);
  RegisterTest('jcore.opf.mapping.automapping', TTestOPFDeleteAutoMappingTests);

end.

