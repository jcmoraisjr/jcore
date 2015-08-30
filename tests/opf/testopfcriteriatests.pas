unit TestOPFCriteriaTests;

{$mode objfpc}{$H+}

interface

uses
  TestOPFConfig, JCoreOPFCriteria;

type

  { TTestOPFCriteriaTest }

  TTestOPFCriteriaTest = class(TTestOPFInvoiceAutoMappingTestCase)
  published
    procedure NonFilterSimple;
    procedure NonFilterAbstract;
    procedure NonFilterInherited;
    procedure FilterEqualsSimple;
    procedure FilterEqualsAbstract;
    procedure FilterEqualsInherited;
    procedure FilterAttributeNotFound;
    procedure FilterInteger;
    procedure RetrieveListFromOne;
    procedure RetrieveListFromTwo;
    procedure RetrieveUniqueFromOne;
    procedure RetrieveUniqueFromTwo;
  end;

implementation

uses
  sysutils,
  contnrs,
  testregistry,
  JCoreClasses,
  JCoreOPFCriteriaSQL,
  TestOPFModelInvoice;

{ TTestOPFCriteriaTest }

procedure TTestOPFCriteriaTest.NonFilterSimple;
var
  VList: TObjectList;
begin
  VList := Session.CreateCriteria(TProduct).RetrieveList;
  try
    AssertSQLDriverCommands([
     'ExecSQL SELECT ID,NAME FROM PRODUCT']);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.NonFilterAbstract;
var
  VList: TObjectList;
begin
  VList := Session.CreateCriteria(TClient).RetrieveList;
  try
    AssertSQLDriverCommands([
     'ExecSQL SELECT t0.ID,t1.ID,t2.ID,t0.NAME,t0.ADDRESS FROM CLIENT t0 LEFT OUTER JOIN PERSON t1 ON t0.ID = t1.ID LEFT OUTER JOIN COMPANY t2 ON t0.ID = t2.ID']);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.NonFilterInherited;
var
  VList: TObjectList;
begin
  VList := Session.CreateCriteria(TPerson).RetrieveList;
  try
    AssertSQLDriverCommands([
     'ExecSQL SELECT t0.ID,t0.NAME,t0.ADDRESS,t1.NICK FROM CLIENT t0 INNER JOIN PERSON t1 ON t0.ID = t1.ID']);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.FilterEqualsSimple;
var
  VList: TObjectList;
begin
  VList := Session.CreateCriteria(TProduct)
    .Add(TJCoreOPFCriteriaRestriction.Eq('name', 'pencil'))
    .RetrieveList;
  try
    AssertSQLDriverCommands([
     'WriteString pencil',
     'ExecSQL SELECT ID,NAME FROM PRODUCT WHERE NAME = ?']);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.FilterEqualsAbstract;
var
  VList: TObjectList;
begin
  VList := Session.CreateCriteria(TClient)
    .Add(TJCoreOPFCriteriaRestriction.Eq('name', 'joe'))
    .RetrieveList;
  try
    AssertSQLDriverCommands([
     'WriteString joe',
     'ExecSQL SELECT t0.ID,t1.ID,t2.ID,t0.NAME,t0.ADDRESS FROM CLIENT t0 LEFT OUTER JOIN PERSON t1 ON t0.ID = t1.ID LEFT OUTER JOIN COMPANY t2 ON t0.ID = t2.ID WHERE t0.NAME = ?']);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.FilterEqualsInherited;
var
  VList: TObjectList;
begin
  VList := Session.CreateCriteria(TPerson)
    .Add(TJCoreOPFCriteriaRestriction.Eq('nick', 'jack'))
    .RetrieveList;
  try
    AssertSQLDriverCommands([
     'WriteString jack',
     'ExecSQL SELECT t0.ID,t0.NAME,t0.ADDRESS,t1.NICK FROM CLIENT t0 INNER JOIN PERSON t1 ON t0.ID = t1.ID WHERE t1.NICK = ?']);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.FilterAttributeNotFound;
var
  VList: TObjectList;
begin
  VList := nil;
  try
    try
      VList := Session.CreateCriteria(TPerson)
        .Add(TJCoreOPFCriteriaRestriction.Eq('doesnotexist', 'jack'))
        .RetrieveList;
      Fail('EJCoreOPF(2127) expected');
    except
      on E: EJCoreOPF do
        if E.Code <> 2127 then
          raise;
    end;
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.FilterInteger;
var
  VList: TObjectList;
begin
  VList := Session.CreateCriteria(TInvoiceItem)
    .Add(TJCoreOPFCriteriaRestriction.Eq('total', 10))
    .RetrieveList;
  try
    AssertSQLDriverCommands([
     'WriteInt32 10',
     'ExecSQL SELECT t0.ID,t1.ID,t2.ID,t0.TOTAL FROM INVOICEITEM t0 LEFT OUTER JOIN INVOICEITEMPRODUCT t1 ON t0.ID = t1.ID LEFT OUTER JOIN INVOICEITEMSERVICE t2 ON t0.ID = t2.ID WHERE t0.TOTAL = ?']);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.RetrieveListFromOne;
var
  VList: TObjectList;
begin
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('1');
  TTestSQLDriver.Data.Add('book');
  VList := Session.CreateCriteria(TProduct)
    .Add(TJCoreOPFCriteriaRestriction.Eq('name', 'book'))
    .RetrieveList;
  try
    AssertSQLDriverCommands([
     'WriteString book',
     'ExecSQL SELECT ID,NAME FROM PRODUCT WHERE NAME = ?']);
    AssertEquals('list.count', 1, VList.Count);
    AssertEquals('list[0].name', 'book', (VList[0] as TProduct).Name);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.RetrieveListFromTwo;
var
  VList: TObjectList;
begin
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('1');
  TTestSQLDriver.Data.Add('pen');
  TTestSQLDriver.Data.Add('2');
  TTestSQLDriver.Data.Add('pencil');
  VList := Session.CreateCriteria(TProduct)
    .Add(TJCoreOPFCriteriaRestriction.Like('name', 'pen%'))
    .RetrieveList;
  try
    AssertSQLDriverCommands([
     'WriteString pen%',
     'ExecSQL SELECT ID,NAME FROM PRODUCT WHERE NAME LIKE ?']);
    AssertEquals('list.count', 2, VList.Count);
    AssertEquals('list[0].name', 'pen', (VList[0] as TProduct).Name);
    AssertEquals('list[1].name', 'pencil', (VList[1] as TProduct).Name);
  finally
    FreeAndNil(VList);
  end;
end;

procedure TTestOPFCriteriaTest.RetrieveUniqueFromOne;
var
  VProduct: TProduct;
begin
  TTestSQLDriver.ExpectedResultsets.Add(1);
  TTestSQLDriver.Data.Add('1');
  TTestSQLDriver.Data.Add('pen');
  VProduct := Session.CreateCriteria(TProduct)
    .Add(TJCoreOPFCriteriaRestriction.Eq('name', 'pen'))
    .RetrieveUnique as TProduct;
  try
    AssertSQLDriverCommands([
     'WriteString pen',
     'ExecSQL SELECT ID,NAME FROM PRODUCT WHERE NAME = ?']);
    AssertNotNull('product', VProduct);
    AssertEquals('product.name', 'pen', VProduct.Name);
  finally
    FreeAndNil(VProduct);
  end;
end;

procedure TTestOPFCriteriaTest.RetrieveUniqueFromTwo;
var
  VProduct: TProduct;
begin
  TTestSQLDriver.ExpectedResultsets.Add(2);
  TTestSQLDriver.Data.Add('1');
  TTestSQLDriver.Data.Add('pen');
  TTestSQLDriver.Data.Add('2');
  TTestSQLDriver.Data.Add('pen');
  VProduct := nil;
  try
    try
      VProduct := Session.CreateCriteria(TProduct)
        .Add(TJCoreOPFCriteriaRestriction.Eq('name', 'pen'))
        .RetrieveUnique as TProduct;
      Fail('EJCoreOPF(2113) expected');
    except
      on E: EJCoreOPF do
        if E.Code <> 2113 then
          raise;
    end;
  finally
    FreeAndNil(VProduct);
  end;
end;

initialization
  RegisterTest('jcore.opf.criteria', TTestOPFCriteriaTest);

end.

