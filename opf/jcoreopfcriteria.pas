(*
  JCore, OPF Criteria Interfaces
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFCriteria;

{$I jcore.inc}

interface

uses
  contnrs,
  JCoreClasses,
  JCoreOPFDriver,
  JCoreOPFMetadata;

type

  { IJCoreOPFCriteriaRetriever }

  IJCoreOPFCriteriaRetriever = interface
    function RetrieveResultSet(const AClass: TClass; const AResultSet: IJCoreOPFResultSet): TJCoreObjectArray;
  end;

  { IJCoreOPFSQLCriteriaResolver }

  IJCoreOPFSQLCriteriaResolver = interface
    function FieldNameByAttributeName(const AAttributeName: string): string;
    function Params: IJCoreOPFParams;
  end;

  IJCoreOPFSQLCriteria = interface;

  { IJCoreOPFSQLCriterion }

  IJCoreOPFSQLCriterion = interface(IInterface)
  ['{63A353DC-ACD4-8349-F8A3-EA54696EF7F5}']
    function GetSQL(const AResolver: IJCoreOPFSQLCriteriaResolver): string;
  end;

  { IJCoreOPFSQLCriteria }

  IJCoreOPFSQLCriteria = interface(IInterface)
  ['{1CFE973F-8C18-7D81-3838-BDE97FA5E018}']
    function Add(const ACriterion: IJCoreOPFSQLCriterion): IJCoreOPFSQLCriteria;
    function Metadata: TJCoreOPFClassMetadata;
    function RetrieveList: TObjectList;
    function RetrieveResultSet: IJCoreOPFSQLResultSet;
    function RetrieveUnique: TObject;
  end;

implementation

end.

