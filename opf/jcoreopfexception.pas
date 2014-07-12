(*
  JCore, OPF Exception Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFException;

{$I jcore.inc}

interface

uses
  typinfo,
  sysutils,
  JCoreClasses;

type

  { EJCoreOPF }

  EJCoreOPF = class(EJCoreException)
  end;

  { EJCoreOPFDriverNotFound }

  EJCoreOPFDriverNotFound = class(EJCoreOPF)
  public
    constructor Create(const ADriverName: string);
  end;

  { EJCoreOPFUndefinedDriver }

  EJCoreOPFUndefinedDriver = class(EJCoreOPF)
  public
    constructor Create;
  end;

  { EJCoreOPFPersistentIDNotFound }

  EJCoreOPFPersistentIDNotFound = class(EJCoreOPF)
  public
    constructor Create(const AClassName: string);
  end;

  { EJCoreOPFUnsupportedDriver }

  EJCoreOPFUnsupportedDriver = class(EJCoreOPF)
  public
    constructor Create(const AClassName: string);
  end;

  { EJCoreOPFEmptyResultSet }

  EJCoreOPFEmptyResultSet = class(EJCoreOPF)
  public
    constructor Create(const AExpectedSize: Integer);
  end;

  { EJCoreOPFEmptyOID }

  EJCoreOPFEmptyOID = class(EJCoreOPF)
  public
    constructor Create;
  end;

  { EJCoreOPFObjectNotFound }

  EJCoreOPFObjectNotFound = class(EJCoreOPF)
  public
    constructor Create(const AOID: string);
  end;

  { EJCoreOPFUnexpectedResultSetSize }

  EJCoreOPFUnexpectedResultSetSize = class(EJCoreOPF)
  public
    constructor Create(const AExpectedSize, AActualSize: Integer);
  end;

  { EJCoreOPFMappingNotFound }

  EJCoreOPFMappingNotFound = class(EJCoreOPF)
  public
    constructor Create(const AEntityClassName: string);
  end;

  { EJCoreOPFInconsistentMappingSizes }

  EJCoreOPFInconsistentMappingSizes = class(EJCoreOPF)
  public
    constructor Create(const AExpectedSize, AActualSize: Integer);
  end;

  { EJCoreOPFAmbiguousInstanceClass }

  EJCoreOPFAmbiguousInstanceClass = class(EJCoreOPF)
  public
    constructor Create(const AClass1, AClass2: TClass);
  end;

  { EJCoreOPFCannotAssignOIDPersistent }

  EJCoreOPFCannotAssignOIDPersistent = class(EJCoreOPF)
  public
    constructor Create;
  end;

  { EJCoreOPFObjectAlreadyOwned }

  EJCoreOPFObjectAlreadyOwned = class(EJCoreOPF)
  public
    constructor Create(const AOwnedClassName, AOwnerClassName: string);
  end;

  { EJCoreOPFUnsupportedFeature }

  EJCoreOPFUnsupportedFeature = class(EJCoreOPF)
  public
    constructor Create(const AName: string);
  end;

  { EJCoreOPFUnsupportedLoadOperation }

  EJCoreOPFUnsupportedLoadOperation = class(EJCoreOPF)
  public
    constructor Create(const AAttributeTypeName: string);
  end;

  { EJCoreOPFUnsupportedAttributeType }

  EJCoreOPFUnsupportedAttributeType = class(EJCoreOPF)
  public
    constructor Create(const AAttrTypeInfo: PTypeInfo);
    constructor Create(const AClassName: string);
  end;

  { EJCoreOPFEntityADMExpected }

  EJCoreOPFEntityADMExpected = class(EJCoreOPF)
  public
    constructor Create(const AClassName, AAttributeName: string);
  end;

  { EJCoreOPFCollectionADMExpected }

  EJCoreOPFCollectionADMExpected = class(EJCoreOPF)
  public
    constructor Create(const AClassName, AAttributeName: string);
  end;

implementation

{ EJCoreOPFDriverNotFound }

constructor EJCoreOPFDriverNotFound.Create(const ADriverName: string);
begin
end;

{ EJCoreOPFUndefinedDriver }

constructor EJCoreOPFUndefinedDriver.Create;
begin
end;

{ EJCoreOPFPersistentIDNotFound }

constructor EJCoreOPFPersistentIDNotFound.Create(const AClassName: string);
begin
end;

{ EJCoreOPFUnsupportedDriver }

constructor EJCoreOPFUnsupportedDriver.Create(const AClassName: string);
begin
end;

{ EJCoreOPFEmptyResultSet }

constructor EJCoreOPFEmptyResultSet.Create(const AExpectedSize: Integer);
begin
end;

{ EJCoreOPFEmptyOID }

constructor EJCoreOPFEmptyOID.Create;
begin
end;

{ EJCoreOPFObjectNotFound }

constructor EJCoreOPFObjectNotFound.Create(const AOID: string);
begin
end;

{ EJCoreOPFUnexpectedResultSetSize }

constructor EJCoreOPFUnexpectedResultSetSize.Create(const AExpectedSize,
  AActualSize: Integer);
begin
end;

{ EJCoreOPFMappingNotFound }

constructor EJCoreOPFMappingNotFound.Create(const AEntityClassName: string);
begin
end;

{ EJCoreOPFInconsistentMappingSizes }

constructor EJCoreOPFInconsistentMappingSizes.Create(const AExpectedSize, AActualSize: Integer);
begin
end;

{ EJCoreOPFAmbiguousInstanceClass }

constructor EJCoreOPFAmbiguousInstanceClass.Create(const AClass1, AClass2: TClass);
begin
end;

{ EJCoreOPFCannotAssignOIDPersistent }

constructor EJCoreOPFCannotAssignOIDPersistent.Create;
begin
end;

{ EJCoreOPFObjectAlreadyOwned }

constructor EJCoreOPFObjectAlreadyOwned.Create(const AOwnedClassName,
  AOwnerClassName: string);
begin
end;

{ EJCoreOPFUnsupportedFeature }

constructor EJCoreOPFUnsupportedFeature.Create(const AName: string);
begin
end;

{ EJCoreOPFUnsupportedLoadOperation }

constructor EJCoreOPFUnsupportedLoadOperation.Create(const AAttributeTypeName: string);
begin
end;

{ EJCoreOPFUnsupportedAttributeType }

constructor EJCoreOPFUnsupportedAttributeType.Create(
  const AAttrTypeInfo: PTypeInfo);
begin
  inherited Create(AAttrTypeInfo^.Name);
end;

constructor EJCoreOPFUnsupportedAttributeType.Create(const AClassName: string);
begin
  inherited Create(AClassName);
end;

{ EJCoreOPFEntityADMExpected }

constructor EJCoreOPFEntityADMExpected.Create(const AClassName,
  AAttributeName: string);
begin
end;

{ EJCoreOPFCollectionADMExpected }

constructor EJCoreOPFCollectionADMExpected.Create(const AClassName,
  AAttributeName: string);
begin
end;

end.

