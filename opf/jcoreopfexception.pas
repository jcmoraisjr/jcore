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

  { EJCoreOPFMetadataNotFound }

  EJCoreOPFMetadataNotFound = class(EJCoreOPF)
  public
    constructor Create(const AClass: TClass);
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

  { EJCoreOPFPersistentIDFieldNotFound }

  EJCoreOPFPersistentIDFieldNotFound = class(EJCoreOPF)
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

  { EJCoreOPFUnsupportedListOperations }

  EJCoreOPFUnsupportedListOperations = class(EJCoreOPF)
  public
    constructor Create;
  end;

  { EJCoreOPFAttributeNotFound }

  EJCoreOPFAttributeNotFound = class(EJCoreOPF)
  public
    constructor Create(const AClassName, AAttributeName: string);
  end;

  { EJCoreOPFUnsupportedAttributeType }

  EJCoreOPFUnsupportedAttributeType = class(EJCoreOPF)
  public
    constructor Create(const AAttrTypeInfo: PTypeInfo);
  end;

implementation

{ EJCoreOPFMetadataNotFound }

constructor EJCoreOPFMetadataNotFound.Create(const AClass: TClass);
begin
end;

{ EJCoreOPFDriverNotFound }

constructor EJCoreOPFDriverNotFound.Create(const ADriverName: string);
begin
end;

{ EJCoreOPFUndefinedDriver }

constructor EJCoreOPFUndefinedDriver.Create;
begin
end;

{ EJCoreOPFPersistentIDFieldNotFound }

constructor EJCoreOPFPersistentIDFieldNotFound.Create(const AClassName: string);
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

{ EJCoreOPFUnexpectedResultSetSize }

constructor EJCoreOPFUnexpectedResultSetSize.Create(const AExpectedSize,
  AActualSize: Integer);
begin
end;

{ EJCoreOPFMappingNotFound }

constructor EJCoreOPFMappingNotFound.Create(const AEntityClassName: string);
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

{ EJCoreOPFUnsupportedListOperations }

constructor EJCoreOPFUnsupportedListOperations.Create;
begin
end;

{ EJCoreOPFAttributeNotFound }

constructor EJCoreOPFAttributeNotFound.Create(const AClassName,
  AAttributeName: string);
begin
end;

{ EJCoreOPFUnsupportedAttributeType }

constructor EJCoreOPFUnsupportedAttributeType.Create(
  const AAttrTypeInfo: PTypeInfo);
begin
  inherited Create(AAttrTypeInfo^.Name);
end;

end.

