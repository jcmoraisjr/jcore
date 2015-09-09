(*
  JCore, Consts unit
  Copyright (C) 2013 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreConsts;

{$I jcore.inc}

interface

const
  SJCoreTrueString = 'True';
  SJCoreFalseString = 'False';
  SJCoreID = 'ID';
  SJCorePID = '_pid';
  SJCoreProxy = '_proxy';
  SJCoreController = 'JCoreController';
  SJCoreMethod = 'JCoreMethod';

resourcestring
  SJCoreBooleanValueMsg = 'Boolean value';
  SJCoreDefaultQualifier = '[Default]';
  SJCoreEofMsg = 'End of file';
  SJCoreIdentifierMsg = 'Identifier';
  SJCoreIntegerValueMsg = 'Integer';
  SJCoreInt64ValueMsg = 'Int64';
  SJCoreLineBreakMsg = 'Line break';
  SJCoreNumberValueMsg = 'Number';
  SJCoreStringDelimiterMsg = 'String delimiter';
  SJCoreStringValueMsg = 'String';

  //// Messages
  SJCoreExceptionRaisedWithMessage = '%s exception was raised with the following message: %s';

  //// Messages from exceptions
  // EJCoreNilPointer
  S0101_NilPointer = 'Nil pointer';
  // EJCoreClasses
  S0201_AmbiguousImplementation = 'Ambiguous implementation between classes ''%s'' and ''%s''';
  S0202_ListTypeExpected = '''%s'' was expected';
  S0203_ListIsEmpty = 'List is empty';
  S0204_CannotReleaseInstance = 'Cannot release instance ''%s''';
  S0205_InvalidIdentifier = 'Invalid identifier ''%s''';
  S0206_UnsupportedTypeOrderedList = 'Ordered list does not support type ''%d''';
  // EJCoreDIC
  S0301_UnsupportedInterface = 'Class ''%s'' does not support interface ''%s''';
  S0302_AmbiguousImplementation = 'Ambiguous Implementation for Interface ''%s'': ''%s'' and ''%s''';
  S0303_ImplementationNotFound = 'Cannot find implementation for interface ''%s'' with qualifier ''%s''';
  // EJCoreParser
  S0401_StringLengthOutOfBounds = 'String length out of bounds';
  S0402_TokenExpected = '''%s'' was expected, but ''%s'' was found';
  S0403_TokenLengthOutOfBounds = 'Token length out of bounds';
  S0404_UnexpectedEof = 'Unexpected end of file';
  // EJCoreMetadata
  S0501_AttributeNotFound = 'Attribute %s(''%s'') was not found';
  S0502_MetadataAlreadyOwned = 'Metadata already owned';
  S0503_InvalidOwnerAttr = 'Owner attribute ''%s'' does not have an owner class';
  // EJCoreOPF
  S2101_DriverNotFound = 'Driver ''%s'' was not found';
  S2102_UndefinedDriver = 'Driver was not defined';
  S2103_ClosedDataset = 'Closed dataset';
  S2104_UnsupportedType = 'Unsupported type ''%s''';
  S2105_ConnectionNotFound = 'Connection not found: %s';
  S2106_UnsupportedConnection = 'Unsupported connection: %s';
  S2107_DetachedStatement = 'Detached statement';
  S2108_PersistentIDNotFound = 'PersistentID field not found';
  S2109_DriverIsNotSQL = 'Only SQL drivers are supported. Driver: ''%s''';
  S2110_EmptyResultSet = 'Resultset is empty. Expected resultset size is ''%d''';
  S2111_UnassignedParams = 'Unassigned params';
  S2112_StatementOnQueue = 'There are statement(s) on the queue';
  S2113_UnexpectedResultSetSize = 'Unexpected resultset size. Expected: %d, current: %d';
  S2114_ObjectNotFound = 'Object %s was not found';
  S2115_EmptyOID = 'Object ID is empty';
  S2116_MappingNotFound = 'Class ''%s'' does not have a corresponding mapping class';
  S2117_InconsistentMappingSizes = 'Inconsistent mapping sizes on ''%s''';
  S2119_CannotAssignOIDPersistent = 'Cannot reassign persistent OID';
  S2120_ObjectAlreadyOwned = 'Cannot assign owner relationship. ''%s'' already owns ''%s''';
  S2121_UnsupportedAttributeType = 'Unsupported attribute type ''%s''';
  S2122_UnsupportedLoadOperation = '''%'' does not support load operation';
  S2123_EntityADMExpected = '''%s(%s)'' was expected to be an entity attribute';
  //S2124_EmptyOIDList
  S2125_CollectionADMExpected = '''%s(%s)'' was expected to be a collection attribute';
  S2126_UnsupportedOIDArray = 'Only arrays of one or two OIDs are supported';
  S2127_AttributeNotFound = 'Attribute %s(''%s'') was not found';
  S2128_UndefinedJoinAlias = 'Cannot join ''%s'': join alias was not defined';
  S2129_IncompatibleJoinOID = 'Cannot join ''%s'': incompatible OID';
  S2130_DbSequenceUnsupported = 'Database ''%s'' does not support sequences';
  S2131_DbAutoincUnsupported = 'Database ''%s'' does not support auto-increment field';
  // EJCoreWS
  S3101_UnsupportedMethod = 'Method ''%s.%s'' does not have a supported invoker';
  S3102_TypeinfoIsNotMethod = 'Typeinfo ''%s.%s'' is not a tkMethod';
  S3103_TypeinfoIsNotMethod = 'Typeinfo is not a tkMethod';
  S3104_AmbiguousMethodInvokers = 'Ambiguous method invoker implementation: ''%s'' and ''%s''';
  // EJCoreWS responses
  S3601_InternalServerError500 = 'Internal server error';
  S3602_UnknownStatus = 'Unknown status ''%d''';
  S3603_URLNotFound = 'The requested URL ''%s'' was not found';
  S3604_URLNotAllowed = 'The requested URL ''%s'' does not allow method ''%s''';

implementation

end.
