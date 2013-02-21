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

resourcestring
  SJCoreTrueString = 'True';
  SJCoreFalseString = 'False';

  SJCoreBooleanValueMsg = 'Boolean value';
  SJCoreEofMsg = 'End of file';
  SJCoreIdentifierMsg = 'Identifier';
  SJCoreIntegerValueMsg = 'Integer';
  SJCoreLineBreakMsg = 'Line break';
  SJCoreNumberValueMsg = 'Number';
  SJCoreStringDelimiterMsg = 'String delimiter';
  SJCoreStringValueMsg = 'String';

  SJCoreAmbiguousImplementation = 'Ambiguous Implementation for Interface ''%s'': ''%s'' and ''%s''';
  SJCoreCannotReleaseInstance = 'Cannot release instance ''%s''';
  SJCoreInterfaceNotFound = 'Interface not found: ''%s''';
  SJCoreInvalidIdentifier = 'Invalid identifier ''%s''';
  SJCoreNilPointer = 'Nil pointer';
  SJCoreStringLengthOutOfBounds = 'String length out of bounds';
  SJCoreTokenExpected = '''%s'' was expected, but ''%s'' was found';
  SJCoreTokenLengthOutOfBounds = 'Token length out of bounds';
  SJCoreUnexpectedEof = 'Unexpected end of file';
  SJCoreUnsupportedInterface = 'Class ''%s'' does not support interface ''%s''';

implementation

end.
