(*
  JCore, Consts unit
  Copyright (C) 2012 Joao Morais

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

  // Error messages from core and adjacent classes
  SCannotReleaseInstance = 'Cannot release instance ''%s''';
  SInvalidIdentifier = 'Invalid identifier ''%s''';
  SStringLengthOutOfBounds = 'String length out of bounds';
  STokenExpected = '''%s'' was expected, but ''%s'' was found';
  STokenLengthOutOfBounds = 'Token length out of bounds';
  SUnexpectedEof = 'Unexpected end of file';

  // Partial translations for parsers or another error messages
  SJCoreBooleanValueMsg = 'Boolean value';
  SJCoreEofMsg = 'End of file';
  SJCoreIdentifierMsg = 'Identifier';
  SJCoreIntegerValueMsg = 'Integer';
  SJCoreLineBreakMsg = 'Line break';
  SJCoreNumberValueMsg = 'Number';
  SJCoreStringDelimiterMsg = 'String delimiter';
  SJCoreStringValueMsg = 'String';

implementation

end.
