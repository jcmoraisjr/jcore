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

  SJCoreBooleanValueMsg = 'Boolean value';
  SJCoreEofMsg = 'End of file';
  SJCoreIdentifierMsg = 'Identifier';
  SJCoreIntegerValueMsg = 'Integer';
  SJCoreLineBreakMsg = 'Line break';
  SJCoreNumberValueMsg = 'Number';
  SJCoreStringDelimiterMsg = 'String delimiter';
  SJCoreStringValueMsg = 'String';

  SAmbiguousClass = 'Ambiguous classes: ''%s'' and ''%s''';
  SCannotReleaseInstance = 'Cannot release instance ''%s''';
  SInterfaceNotFound = 'Interface not found: ''%s''';
  SInvalidIdentifier = 'Invalid identifier ''%s''';
  SNilPointer = 'Nil pointer';
  SStringLengthOutOfBounds = 'String length out of bounds';
  STokenExpected = '''%s'' was expected, but ''%s'' was found';
  STokenLengthOutOfBounds = 'Token length out of bounds';
  SUnexpectedEof = 'Unexpected end of file';
  SUnsupportedInterface = 'Class ''%s'' does not support interface ''%s''';

implementation

end.
