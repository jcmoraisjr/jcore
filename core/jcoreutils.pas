(*
  JCore, Utilities Unit
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreUtils;

{$I jcore.inc}

interface

function JCoreDumpStackTrace: string;

implementation

// From: http://wiki.freepascal.org/Logging_exceptions#Dump_current_call_stack
function JCoreDumpStackTrace: string;
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
  Report: string;
const
  MaxDepth = 50;
begin
  Report := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do begin
      CallerAddress := get_caller_addr(bp);
      CallerFrame := get_caller_frame(bp);
      if (CallerAddress = nil) then
        Break;
      Report := Report + BackTraceStrFunc(CallerAddress) + LineEnding;
      Inc(I);
      if (I >= MaxDepth) or (CallerFrame = nil) then
        Break;
      prevbp := bp;
      bp := CallerFrame;
    end;
  except
    { prevent endless dump if an exception occured }
  end;
  Result := Report;
end;

end.

