(*
  JCore, OPF Generator Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFGenerator;

{$I jcore.inc}

interface

type

  TJCoreOPFGeneratorStrategy = (jgsGUID, jgsCustom);

  IJCoreOPFGenerator = interface(IInterface)
    procedure GenerateOIDs(const AOIDCount: Integer);
    function ReadInt64: Int64;
    function ReadString: string;
  end;

  { TJCoreOPFGeneratorGUID }

  TJCoreOPFGeneratorGUID = class(TInterfacedObject, IJCoreOPFGenerator)
  public
    procedure GenerateOIDs(const AOIDCount: Integer);
    function ReadInt64: Int64;
    function ReadString: string;
  end;

implementation

uses
  sysutils,
  JCoreOPFException;

{ TJCoreOPFGeneratorGUID }

procedure TJCoreOPFGeneratorGUID.GenerateOIDs(const AOIDCount: Integer);
begin
end;

{$warn 5033 off}
function TJCoreOPFGeneratorGUID.ReadInt64: Int64;
begin
  raise EJCoreOPFUnsupportedAttributeType.Create(TypeInfo(Int64));
end;
{$warn 5033 on}

function TJCoreOPFGeneratorGUID.ReadString: string;
var
  VGUID: array[0..15] of Byte;
  I: Integer;
begin
  { TODO : Check CreateGUID result }
  CreateGUID(TGUID(VGUID));
  SetLength(Result, 32);
  for I := 0 to 15 do
    Move(IntToHex(VGUID[I], 2)[1], Result[2*I+1], 2);
end;

end.

