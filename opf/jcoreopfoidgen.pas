(*
  JCore, OPF OID Generator Classes
  Copyright (C) 2014 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreOPFOIDGen;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  JCoreClasses;

type

  IJCoreOPFOIDGenerator = interface(IInterface)
    procedure GenerateOIDs(const AOIDCount: Integer);
    function ReadInt64: Int64;
    function ReadString: string;
  end;

  { TJCoreOPFOIDGeneratorSequence }

  TJCoreOPFOIDGeneratorSequence = class(TInterfacedObject, IJCoreOPFOIDGenerator)
  private
    FCurrent: Int64;
    FOIDList: TJCoreInt64List;
  protected
    procedure InternalGenerateOIDs(const AOIDCount: Integer); virtual; abstract;
    property Current: Int64 read FCurrent;
    property OIDList: TJCoreInt64List read FOIDList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GenerateOIDs(const AOIDCount: Integer);
    function ReadInt64: Int64;
    function ReadString: string;
  end;

  { TJCoreOPFOIDGeneratorGUID }

  TJCoreOPFOIDGeneratorGUID = class(TInterfacedObject, IJCoreOPFOIDGenerator)
  public
    procedure GenerateOIDs(const AOIDCount: Integer);
    function ReadInt64: Int64;
    function ReadString: string;
  end;

implementation

uses
  sysutils,
  JCoreConsts;

{ TJCoreOPFOIDGeneratorSequence }

constructor TJCoreOPFOIDGeneratorSequence.Create;
begin
  inherited Create;
  FOIDList := TJCoreInt64List.Create;
end;

destructor TJCoreOPFOIDGeneratorSequence.Destroy;
begin
  FreeAndNil(FOIDList);
  inherited Destroy;
end;

procedure TJCoreOPFOIDGeneratorSequence.GenerateOIDs(const AOIDCount: Integer);
begin
  while Current > 0 do
  begin
    { TODO : how to improve, truncate? copy list? use an array of int64? }
    OIDList.Delete(Current - 1);
    Dec(FCurrent);
  end;
  InternalGenerateOIDs(AOIDCount);
end;

function TJCoreOPFOIDGeneratorSequence.ReadInt64: Int64;
begin
  if Current < OIDList.Count then
  begin
    Result := OIDList[Current];
    Inc(FCurrent);
  end else
    raise EJCoreOPF.Create(2124, S2124_EmptyOIDList, []);
end;

function TJCoreOPFOIDGeneratorSequence.ReadString: string;
begin
  Result := IntToStr(ReadInt64);
end;

{ TJCoreOPFOIDGeneratorGUID }

procedure TJCoreOPFOIDGeneratorGUID.GenerateOIDs(const AOIDCount: Integer);
begin
end;

{$warn 5033 off}
function TJCoreOPFOIDGeneratorGUID.ReadInt64: Int64;
begin
  raise EJCoreOPF.Create(2121, S2121_UnsupportedAttributeType, [SJCoreInt64ValueMsg]);
end;
{$warn 5033 on}

function TJCoreOPFOIDGeneratorGUID.ReadString: string;
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

