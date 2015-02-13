(*
  JCore, Data Type Classes
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreTypes;

{$I jcore.inc}

interface

type

  IJCoreInteger = interface(IInterface)
  ['{B76F2D75-047D-9B2C-BB2B-456010E31664}']
    function AsInteger: Integer;
    function AsString: string;
    function EqualsTo(const AOther: IJCoreInteger): Boolean;
  end;

  { TJCoreInteger }

  TJCoreInteger = class(TInterfacedObject, IJCoreInteger)
  private
    FValue: Integer;
    function GetAsString: string;
    function IGetAsInteger: Integer;
    function IJCoreInteger.AsInteger = IGetAsInteger;
    function IJCoreInteger.AsString = GetAsString;
  public
    constructor Create(const AValue: Integer);
    function EqualsTo(const AOther: IJCoreInteger): Boolean;
    class function Parse(const AValue: string): IJCoreInteger;
    class function ValueOf(const AValue: Integer): IJCoreInteger;
    property AsInteger: Integer read FValue;
    property AsString: string read GetAsString;
  end;

implementation

uses
  sysutils;

{ TJCoreInteger }

function TJCoreInteger.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TJCoreInteger.IGetAsInteger: Integer;
begin
  Result := FValue;
end;

constructor TJCoreInteger.Create(const AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

function TJCoreInteger.EqualsTo(const AOther: IJCoreInteger): Boolean;
begin
  Result := Assigned(AOther) and (AOther.AsInteger = AsInteger);
end;

class function TJCoreInteger.Parse(const AValue: string): IJCoreInteger;
begin
  Result := ValueOf(StrToInt(AValue));
end;

class function TJCoreInteger.ValueOf(const AValue: Integer): IJCoreInteger;
begin
  { TODO : Cache }
  Result := Create(AValue);
end;

end.

