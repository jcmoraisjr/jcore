(*
  JCore WebServices, JSON Streaming Classes
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreWSJSON;

{$I jcore.inc}

interface

uses
  typinfo,
  contnrs,
  fpjson,
  fpjsonrtti;

type

  { TJCoreWSJSONSerializer }

  TJCoreWSJSONSerializer = class(TJSONStreamer)
  protected
    function StreamClassProperty(const AObject: TObject): TJSONData; override;
    function StreamObjectList(const AObjectList: TObjectList): TJSONArray;
  end;

  { TJCoreWSJSONUnserializer }

  TJCoreWSJSONUnserializer = class(TJSONDeStreamer)
  protected
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo; PropData: TJSONData); override;
    procedure RestoreObjectList(const AObject: TObject; const APropInfo: PPropInfo; const APropData: TJSONData);
  end;

implementation

uses
  sysutils;

{ TJCoreWSJSONSerializer }

function TJCoreWSJSONSerializer.StreamClassProperty(const AObject: TObject): TJSONData;
begin
  if AObject is TObjectList then
    Result := StreamObjectList(TObjectList(AObject))
  else
    Result := inherited StreamClassProperty(AObject);
end;

function TJCoreWSJSONSerializer.StreamObjectList(const AObjectList: TObjectList): TJSONArray;
var
  I: Integer;
begin
  Result := TJSONArray.Create;
  try
    for I := 0 to Pred(AObjectList.Count) do
      Result.Add(ObjectToJSON(AObjectList.Items[I]));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TJCoreWSJSONUnserializer }

procedure TJCoreWSJSONUnserializer.DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo;
  PropData: TJSONData);
begin
  if (PropData is TJSONArray) and GetTypeData(PropInfo^.PropType)^.ClassType.InheritsFrom(TObjectList) then
    RestoreObjectList(AObject, PropInfo, PropData)
  else
    inherited DoRestoreProperty(AObject, PropInfo, PropData);
end;

procedure TJCoreWSJSONUnserializer.RestoreObjectList(const AObject: TObject; const APropInfo: PPropInfo;
  const APropData: TJSONData);
var
  VList: TObjectList;
  VArray: TJSONArray;
  VItem: TObject;
  I: Integer;
begin
  VList := TObjectList.Create(True);
  SetObjectProp(AObject, APropInfo, VList);
  VArray := TJSONArray(APropData);
  for I := 0 to Pred(VArray.Count) do
  begin
    { TODO : Take the item classes of the list from the model }
    VItem := TObject.Create;
    VList.Add(VItem);
    JSONToObject(VArray[I] as TJSONObject, VItem);
  end;
end;

initialization
{$ifdef VER3}
  TJSONObject.CompressedJSON := True;
{$endif}

end.

