(*
  JCore WebServices, Method Invoker Classes
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreWSInvokers;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  typinfo,
  fgl,
  HTTPDefs;

type

  { TJCoreWSMethodParam }

  TJCoreWSMethodParam = record
    Flags: TParamFlags;
    ParamName: string;
    ParamType: string;
    ParamTypeInfo: PTypeInfo;
  end;

  TJCoreWSMethodParamArray = array of TJCoreWSMethodParam;

  { TJCoreWSMethodData }

  TJCoreWSMethodData = class(TObject)
  private
    FCConv: TCallConv;
    FMethodKind: TMethodKind;
    FParams: TJCoreWSMethodParamArray;
    FResultType: PTypeInfo;
  public
    constructor Create(const ATypeInfo: PTypeInfo);
    function IsFunctionAsObject: Boolean;
    function IsProcedureConstObject: Boolean;
    property CConv: TCallConv read FCConv;
    property MethodKind: TMethodKind read FMethodKind;
    property Params: TJCoreWSMethodParamArray read FParams;
    property ResultType: PTypeInfo read FResultType;
  end;

  { TJCoreWSMethodInvoker }

  TJCoreWSMethodInvoker = class(TObject)
  public
    function Match(const AMethodData: TJCoreWSMethodData): Boolean; virtual; abstract;
    procedure Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest; const AResponse: TResponse; const AMethod: TMethod); virtual; abstract;
  end;

  TJCoreWSMethodInvokerClass = class of TJCoreWSMethodInvoker;
  TJCoreWSMethodInvokerList = specialize TFPGObjectList<TJCoreWSMethodInvoker>;

  { TJCoreWSProcObjectInvoker }

  TJCoreWSProcObjectInvoker = class(TJCoreWSMethodInvoker)
  public
    function Match(const AMethodData: TJCoreWSMethodData): Boolean; override;
    procedure Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest; const AResponse: TResponse; const AMethod: TMethod); override;
  end;

  { TJCoreWSFncObjectInvoker }

  TJCoreWSFncObjectInvoker = class(TJCoreWSMethodInvoker)
  public
    function Match(const AMethodData: TJCoreWSMethodData): Boolean; override;
    procedure Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest; const AResponse: TResponse; const AMethod: TMethod); override;
  end;

implementation

uses
  sysutils,
  JCoreConsts,
  JCoreClasses,
  JCoreWSJSON;

{ TJCoreWSMethodData }

constructor TJCoreWSMethodData.Create(const ATypeInfo: PTypeInfo);
{
  Lay out of ParamList from FPC 3.0
  ParamCount: Byte;
  ParamList: array[1..ParamCount] of record
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
  end;
  ResultType: ShortString;   // for mkFunction and mkClassFunction only
  ResultTypeRef: PTypeInfo;  // for mkFunction and mkClassFunction only
  CC: TCallConv;
  ParamTypeRefs: array[1..ParamCount] of PTypeInfo;
}
var
  VTypeData: PTypeData;
  VParamItem: PByte;
  I: Integer;
begin
  if ATypeInfo^.Kind <> tkMethod then
    raise EJCoreWS.Create(3103, S3103_TypeinfoIsNotMethod, []);
  inherited Create;
  VTypeData := GetTypeData(ATypeInfo);
  FMethodKind := VTypeData^.MethodKind;
  VParamItem := PByte(@VTypeData^.ParamList[0]);
  SetLength(FParams, VTypeData^.ParamCount);
  for I := Low(FParams) to High(FParams) do
  begin
    FParams[I].Flags := TParamFlags(VParamItem^);
    Inc(VParamItem, SizeOf(TParamFlags));
    FParams[I].ParamName := PShortString(VParamItem)^;
    Inc(VParamItem, VParamItem^ + 1);
    FParams[I].ParamType := PShortString(VParamItem)^;
    Inc(VParamItem, VParamItem^ + 1);
  end;
  if FMethodKind in [mkFunction, mkClassFunction] then
  begin
    Inc(VParamItem, VParamItem^ + 1); // skip shortstring of result type
    FResultType := PPTypeInfo(VParamItem)^;
    Inc(VParamItem, SizeOf(PTypeInfo));
  end else
    FResultType := nil;
  FCConv := TCallConv(VParamItem^);
  Inc(VParamItem, SizeOf(TCallConv));
  for I := Low(FParams) to High(FParams) do
  begin
    FParams[I].ParamTypeInfo := PPTypeInfo(VParamItem)^;
    Inc(VParamItem, SizeOf(PTypeInfo));
  end;
end;

function TJCoreWSMethodData.IsFunctionAsObject: Boolean;
begin
  Result :=
   (MethodKind = mkFunction) and (Length(Params) = 0) and (ResultType^.Kind = tkClass) and (CConv = ccReg);
end;

function TJCoreWSMethodData.IsProcedureConstObject: Boolean;
begin
  Result :=
   (MethodKind = mkProcedure) and (Length(Params) = 1) and
   (pfConst in Params[0].Flags) and (Params[0].ParamTypeInfo^.Kind = tkClass) and
   (CConv = ccReg);
end;

{ TJCoreWSProcObjectInvoker }

function TJCoreWSProcObjectInvoker.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := AMethodData.IsProcedureConstObject;
end;

procedure TJCoreWSProcObjectInvoker.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
type
  TInvokeMethod = procedure(const AObj: TObject) of object;
var
  VDestreamer: TJCoreWSJSONUnserializer;
  VContent: string;
  VObj: TObject;
begin
  VObj := nil;
  try
    VDestreamer := TJCoreWSJSONUnserializer.Create(nil);
    try
      VContent := ARequest.Content;
      if VContent <> '' then
      begin
        VObj := GetTypeData(AMethodData.Params[0].ParamTypeInfo)^.ClassType.Create;
        VDestreamer.JSONToObject(VContent, VObj);
      end;
      TInvokeMethod(AMethod)(VObj);
    finally
      FreeAndNil(VDestreamer);
    end;
  finally
    FreeAndNil(VObj);
  end;
end;

{ TJCoreWSFncObjectInvoker }

function TJCoreWSFncObjectInvoker.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := AMethodData.IsFunctionAsObject;
end;

procedure TJCoreWSFncObjectInvoker.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
type
  TInvokeMethod = function: TObject of object;
var
  VObj: TObject;
  VStreamer: TJCoreWSJSONSerializer;
begin
  AResponse.ContentType := 'application/json';
  VObj := TInvokeMethod(AMethod)();
  if Assigned(VObj) then
  begin
    try
      VStreamer := TJCoreWSJSONSerializer.Create(nil);
      try
        AResponse.Content := VStreamer.ObjectToJSONString(VObj);
      finally
        FreeAndNil(VStreamer);
      end;
    finally
      FreeAndNil(VObj);
    end;
  end else
    AResponse.Content := '';
end;

end.

