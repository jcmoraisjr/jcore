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
    procedure ReadTypeInfo(const ATypeInfo: PTypeInfo);
  public
    constructor Create(const ATypeInfo: PTypeInfo);
    function MatchFunction(const AParams: array of const; const AResult: TTypeKind): Boolean;
    function MatchFunction(const AParams: array of const; const AResult: TClass): Boolean;
    function MatchParams(const AParams: array of const): Boolean;
    function MatchProcedure(const AParams: array of const): Boolean;
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

procedure TJCoreWSMethodData.ReadTypeInfo(const ATypeInfo: PTypeInfo);
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

constructor TJCoreWSMethodData.Create(const ATypeInfo: PTypeInfo);
begin
  if ATypeInfo^.Kind <> tkMethod then
    raise EJCoreWS.Create(3103, S3103_TypeinfoIsNotMethod, []);
  inherited Create;
  ReadTypeInfo(ATypeInfo);
end;

function TJCoreWSMethodData.MatchFunction(const AParams: array of const; const AResult: TTypeKind): Boolean;
begin
  Result :=
   (MethodKind = mkFunction) and (ResultType^.Kind = AResult) and (CConv = ccReg) and MatchParams(AParams);
end;

function TJCoreWSMethodData.MatchFunction(const AParams: array of const; const AResult: TClass): Boolean;
begin
  Result := (MethodKind = mkFunction) and (ResultType^.Kind = tkClass) and (CConv = ccReg)
   and (GetTypeData(ResultType)^.ClassType.InheritsFrom(AResult)) and MatchParams(AParams);
end;

function TJCoreWSMethodData.MatchParams(const AParams: array of const): Boolean;
var
  VKind: TTypeKind;
  I: Integer;
begin
  Result := Length(AParams) = Length(Params);
  if Result then
    for I := Low(AParams) to High(AParams) do
    begin
      Result := pfConst in Params[I].Flags;
      if Result then
      begin
        VKind := Params[I].ParamTypeInfo^.Kind;
        case AParams[I].VType of
          vtInteger{TTypeKind}: Result := VKind = TTypeKind(AParams[I].VInteger);
          vtClass: Result := (VKind = tkClass)
           and (GetTypeData(Params[I].ParamTypeInfo)^.ClassType.InheritsFrom(AParams[I].VClass));
          else Result := False;
        end;
      end;
      if not Result then
        Exit;
    end;
end;

function TJCoreWSMethodData.MatchProcedure(const AParams: array of const): Boolean;
begin
  Result := (MethodKind = mkProcedure) and (CConv = ccReg) and MatchParams(AParams);
end;

{ TJCoreWSProcObjectInvoker }

function TJCoreWSProcObjectInvoker.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := AMethodData.MatchProcedure([TObject]);
end;

procedure TJCoreWSProcObjectInvoker.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
type
  TInvokeMethod = procedure(const AObj: TObject) of object;
var
  VUnserializer: TJCoreWSJSONUnserializer;
  VContent: string;
  VObj: TObject;
begin
  VObj := nil;
  try
    VUnserializer := TJCoreWSJSONUnserializer.Create(nil);
    try
      VContent := ARequest.Content;
      if VContent <> '' then
      begin
        VObj := GetTypeData(AMethodData.Params[0].ParamTypeInfo)^.ClassType.Create;
        VUnserializer.JSONToObject(VContent, VObj);
      end;
      TInvokeMethod(AMethod)(VObj);
    finally
      FreeAndNil(VUnserializer);
    end;
  finally
    FreeAndNil(VObj);
  end;
end;

{ TJCoreWSFncObjectInvoker }

function TJCoreWSFncObjectInvoker.Match(const AMethodData: TJCoreWSMethodData): Boolean;
begin
  Result := AMethodData.MatchFunction([], TObject);
end;

procedure TJCoreWSFncObjectInvoker.Invoke(const AMethodData: TJCoreWSMethodData; const ARequest: TRequest;
  const AResponse: TResponse; const AMethod: TMethod);
type
  TInvokeMethod = function: TObject of object;
var
  VObj: TObject;
  VSerializer: TJCoreWSJSONSerializer;
begin
  AResponse.ContentType := 'application/json';
  VObj := TInvokeMethod(AMethod)();
  if Assigned(VObj) then
  begin
    try
      VSerializer := TJCoreWSJSONSerializer.Create(nil);
      try
        AResponse.Content := VSerializer.ObjectToJSONString(VObj);
      finally
        FreeAndNil(VSerializer);
      end;
    finally
      FreeAndNil(VObj);
    end;
  end else
    AResponse.Content := '';
end;

end.

