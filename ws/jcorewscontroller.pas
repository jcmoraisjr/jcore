(*
  JCore WebServices, Controller Handler Classes
  Copyright (C) 2015 Joao Morais

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JCoreWSController;

{$I jcore.inc}
{$WARN 5024 OFF} // hint 'parameter not used'

interface

uses
  typinfo,
  fgl,
  HTTPDefs,
  JCoreClasses,
  JCoreWSInvokers;

type

  { IJCoreWSMethodRegistry }

  IJCoreWSMethodRegistry = interface(IInterface)
  ['{BEBE3BC1-E802-D43D-6F49-E95E733C9439}']
    procedure AddInvoker(const AInvokers: array of TJCoreWSMethodInvokerClass);
    function FindInvoker(const AMethodData: TJCoreWSMethodData): TJCoreWSMethodInvoker;
  end;

  { TJCoreWSMethodRegistry }

  TJCoreWSMethodRegistry = class(TInterfacedObject, IJCoreWSMethodRegistry)
  private
    FInvokers: TJCoreWSMethodInvokerList;
  public
    destructor Destroy; override;
    procedure AddInvoker(const AInvokers: array of TJCoreWSMethodInvokerClass);
    function FindInvoker(const AMethodData: TJCoreWSMethodData): TJCoreWSMethodInvoker;
  end;

  { TJCoreWSControllerMethod }

  TJCoreWSControllerMethod = class(TObject)
  private
    FControllerClass: TClass;
    FInvoker: TJCoreWSMethodInvoker;
    FMethodAddr: Pointer;
    FMethodData: TJCoreWSMethodData;
    FMethodName: string;
    FMethodPattern: string;
    FMethodRegistry: IJCoreWSMethodRegistry;
    FMethodURLFrag: string;
    function GetInvoker: TJCoreWSMethodInvoker;
  protected
    property Invoker: TJCoreWSMethodInvoker read GetInvoker;
    property MethodData: TJCoreWSMethodData read FMethodData;
  public
    constructor Create(const AControllerClass: TClass; const AMethodAddr: CodePointer; const AMethodTypeInfo: PTypeInfo; const AMethodPattern: string = '');
    destructor Destroy; override;
    function AcceptRequestMethod(const ARequestMethod: string): Boolean;
    procedure HandleRequest(const ARequest: TRequest; const AResponse: TResponse);
    property ControllerClass: TClass read FControllerClass;
    property MethodPattern: string read FMethodPattern;
    property MethodURLFrag: string read FMethodURLFrag;
  end;

  TJCoreWSControllerMethodList = specialize TFPGObjectList<TJCoreWSControllerMethod>;

  { TJCoreWSControllerClass }

  TJCoreWSControllerClass = class(TObject)
  private
    FControllerClass: TClass;
    FControllerURLFrag: string;
    FMethods: TJCoreWSControllerMethodList;
    function FormatControllerURLFrag(const AControllerClass: TClass): string;
  protected
    property Methods: TJCoreWSControllerMethodList read FMethods;
  public
    constructor Create(const AControllerClass: TClass; const AControllerURLFrag: string = '');
    destructor Destroy; override;
    function AddMethod(const AMethodAddr: CodePointer; const AMethodTypeInfo: PTypeInfo; const AMethodPattern: string = ''): TJCoreWSControllerClass;
    function FindMethod(const AMethodURLFrag: string): TJCoreWSControllerMethod;
    property ControllerClass: TClass read FControllerClass;
    property ControllerURLFrag: string read FControllerURLFrag;
  end;

  TJCoreWSControllerClassList = specialize TFPGObjectList<TJCoreWSControllerClass>;

implementation

uses
  sysutils,
  JCoreConsts,
  JCoreDIC;

{ TJCoreWSMethodRegistry }

destructor TJCoreWSMethodRegistry.Destroy;
begin
  FreeAndNil(FInvokers);
  inherited Destroy;
end;

procedure TJCoreWSMethodRegistry.AddInvoker(const AInvokers: array of TJCoreWSMethodInvokerClass);
var
  VInvoker: TJCoreWSMethodInvokerClass;
begin
  if not Assigned(FInvokers) then
    FInvokers := TJCoreWSMethodInvokerList.Create(True);
  for VInvoker in AInvokers do
    FInvokers.Add(VInvoker.Create);
end;

function TJCoreWSMethodRegistry.FindInvoker(const AMethodData: TJCoreWSMethodData): TJCoreWSMethodInvoker;
var
  VCurrent: TJCoreWSMethodInvoker;
  I: Integer;
begin
  Result := nil;
  if Assigned(FInvokers) then
    for I := 0 to Pred(FInvokers.Count) do
      if FInvokers[I].Match(AMethodData) then
      begin
        VCurrent := FInvokers[I];
        if not Assigned(Result) or VCurrent.InheritsFrom(Result.ClassType) then
          Result := VCurrent
        else if not Result.InheritsFrom(VCurrent.ClassType) then
          raise EJCoreWS.Create(3104, S3104_AmbiguousMethodInvokers, [
           VCurrent.ClassName, Result.ClassName]);
      end;
end;

{ TJCoreWSControllerMethod }

function TJCoreWSControllerMethod.GetInvoker: TJCoreWSMethodInvoker;
begin
  if not Assigned(FInvoker) then
  begin
    FInvoker := FMethodRegistry.FindInvoker(MethodData);
    if not Assigned(FInvoker) then
      raise EJCoreWS.Create(3101, S3101_UnsupportedMethod, [ControllerClass.ClassName, FMethodName]);
  end;
  Result := FInvoker;
end;

constructor TJCoreWSControllerMethod.Create(const AControllerClass: TClass; const AMethodAddr: CodePointer;
  const AMethodTypeInfo: PTypeInfo; const AMethodPattern: string);
begin
  inherited Create;
  FControllerClass := AControllerClass;
  FMethodAddr := AMethodAddr;
  FMethodPattern := AMethodPattern;
  FMethodName := FControllerClass.MethodName(FMethodAddr);
  FMethodURLFrag := LowerCase(FMethodName);
  if AMethodTypeInfo^.Kind <> tkMethod then
    raise EJCoreWS.Create(3102, S3102_TypeinfoIsNotMethod, [FControllerClass.ClassName, FMethodName]);
  FMethodData := TJCoreWSMethodData.Create(AMethodTypeInfo);
  TJCoreDIC.Locate(IJCoreWSMethodRegistry, FMethodRegistry);
end;

destructor TJCoreWSControllerMethod.Destroy;
begin
  FreeAndNil(FMethodData);
  inherited Destroy;
end;

function TJCoreWSControllerMethod.AcceptRequestMethod(const ARequestMethod: string): Boolean;
begin
  { TODO : Implement }
  Result := True;
end;

procedure TJCoreWSControllerMethod.HandleRequest(const ARequest: TRequest; const AResponse: TResponse);
var
  VController: TObject;
  VMethod: TMethod;
begin
  VController := ControllerClass.Create;
  try
    VMethod.Data := VController;
    VMethod.Code := FMethodAddr;
    Invoker.Invoke(MethodData, ARequest, AResponse, VMethod);
  finally
    FreeAndNil(VController);
  end;
end;

{ TJCoreWSControllerClass }

function TJCoreWSControllerClass.FormatControllerURLFrag(const AControllerClass: TClass): string;
var
  VClassName, VControllerURLFrag: string;
begin
  VClassName := AControllerClass.ClassName;
  if LowerCase(VClassName[1]) = 't' then
    VControllerURLFrag := Copy(VClassName, 2, Length(VClassName))
  else
    VControllerURLFrag := VClassName;
  if LowerCase(RightStr(VControllerURLFrag, 10)) = 'controller' then
    VControllerURLFrag := LeftStr(VControllerURLFrag, Length(VControllerURLFrag) - 10);
  Result := LowerCase(VControllerURLFrag);
end;

constructor TJCoreWSControllerClass.Create(const AControllerClass: TClass;
  const AControllerURLFrag: string);
begin
  inherited Create;
  FControllerClass := AControllerClass;
  if AControllerURLFrag <> '' then
    FControllerURLFrag := AControllerURLFrag
  else
    FControllerURLFrag := FormatControllerURLFrag(FControllerClass);
  FMethods := TJCoreWSControllerMethodList.Create(True);
end;

destructor TJCoreWSControllerClass.Destroy;
begin
  FreeAndNil(FMethods);
  inherited Destroy;
end;

function TJCoreWSControllerClass.AddMethod(const AMethodAddr: CodePointer;
  const AMethodTypeInfo: PTypeInfo; const AMethodPattern: string): TJCoreWSControllerClass;
var
  VMethod: TJCoreWSControllerMethod;
begin
  VMethod := TJCoreWSControllerMethod.Create(ControllerClass, AMethodAddr, AMethodTypeInfo, AMethodPattern);
  Methods.Add(VMethod);
  Result := Self;
end;

function TJCoreWSControllerClass.FindMethod(const AMethodURLFrag: string): TJCoreWSControllerMethod;
var
  I: Integer;
begin
  for I := 0 to Pred(Methods.Count) do
  begin
    Result := Methods[I];
    if Result.MethodURLFrag = AMethodURLFrag then
      Exit;
  end;
  Result := nil;
end;

initialization
  TJCoreDIC.LazyRegister(IJCoreWSMethodRegistry, TJCoreWSMethodRegistry, jdsApplication);

finalization
  TJCoreDIC.Unregister(IJCoreWSMethodRegistry, TJCoreWSMethodRegistry);

end.

