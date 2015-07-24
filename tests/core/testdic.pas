unit TestDIC;

{$mode objfpc}{$H+}

interface

uses
  fpcunit,
  JCoreDIC;

type

  { TTestDIC }

  TTestDIC = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FindImplementation;
    procedure FindInheritedImplementation;
    procedure FindOverridedImplementation;
    procedure FindQualifiedImplementation;
    procedure InterfaceNotFoundCheck;
    procedure AmbiguousImplementationCheck;
    procedure LazyRegistrationCheck;
    procedure LastLazyRegisterWinsCheck;
    procedure SingletonFactory;
    procedure InstanceFactory;
    procedure SingletonAndInstanceFactories;
  end;

  IHotColorFacade = interface
  ['{780FB9D4-63FC-46C2-B70C-498A62EB1C3C}']
    function GetValue: string;
    procedure SetValue(const AValue: string);
    function HotColorName: string;
    function MyClassName: string;
    property Value: string read GetValue write SetValue;
  end;

  IColdColorFacade = interface
  ['{997A211E-5D72-4194-96A1-77CB98C244FD}']
    function MyClassName: string;
    function ColdColorName: string;
  end;

  { TRedFacade }

  TRedFacade = class(TInterfacedObject, IHotColorFacade)
  private
    FValue: string;
    function GetValue: string;
    procedure SetValue(const AValue: string);
  public
    function HotColorName: string;
    function MyClassName: string;
  end;

  { TRubyRedFacade }

  TRubyRedFacade = class(TRedFacade)
  public
    function HotColorName: string;
    function MyClassName: string;
  end;

  { TYellowFacade }

  TYellowFacade = class(TInterfacedObject, IHotColorFacade)
  private
    FValue: string;
    function GetValue: string;
    procedure SetValue(const AValue: string);
  public
    function HotColorName: string;
    function MyClassName: string;
  end;

  { TBlueFacade }

  TBlueFacade = class(TInterfacedObject, IColdColorFacade)
  public
    function ColdColorName: string;
    function MyClassName: string;
  end;

  { TNavyBlueFacade }

  TNavyBlueFacade = class(TBlueFacade)
  public
    function ColdColorName: string;
    function MyClassName: string;
  end;

  { TGreenFacade }

  TGreenFacade = class(TInterfacedObject, IColdColorFacade)
  public
    function ColdColorName: string;
    function MyClassName: string;
  end;

implementation

uses
  testregistry,
  JCoreClasses;

{ TTestDIC }

procedure TTestDIC.SetUp;
begin
end;

procedure TTestDIC.TearDown;
begin
end;

procedure TTestDIC.FindImplementation;
var
  VFacade: IHotColorFacade;
begin
  TJCoreDIC.Register(IHotColorFacade, TRedFacade, jdsApplication);
  try
    TJCoreDIC.Locate(IHotColorFacade, VFacade);
    AssertEquals(VFacade.MyClassName, TRedFacade.ClassName);
  finally
    AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TRedFacade));
  end;
end;

procedure TTestDIC.FindInheritedImplementation;
var
  VFacade: IHotColorFacade;
begin
  TJCoreDIC.Register(IHotColorFacade, TRubyRedFacade, jdsApplication);
  try
    try
      TJCoreDIC.Locate(IHotColorFacade, VFacade);
      AssertEquals(VFacade.MyClassName, TRubyRedFacade.ClassName);
      TJCoreDIC.Register(IHotColorFacade, TRedFacade, jdsApplication);
      TJCoreDIC.Locate(IHotColorFacade, VFacade);
      AssertEquals(VFacade.MyClassName, TRubyRedFacade.ClassName);
    finally
      AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TRubyRedFacade));
    end;
    TJCoreDIC.Locate(IHotColorFacade, VFacade);
    AssertEquals(VFacade.MyClassName, TRedFacade.ClassName);
  finally
    AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TRedFacade));
  end;
end;

procedure TTestDIC.FindOverridedImplementation;
var
  VFacade: IColdColorFacade;
begin
  TJCoreDIC.Register(IColdColorFacade, TNavyBlueFacade, jdsApplication);
  try
    TJCoreDIC.Locate(IColdColorFacade, VFacade);
    AssertEquals(TNavyBlueFacade.ClassName, VFacade.MyClassName);
    TJCoreDIC.Register(IColdColorFacade, TGreenFacade, jdsApplication, TNavyBlueFacade);
    try
      TJCoreDIC.Locate(IColdColorFacade, VFacade);
      AssertEquals(TGreenFacade.ClassName, VFacade.MyClassName);
    finally
      AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TGreenFacade));
    end;
    TJCoreDIC.Locate(IColdColorFacade, VFacade);
    AssertEquals(TNavyBlueFacade.ClassName, VFacade.MyClassName);
  finally
    AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TNavyBlueFacade));
  end;
end;

procedure TTestDIC.FindQualifiedImplementation;
var
  VHotColor: IHotColorFacade;
begin
  TJCoreDIC.Register(IHotColorFacade, 'dark', TRubyRedFacade, jdsApplication);
  try
    TJCoreDIC.Register(IHotColorFacade, 'medium', TRedFacade, jdsApplication);
    try
      TJCoreDIC.Register(IHotColorFacade, 'light', TYellowFacade, jdsApplication);
      try
        TJCoreDIC.Locate(IHotColorFacade, 'medium', VHotColor);
        AssertEquals(TRedFacade.ClassName, VHotColor.MyClassName);
        TJCoreDIC.Locate(IHotColorFacade, 'dark', VHotColor);
        AssertEquals(TRubyRedFacade.ClassName, VHotColor.MyClassName);
        TJCoreDIC.Locate(IHotColorFacade, 'light', VHotColor);
        AssertEquals(TYellowFacade.ClassName, VHotColor.MyClassName);
      finally
        AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TYellowFacade));
      end;
    finally
      AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TRedFacade));
    end;
  finally
    AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TRubyRedFacade));
  end;
end;

procedure TTestDIC.InterfaceNotFoundCheck;
var
  VFacade: IHotColorFacade;
begin
  try
    TJCoreDIC.Locate(IHotColorFacade, VFacade);
    Fail('EJCoreDIC(0303) expected')
  except
    on E: EJCoreDIC do
      if E.Code <> 303 then
        raise;
  end;
end;

procedure TTestDIC.AmbiguousImplementationCheck;
begin
  TJCoreDIC.Register(IColdColorFacade, TNavyBlueFacade, jdsApplication);
  try
    try
      try
        TJCoreDIC.Register(IColdColorFacade, TGreenFacade, jdsApplication);
        Fail('EJCoreDIC(0302) expected');
      except
        on E: EJCoreDIC do
          if E.Code <> 302 then
            raise;
      end;
    finally
      AssertFalse(TJCoreDIC.Unregister(IColdColorFacade, TGreenFacade));
    end;
  finally
    AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TNavyBlueFacade));
  end;
end;

procedure TTestDIC.LazyRegistrationCheck;
var
  VColdColor: IColdColorFacade;
begin
  TJCoreDIC.LazyRegister(IColdColorFacade, TNavyBlueFacade, jdsApplication);
  try
    TJCoreDIC.Locate(IColdColorFacade, VColdColor);
    AssertEquals(TNavyBlueFacade.ClassName, VColdColor.MyClassName);
    TJCoreDIC.Register(IColdColorFacade, TGreenFacade, jdsApplication);
    try
      TJCoreDIC.Locate(IColdColorFacade, VColdColor);
      AssertEquals(TGreenFacade.ClassName, VColdColor.MyClassName);
    finally
      AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TGreenFacade));
    end;
    TJCoreDIC.Locate(IColdColorFacade, VColdColor);
    AssertEquals(TNavyBlueFacade.ClassName, VColdColor.MyClassName);
  finally
    AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TNavyBlueFacade));
  end;

  TJCoreDIC.Register(IColdColorFacade, TNavyBlueFacade, jdsApplication);
  try
    TJCoreDIC.LazyRegister(IColdColorFacade, TGreenFacade, jdsApplication);
    try
      TJCoreDIC.Locate(IColdColorFacade, VColdColor);
      AssertEquals(TNavyBlueFacade.ClassName, VColdColor.MyClassName);
    finally
      AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TGreenFacade));
    end;
  finally
    AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TNavyBlueFacade));
  end;
end;

procedure TTestDIC.LastLazyRegisterWinsCheck;
var
  VColdColor: IColdColorFacade;
begin
  TJCoreDIC.LazyRegister(IColdColorFacade, TNavyBlueFacade, jdsApplication);
  try
    TJCoreDIC.LazyRegister(IColdColorFacade, TGreenFacade, jdsApplication);
    try
      TJCoreDIC.Locate(IColdColorFacade, VColdColor);
      AssertEquals(TGreenFacade.ClassName, VColdColor.MyClassName);
      TJCoreDIC.Register(IColdColorFacade, TBlueFacade, jdsApplication);
      try
        TJCoreDIC.Locate(IColdColorFacade, VColdColor);
        AssertEquals(TBlueFacade.ClassName, VColdColor.MyClassName);
      finally
        AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TBlueFacade));
      end;
      TJCoreDIC.Locate(IColdColorFacade, VColdColor);
      AssertEquals(TGreenFacade.ClassName, VColdColor.MyClassName);
    finally
      AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TGreenFacade));
    end;
    TJCoreDIC.Locate(IColdColorFacade, VColdColor);
    AssertEquals(TNavyBlueFacade.ClassName, VColdColor.MyClassName);
  finally
    AssertTrue(TJCoreDIC.Unregister(IColdColorFacade, TNavyBlueFacade));
  end;
end;

procedure TTestDIC.SingletonFactory;
const
  CSingletonConfig = 'Singleton configuration';
var
  VFacade: IHotColorFacade;
begin
  TJCoreDIC.Register(IHotColorFacade, TRedFacade, jdsApplication);
  TJCoreDIC.Locate(IHotColorFacade, VFacade);
  VFacade.Value := CSingletonConfig;
  VFacade := nil;
  TJCoreDIC.Locate(IHotColorFacade, VFacade);
  AssertEquals(CSingletonConfig, VFacade.Value);
  AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TRedFacade));
end;

procedure TTestDIC.InstanceFactory;
const
  CInstanceConfig = 'Instance configuration';
var
  VFacade: IHotColorFacade;
begin
  TJCoreDIC.Register(IHotColorFacade, TRedFacade, jdsRequest);
  TJCoreDIC.Locate(IHotColorFacade, VFacade);
  VFacade.Value := CInstanceConfig;
  VFacade := nil;
  TJCoreDIC.Locate(IHotColorFacade, VFacade);
  AssertEquals('', VFacade.Value);
  AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TRedFacade));
end;

procedure TTestDIC.SingletonAndInstanceFactories;
const
  CSingletonConfig = 'Singleton configuration';
  CInstanceConfig = 'Instance configuration';
var
  VFacade: IHotColorFacade;
begin
  TJCoreDIC.Register(IHotColorFacade, TRedFacade, jdsApplication);
  TJCoreDIC.Locate(IHotColorFacade, VFacade);
  VFacade.Value := CSingletonConfig;
  VFacade := nil;
  TJCoreDIC.Locate(IHotColorFacade, VFacade);
  AssertEquals(CSingletonConfig, VFacade.Value);
  TJCoreDIC.Register(IHotColorFacade, TYellowFacade, jdsRequest, TRedFacade);
  TJCoreDIC.Locate(IHotColorFacade, VFacade);
  VFacade.Value := CInstanceConfig;
  VFacade := nil;
  TJCoreDIC.Locate(IHotColorFacade, VFacade);
  AssertEquals(TYellowFacade.ClassName, VFacade.MyClassName);
  AssertEquals('', VFacade.Value);
  AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TRedFacade));
  AssertTrue(TJCoreDIC.Unregister(IHotColorFacade, TYellowFacade));
end;

{ TRedFacade }

function TRedFacade.GetValue: string;
begin
  Result := FValue;
end;

procedure TRedFacade.SetValue(const AValue: string);
begin
  FValue := AValue;
end;

function TRedFacade.HotColorName: string;
begin
  Result := 'Red';
end;

function TRedFacade.MyClassName: string;
begin
  Result := ClassName;
end;

{ TRubyRedFacade }

function TRubyRedFacade.HotColorName: string;
begin
  Result := 'RubyRed';
end;

function TRubyRedFacade.MyClassName: string;
begin
  Result := ClassName;
end;

{ TYellowFacade }

function TYellowFacade.GetValue: string;
begin
  Result := FValue;
end;

procedure TYellowFacade.SetValue(const AValue: string);
begin
  FValue := AValue;
end;

function TYellowFacade.HotColorName: string;
begin
  Result := 'Yellow';
end;

function TYellowFacade.MyClassName: string;
begin
  Result := ClassName;
end;

{ TBlueFacade }

function TBlueFacade.ColdColorName: string;
begin
  Result := 'Blue';
end;

function TBlueFacade.MyClassName: string;
begin
  Result := ClassName;
end;

{ TNavyBlueFacade }

function TNavyBlueFacade.ColdColorName: string;
begin
  Result := 'NavyBlue';
end;

function TNavyBlueFacade.MyClassName: string;
begin
  Result := ClassName;
end;

{ TGreenFacade }

function TGreenFacade.ColdColorName: string;
begin
  Result := 'Green';
end;

function TGreenFacade.MyClassName: string;
begin
  Result := ClassName;
end;

initialization
  RegisterTest('jcore.core.dic', TTestDIC);

end.

