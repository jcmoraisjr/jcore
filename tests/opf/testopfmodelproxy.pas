unit TestOPFModelProxy;

{$mode objfpc}{$H+}

interface

uses
  JCoreClasses,
  JCoreEntity;

type

  { TTestProxyBase }

  TTestProxyBase = class(TJCoreManagedObject)
    _Proxy: TJCoreEntityProxy;
  protected
    procedure Finit; override;
  end;

  { TTestProxyCity }

  TTestProxyCity = class(TTestProxyBase)
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

implementation

uses
  sysutils;

{ TTestProxyBase }

procedure TTestProxyBase.Finit;
begin
  FreeAndNil(_Proxy);
  inherited Finit;
end;

end.

