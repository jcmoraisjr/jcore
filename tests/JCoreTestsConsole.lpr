program JCoreTestsConsole;

{$mode objfpc}{$H+}

uses
  heaptrc, Classes, consoletestrunner, testexpression, testdic, testopf;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
