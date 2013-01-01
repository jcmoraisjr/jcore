program JCoreTestsGUI;

{$mode objfpc}{$H+}

uses
  heaptrc, Interfaces, Forms, GuiTestRunner, testexpression;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

