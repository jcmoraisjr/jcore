program JCoreTestsGUI;

{$mode objfpc}{$H+}

uses
  heaptrc, sysutils, Interfaces, Forms, GuiTestRunner,
  TestExpression, TestDIC;

{$R *.res}

begin
  DeleteFile('heaptrace.log');
  SetHeapTraceOutput('heaptrace.log');
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

