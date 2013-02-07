program JCoreTestsGUI;

{$mode objfpc}{$H+}

uses
  heaptrc, sysutils, Interfaces, Forms, GuiTestRunner,
  TestExpression, TestDIC;

{$R *.res}

begin
{$ifdef windows}
  DeleteFile('heaptrace.log');
  SetHeapTraceOutput('heaptrace.log');
{$endif}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

