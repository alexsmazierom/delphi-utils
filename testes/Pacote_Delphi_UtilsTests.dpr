program Pacote_Delphi_UtilsTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  Test.DelphiUtils.Genericos in 'Test.DelphiUtils.Genericos.pas',
  Test.DelphiUtils.MetodosAnonimos in 'Test.DelphiUtils.MetodosAnonimos.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

