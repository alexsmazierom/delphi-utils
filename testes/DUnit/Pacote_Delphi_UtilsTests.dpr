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
  Test.DelphiUtils.Genericos in 'src\Test.DelphiUtils.Genericos.pas',
  Test.DelphiUtils.MetodosAnonimos in 'src\Test.DelphiUtils.MetodosAnonimos.pas',
  Test.DelphiUtils.ExpressoesRegulares in 'src\Test.DelphiUtils.ExpressoesRegulares.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

