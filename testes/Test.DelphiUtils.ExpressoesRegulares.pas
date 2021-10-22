unit Test.DelphiUtils.ExpressoesRegulares;

interface

uses
  TestFramework,
  System.SysUtils, System.StrUtils, System.Classes, System.Generics.Collections,
{ delphi-utils/fontes }
  DelphiUtils.Hub;

type
  /// <summary>
  ///   Caso de testes da classe utilitária TDelphiUtilExpressoesRegulares
  /// </summary>
  TestCase_classe_TDelphiUtilExpressoesRegulares = class(TTestCase)
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testar_metodo_classe_StringDeLetras_amostra_letras_maiusculas_minusculas_esperado_True;
    procedure testar_metodo_classe_StringDeLetras_amostra_letras_maiusculas_minusculas_digitos_esperado_False;
    procedure testar_metodo_classe_StringDeLetras_amostra_digitos_esperado_False;
    procedure testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_maiusculas_esperado_True;
    procedure testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_minusculas_esperado_False;
    procedure testar_metodo_classe_StringDeLetrasMaiusculas_amostra_digitos_esperado_False;
    procedure testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_minusculas_esperado_True;
    procedure testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_maiusculas_esperado_False;
    procedure testar_metodo_classe_StringDeLetrasMinusculas_amostra_digitos_esperado_False;
    procedure testar_metodo_classe_StringDeDigitos_amostra_digitos_esperado_True;
    procedure testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_minusculas_esperado_False;
    procedure testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_esperado_False;
    procedure testar_metodo_classe_StringDeDigitos_amostra_letras_minusculas_esperado_False;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_simbolos_esperado_True;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_minusculas_esperado_False;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_esperado_False;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_letras_minusculas_esperado_False;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_digitos_esperado_False;
  end;

implementation

const
  C_STR_LETRAS_MINUSCULAS_MAIUSCULAS = 'aAzZÇçáÁ';
  C_STR_LETRAS_MINUSCULAS_MAIUSCULAS_DIGITOS = 'aAzZÇçáÁ2021';
  C_STR_LETRAS_MINUSCULAS = 'aáäãz';
  C_STR_LETRAS_MINUSCULAS_DIGITOS = 'aáäãz2021';
  C_STR_LETRAS_MAIUSCULAS = 'AÁÄÃZ';
  C_STR_LETRAS_MAIUSCULAS_DIGITOS = 'AÁÄÃZ2021';
  C_STR_DIGITOS = '0123456789';
  C_STR_SIMBOLOS = '.-/,''"~`@#$%&*()=[]{};<>?\';  // [-!$%^&*()_+|~=`´{}\[\]:";''<>?,.\/@#\\];

{ TestCase_classe_TDelphiUtilExpressoesRegulares }

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.SetUp;
begin
  ReportMemoryLeaksOnShutdown := True;
  FailsOnMemoryLeak := False;

end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.TearDown;
begin

end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetras_amostra_letras_maiusculas_minusculas_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeLetras(C_STR_LETRAS_MINUSCULAS_MAIUSCULAS),
             'testar_metodo_classe_StringDeLetras_amostra_letras_maiusculas_minusculas_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetras_amostra_letras_maiusculas_minusculas_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetras(C_STR_LETRAS_MINUSCULAS_MAIUSCULAS_DIGITOS),
              'testar_metodo_classe_StringDeLetras_amostra_letras_maiusculas_minusculas_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetras_amostra_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetras(C_STR_DIGITOS),
              'testar_metodo_classe_StringDeLetras_amostra_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_maiusculas_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeLetrasMaiusculas(C_STR_LETRAS_MAIUSCULAS),
             'testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_maiusculas_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_minusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetrasMaiusculas(C_STR_LETRAS_MINUSCULAS),
              'testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_minusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMaiusculas_amostra_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetrasMaiusculas(C_STR_DIGITOS),
              'testar_metodo_classe_StringDeLetrasMaiusculas_amostra_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_minusculas_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeLetrasMinusculas(C_STR_LETRAS_MINUSCULAS),
             'testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_minusculas_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_maiusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetrasMinusculas(C_STR_LETRAS_MAIUSCULAS),
              'testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_maiusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMinusculas_amostra_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetrasMinusculas(C_STR_DIGITOS),
              'testar_metodo_classe_StringDeLetrasMinusculas_amostra_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeDigitos_amostra_digitos_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeDigitos(C_STR_DIGITOS),
             'testar_metodo_classe_StringDeDigitos_amostra_digitos_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_minusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeDigitos(C_STR_LETRAS_MINUSCULAS_MAIUSCULAS),
              'testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_minusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeDigitos(C_STR_LETRAS_MAIUSCULAS),
              'testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeDigitos_amostra_letras_minusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeDigitos(C_STR_LETRAS_MINUSCULAS),
              'testar_metodo_classe_StringDeDigitos_amostra_letras_minusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_simbolos_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeSimbolos(C_STR_SIMBOLOS),
             'testar_metodo_classe_StringDeSimbolos_amostra_simbolos_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_minusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeSimbolos(C_STR_LETRAS_MINUSCULAS_MAIUSCULAS),
              'testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_minusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeSimbolos(C_STR_LETRAS_MAIUSCULAS),
              'testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_letras_minusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeSimbolos(C_STR_LETRAS_MINUSCULAS),
              'testar_metodo_classe_StringDeSimbolos_amostra_letras_minusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeSimbolos(C_STR_DIGITOS),
              'testar_metodo_classe_StringDeSimbolos_amostra_digitos_esperado_False: falhou' );
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestCase_classe_TDelphiUtilExpressoesRegulares.Suite);

end.
