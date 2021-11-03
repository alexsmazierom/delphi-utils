unit Test.DelphiUtils.ExpressoesRegulares;

interface

uses
  TestFramework,
  System.SysUtils, System.StrUtils, System.Classes, System.Generics.Collections, System.RegularExpressions,
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
    procedure testar_metodo_classe_StringDeLetras_amostra_letras_esperado_True;
    procedure testar_metodo_classe_StringDeLetras_amostra_letras_digitos_esperado_False;
    procedure testar_metodo_classe_StringDeLetras_amostra_digitos_esperado_False;
    procedure testar_metodo_classe_StringDeLetras_amostra_letras_digitos_simbolos_esperado_False;
    procedure testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_maiusculas_esperado_True;
    procedure testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_minusculas_esperado_False;
    procedure testar_metodo_classe_StringDeLetrasMaiusculas_amostra_digitos_esperado_False;
    procedure testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_minusculas_esperado_True;
    procedure testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_maiusculas_esperado_False;
    procedure testar_metodo_classe_StringDeLetrasMinusculas_amostra_digitos_esperado_False;
    procedure testar_metodo_classe_StringDeDigitos_amostra_digitos_esperado_True;
    procedure testar_metodo_classe_StringDeDigitos_amostra_letras_esperado_False;
    procedure testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_esperado_False;
    procedure testar_metodo_classe_StringDeDigitos_amostra_letras_minusculas_esperado_False;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_simbolos_esperado_True;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_letras_esperado_False;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_esperado_False;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_letras_minusculas_esperado_False;
    procedure testar_metodo_classe_StringDeSimbolos_amostra_digitos_esperado_False;
    procedure testar_metodo_classe_StringAlfanumerica_amostra_alfanumerica_esperado_True;
    procedure testar_metodo_classe_RemoverEspacos_amostra_palavras_separadas_esperado_Pos_stringVazia_False;
    procedure testar_metodo_classe_EmailValido_amostra_email_valido_esperado_True;
    procedure testar_metodo_classe_EmailValido_amostra_email_invalido_esperado_False;
  end;

implementation

const
  C_AMOSTRA_LETRAS = 'aAzZÇçáÁ';
  C_AMOSTRA_LETRAS_DIGITOS = 'aAzZÇçáÁ2021';
  C_AMOSTRA_LETRAS_DIGITOS_SIMBOLOS = 'aA/zZ|Çç(áÁ)-2021';
  C_AMOSTRA_ALFANUMERICA = 'aAzZ2021';
  C_AMOSTRA_LETRAS_MINUSCULAS = 'aáäãz';
  C_AMOSTRA_LETRAS_MINUSCULAS_DIGITOS = 'aáäãz2021';
  C_AMOSTRA_LETRAS_MAIUSCULAS = 'AÁÄÃZ';
  C_AMOSTRA_LETRAS_MAIUSCULAS_DIGITOS = 'AÁÄÃZ2021';
  C_AMOSTRA_DIGITOS = '0123456789';
  C_AMOSTRA_SIMBOLOS = '.-/,''"~`@#$%&*()=[]{};<>?\';  // [-!$%^&*()_+|~=`´{}\[\]:";''<>?,.\/@#\\];
  C_AMOSTRA_EMAIL = 'nome@provedor.com';
  C_AMOSTRA_EMAIL_INVALIDO = '@provedor.com';

{ TestCase_classe_TDelphiUtilExpressoesRegulares }

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.SetUp;
begin
  ReportMemoryLeaksOnShutdown := True;
  FailsOnMemoryLeak := False;

end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.TearDown;
begin

end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetras_amostra_letras_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeLetras(C_AMOSTRA_LETRAS),
             'testar_metodo_classe_StringDeLetras_amostra_letras_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetras_amostra_letras_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetras(C_AMOSTRA_LETRAS_DIGITOS),
              'testar_metodo_classe_StringDeLetras_amostra_letras_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetras_amostra_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetras(C_AMOSTRA_DIGITOS),
              'testar_metodo_classe_StringDeLetras_amostra_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetras_amostra_letras_digitos_simbolos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetras(C_AMOSTRA_LETRAS_DIGITOS_SIMBOLOS),
              'testar_metodo_classe_StringDeLetras_amostra_letras_digitos_simbolos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_maiusculas_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeLetrasMaiusculas(C_AMOSTRA_LETRAS_MAIUSCULAS),
             'testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_maiusculas_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_minusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetrasMaiusculas(C_AMOSTRA_LETRAS_MINUSCULAS),
              'testar_metodo_classe_StringDeLetrasMaiusculas_amostra_letras_minusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMaiusculas_amostra_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetrasMaiusculas(C_AMOSTRA_DIGITOS),
              'testar_metodo_classe_StringDeLetrasMaiusculas_amostra_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_minusculas_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeLetrasMinusculas(C_AMOSTRA_LETRAS_MINUSCULAS),
             'testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_minusculas_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_maiusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetrasMinusculas(C_AMOSTRA_LETRAS_MAIUSCULAS),
              'testar_metodo_classe_StringDeLetrasMinusculas_amostra_letras_maiusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeLetrasMinusculas_amostra_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeLetrasMinusculas(C_AMOSTRA_DIGITOS),
              'testar_metodo_classe_StringDeLetrasMinusculas_amostra_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeDigitos_amostra_digitos_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeDigitos(C_AMOSTRA_DIGITOS),
             'testar_metodo_classe_StringDeDigitos_amostra_digitos_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeDigitos_amostra_letras_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeDigitos(C_AMOSTRA_LETRAS),
              'testar_metodo_classe_StringDeDigitos_amostra_letras_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeDigitos(C_AMOSTRA_LETRAS_MAIUSCULAS),
              'testar_metodo_classe_StringDeDigitos_amostra_letras_maiusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeDigitos_amostra_letras_minusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeDigitos(C_AMOSTRA_LETRAS_MINUSCULAS),
              'testar_metodo_classe_StringDeDigitos_amostra_letras_minusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_simbolos_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringDeSimbolos(C_AMOSTRA_SIMBOLOS),
             'testar_metodo_classe_StringDeSimbolos_amostra_simbolos_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_letras_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeSimbolos(C_AMOSTRA_LETRAS),
              'testar_metodo_classe_StringDeSimbolos_amostra_letras_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeSimbolos(C_AMOSTRA_LETRAS_MAIUSCULAS),
              'testar_metodo_classe_StringDeSimbolos_amostra_letras_maiusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_letras_minusculas_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeSimbolos(C_AMOSTRA_LETRAS_MINUSCULAS),
              'testar_metodo_classe_StringDeSimbolos_amostra_letras_minusculas_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringDeSimbolos_amostra_digitos_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.StringDeSimbolos(C_AMOSTRA_DIGITOS),
              'testar_metodo_classe_StringDeSimbolos_amostra_digitos_esperado_False: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_StringAlfanumerica_amostra_alfanumerica_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.StringAlfanumerica(C_AMOSTRA_ALFANUMERICA),
             'testar_metodo_classe_StringAlfanumerica_amostra_alfanumerica_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_RemoverEspacos_amostra_palavras_separadas_esperado_Pos_stringVazia_False;
const
  C_PALAVRAS = ' Delphi    CE  10.3.3   ' + 'Rio';
var
  LResultado: string;
begin
//  LResultado := C_PALAVRAS.Replace(' ',''); { obteve mesmo resultado }
  LResultado := TExpressoesRegularesUtil.RemoverEspacos(C_PALAVRAS);
  CheckFalse( Pos(' ', LResultado) = -1,
              'testar_metodo_classe_RemoverEspacos_amostra_palavras_separadas_esperado_Pos_stringVazia_False: falhou!' )
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_EmailValido_amostra_email_valido_esperado_True;
begin
  CheckTrue( TExpressoesRegularesUtil.EmailValido(C_AMOSTRA_EMAIL),
             'testar_metodo_classe_EmailValido_amostra_email_valido_esperado_True: falhou' );
end;

procedure TestCase_classe_TDelphiUtilExpressoesRegulares.testar_metodo_classe_EmailValido_amostra_email_invalido_esperado_False;
begin
  CheckFalse( TExpressoesRegularesUtil.EmailValido(C_AMOSTRA_EMAIL_INVALIDO),
              'testar_metodo_classe_EmailValido_amostra_email_invalido_esperado_False: falhou' );
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestCase_classe_TDelphiUtilExpressoesRegulares.Suite);

end.
