unit Test.DelphiUtils.MetodosAnonimos;

interface

uses
  TestFramework,
  System.SysUtils, System.StrUtils, System.Classes, System.Generics.Collections, System.IOUtils,
{ delphi-utils/fontes }
  DelphiUtils.Hub;

type
  /// <summary>
  ///   Caso de testes da classe utilitária TDelphiUtilMetodosAnonimos
  /// </summary>
  TestCase_classe_TDelphiUtilMetodosAnonimos = class(TTestCase)
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testar_metodo_classe_Iterar_de0a9_verificar_10_iteracoes_esperadas;
    procedure testar_metodo_classe_Iterar_de0a0_verificar_1_iteracao_esperada;
    procedure testar_metodo_classe_Iterar_de0a9_adicionando_TList_Integer_verificar_count;
    procedure testar_metodo_classe_Iterar_de0a9_incrementando_2_verificar_5_iteracoes_esperadas;
    procedure testar_metodo_classe_IterarReverso_de9a0_verificar_10_iteracoes_esperadas;
    procedure testar_metodo_classe_IterarReverso_de0a0_verificar_1_iteracao_esperada;
    procedure testar_metodo_classe_IterarReverso_de9a0_adicionando_TList_Integer_verificar_count;
    procedure testar_metodo_classe_IterarReverso_de9a0_decrementando_2_verificar_5_iteracoes_esperadas;
  end;

implementation

{ TestCase_classe_TDelphiUtilMetodosAnonimos }

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.SetUp;
begin
  ReportMemoryLeaksOnShutdown := True;
  FailsOnMemoryLeak := False;

end;

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.TearDown;
begin

end;


procedure TestCase_classe_TDelphiUtilMetodosAnonimos.testar_metodo_classe_Iterar_de0a9_verificar_10_iteracoes_esperadas;
const
  C_ESPERADO = 10;
var
  LDe, LAte, LAtual: Integer;
begin
  LDe := 0;
  LAte := 9;
  LAtual := 0;

  TMetodosAnonimosUtil.Iterar(LDe, LAte,
    procedure(I: Int64)
    begin
      Inc(LAtual);
    end);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_Iterar_de0a9_verificar_10_iteracoes_esperadas: falhou');
end;

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.testar_metodo_classe_Iterar_de0a0_verificar_1_iteracao_esperada;
const
  C_ESPERADO = 1;
var
  LDe, LAte, LAtual: Integer;
begin
  LDe := 0;
  LAte := 0;
  LAtual := 0;

  TMetodosAnonimosUtil.Iterar(LDe, LAte,
    procedure(I: Int64)
    begin
      Inc(LAtual);
    end);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_Iterar_de0a0_verificar_1_iteracao_esperada: falhou');
end;

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.testar_metodo_classe_Iterar_de0a9_adicionando_TList_Integer_verificar_count;
const
  C_ESPERADO = 10;
var
  LDe, LAte, LAtual: Integer;
  LIntLista: TList<Integer>;
begin
  LDe := 0;
  LAte := 9;
  LAtual := 0;

  LIntLista := TList<Integer>.Create();
  try
    TMetodosAnonimosUtil.Iterar(LDe, LAte,
      procedure(I: Int64)
      begin
        LIntLista.Add(I);
      end);
    LAtual := LIntLista.Count;
  finally
    LIntLista.Free;
  end;

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_Iterar_de0a9_adicionando_TList_Integer_verificar_count: falhou');
end;

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.testar_metodo_classe_Iterar_de0a9_incrementando_2_verificar_5_iteracoes_esperadas;
const
  C_ESPERADO = 5;
  C_INCREMENTO = 2;
var
  LDe, LAte, LAtual: Integer;
begin
  LDe := 0;
  LAte := 9;
  LAtual := 0;

  TMetodosAnonimosUtil.Iterar(LDe, LAte,
    procedure(I: Int64)
    begin
      Inc(LAtual);
    end, C_INCREMENTO);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_Iterar_de0a9_incrementando_2_verificar_5_iteracoes_esperadas: falhou');
end;

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.testar_metodo_classe_IterarReverso_de9a0_verificar_10_iteracoes_esperadas;
const
  C_ESPERADO = 10;
var
  LDe, LAte, LAtual: Integer;
begin
  LDe := 9;
  LAte := 0;
  LAtual := 0;

  TMetodosAnonimosUtil.IterarReverso(LDe, LAte,
    procedure(I: Int64)
    begin
      Inc(LAtual);
    end);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_IterarReverso_de9a0_verificar_10_iteracoes_esperadas: falhou');
end;

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.testar_metodo_classe_IterarReverso_de0a0_verificar_1_iteracao_esperada;
const
  C_ESPERADO = 1;
var
  LDe, LAte, LAtual: Integer;
begin
  LDe := 0;
  LAte := 0;
  LAtual := 0;

  TMetodosAnonimosUtil.IterarReverso(LDe, LAte,
    procedure(I: Int64)
    begin
      Inc(LAtual);
    end);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_IterarReverso_de0a0_verificar_1_iteracao_esperada: falhou');
end;

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.testar_metodo_classe_IterarReverso_de9a0_adicionando_TList_Integer_verificar_count;
const
  C_ESPERADO = 10;
var
  LDe, LAte, LAtual: Integer;
  LIntLista: TList<Integer>;
begin
  LDe := 9;
  LAte := 0;
  LAtual := 0;

  LIntLista := TList<Integer>.Create();
  try
    TMetodosAnonimosUtil.IterarReverso(LDe, LAte,
      procedure(I: Int64)
      begin
        LIntLista.Add(I);
      end);
    LAtual := LIntLista.Count;
  finally
    LIntLista.Free;
  end;

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_IterarReverso_de9a0_adicionando_TList_Integer_verificar_count: falhou');
end;

procedure TestCase_classe_TDelphiUtilMetodosAnonimos.testar_metodo_classe_IterarReverso_de9a0_decrementando_2_verificar_5_iteracoes_esperadas;
const
  C_ESPERADO = 5;
  C_DECREMENTO = 2;
var
  LDe, LAte, LAtual: Integer;
begin
  LDe := 9;
  LAte := 0;
  LAtual := 0;

  TMetodosAnonimosUtil.IterarReverso(LDe, LAte,
    procedure(I: Int64)
    begin
      Inc(LAtual);
    end, C_DECREMENTO);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_IterarReverso_de9a0_decrementando_2_verificar_5_iteracoes_esperadas: falhou');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestCase_classe_TDelphiUtilMetodosAnonimos.Suite);

end.
