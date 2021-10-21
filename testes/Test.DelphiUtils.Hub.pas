unit Test.DelphiUtils.Hub;

interface

uses
  TestFramework,
  System.SysUtils, System.StrUtils, System.Classes, System.Generics.Collections, System.IOUtils,
  Vcl.Dialogs,
  Datasnap.DBClient,
  Data.DB,
{ delphi-utils/fontes }
  DelphiUtils.Hub;

type
  /// <summary>
  ///   Caso de testes da classe utilitária TGenericosUtil
  /// </summary>
  TestCase_classe_TGenericosUtil = class(TTestCase)
  private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testar_metodo_classe_Usar_instanciando_TStringList_verificar_instancia;
    procedure testar_metodo_classe_Usar_instanciando_TStringList_com_tratamento_excecao_EDivByZero;
    procedure testar_metodo_classe_Usar_instanciando_TStringList_com_tratamento_excecao_silenciosa;
    procedure testar_metodo_classe_Usar_instanciando_TClientDataSet_CreateDataSet_Append_1_registro;
    procedure testar_metodo_classe_Retornar_instanciando_TStringList_retornando_Integer_qtde_itens_adicionados;
    procedure testar_metodo_classe_Retornar_instanciando_TClientDataSet_retornando_Integer_qtde_registros_adicionados;
  end;

implementation

{ TestCase_classe_TGenericosUtil }

procedure TestCase_classe_TGenericosUtil.SetUp;
begin
  ReportMemoryLeaksOnShutdown := True;
  FailsOnMemoryLeak := False;

end;

procedure TestCase_classe_TGenericosUtil.TearDown;
begin

end;

procedure TestCase_classe_TGenericosUtil.testar_metodo_classe_Usar_instanciando_TStringList_verificar_instancia;
const
  C_ESPERADO = True;
var
  LAtual: Boolean;
begin
  TGenericosUtil.Usar<TStringList>(
    TStringList.Create,
    procedure(Lista: TStringList)
    begin
      LAtual := Assigned(Lista);
    end);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_Usar_instanciando_TStringList_verificar_instancia: falhou');
end;

procedure TestCase_classe_TGenericosUtil.testar_metodo_classe_Usar_instanciando_TStringList_com_tratamento_excecao_EDivByZero;
var
  LDetectouErroDivisaoPorZero: Boolean;
begin
  LDetectouErroDivisaoPorZero := False;

  TGenericosUtil.Usar<TStringList>(
    TStringList.Create,
    procedure(Lista: TStringList)
    begin
      raise EDivByZero.Create('Erro EDivByZero: divisão por zero.');
    end,
    procedure(E: Exception)
    begin
      if E is EDivByZero then begin
        LDetectouErroDivisaoPorZero := True;
        ShowMessage(E.ToString);
      end;
    end);

  CheckTrue(LDetectouErroDivisaoPorZero, 'testar_metodo_classe_Usar_instanciando_TStringList_com_tratamento_excecao_EDivByZero: falhou');
end;

procedure TestCase_classe_TGenericosUtil.testar_metodo_classe_Usar_instanciando_TStringList_com_tratamento_excecao_silenciosa;
var
  LDetectouErroDivisaoPorZero: Boolean;
begin
  LDetectouErroDivisaoPorZero := False;

  TGenericosUtil.Usar<TStringList>(
    TStringList.Create,
    procedure(Lista: TStringList)
    begin
      raise EDivByZero.Create('Erro EDivByZero: divisão por zero.');
    end,
    procedure(E: Exception)
    begin
      if E is EDivByZero then
        LDetectouErroDivisaoPorZero := True;
    end);

  CheckTrue(LDetectouErroDivisaoPorZero, 'testar_metodo_classe_Usar_instanciando_TStringList_com_tratamento_excecao_silenciosa: falhou');
end;

procedure TestCase_classe_TGenericosUtil.testar_metodo_classe_Usar_instanciando_TClientDataSet_CreateDataSet_Append_1_registro;
const
  C_ESPERADO = 1;
var
  LAtual: Integer;
begin
  LAtual := 0;

  TGenericosUtil.Usar<TClientDataSet>(
    TClientDataSet.Create(nil),
    procedure(CDS: TClientDataSet)
    begin
      CDS.FieldDefs.Add('ID', ftInteger, 0, True);
      CDS.FieldDefs.Add('NAME', ftString, 80, True);
      CDS.CreateDataSet;
      CDS.Append;
      CDS.FieldByName('ID').AsInteger := 1;
      CDS.FieldByName('NAME').AsString := UnitName;
      CDS.Post;
      LAtual := CDS.RecordCount;
    end);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_Usar_instanciando_TClientDataSet_CreateDataSet_Append_1_registro: falhou');
end;

procedure TestCase_classe_TGenericosUtil.testar_metodo_classe_Retornar_instanciando_TStringList_retornando_Integer_qtde_itens_adicionados;
const
  C_ESPERADO = 1;
var
  LAtual: Integer;
begin
  LAtual :=
    TGenericosUtil.Retornar<TStringList, Integer>(
      TStringList.Create,
      function(Lista: TStringList): Integer
      begin
        Result := 0;
        Lista.Add(UnitName);
        Result := Lista.Count;
      end);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_Retornar_instanciando_TStringList_retornando_Integer_qtde_itens_adicionados: falhou');
end;

procedure TestCase_classe_TGenericosUtil.testar_metodo_classe_Retornar_instanciando_TClientDataSet_retornando_Integer_qtde_registros_adicionados;
const
  C_ESPERADO = 1;
var
  LAtual: Integer;
begin
  LAtual :=
    TGenericosUtil.Retornar<TClientDataSet, Integer>(
      TClientDataSet.Create(nil),
      function(CDS: TClientDataSet): Integer
      begin
        Result := 0;
        CDS.FieldDefs.Add('ID', ftInteger, 0, True);
        CDS.FieldDefs.Add('NAME', ftString, 80, True);
        CDS.CreateDataSet;
        CDS.Append;
        CDS.FieldByName('ID').AsInteger := 1;
        CDS.FieldByName('NAME').AsString := UnitName;
        CDS.Post;
        Result := CDS.RecordCount;
      end);

  CheckEquals(C_ESPERADO, LAtual, 'testar_metodo_classe_Retornar_instanciando_TClientDataSet_retornando_Integer_qtde_registros_adicionados: falhou');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestCase_classe_TGenericosUtil.Suite);

end.
