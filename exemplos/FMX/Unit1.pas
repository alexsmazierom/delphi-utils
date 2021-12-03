unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.IOUtils, System.StrUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI,
{ delphi-utils/fontes }
  DelphiUtils.Hub;

type
  TForm1 = class(TForm)
    ButtonUsarStringList: TButton;
    ButtonUsarFDQuery: TButton;
    StatusBar1: TStatusBar;
    FDConnectionSQLiteMemory: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDQuery1: TFDQuery;
    ButtonIterar: TButton;
    ButtonIterarReverso: TButton;
    GroupBox1: TGroupBox;
    ProgressBar1: TProgressBar;
    ButtonIterarIncremento5: TButton;
    ButtonStringDeLetras: TButton;
    ButtonStringDeDigitos: TButton;
    ButtonRemoverDigitos: TButton;
    ButtonEmailValido: TButton;
    ButtonArrayMapJS: TButton;
    ButtonArrayReduceJS: TButton;
    ButtonArrayFilterJS: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonUsarStringListClick(Sender: TObject);
    procedure ButtonUsarFDQueryClick(Sender: TObject);
    procedure ButtonIterarClick(Sender: TObject);
    procedure ButtonIterarReversoClick(Sender: TObject);
    procedure ButtonIterarIncremento5Click(Sender: TObject);
    procedure ButtonStringDeLetrasClick(Sender: TObject);
    procedure ButtonStringDeDigitosClick(Sender: TObject);
    procedure ButtonRemoverDigitosClick(Sender: TObject);
    procedure ButtonEmailValidoClick(Sender: TObject);
    procedure ButtonArrayMapJSClick(Sender: TObject);
    procedure ButtonArrayReduceJSClick(Sender: TObject);
    procedure ButtonArrayFilterJSClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
const
  C_DML_ADD_TEMPLATE = 'insert into COMPONENTE values (''%s'', %d, ''%s'')';
begin
  // iniciando base de dados SQLite em memória
  FDConnectionSQLiteMemory.Params.Database := ':memory:';
  FDConnectionSQLiteMemory.ExecSQL('ATTACH DATABASE ''file::memory:'' AS temp_schema');
  FDConnectionSQLiteMemory.Open;

  // criando tabela exemplo
  FDConnectionSQLiteMemory.ExecSQL('create table COMPONENTE (Nome TEXT NOT NULL, Tag Integer, Classe TEXT);');

  // populando com componentes da tela
  TMetodosAnonimosUtil.Iterar(0, ComponentCount - 1,
    procedure (I: Integer)
    begin
      FDConnectionSQLiteMemory.ExecSQL(
        Format(C_DML_ADD_TEMPLATE, [Components[I].Name, Components[I].Tag, Components[I].ClassName])
      );
    end);

end;

procedure TForm1.ButtonUsarStringListClick(Sender: TObject);
var
  LItens: Integer;
begin
  LItens := 0;

  { NOTA : classe TGenericosUtil não precisa ser instanciada pois utiliza métodos de classe,
    similar ao conceito de método estático no Java. }

  TGenericosUtil.Usar<TStringList>(
    // instanciando diretamente a lista
    TStringList.Create,
    // método anônimo com instância da lista
    procedure(StrLista: TStringList)
    begin
      StrLista.Add('#1 não requer de variável local para trabalhar com TStringList');
      StrLista.Add('#2 não provoca vazamentos de memória ("memory leaks") pois é liberado automaticamente');
      LItens := StrLista.Count;
    end);

  // formatando uma mensagem na tela para simples verificação
  ShowMessageFmt('Quantidade itens na lista: %d.', [LItens]);
end;

procedure TForm1.ButtonUsarFDQueryClick(Sender: TObject);
var
  LEncontrouComponente: Boolean;
  LNomeComponente: string;
begin
  LEncontrouComponente := False;
  LNomeComponente := (Sender as TComponent).Name;

  TGenericosUtil.Usar<TFDQuery>(
    TFDQuery.Create(nil),
    procedure(Query: TFDQuery)
    var
      LSQLConsulta: string;
    begin
      LSQLConsulta := Format('select * from COMPONENTE where Nome = ''%s''', [LNomeComponente]);
      Query.Connection := FDConnectionSQLiteMemory;
      Query.Open(LSQLConsulta);
      LEncontrouComponente := Query.RecordCount >= 1;
    end);

  ShowMessageFmt('Componente "%s" encontrado: %s', [LNomeComponente, BoolToStr(LEncontrouComponente, True)]);
end;

procedure TForm1.ButtonIterarClick(Sender: TObject);
var
  LIteracoes: Integer;
begin
  LIteracoes := 0;

  // for I := 1 to 10 do

  TMetodosAnonimosUtil.Iterar(1, 10,
    procedure(I: Int64)
    begin
      Inc(LIteracoes);
    end);

  ShowMessageFmt('Número de iterações: %d', [LIteracoes]);
end;

procedure TForm1.ButtonIterarReversoClick(Sender: TObject);
var
  LIteracoes: Integer;
begin
  LIteracoes := 0;

  // for I := 10 downto 1 do

  TMetodosAnonimosUtil.IterarReverso(10, 1,
    procedure(I: Int64)
    begin
      Inc(LIteracoes);
    end);

  ShowMessageFmt('Número de iterações: %d', [LIteracoes]);
end;

procedure TForm1.ButtonIterarIncremento5Click(Sender: TObject);
const
  C_INCREMENTO: Cardinal = 5;
var
  LIteracoes: Integer;
begin
  LIteracoes := 0;

  TButton(Sender).Enabled := False;
  try
    ProgressBar1.Value := 0;
    ProgressBar1.Max := 100;

    // for (int i = 0; i <= 100; i += 5) [Java]

    TMetodosAnonimosUtil.Iterar(ProgressBar1.Value, ProgressBar1.Max,
      procedure(I: Single)
      begin
        ProgressBar1.Value := I;
        Inc(LIteracoes);
        Sleep(100);
        Application.ProcessMessages;
      end,
      // modificado o incremento padrao de 1 em 1 para 5
      C_INCREMENTO);

  finally
    TButton(Sender).Text := 'Reiniciar';
    TButton(Sender).Enabled := True;
  end;

  ShowMessageFmt('Progresso atingiu %.2f%% com %d iterações.', [ProgressBar1.Value, LIteracoes]);
end;

procedure TForm1.ButtonStringDeLetrasClick(Sender: TObject);
const
  C_AMOSTRA_LETRAS_DIGITOS = 'aAzZÇçáÁ2021';
var
  LResultado: Boolean;
begin
  LResultado := TExpressoesRegularesUtil.StringDeLetras(C_AMOSTRA_LETRAS_DIGITOS);

  // resultado esperado: False (falso)
  ShowMessageFmt('Amostra "%s" é uma string de letras apenas? %s',
    [C_AMOSTRA_LETRAS_DIGITOS, BoolToStr(LResultado, True)]);
end;

procedure TForm1.ButtonStringDeDigitosClick(Sender: TObject);
const
  C_AMOSTRA_DIGITOS = '0123456789';
var
  LResultado: Boolean;
begin
  LResultado := TExpressoesRegularesUtil.StringDeDigitos(C_AMOSTRA_DIGITOS);

  // resultado esperado: True (verdadeiro)
  ShowMessageFmt('Amostra "%s" é uma string dígitos apenas? %s', [C_AMOSTRA_DIGITOS, BoolToStr(LResultado, True)]);
end;

procedure TForm1.ButtonRemoverDigitosClick(Sender: TObject);
const
  C_TEXTO_COM_DIGITOS = 'Exemplo 2021';
var
  LResultado: string;
begin
  LResultado := TExpressoesRegularesUtil.RemoverDigitos(C_TEXTO_COM_DIGITOS);

  // resultado esperado: "Exemplo"
  ShowMessageFmt('Resultado do texto "%s" sem dígitos: "%s"', [C_TEXTO_COM_DIGITOS, LResultado]);
end;

procedure TForm1.ButtonEmailValidoClick(Sender: TObject);
var
  LArrayAmostra: TArray<string>;
  LAmostra, LResultados: string;
begin
  LArrayAmostra := ['nome@', 'nome@provedor.combr', 'nome sobrenome@provedor.com', 'eu@c', '1@1.1', '@provedor', 'nome.provedor.com', 'e@e.co', 'nome@provedor.com.br'];
  LResultados := 'Resultados da validação da amostra de emails:' + sLineBreak;

  for LAmostra in LArrayAmostra do
    LResultados := LResultados
      + ' - '
      + LAmostra.QuotedString('"')
      + ': '
      + IfThen(TExpressoesRegularesUtil.EmailValido(LAmostra), 'válido', 'inválido')
      + sLineBreak;

  ShowMessage(LResultados);
end;

procedure TForm1.ButtonArrayMapJSClick(Sender: TObject);
var
  LArrayEntrada, LArraySaida: TArray<Currency>;
  LResultados: string;
begin
  LArrayEntrada := [10.0, 20.0, 30.0];

  LArraySaida := TMetodosAnonimosUtil.Map<Currency>(
    LArrayEntrada,
    function(Elemento: Currency): Currency
    begin
      Result := Elemento * 1.1;
    end);

  LResultados :=
    Format('Array de entrada: [%.1f | %.1f | %.1f]', [ LArrayEntrada[0], LArrayEntrada[1], LArrayEntrada[2] ])
    + sLineBreak
    + Format('Array de saída*: [%.1f | %.1f | %.1f]', [ LArraySaida[0], LArraySaida[1], LArraySaida[2] ])
    + sLineBreak
    + sLineBreak
    + '(*) ajuste de 10%';
  ShowMessage(LResultados);
end;

procedure TForm1.ButtonArrayReduceJSClick(Sender: TObject);
var
  LArrayEntrada: TArray<Currency>;
  LSoma: Currency;
  LMensagem: string;
begin
  LArrayEntrada := [1.99, 9.99, 1041.17];

  LSoma := TMetodosAnonimosUtil.Reduce<Currency>(
    LArrayEntrada,
    function(ValorAcumulado, Item: Currency): Currency
    begin
      Result := ValorAcumulado + Item;
    end,
    0.00);

  LMensagem :=
    Format('Soma: %.2f + %.2f + %.2f = %.2f', [ LArrayEntrada[0], LArrayEntrada[1], LArrayEntrada[2], LSoma ]);

  ShowMessage(LMensagem);
end;

procedure TForm1.ButtonArrayFilterJSClick(Sender: TObject);
var
  I: Integer;
  LMensagem, LNumeros, LPares: string;
  LArrayEntrada, LAarraySaida: TArray<Integer>;
begin
  LArrayEntrada := [1,2,3,4,5,7,9,10,11];

  for I := Low(LArrayEntrada) to High(LArrayEntrada) do
    LNumeros := LNumeros + IfThen(I>0,', ','') + LArrayEntrada[I].ToString;

  LAarraySaida := TMetodosAnonimosUtil.Filter<Integer>(
    LArrayEntrada,
    function(Item: Integer): Boolean
    begin
      Result := Item mod 2 = 0;
    end);

  for I := Low(LAarraySaida) to High(LAarraySaida) do
    LPares := LPares + IfThen(I>0,', ','') + LAarraySaida[I].ToString;

  LMensagem :=
    'Array de entrada : [' + LNumeros + ']'
    + sLineBreak
    + 'Array com filtro de pares: [' + LPares + ']';

  ShowMessage(LMensagem);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
