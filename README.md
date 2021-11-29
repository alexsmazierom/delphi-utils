# delphi-utils
Classes utilitárias usando os recursos mais atuais da linguagem Delphi para simplificar aplicações do dia-a-dia, com código limpo, centralizado, de baixo aclopamento e reutilizável.

IDE utilizada para compilação e testes: Delphi 10.3 CE (Community Edition) com middleware FireDAC.

## Instalação
Adicionar o caminho completo da pasta fontes, ex.: `C:\delphi-utils-main\fontes` ao Library Path da IDE Delphi (Tools > Options > Language > Delphi > Library).

## Uses

Uma vez adicionado a pasta de fontes ao Library Path, adicionar no uses apenas a unit `DelphiUtils.Hub`, que facilita o acesso a todos as classes utilitárias do pacote com o mínimo de acoplamento.

```delphi
uses
{ delphi-utils/fontes }  
  DelphiUtils.Hub;
```

## Exemplos de implementação

### Generics ou "Genéricos"

Trecho de código de exemplo com método `Usar<T: class>(AObjeto: T; AProcedimento: TProc<T>)`:

```delphi
uses
  System.Classes,
  Vcl.Dialogs,
{ delphi-utils/fontes }  
  DelphiUtils.Hub;

procedure TForm1.Button1Click(Sender: TObject);
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
```

Veja outras implementações, como por exemplo, usando TFDQuery (FireDAC) em [Exemplos VCL](https://github.com/alexsmazierom/delphi-utils/tree/main/exemplos/VCL) ou [Exemplos FMX](https://github.com/alexsmazierom/delphi-utils/tree/main/exemplos/FMX).

### Anonymous Methods ou "Métodos Anônimos"

Trecho de código VCL de exemplo com método `Iterar(APosicaoInicial: Int64; const APosicaoParada: Int64; AProc: TProc<Int64>; const AIncremento: Cardinal = 1)`:

```delphi
procedure TForm1.Button2Click(Sender: TObject);
var
  LIteracoes: Integer;
begin
  LIteracoes := 0;

  // for I := 1 to 10 do

  TMetodosAnonimosUtil.Iterar(1, 10,
    procedure(I: Int64)
    begin
      Inc(LIteracoes);
      // I armazena a posição atual do iterador e também pode ser utilizada dentro do método anônimo
    end);

  ShowMessageFmt('Número de iterações: %d', [LIteracoes]);
end;
```

Modificando o incremento neste trecho de código FMX utilizando uma a sobrecarga para Single do método `Iterar(APosicaoInicial: Single; const APosicaoParada: Single; AProc: TProc<Single>; const AIncremento: Cardinal = 1)`, com incremento decimal:

```delphi
procedure TForm1.Button4Click(Sender: TObject);
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
```

Trecho de código de exemplo com método `IterarReverso(APosicaoInicial: Int64; const APosicaoParada: Int64; AProc: TProc<Int64>; const ADecremento: Integer = -1)`:

```delphi
procedure TForm1.Button3Click(Sender: TObject);
var
  LIteracoes: Integer;
begin
  LIteracoes := 0;

  // for I := 10 downto 1 do

  TMetodosAnonimosUtil.IterarReverso(10, 1,
    procedure(I: Int64)
    begin
      Inc(LIteracoes);
      // I armazena a posição atual do iterador e também pode ser utilizada dentro do método anônimo
    end);

  ShowMessageFmt('Número de iterações: %d', [LIteracoes]);
end;
```

### Regular Expressions (RegEx) ou "Expressões Regulares"

Trecho de código de exemplo com método `StringDeLetras(const ATexto: string): Boolean`:

```delphi
procedure TForm1.Button4Click(Sender: TObject);
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
```

Trecho de código de exemplo com método `StringDeDigitos(const ATexto: string): Boolean`:

```delphi
procedure TForm1.Button5Click(Sender: TObject);
const
  C_AMOSTRA_DIGITOS = '0123456789';
var
  LResultado: Boolean;
begin
  LResultado := TExpressoesRegularesUtil.StringDeDigitos(C_AMOSTRA_DIGITOS);

  // resultado esperado: True (verdadeiro)
  ShowMessageFmt('Amostra "%s" é uma string dígitos apenas? %s', 
    [C_AMOSTRA_DIGITOS, BoolToStr(LResultado, True)];
end;
```

Trecho de código de exemplo com método `RemoverDigitos(const ATexto: string; const ASubstituirPor: string = ''): string`:

```delphi
procedure TForm1.Button6Click(Sender: TObject);
const
  C_TEXTO_COM_DIGITOS = 'Exemplo 2021';
var
  LResultado: string;
begin
  LResultado := TExpressoesRegularesUtil.RemoverDigitos(C_TEXTO_COM_DIGITOS);

  // resultado esperado: "Exemplo"
  ShowMessageFmt('Resultado do texto "%s" sem dígitos: %s', 
    [C_TEXTO_COM_DIGITOS, LResultado]);
end;
```

Trecho de código de exemplo com método `EmailValido(const AEmail: string): Boolean`:

```delphi
procedure TForm1.Button7Click(Sender: TObject);
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
```
