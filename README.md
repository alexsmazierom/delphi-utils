# delphi-utils
Classes utilitárias usando os recursos mais atuais da linguagem Delphi.

IDE utilizada para compilação e testes: Delphi 10.3 CE (Community Edition)

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