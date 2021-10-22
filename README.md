# delphi-utils
Classes utilitárias usando os recursos mais atuais da linguagem Delphi.

IDE utilizada para compilação e testes: Delphi 10.3 CE (Community Edition)

## Exemplos de implementação

### Genéricos ou "Generics"

Exemplo de código com `Usar<T: class>(AObjeto: T; AProcedimento: TProc<T>)`:

```delphi
uses
  System.Classes,
  Vcl.Dialogs,
  DelphiUtils.Hub;

procedure TForm1.Button1Click(Sender: TObject);
var
  LVantagens: Integer;
begin
  LVantagens := 0;

  TGenericosUtil.Usar<TStringList>(
    // instanciando diretamente a lista
    TStringList.Create,
    // método anônimo com instância da lista 
    procedure(StrLista: TStringList)
    begin
      StrLista.Add('#1 não requer de variável local para trabalhar com TStringList');
      StrLista.Add('#2 não provoca vazamentos de memória ("memory leaks") pois é liberado automaticamente');
      LVantagens := StrLista.Count;
    end);

  ShowMessageFmt('Vantagens adicionadas a lista: %d.', [LVantagens]);
end;
```