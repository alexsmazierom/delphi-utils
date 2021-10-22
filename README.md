# delphi-utils
Classes utilitárias usando os recursos mais atuais da linguagem Delphi.

IDE utilizada para compilação e testes: Delphi 10.3 CE (Community Edition)

## Exemplos de implementação

Genéricos (generics):

```delphi
uses
  (...)
  DelphiUtils.Hub;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TGenericosUtil.Usar<TStringList>(
    TStringList.Create,
    procedure(StrLista: TStringList)
    begin
      StrLista.Add('Generics sample')
    end);
end;
```