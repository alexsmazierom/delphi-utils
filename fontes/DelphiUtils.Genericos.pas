unit DelphiUtils.Genericos;

interface

uses
  System.SysUtils, System.StrUtils, System.Generics.Collections;

type
  TDelphiUtilGenericos = class
    /// <summary>
    ///   <c>[Sobrecarga 1/2]</c>
    ///   <p>Usar um objeto genérico instanciado dentro do método anônimo,
    ///   que não requer declaração de variável local e que é liberado automaticamente.</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="T">
    ///   genérico utilizado deve ser uma classe
    /// </param>
    /// <param name="AObjeto">
    ///   instanciar objeto da classe T com os parâmetros necessários (se houver)
    /// </param>
    /// <param name="AProcedimento">
    ///   método anônimo que tem como argumento o objeto da classe T instanciado
    /// </param>
    class procedure Usar<T: class>(AObjeto: T; AProcedimento: TProc<T>); overload; static;

    /// <summary>
    ///   <c>[Sobrecarga 2/2]</c>
    ///   <p>Usar um objeto genérico instanciado dentro do método anônimo,
    ///   que não requer declaração de variável local, com método de tratamento de exceção e que é liberado automaticamente.</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-catch-finally</i> para assegurar a liberação do objeto,
    ///   tratamento de exceção e evitando vazamentos de memória.
    /// </remarks>
    /// <param name="T">
    ///   genérico utilizado deve ser uma classe
    /// </param>
    /// <param name="AObjeto">
    ///   instanciar objeto da classe T com os parâmetros necessários (se houver)
    /// </param>
    /// <param name="AProcedimento">
    ///   método anônimo que tem como argumento o objeto da classe T instanciado
    /// </param>
    /// <param name="ATratamentoExcecao">
    ///   método anônimo que tem como argumento o objeto da exceção levantada, permitindo fazer o tratamento
    /// </param>
    class procedure Usar<T: class>(AObjeto: T; AProcedimento: TProc<T>; ATratamentoExcecao: TProc<Exception>); overload; static;

    /// <summary>
    ///   <c>[Sobrecarga 1/2]</c>
    ///   <p>Usar um objeto genérico instanciado dentro do método anônimo,
    ///   que não requer declaração de variável local, liberado automaticamente e um retorno do tipo genérico.</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    ///   O retorno é genérico: desde um tipo primitivo (ex. string, integer, etc), record, interface, classe ou objeto instanciado.
    /// </remarks>
    /// <param name="T">
    ///   genérico utilizado deve ser uma classe
    /// </param>
    /// <param name="TResult">
    ///   tipo genérico a ser retornado
    /// </param>
    /// <param name="AObjeto">
    ///   instanciar objeto da classe T com os parâmetros necessários (se houver)
    /// </param>
    /// <param name="AFuncao">
    ///   método anônimo que tem como argumento o objeto da classe T instanciado com retorno
    /// </param>
    /// <returns>
    ///   tipo genérico
    /// </returns>
    class function Retornar<T,TResult>(AObjeto: T; AFuncao: TFunc<T,TResult>): TResult; overload; static;

    /// <summary>
    ///   <c>[Sobrecarga 2/2]</c>
    ///   <p>Usar um objeto genérico instanciado dentro do método anônimo,
    ///   que não requer declaração de variável local, com método de tratamento de exceção,
    ///   liberado automaticamente e um retorno do tipo genérico.</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-catch-finally</i> para assegurar a liberação do objeto,
    ///   tratamento de exceção e evitando vazamentos de memória.
    ///   O retorno é genérico: desde um tipo primitivo (ex. string, integer, etc), record, interface, classe ou objeto instanciado.
    /// </remarks>
    /// <param name="T">
    ///   genérico utilizado deve ser uma classe
    /// </param>
    /// <param name="TResult">
    ///   tipo genérico a ser retornado
    /// </param>
    /// <param name="AObjeto">
    ///   instanciar objeto da classe T com os parâmetros necessários (se houver)
    /// </param>
    /// <param name="AFuncao">
    ///   método anônimo que tem como argumento o objeto da classe T instanciado com retorno
    /// </param>
    /// <param name="ATratamentoExcecao">
    ///   método anônimo que tem como argumento o objeto da exceção levantada, permitindo fazer o tratamento
    /// </param>
    /// <returns>
    ///   tipo genérico
    /// </returns>
    class function Retornar<T,TResult>(AObjeto: T; AFuncao: TFunc<T,TResult>; ATratamentoExcecao: TProc<Exception>): TResult; overload; static;
  end;

implementation

{ TDelphiUtilGenericos }

class procedure TDelphiUtilGenericos.Usar<T>(AObjeto: T; AProcedimento: TProc<T>);
begin
  try
    AProcedimento(AObjeto);
  finally
  {$IFDEF MSWINDOWS}
    FreeAndNil(AObjeto);
  {$ELSE}
    if Assigned(AObjeto) then
      AObjeto.DisposeOf;
  {$ENDIF}
  end;
end;

class procedure TDelphiUtilGenericos.Usar<T>(AObjeto: T; AProcedimento: TProc<T>;
  ATratamentoExcecao: TProc<Exception>);
begin
  try
    try
      AProcedimento(AObjeto);
    except
      on E: Exception do
        if Assigned(ATratamentoExcecao) then
          ATratamentoExcecao(E);
        //else
          //raise;
    end;
  finally
  {$IFDEF MSWINDOWS}
    FreeAndNil(AObjeto);
  {$ELSE}
    if Assigned(AObjeto) then
      AObjeto.DisposeOf;
  {$ENDIF}
  end;
end;

class function TDelphiUtilGenericos.Retornar<T,TResult>(AObjeto: T; AFuncao: TFunc<T,TResult>): TResult;
begin
  try
    Result := AFuncao(AObjeto);
  finally
  {$IFDEF MSWINDOWS}
    FreeAndNil(AObjeto);
  {$ELSE}
    if Assigned(AObjeto) then
      AObjeto.DisposeOf;
  {$ENDIF}
  end;
end;

class function TDelphiUtilGenericos.Retornar<T, TResult>(AObjeto: T; AFuncao: TFunc<T, TResult>;
  ATratamentoExcecao: TProc<Exception>): TResult;
begin
  try
    try
      Result := AFuncao(AObjeto);
    except
      on E: Exception do
        if Assigned(ATratamentoExcecao) then
          ATratamentoExcecao(E);
        //else
          //raise;
    end;
  finally
  {$IFDEF MSWINDOWS}
    FreeAndNil(AObjeto);
  {$ELSE}
    if Assigned(AObjeto) then
      AObjeto.DisposeOf;
  {$ENDIF}
  end;
end;

end.
