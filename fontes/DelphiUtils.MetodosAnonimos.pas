unit DelphiUtils.MetodosAnonimos;

interface

uses
  System.SysUtils, System.StrUtils, System.Generics.Collections;

type
  TDelphiUtilMetodosAnonimos = class
    /// <summary>
    ///   <p>Iterar usando método anônimo, que não requer declaração de variável local e
    ///   possibilita alterar o fator de incremento padrão</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="APosicaoInicial">
    ///   posição inicial, de partida
    /// </param>
    /// <param name="APosicaoParada">
    ///   posição final, de parada
    /// </param>
    /// <param name="AProc">
    ///   método anônimo que tem como argumento a posição atual do iterador
    /// </param>
    /// <param name="AIncremento">
    ///   fotor de incremento, padrão 1 (pode ser omitido)
    /// </param>
    class procedure Iterar(APosicaoInicial: Int64; const APosicaoParada: Int64; AProc: TProc<Int64>;
      const AIncremento: Cardinal = 1); static;

    /// <summary>
    ///   <p>Iterar de forma reversa usando método anônimo, que não requer declaração de variável local e
    ///   possibilita alterar o fator de decremento padrão</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="APosicaoInicial">
    ///   posição inicial, de partida
    /// </param>
    /// <param name="APosicaoParada">
    ///   posição final, de parada
    /// </param>
    /// <param name="AProc">
    ///   método anônimo que tem como argumento a posição atual do iterador
    /// </param>
    /// <param name="AIncremento">
    ///   fotor de incremento, padrão 1 (pode ser omitido)
    /// </param>
    class procedure IterarReverso(APosicaoInicial: Int64; const APosicaoParada: Int64; AProc: TProc<Int64>;
      const ADecremento: Integer = -1); static;
  end;

implementation

{ TDelphiUtilMetodosAnonimos }

class procedure TDelphiUtilMetodosAnonimos.Iterar(APosicaoInicial: Int64; const APosicaoParada: Int64;
  AProc: TProc<Int64>; const AIncremento: Cardinal);
begin
  if APosicaoInicial > APosicaoParada then Exit;
  if AIncremento = 0 then Exit;

  while (APosicaoInicial <= APosicaoParada) do begin
    AProc(APosicaoInicial);
    Inc(APosicaoInicial, AIncremento);
  end;
end;

class procedure TDelphiUtilMetodosAnonimos.IterarReverso(APosicaoInicial: Int64; const APosicaoParada: Int64;
  AProc: TProc<Int64>; const ADecremento: Integer);
begin
  if APosicaoInicial < APosicaoParada then Exit;
  if ADecremento = 0 then Exit;

  while (APosicaoInicial >= APosicaoParada) do begin
    AProc(APosicaoInicial);
    Dec(APosicaoInicial, Abs(ADecremento));
  end;
end;

end.
