unit DelphiUtils.MetodosAnonimos;

interface

uses
  System.SysUtils, System.StrUtils, System.Generics.Collections;

type
  TDelphiUtilMetodosAnonimos = class
    /// <summary>
    ///   <c>[Sobrecarga 1/3]</c>
    ///   <p>Iterar usando método anônimo, que não requer declaração de variável local e
    ///   possibilita alterar o fator de incremento padrão</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="APosicaoInicial: Int64">
    ///   posição inicial, de partida (inteiro longo)
    /// </param>
    /// <param name="APosicaoParada: Int64">
    ///   posição final, de parada (inteiro longo)
    /// </param>
    /// <param name="AProc">
    ///   método anônimo que tem como argumento a posição atual do iterador
    /// </param>
    /// <param name="AIncremento">
    ///   fotor de incremento, padrão 1 (pode ser omitido)
    /// </param>
    class procedure Iterar(APosicaoInicial: Int64; const APosicaoParada: Int64; AProc: TProc<Int64>;
      const AIncremento: Cardinal = 1); overload; static;

    /// <summary>
    ///   <c>[Sobrecarga 2/3]</c>
    ///   <p>Iterar usando método anônimo, que não requer declaração de variável local e
    ///   possibilita alterar o fator de incremento padrão</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="APosicaoInicial: Integer">
    ///   posição inicial, de partida (inteiro)
    /// </param>
    /// <param name="APosicaoParada: Integer">
    ///   posição final, de parada (inteiro)
    /// </param>
    /// <param name="AProc">
    ///   método anônimo que tem como argumento a posição atual do iterador
    /// </param>
    /// <param name="AIncremento">
    ///   fotor de incremento, padrão 1 (pode ser omitido)
    /// </param>
    class procedure Iterar(APosicaoInicial: Integer; const APosicaoParada: Integer; AProc: TProc<Integer>;
      const AIncremento: Cardinal = 1); overload; static;

    /// <summary>
    ///   <c>[Sobrecarga 3/3]</c>
    ///   <p>Iterar usando método anônimo, que não requer declaração de variável local e
    ///   possibilita alterar o fator de incremento padrão</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="APosicaoInicial: Single">
    ///   posição inicial, de partida (decimal)
    /// </param>
    /// <param name="APosicaoParada: Single">
    ///   posição final, de parada (decimal)
    /// </param>
    /// <param name="AProc">
    ///   método anônimo que tem como argumento a posição atual do iterador
    /// </param>
    /// <param name="AIncremento">
    ///   fotor de incremento, padrão 1 (pode ser omitido)
    /// </param>
    class procedure Iterar(APosicaoInicial: Single; const APosicaoParada: Single; AProc: TProc<Single>;
      const AIncremento: Cardinal = 1); overload; static;

    /// <summary>
    ///   <c>[Sobrecarga 1/3]</c>
    ///   <p>Iterar de forma reversa usando método anônimo, que não requer declaração de variável local e
    ///   possibilita alterar o fator de decremento padrão</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="APosicaoInicial: Int64">
    ///   posição inicial, de partida (inteiro longo)
    /// </param>
    /// <param name="APosicaoParada: Int64">
    ///   posição final, de parada (inteiro longo)
    /// </param>
    /// <param name="AProc">
    ///   método anônimo que tem como argumento a posição atual do iterador
    /// </param>
    /// <param name="AIncremento">
    ///   fotor de incremento, padrão 1 (pode ser omitido)
    /// </param>
    class procedure IterarReverso(APosicaoInicial: Int64; const APosicaoParada: Int64; AProc: TProc<Int64>;
      const ADecremento: Cardinal = 1); overload; static;

    /// <summary>
    ///   <c>[Sobrecarga 2/3]</c>
    ///   <p>Iterar de forma reversa usando método anônimo, que não requer declaração de variável local e
    ///   possibilita alterar o fator de decremento padrão</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="APosicaoInicial: Integer">
    ///   posição inicial, de partida (inteiro)
    /// </param>
    /// <param name="APosicaoParada: Integer">
    ///   posição final, de parada (inteiro)
    /// </param>
    /// <param name="AProc">
    ///   método anônimo que tem como argumento a posição atual do iterador
    /// </param>
    /// <param name="AIncremento">
    ///   fotor de incremento, padrão 1 (pode ser omitido)
    /// </param>
    class procedure IterarReverso(APosicaoInicial: Integer; const APosicaoParada: Integer; AProc: TProc<Integer>;
      const ADecremento: Cardinal = 1); overload; static;

    /// <summary>
    ///   <c>[Sobrecarga 3/3]</c>
    ///   <p>Iterar de forma reversa usando método anônimo, que não requer declaração de variável local e
    ///   possibilita alterar o fator de decremento padrão</p>
    /// </summary>
    /// <remarks>
    ///   Internamente implementa um bloco <i>try-finally</i> para assegurar a liberação do objeto, evitando vazamentos de memória.
    /// </remarks>
    /// <param name="APosicaoInicial: Single">
    ///   posição inicial, de partida (decimal)
    /// </param>
    /// <param name="APosicaoParada: Single">
    ///   posição final, de parada (decimal)
    /// </param>
    /// <param name="AProc">
    ///   método anônimo que tem como argumento a posição atual do iterador
    /// </param>
    /// <param name="AIncremento">
    ///   fotor de incremento, padrão 1 (pode ser omitido)
    /// </param>
    class procedure IterarReverso(APosicaoInicial: Single; const APosicaoParada: Single; AProc: TProc<Single>;
      const ADecremento: Cardinal = 1); overload; static;
  end;

implementation

{ TDelphiUtilMetodosAnonimos }

class procedure TDelphiUtilMetodosAnonimos.Iterar(APosicaoInicial: Int64; const APosicaoParada: Int64;
  AProc: TProc<Int64>; const AIncremento: Cardinal = 1);
begin
  if APosicaoInicial > APosicaoParada then Exit;
  if AIncremento = 0 then Exit;

  while (APosicaoInicial <= APosicaoParada) do begin
    Inc(APosicaoInicial, AIncremento);
    AProc(APosicaoInicial);
  end;
end;

class procedure TDelphiUtilMetodosAnonimos.Iterar(APosicaoInicial: Integer; const APosicaoParada: Integer;
  AProc: TProc<Integer>; const AIncremento: Cardinal = 1);
begin
  if APosicaoInicial > APosicaoParada then Exit;
  if AIncremento = 0 then Exit;

  while (APosicaoInicial <= APosicaoParada) do begin
    Inc(APosicaoInicial, AIncremento);
    AProc(APosicaoInicial);
  end;
end;

class procedure TDelphiUtilMetodosAnonimos.Iterar(APosicaoInicial: Single; const APosicaoParada: Single;
  AProc: TProc<Single>; const AIncremento: Cardinal = 1);
begin
  if APosicaoInicial > APosicaoParada then Exit;
  if AIncremento = 0 then Exit;

  while (APosicaoInicial <= APosicaoParada) do begin
    APosicaoInicial := APosicaoInicial + AIncremento;
    AProc(APosicaoInicial);
  end;
end;

class procedure TDelphiUtilMetodosAnonimos.IterarReverso(APosicaoInicial: Int64; const APosicaoParada: Int64;
  AProc: TProc<Int64>; const ADecremento: Cardinal = 1);
begin
  if APosicaoInicial < APosicaoParada then Exit;
  if ADecremento = 0 then Exit;

  while (APosicaoInicial >= APosicaoParada) do begin
    Dec(APosicaoInicial, ADecremento);
    AProc(APosicaoInicial);
  end;
end;

class procedure TDelphiUtilMetodosAnonimos.IterarReverso(APosicaoInicial: Integer; const APosicaoParada: Integer;
  AProc: TProc<Integer>; const ADecremento: Cardinal = 1);
begin
  if APosicaoInicial < APosicaoParada then Exit;
  if ADecremento = 0 then Exit;

  while (APosicaoInicial >= APosicaoParada) do begin
    Dec(APosicaoInicial, ADecremento);
    AProc(APosicaoInicial);
  end;
end;

class procedure TDelphiUtilMetodosAnonimos.IterarReverso(APosicaoInicial: Single; const APosicaoParada: Single;
  AProc: TProc<Single>; const ADecremento: Cardinal = 1);
begin
  if APosicaoInicial < APosicaoParada then Exit;
  if ADecremento = 0 then Exit;

  while (APosicaoInicial >= APosicaoParada) do begin
    APosicaoInicial := APosicaoInicial - ADecremento;
    AProc(APosicaoInicial);
  end;
end;

end.
