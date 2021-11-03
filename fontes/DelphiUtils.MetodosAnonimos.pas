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

    /// <summary>
    ///   <p>Inspirada no Map do Javascript, este método anônimo utiliza arrays de entrada/saída de tipos genéricos,
    ///   e uma função de callback que será aplicada para cada elemento do array de entrada retornando um novo array de saída</p>
    /// </summary>
    /// <param name="ADadosEntrada">
    ///   array de entrada de tipo genérico com os elementos
    /// </param>
    /// <param name="AFuncManipulacao">
    ///   callback ou função anônima onde o parâmetro e o retorno são do mesmo tipo do array de entrada
    ///   e que será aplicada a todos os elementos
    /// </param>
    /// <returns>
    ///   retorna array com elementos transformados pela função de manipulação
    /// </returns>
    class function Map<T>(ADadosEntrada: TArray<T>; AFuncManipulacao: TFunc<T, T>): TArray<T>; static;

    class function Reduce<T>(ADadosEntrada: TArray<T>; AFuncTotalizacao: TFunc<T, T, T>; AValorInicial: T): T; static;

    class function Filter<T>(ADadosEntrada: TArray<T>; AFuncFiltragem: TFunc<T, Boolean>): TArray<T>; static;
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

class function TDelphiUtilMetodosAnonimos.Map<T>(ADadosEntrada: TArray<T>; AFuncManipulacao: TFunc<T, T>): TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, Length(ADadosEntrada));
  for I := 0 to Length(ADadosEntrada) - 1 do
    Result[I] := AFuncManipulacao(ADadosEntrada[I]);
End;

class function TDelphiUtilMetodosAnonimos.Reduce<T>(ADadosEntrada: TArray<T>; AFuncTotalizacao: TFunc<T, T, T>; AValorInicial: T): T;
var
  I: T;
begin
  Result := AValorInicial;
  for I in ADadosEntrada do
    Result := AFuncTotalizacao(Result, I);
end;

class function TDelphiUtilMetodosAnonimos.Filter<T>(ADadosEntrada: TArray<T>; AFuncFiltragem: TFunc<T, Boolean>): TArray<T>;
var
  I: Integer;
  LAuxLista: TList<T>;
begin
  LAuxLista := TList<T>.Create;
  try
    for I := 0 to Length(ADadosEntrada) - 1 do begin
      if AFuncFiltragem(ADadosEntrada[I]) = True then
        LAuxLista.Add(ADadosEntrada[I]);
    end;
    Result := LAuxLista.ToArray;
  finally
    LAuxLista.Free;
  end;
end;

end.
