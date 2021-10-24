﻿unit DelphiUtils.ExpressoesRegulares;

interface

uses
  System.SysUtils, System.StrUtils, System.RegularExpressions;

type
  /// <summary>
  ///  Mais informações sobre Expressões Regulares Unicode compatíveis:
  ///  <i> http://www.regular-expressions.info/unicode.html </i>
  /// </summary>
  TDelphiUtilExpressoesRegulares = class

    /// <summary>
    ///   Verifica se amostra coincide com letras no geral: maiúsculas e/ou minúsculas com ou sem acentuação
    /// </summary>
    class function StringDeLetras(const Amostra: string): Boolean; static;

    /// <summary>
    ///   Verifica se amostra coincide com letras minúsculas somente
    /// </summary>
    class function StringDeLetrasMinusculas(const Amostra: string): Boolean; static;

    /// <summary>
    ///   Verifica se amostra coincide com letras maiúsculas somente
    /// </summary>
    class function StringDeLetrasMaiusculas(const Amostra: string): Boolean; static;

    /// <summary>
    ///   Verifica se amostra coincide com digitos (0 a 9) somente
    /// </summary>
    class function StringDeDigitos(const Amostra: string): Boolean; static;

    /// <summary>
    ///   Verifica se texto coincide com caracteres que não podem compor uma palavra, símbolos
    /// </summary>
    class function StringDeSimbolos(const Amostra: string): Boolean; static;

    /// <summary>
    ///   Verifica se texto coincide com caracteres/símbolos de uma lista pré-definida, permitida
    /// </summary>
    class function StringDeSimbolosPreDefinidos(const Amostra: string): Boolean; static;
  end;

implementation

{ TDelphiUtilExpressoesRegulares }

const
  C_EXPRESSAO_SIMBOLOS_VALIDOS = '[-!$%^&*()_+|~=`´{}\[\]:";''<>?,.\/@#\\]';

class function TDelphiUtilExpressoesRegulares.StringDeLetras(const Amostra: string): Boolean;
begin
  Result := TRegEx.IsMatch(Amostra, '\p{L}') and (not StringDeDigitos(Amostra)) and
    (not StringDeSimbolosPreDefinidos(Amostra)); (* /^[a-zA-Z\u00C0-\u024F]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeLetrasMinusculas(const Amostra: string): Boolean;
begin
  Result := TRegEx.IsMatch(Amostra, '\p{Ll}'); (* /^[a-z\u00DE-\u00F6\u00F8-\u00FF]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeLetrasMaiusculas(const Amostra: string): Boolean;
begin
  Result := TRegEx.IsMatch(Amostra, '\p{Lu}'); (* /^[A-Z\u00C0-\u00D6\u00D8-\u00DD]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeDigitos(const Amostra: string): Boolean;
begin
  Result := TRegEx.IsMatch(Amostra, '[\d]'); (* /^[\d]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeSimbolos(const Amostra: string): Boolean;
begin
  Result := (not StringDeLetras(Amostra)) and (not StringDeDigitos(Amostra)); (* /^[\W]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeSimbolosPreDefinidos(const Amostra: string): Boolean;
begin
  Result := TRegEx.IsMatch(Amostra, C_EXPRESSAO_SIMBOLOS_VALIDOS);
end;

end.