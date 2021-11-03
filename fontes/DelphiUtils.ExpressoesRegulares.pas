unit DelphiUtils.ExpressoesRegulares;

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
    ///   Remover/substituir digitos de um texto
    /// </summary>
    class function RemoverDigitos(const ATexto: string; const ASubstituirPor: string = ''): string; static;

    /// <summary>
    ///   Remover/substituir letras de um texto
    /// </summary>
    class function RemoverLetras(const ATexto: string; const ASubstituirPor: string = ''): string; static;

    /// <summary>
    ///   Remover/substituir caracteres que não podem compor uma palavra (símbolos) de um texto
    /// </summary>
    class function RemoverSimbolos(const ATexto: string; const ASubstituirPor: string = ''): string; static;

    /// <summary>
    ///   Remover/substituir espacos em branco no texto
    /// </summary>
    class function RemoverEspacos(const ATexto: string; const ASubstituirPor: string = ''): string; static;

    /// <summary>
    ///   Verifica se texto coincide com letras no geral: maiúsculas e/ou minúsculas com ou sem acentuação
    /// </summary>
    class function StringDeLetras(const ATexto: string): Boolean; static;

    /// <summary>
    ///   Verifica se texto coincide com letras minúsculas somente
    /// </summary>
    class function StringDeLetrasMinusculas(const ATexto: string): Boolean; static;

    /// <summary>
    ///   Verifica se texto coincide com letras maiúsculas somente
    /// </summary>
    class function StringDeLetrasMaiusculas(const ATexto: string): Boolean; static;

    /// <summary>
    ///   Verifica se texto coincide com digitos (0 a 9) somente
    /// </summary>
    class function StringDeDigitos(const ATexto: string): Boolean; static;

    /// <summary>
    ///   Verifica se texto coincide com caracteres que não podem compor uma palavra, símbolos
    /// </summary>
    class function StringDeSimbolos(const ATexto: string): Boolean; static;

    /// <summary>
    ///   Verifica se texto coincide com caracteres/símbolos de uma lista pré-definida, permitida
    /// </summary>
    class function StringDeSimbolosPreDefinidos(const ATexto: string): Boolean; static;

    /// <summary>
    ///   Verifica se texto é alfanumérico: contem letras e/ou digitos
    /// </summary>
    class function StringAlfanumerica(const ATexto: string): Boolean; static;

    /// <summary>
    ///   Verifica se email é válido, atendendo requisitos de tamanho mínimo, caracteres utilizados, prefixos e sufixos
    /// </summary>
    class function EmailValido(const AEmail: string): Boolean; static;
  end;

implementation

{ TDelphiUtilExpressoesRegulares }

const
  C_EXPRESSAO_SIMBOLOS_VALIDOS = '[-!$%^&*()_+|~=`´{}\[\]:";''<>?,.\/@#\\]';

class function TDelphiUtilExpressoesRegulares.RemoverDigitos(const ATexto: string; const ASubstituirPor: string = ''): string;
begin
  Result := TRegEx.Replace(ATexto, '[\d]', ASubstituirPor);
end;

class function TDelphiUtilExpressoesRegulares.RemoverEspacos(const ATexto, ASubstituirPor: string): string;
begin
  Result := TRegEx.Replace(ATexto, '[\s]', ASubstituirPor);
end;

class function TDelphiUtilExpressoesRegulares.RemoverLetras(const ATexto: string; const ASubstituirPor: string = ''): string;
begin
  Result := TRegEx.Replace(ATexto, '\p{L}', ASubstituirPor);
end;

class function TDelphiUtilExpressoesRegulares.RemoverSimbolos(const ATexto: string; const ASubstituirPor: string = ''): string;
begin
  Result := TRegEx.Replace(ATexto, '[\W]', ASubstituirPor);
end;

class function TDelphiUtilExpressoesRegulares.StringDeLetras(const ATexto: string): Boolean;
begin
  Result := TRegEx.IsMatch(ATexto, '\p{L}') and (not StringDeDigitos(ATexto)) and
    (not StringDeSimbolosPreDefinidos(ATexto)); (* /^[a-zA-Z\u00C0-\u024F]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeLetrasMinusculas(const ATexto: string): Boolean;
begin
  Result := TRegEx.IsMatch(ATexto, '\p{Ll}'); (* /^[a-z\u00DE-\u00F6\u00F8-\u00FF]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeLetrasMaiusculas(const ATexto: string): Boolean;
begin
  Result := TRegEx.IsMatch(ATexto, '\p{Lu}'); (* /^[A-Z\u00C0-\u00D6\u00D8-\u00DD]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeDigitos(const ATexto: string): Boolean;
begin
  Result := TRegEx.IsMatch(ATexto, '[\d]'); (* /^[\d]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeSimbolos(const ATexto: string): Boolean;
begin
  Result := (not StringDeLetras(ATexto)) and (not StringDeDigitos(ATexto)); (* /^[\W]+$/g *)
end;

class function TDelphiUtilExpressoesRegulares.StringDeSimbolosPreDefinidos(const ATexto: string): Boolean;
begin
  Result := TRegEx.IsMatch(ATexto, C_EXPRESSAO_SIMBOLOS_VALIDOS);
end;

class function TDelphiUtilExpressoesRegulares.StringAlfanumerica(const ATexto: string): Boolean;
var
  LAmostraSemDigitos, LAmostraSemLetrasSimbolos: string;
begin
//  LAmostraSemDigitos := TRegEx.Replace(ATexto, '[\d]', '');
//  LAmostraSemLetrasSimbolos := TRegEx.Replace( TRegEx.Replace(ATexto, '\p{L}', ''), '[\W]', '');
  LAmostraSemDigitos := RemoverDigitos(ATexto);
  LAmostraSemLetrasSimbolos := RemoverSimbolos( RemoverLetras(ATexto) );

  Result := (StringDeLetras(LAmostraSemDigitos) or LAmostraSemDigitos.IsEmpty) or
            (StringDeDigitos(LAmostraSemLetrasSimbolos) or LAmostraSemLetrasSimbolos.IsEmpty);
end;

class function TDelphiUtilExpressoesRegulares.EmailValido(const AEmail: string): Boolean;
begin
  Result := TRegEx.IsMatch(AEmail, '^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$');
end;

end.
