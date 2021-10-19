unit DelphiUtils.Genericos;

interface

uses
{ RTL }
  System.SysUtils, System.StrUtils, System.Generics.Collections;

type
  TDelphiUtilGenericos = class
    class procedure Usar<T: class>(AInstanciaObj: T; AProcedimento: TProc<T>); overload; static;
    class procedure Usar<T: class>(AInstanciaObj: T; AProcedimento: TProc<T>; ATratamentoExcecao: TProc<Exception>); overload; static;
    class function Retornar<T,TResult>(AInstanciaObj: T; AFuncao: TFunc<T,TResult>): TResult; overload; static;
    class function Retornar<T,TResult>(AInstanciaObj: T; AFuncao: TFunc<T,TResult>; ATratamentoExcecao: TProc<Exception>): TResult; overload; static;
  end;

implementation

{ TDelphiUtilGenericos }

class procedure TDelphiUtilGenericos.Usar<T>(AInstanciaObj: T; AProcedimento: TProc<T>);
begin
  try
    AProcedimento(AInstanciaObj);
  finally
    FreeAndNil(AInstanciaObj);
  end;
end;

class procedure TDelphiUtilGenericos.Usar<T>(AInstanciaObj: T; AProcedimento: TProc<T>;
  ATratamentoExcecao: TProc<Exception>);
begin
  try
    try
      AProcedimento(AInstanciaObj);
    except
      on E: Exception do
        if Assigned(ATratamentoExcecao) then
          ATratamentoExcecao(E);
        //else
          //raise;
    end;
  finally
    FreeAndNil(AInstanciaObj);
  end;
end;

class function TDelphiUtilGenericos.Retornar<T,TResult>(AInstanciaObj: T; AFuncao: TFunc<T,TResult>): TResult;
begin
  try
    Result := AFuncao(AInstanciaObj);
  finally
    FreeAndNil(AInstanciaObj);
  end;
end;

class function TDelphiUtilGenericos.Retornar<T, TResult>(AInstanciaObj: T; AFuncao: TFunc<T, TResult>;
  ATratamentoExcecao: TProc<Exception>): TResult;
begin
  try
    try
      Result := AFuncao(AInstanciaObj);
    except
      on E: Exception do
        if Assigned(ATratamentoExcecao) then
          ATratamentoExcecao(E);
        //else
          //raise;
    end;
  finally
    FreeAndNil(AInstanciaObj);
  end;
end;

end.
