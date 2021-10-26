﻿unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI,
{ delphi-utils/fontes }
  DelphiUtils.Hub;

type
  TForm1 = class(TForm)
    ButtonUsarStringList: TButton;
    ButtonUsarFDQuery: TButton;
    StatusBar1: TStatusBar;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDQuery1: TFDQuery;
    ButtonIterar: TButton;
    ButtonIterarReverso: TButton;
    GroupBox1: TGroupBox;
    ProgressBar1: TProgressBar;
    ButtonIterarIncremento5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonUsarStringListClick(Sender: TObject);
    procedure ButtonUsarFDQueryClick(Sender: TObject);
    procedure ButtonIterarClick(Sender: TObject);
    procedure ButtonIterarReversoClick(Sender: TObject);
    procedure ButtonIterarIncremento5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
const
  C_DML_ADD_TEMPLATE = 'insert into COMPONENTE values (''%s'', %d, ''%s'')';
var
  I, LRegistros: Integer;
  LCaminho: string;
  LBaseExiste: Boolean;
begin
  // iniciando base de dados SQLite standalone
  LCaminho := TPath.Combine(GetCurrentDir, 'BASE_SQLITE.DB');
  LBaseExiste := TFile.Exists(LCaminho);
  FDConnection1.Params.Database := LCaminho;
  FDConnection1.Open;

  // criando tabela exemplo
  if not LBaseExiste then
    FDConnection1.ExecSQL('create table COMPONENTE (Nome TEXT NOT NULL, Tag Integer, Classe TEXT);');

  LRegistros := FDConnection1.ExecSQLScalar('select count(0) from COMPONENTE');

  // populando com componentes da tela
  if LRegistros = 0 then
    for I := 0 to ComponentCount - 1 do begin
      FDConnection1.ExecSQL(
        Format(C_DML_ADD_TEMPLATE, [Components[I].Name, Components[I].Tag, Components[I].ClassName]) );
    end;
end;

procedure TForm1.ButtonUsarStringListClick(Sender: TObject);
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

procedure TForm1.ButtonUsarFDQueryClick(Sender: TObject);
var
  LEncontrouComponente: Boolean;
  LNomeComponente: string;
begin
  LEncontrouComponente := False;
  LNomeComponente := (Sender as TComponent).Name;

  TGenericosUtil.Usar<TFDQuery>(
    TFDQuery.Create(nil),
    procedure(Query: TFDQuery)
    var
      LSQLConsulta: string;
    begin
      LSQLConsulta := Format('select * from COMPONENTE where Nome = ''%s''', [LNomeComponente]);
      Query.Connection := FDConnection1;
      Query.Open(LSQLConsulta);
      LEncontrouComponente := Query.RecordCount >= 1;
    end);

  ShowMessageFmt('Componente "%s" encontrado: %s', [LNomeComponente, BoolToStr(LEncontrouComponente, True)]);
end;

procedure TForm1.ButtonIterarClick(Sender: TObject);
var
  LIteracoes: Integer;
begin
  LIteracoes := 0;

  // for I := 1 to 10 do

  TMetodosAnonimosUtil.Iterar(1, 10,
    procedure(I: Int64)
    begin
      Inc(LIteracoes);
    end);

  ShowMessageFmt('Número de iterações: %d', [LIteracoes]);
end;

procedure TForm1.ButtonIterarReversoClick(Sender: TObject);
var
  LIteracoes: Integer;
begin
  LIteracoes := 0;

  // for I := 10 downto 1 do

  TMetodosAnonimosUtil.IterarReverso(10, 1,
    procedure(I: Int64)
    begin
      Inc(LIteracoes);
    end);

  ShowMessageFmt('Número de iterações: %d', [LIteracoes]);
end;

procedure TForm1.ButtonIterarIncremento5Click(Sender: TObject);
const
  C_INCREMENTO: Cardinal = 5;
var
  LIteracoes: Integer;
begin
  LIteracoes := 0;

  TButton(Sender).Enabled := False;
  try
    ProgressBar1.Value := 0;
    ProgressBar1.Max := 100;

    // for (int i = 0; i <= 100; i += 5) [Java]

    TMetodosAnonimosUtil.Iterar(ProgressBar1.Value, ProgressBar1.Max,
      procedure(I: Single)
      begin
        ProgressBar1.Value := I;
        Inc(LIteracoes);
        Sleep(100);
        Application.ProcessMessages;
      end,
      // modificado o incremento padrao de 1 em 1 para 5
      C_INCREMENTO);

  finally
    TButton(Sender).Text := 'Reiniciar';
    TButton(Sender).Enabled := True;
  end;

  ShowMessageFmt('Progresso atingiu %.2f%% com %d iterações.', [ProgressBar1.Value, LIteracoes]);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
