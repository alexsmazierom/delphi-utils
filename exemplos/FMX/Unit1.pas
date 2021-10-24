unit Unit1;

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
  TForm10 = class(TForm)
    ButtonUsarStringList: TButton;
    ButtonUsarFDQuery: TButton;
    StatusBar1: TStatusBar;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDQuery1: TFDQuery;
    procedure FormCreate(Sender: TObject);
    procedure ButtonUsarStringListClick(Sender: TObject);
    procedure ButtonUsarFDQueryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

{$R *.fmx}

procedure TForm10.FormCreate(Sender: TObject);
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

procedure TForm10.ButtonUsarStringListClick(Sender: TObject);
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

procedure TForm10.ButtonUsarFDQueryClick(Sender: TObject);
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

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
