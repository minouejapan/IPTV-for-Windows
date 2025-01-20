(*
  プレイリストグループ編集
*)
unit geditunit;

{$IFDEF FPC}
  {$MODE Delphi}
  {$codepage utf8}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, Grids,
  StdCtrls, LazUTF8;

type

  { TGrpEdit }

  TGrpEdit = class(TForm)
    EPGurl: TEdit;
    Label2: TLabel;
    UpBtn: TSpeedButton;
    DwnBtn: TSpeedButton;
    Label1: TLabel;
    OKBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    AddBtn: TSpeedButton;
    DelBtn: TSpeedButton;
    BrsBtn: TSpeedButton;
    GrpEdit: TStringGrid;
    OD: TOpenDialog;
    procedure AddBtnClick(Sender: TObject);
    procedure BrsBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure DwnBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure GrpEditExit(Sender: TObject);
    procedure GrpEditSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure OKBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
  private

  public

  end;

var
  GrpEdit: TGrpEdit;

implementation

{$R *.lfm}

{ TGrpEdit }

procedure TGrpEdit.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Close;
end;

procedure TGrpEdit.GrpEditExit(Sender: TObject);
begin
  BrsBtn.Enabled    := False;
  BrsBtn.Font.Color := clGray;
  DelBtn.Enabled    := False;
  DelBtn.Font.Color := clGray;
  DwnBtn.Enabled    := False;
  DwnBtn.Font.Color := clGray;
  UpBtn.Enabled     := False;
  UpBtn.Font.Color  := clGray;
end;

procedure TGrpEdit.GrpEditSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if aCol = 1 then
  begin
    BrsBtn.Enabled    := True;
    BrsBtn.Font.Color := clWhite;
  end else begin
    BrsBtn.Enabled    := False;
    BrsBtn.Font.Color := clGray;
  end;
  DelBtn.Enabled    := True;
  DelBtn.Font.Color := clWhite;
  if aRow > 0 then
  begin
    UpBtn.Enabled     := True;
    UpBtn.Font.Color  := clWhite;
  end else begin
    UpBtn.Enabled     := False;
    UpBtn.Font.Color  := clGray;
  end;
  if aRow = GrpEdit.RowCount - 1 then
  begin
    DwnBtn.Enabled    := False;
    DwnBtn.Font.Color := clGray;
  end else begin
    DwnBtn.Enabled    := True;
    DwnBtn.Font.Color := clWhite;
  end;
end;

procedure TGrpEdit.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TGrpEdit.DelBtnClick(Sender: TObject);
begin
  GrpEdit.DeleteRow(GrpEdit.Row);
end;

procedure TGrpEdit.DwnBtnClick(Sender: TObject);
var
  ups, dns: string;
begin
  ups := GrpEdit.Rows[GrpEdit.Row + 1].CommaText;
  dns := GrpEdit.Rows[GrpEdit.Row].CommaText;
  GrpEdit.Rows[GrpEdit.Row].CommaText     := ups;
  GrpEdit.Rows[GrpEdit.Row + 1].CommaText := dns;
  GrpEdit.Row := GrpEdit.Row + 1;
end;

procedure TGrpEdit.AddBtnClick(Sender: TObject);
begin
  if GrpEdit.Row = GrpEdit.RowCount then
    GrpEdit.RowCount := GrpEdit.RowCount + 1;
  GrpEdit.InsertColRow(False, GrpEdit.Row + 1);
  GrpEdit.Row := GrpEdit.Row + 1;
end;

procedure TGrpEdit.BrsBtnClick(Sender: TObject);
var
  f: string;
begin
  f := GrpEdit.Cells[1, GrpEdit.Row];
  with OD do
  begin
    if FileExists(f) then
    begin
      InitialDir := ExtractFileDir(f);
      FileName   := ExtractFileName(f);
      if InitialDir = '' then
        InitialDir := ExtractFileDir(Application.ExeName);
    end;
    if Execute then
      GrpEdit.Cells[GrpEdit.Col, GrpEdit.Row] := FileName;
  end;
end;

procedure TGrpEdit.FormCreate(Sender: TObject);
var
  gf, s: string;
  s1, s2: TStringList;
  i, n: integer;
begin
  gf := ExtractFilePath(Application.ExeName) + 'GRPLIST.TXT';
  if FileExists(gf) then
  begin
    s1 := TStringList.Create;
    s2 := TStringList.Create;
    s2.Delimiter := ',';
    s2.StrictDelimiter := True;
    try
      s1.LoadFromFile(gf, TEncoding.UTF8);
      if s1.Count > 0 then
      begin
        GrpEdit.RowCount := s1.Count;
        n := 0;
        for i := 0 to s1.Count - 1 do
        begin
          s := s1.Strings[i];
          if UTF8Pos('$', s) = 1 then
          begin
            EPGurl.Text := Copy(s, 2, UTF8Length(s));
            GrpEdit.RowCount := s1.Count - 1;
          end else begin
            s2.CommaText := s;
            GrpEdit.Cells[0, n] := s2.Strings[0];
            GrpEdit.Cells[1, n] := s2.Strings[1];
            Inc(n);
          end;
        end;
      end;
    finally
      s1.Free;
      s2.Free;
    end;
  end;
end;

procedure TGrpEdit.OKBtnClick(Sender: TObject);
var
  gf: string;
  sl: TStringList;
  i: integer;
begin
  gf := ExtractFilePath(Application.ExeName) + 'GRPLIST.TXT';
  sl := TStringList.Create;
  try
    sl.Add('$' + EPGurl.Text);
    for i := 0 to GrpEdit.RowCount - 1 do
      sl.Add(GrpEdit.Rows[i].CommaText);
    sl.SaveToFile(gf, TEncoding.UTF8);
  finally
    sl.Free;
  end;
  ModalResult := mrOK;
  //Close;
end;

procedure TGrpEdit.UpBtnClick(Sender: TObject);
var
  ups, dns: string;
begin
  ups := GrpEdit.Rows[GrpEdit.Row].CommaText;
  dns := GrpEdit.Rows[GrpEdit.Row - 1].CommaText;
  GrpEdit.Rows[GrpEdit.Row].CommaText     := dns;
  GrpEdit.Rows[GrpEdit.Row - 1].CommaText := ups;
  GrpEdit.Row := GrpEdit.Row - 1;
end;

end.

