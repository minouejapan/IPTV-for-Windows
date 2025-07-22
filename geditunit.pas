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
  StdCtrls, CustomDrawnControls, ColorBox, Spin, LazUTF8, fpJSON, JSONIni;

type
  { TGrpEdit }
  TGrpEdit = class(TForm)
    EPGColor: TColorBox;
    EPGsw: TCDCheckBox;
    EPGurl: TEdit;
    Label2: TLabel;
    MkFSize: TSpinEdit;
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
    GLJsonFile: string;                 // CHグループリストJSONファイル名
    procedure LoadGLTxt(GLTxt: string); // 旧バージョン用CHグループリストテキストファイル読み込み
    procedure LoadGLJson;               // CHグループリストJSONファイル読み込み
    procedure SaveGLJson;
  public
    MkColor: integer;
    procedure JsonConverter;            // GRPLIST.TXTをgrplist.jsonファイルにコンバートする
  end;

const
  mkcol_Default = $00FFFFFF;
  mkcol_Black   = $00000000;
  mkcol_Gray    = $00808080;
  mkcol_Silver  = $00C0C0C0;
  mkcol_White   = $00FFFFFF;

  // VCLで用いているRGB構成とWindowsのRGB構成が異なるためこのテーブルで変換する
  //              RR      GG             BB
  mkcol_Maroon  = $80 or ($00 shl 8) or ($00 shl 16);
  mkcol_Red     = $FF or ($00 shl 8) or ($00 shl 16);
  mkcol_Fuchsia = $FF or ($00 shl 8) or ($FF shl 16);
  mkcol_Yellow  = $FF or ($FF shl 8) or ($00 shl 16);
  mkcol_Olive   = $80 or ($80 shl 8) or ($00 shl 16);
  mkcol_Green   = $00 or ($80 shl 8) or ($00 shl 16);
  mkcol_Teal    = $00 or ($80 shl 8) or ($80 shl 16);
  mkcol_Lime    = $00 or ($FF shl 8) or ($00 shl 16);
  mkcol_Purple  = $80 or ($00 shl 8) or ($80 shl 16);
  mkcol_Navy    = $00 or ($00 shl 8) or ($80 shl 16);
  mkcol_Blue    = $00 or ($00 shl 8) or ($FF shl 16);
  mkcol_Aqua    = $00 or ($FF shl 8) or ($FF shl 16);
  MarqueeColor: array[0..15] of integer =
                    (
                      mkcol_Black, mkcol_Navy, mkcol_Green, mkcol_Teal, mkcol_Maroon, mkcol_Purple, mkcol_olive, mkcol_Gray,
                      mkcol_Silver, mkcol_Blue, mkcol_Lime, mkcol_Aqua, mkcol_Red, mkcol_Fuchsia, mkcol_Yellow, mkcol_White
                    );
  WindowsColor: array[0..15] of integer =
                    (
                      clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
                      clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite
                    );

var
  GrpEdit: TGrpEdit;

implementation

{$R *.lfm}


{ TGrpEdit }

// ver2.2限定
// CHグループリストをGRPLIST.TXTからgrplist.jsonに変更したことに伴い、
// ver2.2を起動した初回だけ実行してファイルをコンバートする
procedure TGrpEdit.JsonConverter;
begin
  LoadGLTxt(ExtractFilePath(Application.ExeName) + 'grplist.txt');
  OKBtnClick(nil);
end;

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

procedure TGrpEdit.LoadGLTxt(GLTxt: string);
var
  s, ep: string;
  s1, s2, s3: TStringList;
  i, n, cl, j: integer;
begin
  if FileExists(GLTxt) then
  begin
    s1 := TStringList.Create;
    s2 := TStringList.Create;
    s3 := TStringList.Create;
    s2.Delimiter := ',';
    s2.StrictDelimiter := True;
    try
      s1.LoadFromFile(GLTxt, TEncoding.UTF8);
      if s1.Count > 0 then
      begin
        GrpEdit.RowCount := s1.Count;
        n := 0;
        for i := 0 to s1.Count - 1 do
        begin
          s := s1.Strings[i];
          if UTF8Pos('$', s) = 1 then
          begin
            ep := Copy(s, 2, UTF8Length(s));
            s3.CommaText := ep;
            EPGurl.Text := s3.Strings[0];
            if s3.Count > 1 then
              EPGsw.Checked := s3.Strings[1] = '1'
            else
              EPGsw.Checked := True;
            if s3.Count > 2 then
            begin
              cl := StrToInt(s3.Strings[2]);
              for j := 0 to 15 do
                if cl = MarqueeColor[j] then
                begin
                  EPGColor.Selected := WindowsColor[j];
                  Break;
                end;
            end;
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
      s3.Free;
    end;
  end;
end;

procedure TGrpEdit.LoadGLJson;
var
  item, value: string;
  sl: TStringList;
  jo: TJSONObject;
  i, n: integer;
begin
  if FileExists(GLJsonFile) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(GLJsonFile, TEncoding.UTF8);
      jo := TJSONObject(GetJSON(sl.Text));
      if jo.Find('CHGroup') <> nil then
      begin
        n := jo.Objects['CHGroup'].Count;
        GrpEdit.RowCount := n;
        for i := 0 to n - 1 do
        begin
          item  := jo.Objects['CHGroup'].Names[i];
          value := jo.Objects['CHGroup'].Strings[item];
          GrpEdit.Cells[0, i] := item;
          GrpEdit.Cells[1, i] := value;
        end;
      end;
    finally
      sl.Free;
      jo.Free;
    end;
  end;
end;

procedure TGrpEdit.SaveGLJson;
var
  sl: TStringList;
  jo: TJSONObject;
  jd: TJSONData;
  i, n: integer;
begin
  jo := TJSONObject(GetJSON('{ "CHGroup" : {}}'));
  jd := jo;
  n  := GrpEdit.RowCount;
  sl := TStringList.Create;
  try
    for i := 0 to n - 1 do
      jo.Objects['CHGroup'].Add(GrpEdit.Cells[0, i], GrpEdit.Cells[1, i]);
    sl.Text := jd.FormatJSON;
    sl.SaveToFile(GLJsonFile, TEncoding.UTF8);
  finally
    sl.Free;
    jd.Free;
  end;
end;

procedure TGrpEdit.FormCreate(Sender: TObject);
var
  gltxt: string;
begin
  gltxt      := ExtractFilePath(Application.ExeName) + 'grplist.txt';
  GLJsonFile := ExtractFilePath(Application.ExeName) + 'grplist.json';

  if FIleExists(GLJsonFile) then
    LoadGLJSON
  // jsonファイルがなくgrplist.txtファイルがある場合はgrplist.txtを
  // 読み込んでjsonファイルを作成する
  else if FileExists(gltxt) then
  begin
    LoadGLTxt(gltxt);
  end;
end;

procedure TGrpEdit.OKBtnClick(Sender: TObject);
var
  fn, c1, c2: string;
  sl: TStringList;
  i, n: integer;
  ini: TJSONIni;
  jo: TJSONObject;
  jd: TJSONData;
begin
  ini := TJSONIni.Create;
  jo  := TJSONObject.Create;
  sl  := TStringList.Create;
  jd  := jo;
  try
    fn := ExtractFilePath(ini.FileName) + 'grplist.json';
    jo.Add('CHGroup', TJSONObject.Create);
    for i := 0 to GrpEdit.RowCount - 1 do
    begin
      n := 1;
      c1 := GrpEdit.Cells[0, i];
      c2 := c1;
      if jo.Objects['CHGroup'].Find(c1) <> nil then
      begin
        repeat
          c2 := c1 + '(' + IntToStr(n) + ')';
          Inc(n);
        until jo.Objects['CHGroup'].Find(c2) = nil;
      end;
      jo.Objects['CHGroup'].Add(c2, GrpEdit.Cells[1, i]);
    end;
    sl.Text := jd.FormatJSON;
    sl.SaveToFile(fn, TEncoding.utf8);
  finally
    sl.Free;
    jo.Free;
    ini.Free;
  end;
  ModalResult := mrOK;
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

