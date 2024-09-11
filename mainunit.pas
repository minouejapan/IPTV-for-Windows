(*

  WinIPTV(lazIPTV)

  ver1.0  2024/09/11  IPTVを自分で作ってみた

*)
unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, MPVPlayer, LCLType, ComCtrls, IniFiles, Windows, ActiveX, ShlObj;

type

  { TMainForm }

  TMainForm = class(TForm)
    CloseBtn: TSpeedButton;
    CateList: TListBox;
    ChList: TListBox;
    Image1: TImage;
    Label1: TLabel;
    URLLabel: TLabel;
    VolValue: TLabel;
    MPV: TMPVPlayer;
    MenuPanel: TPanel;
    OD: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    ListOpenBtn: TSpeedButton;
    VolBar: TTrackBar;
    procedure CateListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CateListSelectionChange(Sender: TObject; User: boolean);
    procedure ChListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ChListSelectionChange(Sender: TObject; User: boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MPVDblClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure ListOpenBtnClick(Sender: TObject);
    procedure VolBarChange(Sender: TObject);
  private
    FullScrMode,
    MenuOpen: Boolean;
    MXO, MXC: integer;
    M3uFile: string;
    TVChList: array of string;  // グループ名,CH名#9URL,CH名|URL,CH名 URL,....
    ChURL: array of string;     // CH名に対応したストリーム再生URL
    Ini: TIniFile;
    procedure LoadCHList(FileName: string);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

// プレイリストファイル(*.m3u,*.m3u8)を読み込んでチャンネル情報を登録する
procedure TMainForm.LoadCHList(FileName: string);
var
  fs, ls, ld: TStringList;
  i, j, idx, l: integer;
  s, sc,
  TVgrp, TVid, TVadr: string;
begin
  SetLength(TVChList, 0);   //  CHリストをクリア
  CateList.Clear;
  ChList.Clear;
  fs := TStringList.Create;
  ls := TStringList.Create; // #EXTINF行分解用
  ld := TStringList.Create; // #EXTINFの最後のチャンネル名分離用(,だけで分離)
  ld.Delimiter := ',';
  ld.StrictDelimiter := True;;
  try
    fs.LoadFromFile(FileName, TEncoding.UTF8);
    if fs.Count > 2 then
    begin
      s := Trim(fs.Strings[0]);
      if Pos('#EXTM3U', s) = 1 then
      begin
        i := 1;
        while i <= fs.Count -1 do
        begin
          s := Trim(fs.Strings[i]);
          // #EXTINF:ヘッダーがなければスキップする
          if Pos('#EXTINF:', s) <> 1 then
          begin
            Inc(i);
            Continue;
          end;
          s := StringReplace(s, '"', '', [rfReplaceAll]);
          if Pos('group-title=', s) = 0 then  // カテゴリーがない場合は空白カテゴリを挿入する
            s := s + ' group-title=　　';
          // ,の後ろにあるチャンネル名を分離する
          TVid := '';
          ld.CommaText := s;
          if ld.Count = 2 then
            TVid := ld.Strings[1];
          // 要素を分ける
          ls.CommaText := s;
          for j := 0 to ls.Count - 1 do
          begin
            idx := -1;
            sc := Trim(ls.Strings[j]);
            // グループカテゴリーを抽出する
            if Pos('group-title=', sc) > 0 then
            begin
              Delete(sc, 1, Length('group-title='));
              TVgrp := sc;
            end;
            // チャンネルIDが所得出来ていなければtvg-name=から抽出する
            if (TVid = '') and (Pos('tvg-name=', sc) = 1) then
            begin
              Delete(sc, 1, Length('tvg-name='));
              TVid := sc;
            end;
          end;
          // URLを取得する
          Inc(i);
          TVadr := fs.Strings[i];
          // 取得したグループ名、CH名、URLを登録する
          l := Length(TVChList);
          idx := -1;
          // 未登録
          if l = 0 then
          begin
            SetLength(TVChList, 1);
            idx := 0;
            TVChList[idx] := TVgrp;
          // 既に何らかのグループ名が登録されている場合は
          // TVgrpがその中に存在するかどうかチェックする
          end else begin
            for j := 0 to l - 1 do
            begin
              if Pos(TVgrp, TVChList[j]) = 1 then
              begin
                idx := j;
                Break;
              end;
            end;
            if idx = -1 then
            begin
              SetLength(TVChList, l + 1);
              idx := l;
              TVChList[idx] := TVgrp;
            end;
          end;
          // CH名とURL間を'|'で連結して保存する
          TVChList[idx] := TVChList[idx] + ',' + TVid + '|' + TVadr;
        end;
      end;
    end;
    CateList.Items.Clear;
    for i := 0 to Length(TVChList) - 1 do
    begin
      ld.CommaText := TVChList[i];
      CateList.Items.Add(ld.Strings[0]);
    end;
  finally
    ld.Free;
    ls.Free;
    fs.Free;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Ini.WriteInteger('Options', 'Volume', VolBar.Position);
  MPV.Stop;
  Ini.Free;
end;

// チャンネル選択・再生処理
procedure TMainForm.ChListSelectionChange(Sender: TObject; User: boolean);
var
  url: string;
begin
  if ChList.ItemIndex = -1 then
    Exit;
  // 選択したCHリストインデックスからURLを取得して再生する
  url := ChURL[ChList.ItemIndex];
  MenuOpen := False;
  if url <> '' then
  begin
    URLLabel.Caption := url;
    URLLabel.Hint := url;
    MPV.Play(url);
    MPV.SetAudioVolume(VolBar.Position);
    VolValue.Caption :=IntToStr(MPV.GetAudioVolume);
  end;
end;

// 選択したグループカテゴリーの登録チャンネルをリストに登録・表示する
procedure TMainForm.CateListSelectionChange(Sender: TObject; User: boolean);
var
  i: integer;
  grp, chl: TStringList;
  s, ch: string;
begin
  if CateList.ItemIndex = -1 then
    Exit;

  ChList.Items.Clear;
  SetLength(ChURL, 0);
  grp := TStringList.Create;
  chl := TStringList.Create;
  try
    grp.StrictDelimiter := True;
    grp.Delimiter := ',';
    chl.StrictDelimiter := True;
    chl.Delimiter := ',';
    s := TVChList[CateList.ItemIndex];
    grp.CommaText := s;
    SetLength(ChURL, grp.Count - 1);
    for i := 1 to grp.Count - 1 do
    begin
      ch := grp.Strings[i];
      ch := StringReplace(ch, '|', ',', [rfReplaceAll]);
      chl.CommaText := ch;
      if chl.Count <2 then
        Continue;
      ChList.Items.Add(chl.Strings[0]);     // リストにCH名を登録
      ChURL[i - 1] := chl.Strings[1];       // URLリストにURLを登録
    end;
  finally
    grp.Free;
    chl.Free;
  end;
end;

// カテゴリーリストのカスタム描画
procedure TMainForm.CateListDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  s: string;
begin
  with CateList do
  begin
    s := items[index];
    if odSelected in State then
      Canvas.Brush.Color := $F28909
    else
      Canvas.Brush.color := $333333;
    Canvas.FillRect(ARect);
    Canvas.font.Color := clWhite;
    Canvas.TextOut(ARect.Left + 4, ARect.top + 2, S);
  end;
end;

// チャンネルリストのカスタム描画
procedure TMainForm.ChListDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  s: string;
begin
  with ChList do
  begin
    s := items[index];
    if odSelected in State then
      Canvas.Brush.Color := $F28909
    else
      Canvas.Brush.color := $333333;
    Canvas.FillRect(ARect);
    Canvas.font.Color := clWhite;
    Canvas.TextOut(ARect.Left + 4, ARect.top + 2, S);
  end;
end;

function GetSpecialFolder(const iFolder: DWORD): String;
var
  IDL: PItemIDList;
  r2: bool;
  sf: string;
begin
  sf := '';
  SHGetSpecialFolderLocation(Application.MainFormHandle, iFolder, IDL);
  try
    sf := StringOfChar(#0, MAX_PATH);
    r2 := SHGetPathFromIDList(IDL, PChar(sf));
    if (IDL <> nil) and r2 then
    sf := Trim(sf);
  finally
    CoTaskMemFree(IDL);
  end;
  GetSpecialFolder := sf;
end;

// Iniファイルアクセス用のフォルダ・ファイル名生成
function GetIniFileName: string;
var
  s: string;
begin
	s := GetSpecialFolder(CSIDL_APPDATA) + '\'  // \user\user\AddData\Roaming\
       + ChangeFileExt(ExtractFileName(Application.ExeName),'');
  if not DirectoryExists(s) then
  	ForceDirectories(S);
  Result := S + '\' + ChangeFileExt(ExtractFileName(Application.ExeName),'.ini');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // libmpv-2.dllがなければ警告を表示して終了する
  if not MPV.IsLibMPVAvailable then
  begin
    ShowMessage('libmpv-2.dllが必要です.'#13#10'https://github.com/zhongfly/mpv-winbuild/releases');
    Close;
  end;

  FullScrMode := False;
  MXO := Width - 20;
  MXC := Width - 200;
  MenuOpen := True;

  // Iniファイルからプレイリストファイルを読み込む
  Ini := TIniFile.Create(GetIniFileName);
  M3uFile := Ini.ReadString('Options', 'm3ufile', '');
  VolBar.Position := Ini.ReadInteger('Options', 'Volume', 100);
  // プレイリストが登録されていれば読み込む
  if (M3uFile <> '') and FileExists(m3ufile) then
  begin
    LoadCHList(M3ufile);
  end else begin
    // 登録されているプレイリストファイルがない
    if M3uFile <> '' then
    begin
      MessageDlg('読み込みエラー', M3uFile + ' がありません.'#13#10'再度TVチャンネルリストファイルを指定して下さい.', mtWarning, [mbOK], 0);
      M3uFile := '';
    // プレイリストが登録されていない場合はファイル洗濯ダイアログを開く
    end else begin
      MessageDlg('TVチャンネルファイル登録', '最初にTVチャンネルリストファイルを登録して下さい.', mtWarning, [mbOK], 0);
      ListOpenBtnClick(nil);
    end;
  end;
end;

// ESCキーが押されたら強制的に終了する
procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Close;
end;

// マウスカーソルの位置によってチャンネル選択メニューを開閉させる
procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MenuOpen then
    Exit;
  if X > MXO then
    MenuPanel.Visible := True;
  if X < MXC then
    MenuPanel.Visible := False;
end;

// フォームサイズが変更されたらマウスカーソル判定位置を更新する
procedure TMainForm.FormResize(Sender: TObject);
begin
  inherited;

  MXO := Width - 20;
  MXC := Width - 200;
end;

// フォーム表示時にカテゴリーアイテムがあり未選択の場合は一番最初のアイテムを選択させる
procedure TMainForm.FormShow(Sender: TObject);
begin
  inherited;

  if (CateList.Count > -1) and (CateList.ItemIndex = -1) then
    CateList.ItemIndex := 0;
end;

// 再生画面のダブルクリックでフルスクリーン・ウィンドウモードを切り替える
procedure TMainForm.MPVDblClick(Sender: TObject);
begin
  if not FullScrMode then
  begin
    FullScrMode   := True;
    WindowState := wsFullScreen;
  end else begin
    FullScrMode := False;
    WindowState := wsNormal;
  end;
end;

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

// プレイリストを開いて登録する
procedure TMainForm.ListOpenBtnClick(Sender: TObject);
begin
  with OD do
  begin
    if Execute then
    begin
      LoadCHList(FileName);
      Ini.WriteString('Options', 'm3ufile', FileName);
    end;
  end;
end;

procedure TMainForm.VolBarChange(Sender: TObject);
begin
  MPV.SetAudioVolume(VolBar.Position);
  VolValue.Caption := IntToStr(VolBar.Position);
end;



end.

