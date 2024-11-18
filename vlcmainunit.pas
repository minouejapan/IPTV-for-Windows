(*

  lazIPTV

  ver1.5  2024/11/18  ネット上から取得したプレイリストをファイルに保存出来るようにした
                      再生ライブラリをVLCライブラリに変更した
  ver1.4  2024/10/30  ネット上からプレイリストを取得出来るようにした
  ver1.3  2024/09/30  ミュートボタンを追加した
                      ウィンドウ表示時に位置を移動するためのキャプションバーを追加した
  ver1.2  2024/09/16  全画面にした際にマウスカーソルを消すようにした
                      URLに","が含まれていると正常にチャンネル登録が出来ない不具合を修正した
  ver1.1  2024/09/13  CHリスト(プレイリスト)を複数登録してグループを切り替え出来るようにした
                      合わせてCHグループ編集機能を追加した
  ver1.0  2024/09/11  IPTVを自分で作ってみた

*)
unit vlcmainunit;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LazUTF8, Buttons, LCLType, ComCtrls, Menus, IniFiles, Windows,
  ActiveX, ShlObj, Types, WinINet, RegExpr, ClipBrd,
  PasLibVlcPlayerUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    CloseBtn: TSpeedButton;
    CateList: TListBox;
    ChList: TListBox;
    GrpSelect: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    PLexport: TSpeedButton;
    MnuCopy: TMenuItem;
    MuteBtn: TSpeedButton;
    CaptPanel: TPanel;
    SD: TSaveDialog;
    VLC: TPasLibVlcPlayer;
    PopupMenu1: TPopupMenu;
    URLLabel: TLabel;
    VolValue: TLabel;
    MenuPanel: TPanel;
    Panel1: TPanel;
    OpPanel: TPanel;
    ListOpenBtn: TSpeedButton;
    VolBar: TTrackBar;
    procedure CaptPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CateListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CateListSelectionChange(Sender: TObject; User: boolean);
    procedure ChListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ChListSelectionChange(Sender: TObject; User: boolean);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GrpSelectChange(Sender: TObject);
    procedure GrpSelectDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GrpSelectSelect(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    procedure MPVDblClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure ListOpenBtnClick(Sender: TObject);
    procedure MuteBtnClick(Sender: TObject);
    procedure PLexportClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure VolBarChange(Sender: TObject);
  private
    FullScrMode,
    MenuOpen: Boolean;
    MXO, MXC, CYO: integer;
    M3uFile: string;
    TVChList: array of string;  // グループ名,CH名#9URL,CH名|URL,CH名 URL,....
    ChURL: array of string;     // CH名に対応したストリーム再生URL
    GrpList: array of string;
    Ini: TIniFile;
    PlainM3u: string;
    procedure LoadCHList(FileName: string);
    function GetGroupList: string;
    function GetOnlineList(aURL: string): string;
    function LoadOnlineM3u(aURL: string): string;
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  GEditUnit;

{$R *.lfm}

{ TMainForm }

// WinINetを用いたHTMLファイルのダウンロード
function LoadFromHTML(URLadr: string): string;
var
  hSession    : HINTERNET;
  hService    : HINTERNET;
  dwBytesRead : DWORD;
  dwFlag      : DWORD;
  lpBuffer    : PChar;
  RBuff       : TMemoryStream;
  TBuff       : TStringList;
begin
  Result   := '';
  hSession := InternetOpen('WinINet', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(hSession) then
  begin
    dwFlag   := INTERNET_FLAG_RELOAD;
    hService := InternetOpenUrl(hSession, PChar(URLadr), nil, 0, dwFlag, 0);
    if Assigned(hService ) then
    begin
      RBuff := TMemoryStream.Create;
      try
        lpBuffer := AllocMem(65536);
        try
          dwBytesRead := 65535;
          while True do
          begin
            if InternetReadFile(hService, lpBuffer, 65535,{SizeOf(lpBuffer),}dwBytesRead) then
            begin
              if dwBytesRead = 0 then
                break;
              RBuff.WriteBuffer(lpBuffer^, dwBytesRead);
            end else
              break;
          end;
        finally
          FreeMem(lpBuffer);
        end;
        TBuff := TStringList.Create;
        try
          RBuff.Position := 0;
          TBuff.LoadFromStream(RBuff, TEncoding.UTF8);
          Result := TBuff.Text;
        finally
          TBuff.Free;
        end;
      finally
        RBuff.Free;
      end;
    end;
    InternetCloseHandle(hService);
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

function TMainForm.GetOnlineList(aURL: string): string;
var
  plist: TStringList;
begin
  PlainM3u := '';
  Result := '';
  plist := TStringList.Create;
  try
    plist.Text := LoadFromHTML(aURL);
    if plist.Count > 2 then
    begin
      PlainM3u := plist.Text;
      Result := ExtractFilePath(GetIniFileName) + 'playlist.m3u';
      plist.SaveToFile(Result, TEncoding.UTF8);
    end;
  finally
    plist.Free;
  end;
end;

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
      if Utf8Pos('#EXTM3U', s) = 1 then
      begin
        i := 1;
        while i <= fs.Count -1 do
        begin
          s := Trim(fs.Strings[i]);
          // #EXTINF:ヘッダーがなければスキップする
          if Utf8Pos('#EXTINF:', s) <> 1 then
          begin
            Inc(i);
            Continue;
          end;
          s := UTF8StringReplace(s, '"', '', [rfReplaceAll]);
          if Utf8Pos('group-title=', s) = 0 then  // カテゴリーがない場合は空白カテゴリを挿入する
            s := s + ' group-title=なし';
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
            if Utf8Pos('group-title=', sc) > 0 then
            begin
              Utf8Delete(sc, 1, Length('group-title='));
              TVgrp := sc;
            end;
            // チャンネルIDが取得出来ていなければtvg-name=から抽出する
            if (TVid = '') and (Utf8Pos('tvg-name=', sc) = 1) then
            begin
              Utf8Delete(sc, 1, Length('tvg-name='));
              TVid := sc;
            // チャンネルIDの取得が出来なかった場合は登録をスキップする
            end;
            if TVid = '' then
            begin
              Inc(i);
              Continue;
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
              if Utf8Pos(TVgrp, TVChList[j]) = 1 then
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
          TVChList[idx] := TVChList[idx] + ',' + TVid + '|"' + TVadr + '"';
        end;
      end;
    end;
    CateList.Items.Clear;
    if Length(TVChList) > 0 then
    begin
      for i := 0 to Length(TVChList) - 1 do
      begin
        ld.CommaText := TVChList[i];
        CateList.Items.Add(ld.Strings[0]);
      end;
      if CateList.Count > 0 then
        CateList.ItemIndex := 0;
    end;
  finally
    ld.Free;
    ls.Free;
    fs.Free;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  VLC.Stop(100);
  Ini.WriteInteger('Options', 'Volume', VolBar.Position);
  if GrpSelect.ItemIndex > -1 then
    Ini.WriteInteger('Options', 'GroupIndex', GrpSelect.ItemIndex);
  Ini.Free;
end;

// チャンネル選択・再生処理
procedure TMainForm.ChListSelectionChange(Sender: TObject; User: boolean);
var
  url: string;
begin
  inherited;

  if ChList.ItemIndex = -1 then
    Exit;
  // 選択したCHリストインデックスからURLを取得して再生する
  url := ChURL[ChList.ItemIndex];
  MenuOpen := False;
  URLLabel.Caption := url;
  URLLabel.Hint := url;
  if url <> '' then
  begin
    VLC.Play(url);
    VLC.SetAudioVolume(VolBar.Position);
    VolValue.Caption :=IntToStr(VLC.GetAudioVolume);
  end;
end;

procedure TMainForm.GrpSelectChange(Sender: TObject);
begin

end;

procedure TMainForm.GrpSelectDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  s: string;
begin
  with GrpSelect do
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
      ch := Utf8StringReplace(ch, '|', ',', [rfReplaceAll]);
      chl.CommaText := ch;
      if chl.Count < 2 then
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
      Canvas.Brush.color := $282828;
    Canvas.FillRect(ARect);
    Canvas.font.Color := clWhite;
    Canvas.TextOut(ARect.Left + 4, ARect.top + 2, S);
  end;
end;

procedure TMainForm.CaptPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetCapture(Self.Handle);
  ReleaseCapture;
  SendMessage(MainForm.Handle, WM_SYSCOMMAND, SC_MOVE or 2, 0);
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
      Canvas.Brush.color := $282828;
    Canvas.FillRect(ARect);
    Canvas.font.Color := clWhite;
    Canvas.TextOut(ARect.Left + 4, ARect.top + 2, S);
  end;
end;

function TMainForm.GetGroupList: string;
var
  s1, s2: TStringList;
  gf: string;
  i: integer;
begin
  Result := '';
  // 読み込みグループ選択ComboBoxを有効にする
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
        SetLength(GrpList, s1.Count);
        GrpSelect.Visible := True;
        GrpSelect.Items.Clear;
        for i := 0 to s1.Count - 1 do
        begin
          if Utf8Pos(',', s1.Strings[i]) = 0 then
            Continue;
          s2.CommaText := s1.Strings[i];
          GrpSelect.Items.Add(s2.Strings[0]);
          GrpList[i] := s2.Strings[1];
        end;
        i := Ini.ReadInteger('Options', 'GroupIndex', 0);
        if GrpSelect.Items.Count >= i then
          GrpSelect.ItemIndex := i
        else
          GrpSelect.ItemIndex := 0;
        Result := GrpList[GrpSelect.ItemIndex]
      end;
    finally
      s1.Free;
      s2.Free;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FullScrMode := False;
  MXO := Width - 20;
  MXC := Width - 200;
  CYO := 24;
  MenuOpen := True;
  PlainM3u := '';

  // Iniファイルからプレイリストファイルを読み込む
  Ini := TIniFile.Create(GetIniFileName);
  VolBar.Position := Ini.ReadInteger('Options', 'Volume', 100);
  VolBarChange(nil);
  // lazIPTVと同じフォルダ内にGRPLIST.TXTがあればグループリストとして
  // プレイリストが登録されていれば読み込む
  M3uFile := GetGroupList;
  if (M3uFile <> '') and FileExists(M3uFile) then
  begin
    LoadCHList(M3ufile);
    PLexport.Enabled := False;
    PLexport.Font.Color := clGray;
  end else if Pos('https://', M3uFile) = 1 then
  begin
    M3ufile := LoadOnlinem3u(M3uFile);
    if M3ufile <> '' then
    begin
      LoadCHList(M3ufile);
      PLexport.Enabled := True;
      PLexport.Font.Color := clWhite;
    end;
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
  begin
    MenuPanel.Visible := True;
    Screen.Cursor := crDefault;
  end;
  if X < MXC then
  begin
    MenuPanel.Visible := False;
    if FullScrMode then
      Screen.Cursor := crNone;
  end;
  if not FullScrMode then
    CaptPanel.Visible := Y < CYO;
end;

// フォームサイズが変更されたらマウスカーソル判定位置を更新する
procedure TMainForm.FormResize(Sender: TObject);
begin
  inherited;

  MXO := MainForm.Width - 40;
  MXC := MainForm.Width - 200;
end;

// フォーム表示時にカテゴリーアイテムがあり未選択の場合は一番最初のアイテムを選択させる
procedure TMainForm.FormShow(Sender: TObject);
begin
  inherited;

  if (CateList.Count > -1) and (CateList.ItemIndex = -1) then
    CateList.ItemIndex := 0;
end;

// ネット上からプレイリストを取得する
function TMainForm.LoadOnlineM3u(aURL: string): string;
var
  fn, s: string;
  gsrc: TStringList;
  r: TRegExpr;
begin
  fn := '';
  PlainM3u := '';
  if UTF8Pos('https://', aURL) = 1 then
  begin
    if Pos('https://github.com', aURL) = 1 then   // githubからHTMLを取得してプレイリストを抽出する
    begin
      s := LoadFromHTML(aURL);
      if Pos('#EXTM3U', s) > 1 then
      begin
        r := TRegExpr.Create;
        try
          r.Expression :='{"rawLines":\["#EXTM3U.*?\"],"stylingDirectives"';
          r.InputString := s;
          if r.Exec then
          begin
            s := r.Match[0];                        // マッチした部分
            UTF8Delete(s, r.MatchLen[0] - 21, 22);  // "],"stylingDirectives"を削除
            UTF8Delete(s, 1, 14);                   // {"rawLines":["を削除
            s := UTF8StringReplace(s, '\r', '', [rfReplaceAll]);
            s := UTF8StringReplace(s, '","', #13#10, [rfReplaceAll]);
            s := UTF8StringReplace(s, '"]', '', [rfReplaceAll]);
            s := UTF8StringReplace(s, '\"', '"', [rfReplaceAll]);
            PlainM3u := s;
            gsrc := TStringList.Create;
            try
              gsrc.Text := s;
              fn := ExtractFilePath(GetIniFileName) + 'playlist.m3u';
              gsrc.SaveToFile(fn, TEncoding.UTF8);
            finally
              gsrc.Free;
            end;
          end;
        finally
          r.Free;
        end;
      end;
    end else begin  // HTMLからプレーンテキストを取得
      fn := GetOnlineList(aURL);
    end;
  end;
  // オンライン上から取得して一時保存したプレイリストファイル名を返す
  Result := fn;
end;

// CHグループ変更処理
procedure TMainForm.GrpSelectSelect(Sender: TObject);
var
  m3u, fn: string;
begin
  m3u := GrpList[GrpSelect.ItemIndex];
  if FileExists(m3u) then
  begin
    LoadCHList(m3u);
    PLexport.Enabled := False;
    PLexport.Font.Color := clGray;
  end else begin
    if UTF8Pos('https://', m3u) = 1 then
    begin
      fn := LoadOnlinem3u(m3u);
      if fn <> '' then
      begin
        LoadChList(fn);
        PLexport.Enabled := True;
        PLexport.Font.Color := clWhite;
      end;
    end;
  end;
end;

procedure TMainForm.MnuCopyClick(Sender: TObject);
begin
  ClipBoard.AsText := URLlabel.Caption;
end;

// 再生画面のダブルクリックでフルスクリーン・ウィンドウモードを切り替える
procedure TMainForm.MPVDblClick(Sender: TObject);
begin
  if not FullScrMode then
  begin
    FullScrMode := True;
    WindowState := wsFullScreen;
    Screen.Cursor := crNone;
  end else begin
    FullScrMode := False;
    WindowState := wsNormal;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

// プレイリストを開いて登録する
procedure TMainForm.ListOpenBtnClick(Sender: TObject);
var
  ge: TGrpEdit;
  m3u: string;
  i: integer;
begin
  i := GrpSelect.ItemIndex;
  ge := TGrpEdit.Create(Self);
  try
    if ge.ShowModal = mrOK then
    begin
      m3u := GetGroupList;
      if FileExists(m3u) then
        LoadCHList(m3u)
      else begin
        if UTF8Pos('https://', m3u) = 1 then
        begin
          m3u := LoadOnlinem3u(m3u);
          if m3u <> '' then
            LoadChList(m3u);
        end;
      end;
      GrpSelect.ItemIndex := i;
      GrpSelectSelect(nil);
    end;
  finally
    ge.Free;
  end;
end;

procedure TMainForm.MuteBtnClick(Sender: TObject);
begin
  if MuteBtn.Caption = #$E74F then
  begin
    MuteBtn.Caption := #$E767;
    MuteBtn.Hint := 'ミュートする';
    VLC.SetAudioMute(False);
  end else begin
    MuteBtn.Caption := #$E74F;
    MuteBtn.Hint := 'ミュートを解除する';
    VLC.SetAudioMute(True);
  end;
end;

procedure TMainForm.PLexportClick(Sender: TObject);
var
  sl: TStringList;
begin
  if PlainM3u = '' then
    Exit;
  with SD do
  begin
    FileName := GrpSelect.Text;
    if Execute then
    begin
      sl := TStringList.Create;
      try
        sl.Text := PlainM3u;
        sl.SaveToFile(FileName, TEncoding.UTF8);
      finally
        sl.Free;
      end;
    end;
  end;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  MnuCopy.Enabled := URLLabel.Caption <> '';
end;

procedure TMainForm.VolBarChange(Sender: TObject);
begin
  VLC.SetAudioVolume(VolBar.Position);
  VolValue.Caption := IntToStr(VolBar.Position);
end;



end.

