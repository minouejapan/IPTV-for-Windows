(*

  lazIPTV

  ver2.2  2025/07/22  初期化時にメモリアクセス違反が出る場合があった不具合を修正した
                      設定ファイルをIniファイルからJSONファイルに変更した
                      チャンネルグループリストをテキストファイルからJSONファイルに変更した
  ver2.1  2025/07/06  チャンネルID変換テーブルの一部に不備があった不具合を修正した
  ver2.0  2025/07/05  新たな番組情報(EPG)に対応するため、チャンネルID変換テーブルと変換処理を
                      追加した
                      番組情報がない場合、直前のチャンネルの情報が表示される不具合を修正した
                      番組情報表示フォントサイズを変更出来るようにした
                      EPG URL, EPG有効・無効, フォトサイズ,色を保存出来るようにした
  ver1.9  2025/05/29  番組情報(EPG)取得時のタイムアウト設定を追加した
                      EPGを取得出来なかった場合、チャンネルを切り替えるたびにEPGを取得しない
                      ようにした
  ver1.8  2025/03/02  番組情報表示色を変更出来るようにした
  ver1.7  2025/01/29  番組情報表示の有効・無効を選択出来るようにした
  ver1.6  2025/01/20  EPGファイルを読み込んで視聴中の番組情報を表示できるようにした
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

{$IFDEF FPC}
  {$MODE Delphi}
  {$codepage utf8}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LazUTF8, Buttons, LCLType, ComCtrls, Menus, JsonIni, fpJSON,
{$ELSE}
  System.Classes, System.SysUtils, Vcl.Forms, Vcl.Controls, Vcl.Graphics,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Menus,
  LazUTF8wrap,
{$ENDIF}
  IniFiles, Windows, ActiveX, ShlObj, ClipBrd, PasLibVlcPlayerUnit, RegExpr,
  WinInet;

type
  TCHGroup = record
    CHName,
    PlayList: string;
  end;

  { TMainForm }
  TMainForm = class(TForm)
    CloseBtn: TSpeedButton;
    CateList: TListBox;
    ChList: TListBox;
    GrpSelect: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    TVTitle: TLabel;
    VLC: TPasLibVlcPlayer;
    PLexport: TSpeedButton;
    MnuCopy: TMenuItem;
    MuteBtn: TSpeedButton;
    CaptPanel: TPanel;
    SD: TSaveDialog;
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
    procedure VLCClick(Sender: TObject);
    procedure VLCDblClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure ListOpenBtnClick(Sender: TObject);
    procedure MuteBtnClick(Sender: TObject);
    procedure PLexportClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure VolBarChange(Sender: TObject);
  private
    FullScrMode,
    MenuOpen,
    AbortFlag: Boolean;
    MXO, MXC, CYO: integer;
    M3uFile: string;
    TVChList: array of string;  // グループ名,CH名#9URL,CH名|URL,CH名 URL,....
    ChURL: array of string;     // CH名に対応したストリーム再生URL
    GrpList: array of string;   // グループ名
    ChID: array of string;      // EPGデータ取得用チャンネルID
    Ini: TJSONIni;
    PlainM3u: string;
    EPGSW: boolean;
    MkColor,
    MkFSize: integer;
    CHGroup: array of TCHGroup;
    procedure LoadCHList(FileName: string);
    function LoadGroupList: string;
    function GetOnlineList(aURL: string): string;
    function LoadOnlineM3u(aURL: string): string;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  GEditUnit, EPGunit, tvgunit;

{$R *.lfm}

{$I initjson.inc}

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
  ua          : string;
begin
  Result   := '';
              // ユーザエージェントをEdgeに設定する
  ua       := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.117 Safari/537.36 Edg/79.0.309.65';
  hSession := InternetOpen(PChar(ua), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(hSession) then
  begin
    dwFlag   := INTERNET_FLAG_RELOAD;
    hService := InternetOpenUrl(hSession, PChar(URLadr), nil, 0, dwFlag, 0);
    if Assigned(hService) then
    begin
      RBuff := TMemoryStream.Create;
      try
        lpBuffer := AllocMem(65536);
        try
          dwBytesRead := 65535;
          while True do
          begin
            if InternetReadFile(hService, lpBuffer, 65535, dwBytesRead) then
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
      Result := ExtractFilePath(Ini.FileName) + 'playlist.m3u';
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
  TVgrp, TVid, TVadr, Tvg, Gid: string;
  r: TRegExpr;
begin
  SetLength(TVChList, 0);   //  CHリストをクリア
  CateList.Clear;
  ChList.Clear;
  tvg := '';
  fs := TStringList.Create;
  ls := TStringList.Create; // #EXTINF行分解用
  ld := TStringList.Create; // #EXTINFの最後のチャンネル名分離用(,だけで分離)
  ld.Delimiter := ',';
  ld.StrictDelimiter := True;;
  try
    fs.LoadFromFile(FileName, TEncoding.UTF8);
    if fs.Count > 1 then
    begin
      s := Trim(fs.Strings[0]);
      if Utf8Pos('#EXTM3U', s) = 1 then
      begin
        r := TRegExpr.Create;
        try
          r.InputString := fs.Text;
          r.Expression  := '#EXTM3U url-tvg=".*?" tvg-shift=';
          if r.Exec then
          begin
            Tvg := r.Match[0];
            Tvg := ReplaceRegExpr('" tvg-shift=', ReplaceRegExpr('#EXTM3U url-tvg="', Tvg, ''), '');
            Tvg := ReplaceRegExpr(',.*?xml', Tvg, '');
          end;
          fs.Delete(0);
        finally
          r.Free;
        end;
      end;
      i := 0;
      while i <= fs.Count -1 do
      begin
        s := Trim(fs.Strings[i]);
        // #EXTINF:ヘッダーがなければスキップする
        if Utf8Pos('#EXTINF:', s) <> 1 then
        begin
          Inc(i);
          Continue;
        end;
        if Utf8Pos('group-title=', s) = 0 then  // カテゴリーがない場合は空白カテゴリを挿入する
          s := s + ' group-title=なし';
        // チャンネルIDを取得する
        if UTF8Pos('tvg-id="', s) > 0 then
        begin
          r := TRegExpr.Create;
          try
            r.InputString := s;
            r.Expression  := 'tvg-id=".*?"';
            if r.Exec then
            begin
              Gid := r.Match[0];
              Gid := ReplaceRegExpr('"', ReplaceRegExpr('tvg-id="', Gid, ''), '');
            end;
          finally
            r.Free;
          end;
        end;
        s := UTF8StringReplace(s, '"', '', [rfReplaceAll]);
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
        if Gid = '' then
          Gid := 'DUMMY';
        // CH名とURL間を'|'で連結して保存する
        TVChList[idx] := TVChList[idx] + ',' + TVid + '|"' + TVadr + '"|"' + Gid + '"';
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
  //VLC.Stop(0);
  Ini.WriteInt('Options',   'Volume',   VolBar.Position);
  Ini.WriteInt('Options',   'MakeeCol', MkColor);
  Ini.WriteStr('Options',   'EPG_URL',  EPGurl);
  Ini.WriteInt('Options',   'MkFSize',  MkFSize);
  Ini.WriteBool('Options',  'EPG_ON',   EPGsw);

  if GrpSelect.ItemIndex > -1 then
    Ini.WriteInt('Options', 'GroupIndex', GrpSelect.ItemIndex);
  Ini.Free;
end;

// チャンネル選択・再生処理
procedure TMainForm.ChListSelectionChange(Sender: TObject; User: boolean);
var
  url, st, et, ttl: string;
  tvg: TVGuide;
  i: integer;
begin
  inherited;

  i := ChList.ItemIndex;
  if i = -1 then
    Exit;
  // 選択したCHリストインデックスからURLを取得して再生する
  url := ChURL[i];
  MenuOpen := False;
  URLLabel.Caption := url;
  URLLabel.Hint := url;
  if url <> '' then
  begin
    VLC.Play(url);
    VLC.SetAudioVolume(VolBar.Position);
    VolValue.Caption :=IntToStr(VolBar.Position);
    if EPGSW then
      URLLabel.Caption := '番組情報を取得中...お待ちください...';
    Application.ProcessMessages;
    if EPGSW then
    begin
      TVg := GetEPGGuide(Chid[i], Now);
      // EPGデータを取得出来なかった
      if TVg.Title = '' then
      begin
        TVTitle.Caption := '';
        VLC.MarqueeShowText('', 10, 10, MarqueeColor[MkColor], MkFSize, 255, 5000);
      end else begin
        st  := FormatDateTime('hh:nn', TVg.StartT);
        et  := FormatDateTime('hh:nn', TVg.EndT);
        ttl := ChList.Items[i] +  ' [' + st + ' - ' + et + '] ' +  TVg.Title;
        TVTitle.Caption := ttl;
        VLC.MarqueeSetColor(MarqueeColor[MkColor]);
        VLC.MarqueeShowText(ttl, 10, 10, MarqueeColor[MkColor], MkFSize, 255, 5000);
      end;
      URLLabel.Caption := url;
    end;
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
    SetLength(ChID, grp.Count - 1);
    for i := 1 to grp.Count - 1 do
    begin
      ch := grp.Strings[i];
      ch := Utf8StringReplace(ch, '|', ',', [rfReplaceAll]);
      chl.CommaText := ch;
      if chl.Count < 2 then
        Continue;
      try
        ChList.Items.Add(chl.Strings[0]);     // リストにCH名を登録
        ChURL[i - 1]  := chl.Strings[1];      // URLリストにURLを登録
        ChID[i - 1]   := chl.Strings[2];      // 番組情報取得用チャンネルID
      except
        ;
      end;
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

// キャプションバーを掴んだらWindowをマウスで移動できるようにする
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

// JSONファイルからチャンネルグループリストを読み込む
function TMainForm.LoadGroupList: string;
var
  fn, tx, item, value: string;
  sl: TStringList;
  jo: TJSONObject;
  i, n, c: integer;
  ge: TGrpEdit;
begin
  Result := '';
  GrpSelect.Visible := True;
  GrpSelect.Items.Clear;
  fn := ExtractFilePath(Ini.FileName) + 'grplist.json';
  tx := ExtractFilePath(Ini.FileName) + 'grplist.txt';
  // GRPLIST.TXTがあってGRPLIST.JSONがない場合はGRPLIST.TXTをGRPLIST.JSONにコンバートする
  if not FileExists(fn) and FileExists(tx) then
  begin
    ge := TGrpEdit.Create(Self);
    ge.JsonConverter;
    ge.Free;
  end;
  if FileExists(fn) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(fn, TEncoding.UTF8);
      jo := TJSONObject(GetJSON(sl.Text));
      if jo.Find('CHGroup') <> nil then
      begin
        n := jo.Objects['CHGroup'].Count;
        SetLength(GrpList, n);
        for i := 0 to n - 1 do
        begin
          SetLength(CHGroup, i + 1);
          item  := jo.Objects['CHGroup'].Names[i];
          value := jo.Objects['CHGroup'].Strings[item];
          CHGroup[i].CHName   := item;
          CHGroup[i].PlayList := value;
          GrpSelect.Items.Add(item);
          GrpList[i] := value;
        end;
      end;
      n := Ini.ReadInt('Options', 'GroupIndex', 0);
      try
        c := GrpSelect.Items.Count;
        if c = 0 then
          GrpSelect.ItemIndex := -1
        else if n < c then
        begin
          GrpSelect.ItemIndex := n;
          GrpSelectSelect(nil);
        end;
      except
        ;
      end;
      Result := GrpList[n];
    finally
      sl.Free;
      jo.Free;
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
  AbortFlag := False;

  Ini := TJSONIni.Create;
  VolBar.Position := Ini.ReadInt('Options', 'Volume', 100);
  VolValue.Caption:= IntToStr(VolBar.Position);
  VolBarChange(nil);
  MkColor := Ini.ReadInt('Options', 'MakeeCol', 15);
  EpgURL  := Ini.ReadStr('Options', 'EPG_URL',  'https://github.com/karenda-jp/etc/raw/refs/heads/main/guides.xml');
  MkFSize := Ini.ReadInt('Options', 'MkFSize',  32);
  EPGsw   := Ini.ReadBool('Options','EPG_ON',   False);

  // lazIPTVと同じフォルダ内にGRPLIST.TXTがあればグループリストとして
  // プレイリストが登録されていれば読み込む
  M3uFile := LoadGroupList;
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
      ListOpenBtnClick(nil);
    // プレイリストが登録されていない場合はファイル洗濯ダイアログを開く
    end else begin
      MessageDlg('TVチャンネルファイル登録', '最初にTVチャンネルリストファイルを登録して下さい.', mtWarning, [mbOK], 0);
      ListOpenBtnClick(nil);
    end;
    if AbortFlag then
      Close;
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
    // URLがgithubの場合は取得したHTMLからプレイリスト部分だけを抽出する
    if Pos('https://github.com', aURL) = 1 then
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
              fn := ExtractFilePath(iNI.FileName) + 'playlist.m3u';
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
  inherited;

  URLLabel.Caption := '';
  m3u := GrpList[GrpSelect.ItemIndex];
  if FileExists(m3u) then
  begin
    LoadCHList(m3u);
    PLexport.Enabled := False;
    PLexport.Font.Color := clGray;
  end else if UTF8Pos('https://', m3u) = 1 then
  begin
    fn := LoadOnlinem3u(m3u);
    if fn <> '' then
    begin
      try
        LoadChList(fn);
        PLexport.Enabled := True;
        PLexport.Font.Color := clWhite;
      except
        ;
      end;
    end;
  end else
    URLLabel.Caption := 'プレイリストを見つけることが出来ませんでした.';
end;

procedure TMainForm.MnuCopyClick(Sender: TObject);
begin
  ClipBoard.AsText := URLlabel.Caption;
end;

procedure TMainForm.VLCClick(Sender: TObject);
begin
  if EPGSW then
  begin
    VLC.MarqueeSetColor(MarqueeColor[MkColor]);
    VLC.MarqueeShowText(TVTitle.Caption, 10, 10, MarqueeColor[mkColor], MkFSize, 255, 5000);
  end;
end;

// 再生画面のダブルクリックでフルスクリーン・ウィンドウモードを切り替える
procedure TMainForm.VLCDblClick(Sender: TObject);
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
  sl: TStringList;
begin
  i := GrpSelect.ItemIndex;
  ge := TGrpEdit.Create(Self);
  try
    ge.EPGurl.Text  := EPGurl;
    ge.MkFSize.Value:= MkFSize;
    ge.EPGColor.Selected:= WindowsColor[MkColor];
    ge.EPGsw.Checked:= EPGsw;
    if ge.ShowModal = mrOK then
    begin
      EPGurl  := ge.EPGurl.Text;
      MkFSize := ge.MkFSize.Value;
      MkColor := ge.EPGColor.ItemIndex;
      EPGsw   := ge.EPGsw.Checked;
      m3u     := LoadGroupList;
      if FileExists(m3u) then
        LoadCHList(m3u)
      else if UTF8Pos('https://', m3u) = 1 then
      begin
        m3u := LoadOnlinem3u(m3u);
        if m3u <> '' then
          LoadChList(m3u);
      end else begin
        sl := TStringList.Create;
        try
          sl.Text := JGRPLST;
          sl.SaveToFile(ExtractFilePath(Application.ExeName) + 'grplist.json', TEncoding.UTF8);
          sl.Text := SMPLM3U;
          sl.SaveToFile(ExtractFilePath(Application.ExeName) + 'sample.m3u', TEncoding.UTF8);
          LoadCHList(m3u);
          i := 0;
        finally
          sl.Free;
        end;
      end;
      GrpSelect.ItemIndex := i;
      GrpSelectSelect(nil);
    end else begin
      sl := TStringList.Create;
      try
        sl.Text := JGRPLST;
        sl.SaveToFile(ExtractFilePath(Application.ExeName) + 'grplist.json', TEncoding.UTF8);
        sl.Text := SMPLM3U;
        m3u := ExtractFilePath(Application.ExeName) + 'sample.m3u';
        sl.SaveToFile(m3u, TEncoding.UTF8);
        LoadCHList(m3u);
        PLexport.Enabled := True;
        PLexport.Font.Color := clWhite;
        i := 0;
      finally
        sl.Free;
      end;
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
      if FileExists(FileName) then
        if MessageDlg('確認', 'ファイルは既に存在します.上書きしますか?', mtWarning, [mbYes, mbNo], 0) = mrNo then
          Exit;
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

