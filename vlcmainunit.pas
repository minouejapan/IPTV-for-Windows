(*

  lazIPTV

  ver2.5  2026/11/27  EPGリストを取得出来ずにアクセス違反が発生していた不具合を修正した
  ver2.41 2026/11/24  グループ名読み込み処理の不具合を修正した
  ver2.4  2026/11/23  プレイリスト読み込み処理を書き換えてリファラ指定にも対応するようにした
  ver2.31 2026/08/02  プレイリストにグループIDがない場合にCH名に"group-title=なし"が付加される不具合を修正した
  ver2.3  2026/07/30  チャンネル識別IDの置換処理を廃止した
                      EPGリストがgzファイルの場合は展開してxmlファイルを取る出すようにした(Lazarus限定)
                      EPGサイトURLをプレイリストから取得する機能を追加した
                      旧チャンネルリストファイルをJSONファイルに変換する処理を削除した
                      TRegExprを最新版に更新した(ExecNextから抜け出せない不具合が解消した)
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
  Windows, ClipBrd, PasLibVlcPlayerUnit, RegExpr, WinInet, LoadM3U;

type
  TCHGroup = record
    CHName,
    PlayList: string;
  end;
  TCHInfo = record
    ChID,             // チャンネルID
    Name,             // チャンネル名称
    strmURL,          // ストリームURL
    Referrer: string; // リファラ(参照元)URL
	end;

  { TMainForm }
  TMainForm = class(TForm)
    CloseBtn: TSpeedButton;
    CateList: TListBox;
    ChList: TListBox;
    GrpSelect: TComboBox;
    Image1: TImage;
    Label1: TLabel;
		WinMode: TSpeedButton;
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
		procedure CateListResize(Sender: TObject);
    procedure CateListSelectionChange(Sender: TObject; User: boolean);
    procedure ChListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ChListSelectionChange(Sender: TObject; User: boolean);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GrpSelectDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GrpSelectSelect(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
		procedure WinModeClick(Sender: TObject);
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
    GrpList: array of string;   // グループ名
    ChID: array of string;      // EPGデータ取得用チャンネルID
    PL_EPG: string;             // プレイリストのEPGアドレス
    Ini: TJSONIni;
    PlainM3u: string;
    EPGSW, EPGbyPL: boolean;
    MkColor,
    MkFSize: integer;
    CHGroup: array of TCHGroup;
    CHInfo: array of TCHInfo;
    function LoadGroupList: string;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  GEditUnit, EPGunit;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //VLC.Stop(0);
  Ini.WriteInt('Options',   'Volume',   VolBar.Position);
  Ini.WriteInt('Options',   'MakeeCol', MkColor);
  Ini.WriteStr('Options',   'EPG_URL',  EPGurl);
  Ini.WriteInt('Options',   'MkFSize',  MkFSize);
  Ini.WriteBool('Options',  'EPG_ON',   EPGsw);
  Ini.WriteBool('Options',  'EPG_PL',   EPGbyPL);

  if GrpSelect.ItemIndex > -1 then
    Ini.WriteInt('Options', 'GroupIndex', GrpSelect.ItemIndex);
  Ini.Free;
end;

// チャンネル選択・再生処理
procedure TMainForm.ChListSelectionChange(Sender: TObject; User: boolean);
var
  url, reff: WideString;
  st, et, ttl: string;
  tvg: TVGuide;
  i: integer;
  opt: array[0..1] of WideString;
begin
  inherited;

  i := ChList.ItemIndex;
  if i = -1 then
    Exit;
  // 選択したCHリストインデックスからURLを取得して再生する
  url  := CHInfo[i].strmURL;
  reff := CHInfo[i].Referrer;
  MenuOpen := False;
  URLLabel.Caption := url;
  URLLabel.Hint    := url;
  if url <> '' then
  begin
    if reff <> '' then
      opt[0] := ':http-referrer=' + reff
    else
      opt[0] := '';
    if reff = '' then
      URLLabel.Caption := url
    else
      URLLabel.Caption := url + '@' + reff;
    VLC.Play(url, opt);
    VLC.SetAudioVolume(VolBar.Position);
    VolValue.Caption :=IntToStr(VolBar.Position);
    if EPGSW or EPGbyPL then
      URLLabel.Caption := '番組情報を取得中...お待ちください...';
    Application.ProcessMessages;
    if EPGSW or EPGbyPL then
    begin
      if EPGbyPL and (PL_EPG <> '') then
        EPGurl :=  PL_EPG;
      TVg := GetEPGGuide(CHInfo[i].ChID, Now);
      // EPGデータを取得出来なかった
      if TVg.Title = '' then
      begin
        TVTitle.Caption := '';
        VLC.MarqueeShowText('', 10, 10, MarqueeColor[MkColor], MkFSize, 255, 5000);
      end else begin
        st  := FormatDateTime('hh:nn', TVg.StartT);
        et  := FormatDateTime('hh:nn', TVg.EndT);
        ttl := Trim(ChList.Items[i] +  ' [' + st + ' - ' + et + '] ' +  TVg.Title);
        TVTitle.Caption := ttl;
        VLC.MarqueeShowText(ttl, 10, 10, MarqueeColor[MkColor], MkFSize, 255, 5000);
      end;
    end;
  end;
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
  i, n, cn: integer;
  grp, chl: TStringList;
  s, ch: string;
begin
  n := CateList.ItemIndex;
  if n = -1 then
    Exit;

  ChList.Items.Clear;
  SetLength(CHInfo, 0);
  cn := 0;
  for i := 0 to Length(CHData) - 1 do
  begin
    if CHData[i].GroupID = n then
    begin
      ChList.Items.Add(CHData[i].Name);
      Inc(cn);
      SetLength(CHInfo, cn);
      CHInfo[cn - 1].ChID     := CHData[i].ChID;
      CHInfo[cn - 1].Name     := CHData[i].Name;
      CHInfo[cn - 1].strmURL  := CHData[i].URL;
      CHInfo[cn - 1].Referrer := CHData[i].Referrer;
		end;
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

procedure TMainForm.CateListResize(Sender: TObject);
begin

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
  EPGbyPL := Ini.ReadBool('Options','EPG_PL',   False);

  // lazIPTVと同じフォルダ内にGRPLIST.TXTがあればグループリストとして
  // プレイリストが登録されていれば読み込む
  M3uFile := LoadGroupList;

  if M3uFile = '' then
  begin
    MessageDlg('読み込みエラー', M3uFile + ' がありません.'#13#10'再度TVチャンネルリストファイルを指定して下さい.', mtWarning, [mbOK], 0);
    ListOpenBtnClick(nil);
  end;
  if AbortFlag then
    Close;
end;

// ESCキーが押されたら強制的に終了する
procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Close;
end;

// マウスカーソルの位置によってチャンネル選択メニューを開閉させる
procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

// CHグループ変更処理
procedure TMainForm.GrpSelectSelect(Sender: TObject);
var
  m3u, fn: string;
  i: integer;
begin
  inherited;

  URLLabel.Caption := '';
  m3u := GrpList[GrpSelect.ItemIndex];
  if not LoadCHData(m3u) then
  begin
    URLLabel.Caption := 'プレイリストを見つけることが出来ませんでした.';
    Exit;
	end else begin
    if FileExists(m3u) then
    begin
      PLexport.Enabled := False;
      PLexport.Font.Color := clGray;
    end else begin
      PLexport.Enabled := True;
      PLexport.Font.Color := clWhite;
		end;
	end;
  CateList.Items.Clear;
  if Length(GroupName) > 0 then
  begin
    for i := 0 to Length(GroupName) - 1 do
    begin
      CateList.Items.Add(GroupName[i]);
    end;
    if CateList.Count > 0 then
      CateList.ItemIndex := 0;
  end;
end;

procedure TMainForm.MnuCopyClick(Sender: TObject);
begin
  ClipBoard.AsText := URLlabel.Caption;
end;

procedure TMainForm.WinModeClick(Sender: TObject);
begin
  if FullScrMode then
    WinMode.Caption := ''
  else
    WinMode.Caption := '';
  VLCDblClick(Self);
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
    ge.MkFSize.Value := MkFSize;
    ge.EPGColor.Selected := WindowsColor[MkColor];
    ge.EPGsw.Checked := EPGsw;
    ge.IsPlayList.Checked := EPGbyPL;
    if ge.ShowModal = mrOK then
    begin
      EPGurl  := ge.EPGurl.Text;
      MkFSize := ge.MkFSize.Value;
      MkColor := ge.EPGColor.ItemIndex;
      EPGsw   := ge.EPGsw.Checked;
      EPGbyPL := ge.IsPlayList.Checked;
      m3u     := LoadGroupList;
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

