//
// PlayList読み込み
//
unit LoadM3U;

{$mode delphi}
{$codepage utf8}

interface

uses
  Classes, SysUtils, WInINet, LazUTF8;

type
  TCH_Data = record
    GroupID: integer;
    Name,
    URL,
    Referrer: string;
	end;

var
  GroupName: array of string;
  ChData: array of TCH_Data;

function LoadChData(FileName: string): Boolean;
function FindRegExpr(InputStr, Pattern: string): string; overload;
function FindRegExpr(InputStr, PatternL, PatternR: string): string; overload;


implementation

uses
  RegExpr;

// 正規表現検索でマッチした文字列を返す
function FindRegExpr(InputStr, Pattern: string): string;
var
  r: TRegExpr;
  ptn: string;
begin
  Result := '';
  ptn := Pattern;
  r := TRegExpr.Create;
  try
    r.Expression  := ptn;
    r.InputString := InputStr;
    if r.Exec then
    begin
      Result := r.Match[0];
    end;
  finally
    r.Free;
  end;
end;

// 正規表現でPatternL/Rに囲まれた文字列を取り出す
function FindRegExpr(InputStr, PatternL, PatternR: string): string;
var
  r: TRegExpr;
  ptn, s: string;
begin
  Result := '';
  ptn := PatternL + '[\s\S]*?' + PatternR;
  r := TRegExpr.Create;
  try
    r.Expression  := ptn;
    r.InputString := InputStr;
    if r.Exec then
    begin
      s := r.Match[0];
      s := ReplaceRegExpr(PatternR, ReplaceRegExpr(PatternL, s, ''), '');
      Result := s;
    end;
  finally
    r.Free;
  end;
end;

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

function GetHTTP(aURL: string): string;
var
  src: string;
  r: TRegExpr;
begin
  src := LoadFromHTML(aURL);
  // githubの場合はHTNLソースからプレイリスト部分だけを切り出す
  if Pos('https://github.com', aURL) = 1 then
  begin
    r := TRegExpr.Create;
    try
      r.Expression :='{"rawLines":\["#EXTM3U.*?\"],"stylingDirectives"';
      r.InputString := src;
      if r.Exec then
      begin
        src := r.Match[0];                        // マッチした部分
        UTF8Delete(src, r.MatchLen[0] - 21, 22);  // "],"stylingDirectives"を削除
        UTF8Delete(src, 1, 14);                   // {"rawLines":["を削除
        src := UTF8StringReplace(src, '\r',  '',     [rfReplaceAll]);
        src := UTF8StringReplace(src, '","', #13#10, [rfReplaceAll]);
        src := UTF8StringReplace(src, '"]',  '',     [rfReplaceAll]);
        src := UTF8StringReplace(src, '\"',  '"',    [rfReplaceAll]);
      end;
    finally
      r.Free;
    end;
  end;
  Result := src;
end;

// M3Uファイルを読み込んで一行毎に整形する
function GetM3U(FileName: string): string;
var
  sl: TStringList;
  line, m3u: string;
  i: integer;
begin
  Result := '';

  sl := TStringList.Create;
  try
    m3u := '';
    // httpもしくはローカルファイルを読み込む
    if UTF8Pos('https://', FileName) = 1 then
      sl.Text := GetHTTP(FileName)
    else
      sl.LoadFromFile(FileName);
    for i := 0 to sl.Count - 1 do
    begin
      line := sl[i];
      if line = '' then
        continue
      else if line[1] = '#' then
      begin
        if Pos('#EXTM3U', line) > 0 then
        begin
          continue;
				end else if Pos('#EXTINF', line) > 0 then
        begin
          if m3u <> '' then
            m3u := m3u + #13#10;
          m3u := m3u + line + ' ';
				end else if Pos('#EXTVLCOPT', line) > 0 then
        begin
          m3u := m3u + ' ' + line;
				end else begin
          m3u := m3u + ' ' + line;
				end;
			end else begin
        m3u := m3u + ' ' + line;
			end;
		end;
    // TVロゴ情報を削除する
    Result := ReplaceRegExpr('tvg-logo=".*?"', m3u, '');
	finally
    sl.Free;
	end;
end;

function GNExists(GN: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(GroupName) - 1 do
  begin
    if GroupName[i] = GN then
    begin
      Result := i;
      Break;
    end;
  end;
end;

// 指定したM3Uファイルからチャンネルデータを取り出す
function LoadChData(FileName: string): Boolean;
var
  i, gn, cn, p: integer;
  m3u, line, s: string;
  chlist: TStringList;
begin
  Result := False;
  m3u := GetM3U(FileName);
  if m3u = '' then
    Exit;

  gn := 0; cn := 0;
  SetLength(GroupName, 0);
  SetLength(ChData, 0);
  chlist := TStringList.Create;
  try
    chlist.Text := m3u;
    for i := 0 to chlist.Count - 1 do
    begin
      line := chlist[i];
      // 有効なデータかどうかチェックする
      if Pos('#EXTINF:', line) > 0 then
      begin
        Inc(cn);
        SetLength(ChData, Length(ChData) + 1);
  			s := FindRegExpr(line, 'group-title="', '"');
        if s = '' then
          s := 'なし';
        // チャンネルグループ名の重複をチェックして登録する
        if gn = 0 then
        begin
          Inc(gn);
          SetLength(GroupName, 1);
          GroupName[0] := s;
          p := 0;
				end else begin
          p := GNExists(s);
          if p = -1 then
          begin
            Inc(gn);
            SetLength(GroupName, gn);
            GroupName[gn - 1] := s;
            p := 0;
					end;
				end;
        s := FindRegExpr(line, '#EXTVLCOPT:http-referrer=', ' ');
        if s <> '' then
        begin
          CHData[cn - 1].Referrer:= s;
          line := ReplaceRegExpr('#EXTVLCOPT:http-referrer=.*? ', line, '');
				end else
          CHData[cn - 1].Referrer:= '';
  			s := FindRegExpr(line, 'tvg-name="', '"');
        if s = '' then
        begin
          // ,CHラベル https://～から切り出す
          s := FindRegExpr(line, ',', 'http');
          // tvg-nameもCHラベルもない場合はURLを使用する
          if s = '' then
    			  s := FindRegExpr(line, 'tvg-id="', '"');
				end;
        CHData[cn - 1].GroupID := p;
        CHData[cn - 1].Name    := s;
				s := FindRegExpr(line, 'http.*?m3u8');
        if s = '' then
        begin
          s := FindRegExpr(line, 'http.*? ');
          if s <> '' then
          begin
            if s[Length(s)] = ' ' then
              Delete(s, Length(s), 1);
					end else
            s := FindRegExpr(line, 'http.*?\z');
				end;
        CHData[cn - 1].URL     := s;
        Result := True;
			end;
		end;
	finally
    chlist.Free;
	end;
end;

end.

