(*
  EPGリスト処理
*)
unit epgunit;

{$IFDEF FPC}
  {$MODE Delphi}
  {$codepage utf8}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  Classes, SysUtils, DateUtils, LazUTF8,
{$ELSE}
  System.Classes, System.SysUtils, System.DateUtils, LazUTF8wrap,
{$ENDIF}
  WinINet, RegExpr;

type
  TVGuide = record
    Title,
    Description: string;
    StartT,
    EndT: TDateTime;
  end;

function GetEPGGuide(aID: string; PTime: TDateTime): TVGuide;

var
  EPGurl,
  EPGxml: string;


implementation


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

// HTML特殊文字の処理
// 1)エスケープ文字列 → 実際の文字
// 2)&#x????; → 通常の文字
function Restore2RealChar(Base: string): string;
var
  tmp, cd: string;
  w: integer;
  ch: Char;
  r: TRegExpr;
begin
  // エスケープされた文字
  tmp := UTF8StringReplace(Base, '&lt;',      '<',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&gt;',      '>',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&quot;',    '"',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&nbsp;',    ' ',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&yen;',     '\',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&brvbar;',  '|',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&copy;',    '©',  [rfReplaceAll]);
  tmp := UTF8StringReplace(tmp,  '&amp;',     '&',  [rfReplaceAll]);
  // &#????;にエンコードされた文字をデコードする(2023/3/19)
  // 正規表現による処理に変更した(2024/3/9)
  r := TRegExpr.Create;
  try
    r.Expression  := '&#.*?;';
    r.InputString := tmp;
    if r.Exec then
    begin
      repeat
        UTF8Delete(tmp, r.MatchPos[0], r.MatchLen[0]);
        cd := r.Match[0];
        UTF8Delete(cd, 1, 2);           // &#を削除する
        UTF8Delete(cd, UTF8Length(cd), 1);  // 最後の;を削除する
        if cd[1] = 'x' then         // 先頭が16進数を表すxであればDelphiの16進数接頭文字$に変更する
          cd[1] := '$';
        try
          w := StrToInt(cd);
          ch := Char(w);
        except
          ch := '？';
        end;
        UTF8Insert(ch, tmp, r.MatchPos[0]);
      until not r.ExecNext;
    end;
  finally
    r.Free;
  end;
  Result := tmp;
end;

function InitializeEPG: boolean;
begin
  EPGxml := LoadFromHTML(EPGurl);

  Result := UTF8Pos('<channel id=', EPGxml) > 0;
end;

function GetEPGGuide(aID: string; PTime: TDateTime): TVGuide;
var
  sr, guide, sn, en, tl, ttl, exp: string;
  dt, st, et: TDateTime;
  r1, r2: TRegExpr;
begin
  if EPGxml = '' then
  begin
    if EPGurl <> '' then
      InitializeEPG
    else
      Exit;
  end;

  Result.Title := '';
  dt := PTime;
  sr := '<channel id="' + aID + '">.*?<channel';
  r1 := TRegExpr.Create;
  r2 := TRegExpr.Create;
  try
    r1.InputString := EPGxml;
    r1.Expression  := sr;
    if r1.Exec then
    begin
      guide := r1.Match[0];
      guide := ReplaceRegExpr('<channel id="' + aID + '">.*?</channel>', guide, '');
    // リストの最後
    end else begin
      sr := '<channel id="' + aID + '">.*?</tv>';
      r1.Expression  := sr;
      if r1.Exec then
      begin
        guide := r1.Match[0];
        guide := ReplaceRegExpr('<channel id="' + aID + '">.*?</channel>', guide, '');
      end else
        Exit;
    end;
    r1.InputString := guide;
    r1.Expression  := '<programme start=.*?</programme>';
    if r1.Exec then
    begin
      repeat
        sn := r1.Match[0];
        en := sn;
        tl := sn;
        sn := ReplaceRegExpr('<programme start="', sn, '');
              // 20250115000000 を20205/01/15 00:00:00に変換する
        sn := UTF8Copy(sn, 1, 4) + '/' + UTF8Copy(sn, 5, 2) + '/' + UTF8Copy(sn, 7, 2) + ' '
              + UTF8Copy(sn, 9, 2) + ':' + UTF8Copy(sn, 11, 2) + ':' + UTF8Copy(sn, 13, 2);
        st := StrToDateTime(sn);

        en := ReplaceRegExpr('<programme start=".*?stop="', en, '');
              // 20250115000000 を20205/01/15 00:00:00に変換する
        en := UTF8Copy(en, 1, 4) + '/' + UTF8Copy(en, 5, 2) + '/' + UTF8Copy(en, 7, 2) + ' '
              + UTF8Copy(en, 9, 2) + ':' + UTF8Copy(en, 11, 2) + ':' + UTF8Copy(en, 13, 2);
        et := StrToDateTime(en);

        if (st <= dt) and (et > dt) then // 放映中
        begin
          ttl := '';
          r2.InputString := tl;
          r2.Expression  := '<title lang="ja">.*?</title>';
          if r2.Exec then
          begin
            ttl := r2.Match[0];
            ttl := ReplaceRegExpr('</title>', ReplaceRegExpr('<title lang="ja">', ttl, ''), '');
          end;
          r2.Expression  := '<desc lang="ja">.*?</desc>';
          if r2.Exec then
          begin
            exp := r2.Match[0];
            exp := ReplaceRegExpr('</desc>', ReplaceRegExpr('<desc lang="ja">', exp, ''), '');
          end;
          Result.Title       := Restore2RealChar(ttl);
          Result.Description := Restore2RealChar(exp);
          Result.StartT      := st;
          Result.EndT        := et;
          Break;
        end;
      until not r1.ExecNext;
    end;
  finally
    r1.Free;
    r2.Free;
  end;
end;

initialization
  EPGurl := 'http://epg.utako.moe/jcom.xml';
  EPGxml := '';

end.

