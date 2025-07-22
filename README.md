### lazIPTVについて

Windows版のIPTVを作ってみました。
Lazarusで作成したので安直にlazIPTVと名付けました。

実行ファイルを作成するためには、Lazarus ver3.2以降とPasLibVlcPlayerパッケージライブラリとTRegExpr、拙作JSONIniFileが必要です。

PasLibVlcPlayerはhttps://github.com/paweld/PasLibVlc

TRegExprはhttps://github.com/andgineer/TRegExpr

JSONIniFileはhttps://github.com/minouejapan/JSONIniFile

から入手して下さい。


#### lazIPTV実行に必要なライブラリについて

lazIPTVの実行にはVLCライブラリが必要です。Windows環境にVLCメディアプレーヤーがインストールされていれば動作します。
#### その他
ver2.2からチャンネルグループリストと設定ファイルをJSONファイルに保存するようにしました。旧チャンネルグループリストGRPLIST.TXTからは自動的にgrplist.jsonファイルにコンバートします。
ver1.4からオンライン上のプレイリストファイルを登録出来るようになりました。
またgithub上で公開されているプレイリストはHTMLソースを解析してプレイリスト部分だけを抽出して使用します。

ver1.5からオンライン上から取得しているプレイリストをローカルファイルに保存出来るようにしました。

ver1.6で実装した番組情報表示のためにlazIPTVを起動後、最初のチャンネル選択時にEPG情報を読み込むようにしています。この最初の処理に少し時間がかかります。
尚、番組情報はチャンネルを選択・変更した際に5秒間表示して消えます。また、画面をマウスで右クリックすると（何度でも）5秒間表示されます。


### ライセンス
MIT
