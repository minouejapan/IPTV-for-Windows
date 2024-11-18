### lazIPTVについて

Windows版のIPTVを作ってみました。
Lazarusで作成したので安直にlazIPTVと名付けました。

実行ファイルを作成するためには、Lazarus ver3.2以降とPasLibVlcPlayerパッケージ
ライブラリが必要です。
https://github.com/paweld/PasLibVlc



#### lazIPTV実行に必要なライブラリについて

lazIPTVの実行にはVLCライブラリが必要です。Windows環境にVLCメディアプレーヤーがインストールされている必要があります。

#### その他
ver1.4からオンライン上のプレイリストファイルを登録出来るようになりました。
またgithub上で公開されているプレイリストはHTMLソースを解析してプレイリスト部分だけを抽出して使用します。

ver1.5からオンライン上から取得しているプレイリストをローカルファイルに保存出来るようにしました。

### ライセンス
MIT
