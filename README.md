### lazIPTVについて

Windows版のIPTVを作ってみました。
Lazarusで作成したので安直にlazIPTVと名付けました。

実行ファイルを作成するためには、Lazarus ver3.2以降とUW_MPVPlayerパッケージ
ライブラリが必要です。
https://github.com/URUWorks/UW_MPVPlayer

尚、UW_MPVPlayerライブラリが依存しているライブラリがありますので、パケージ
のインストール時にエラーが発生する場合は必要なパッケージライブラリを最初に
インストールしてください。

Lazarusにパッケージライブラリをインストールするためには、Lazarusに対する
ある程度の知識が必要になってきます。これらのことに関する質問にはお答えかね
ますのでご了承ください。

#### lazIPTV実行に必要なライブラリについて

lazIPTVの実行にはVLCライブラリが必要です。Windows環境にVLCメディアプレーヤーがインストールされている必要があります。

ver1.4からオンライン上のプレイリストファイルを登録出来るようになりました。
またgithub上で公開されているプレイリストはHTMLソースを解析してプレイリスト部分だけを抽出して使用します。

ver1.5からオンライン上から取得しているプレイリストをローカルファイルに保存出来るようにしました。

### ライセンス
MIT
