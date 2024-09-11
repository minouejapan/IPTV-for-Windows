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

lazIPTV実行に必要なライブラリについて
lazIPTVの実行にはlibmpv-2.dllが必要です。

https://github.com/zhongfly/mpv-winbuild/releases

からmpv-dev-x86_64-202??????-git-???????.7z(?はバージンで変わります)をダウ
ンロードし、展開したlibmpv-2.dllをlazIPTV.exeと同じフォルダ内にコピーして
ください。


### ライセンス
MIT
