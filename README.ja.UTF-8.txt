Tamago-tsunagi
==============

Last modified date of this document
-----------------------------------

2014-Dec.-13


License
-------

GPL-2.0+ (please see "COPYING" file)


What is Tamago-tsunagi?
-----------------------

「たまご(Tamago・Egg)」は
戸村哲さん(電子技術総合研究所)・NIIBE Yutakaさんらが中心となって
開発されたGNU Emacs上で動く
日本語(および中国語・韓国語)入力環境(Input Method)です。

Egg Version 4.0.xは1997～2004年頃まで積極的に開発され、
2004年までに、
対応するバックエンドサーバ(Input Metho Engine)は
(Free)Wnn・SJ3の他にCanna・Anthyが使えるようになっていました。


しかし、その後、Upstreamでの開発が停滞し、Debianでpatchが当てられ、
FreeBSD・openSUSEなど他のディストリビューションは
Debianのパッケージに個別にpatchを当てるようになりました。

そしてTamago 4を管理していた http://www.m17n.org/tamago は
ドメインごと無くなってしまいました。

このままではTamagoが分裂し、非効率な状態になると考えた
中野充敏はTamagoのupstreamを再び作ることに思い至りました。

http://lists.debian.or.jp/debian-devel/201408/msg00006.html
http://lists.freebsd.org/pipermail/freebsd-users-jp/2014-August/000255.html
http://lists.opensuse.org/opensuse-ja/2014-08/msg00030.html
https://sourceforge.jp/projects/freewnn/lists/archive/users/2014-August/000212.html
https://sourceforge.jp/projects/anthy/lists/archive/dev/2014-August/003854.html
https://sourceforge.jp/projects/canna/lists/archive/dev/2014-August/000377.html

基本的に賛同の意見がきかれました。

しかし、戸村さんとの連絡に時間がかかってしまいました。
また、提案者の中野が2014年10月から11月の間
ネットを離れることになりました。

その間にDebianのunstable版であるsidでは
NIIBEさん・ISHIKAWA Mutsumiさんが主導して
Eggを4.1.8-1にまで上げました。
https://packages.debian.org/ja/sid/egg
https://anonscm.debian.org/cgit/pkg-anthy/egg.git

しかしEgg 4.1.8-1では対応言語は日本語に限られ、
IMEはAnthy・SJ3のみに対応し、(Free)Wnn・Cannaのサポートは
外されました。

(Free)Wnn・Cannaのサポートを外したくない中野は
FreeBSDのtamago-4.0.6.0.20041122.19_7をベースに
新しいprojectを立ち上げることにしました。
それがTamago-tsunagi projectです。
バージョンは tsunagi-5.x.x.xを名乗っています。


How to install
--------------

Please see "INSTALL" file.


Tamago-tsunagi project page
---------------------------

https://sourceforge.jp/projects/tamago-tsunagi/


Tamago-tsunagi Mailing Lists
----------------------------

https://sourceforge.jp/projects/tamago-tsunagi/lists/

Tamago-tsunagi project に参加したい方、
Tamago-tsunagiを使ってみたい方、
Tamago-tsunagi 仕様・実装についての質問・要望、
その他Tamago(Egg)に少しでも興味のある方はぜひご参加願います。


Tamago-tsunagi project manager
------------------------------

中野充敏 (Mitsutoshi NAKANO) <bkbin005@rinku.zaq.ne.jp>
 <ItSANgo@gmail.com> <http://d.hatena.ne.jp/Itisango/>
 <https://twtter.com/ItSANgo>
