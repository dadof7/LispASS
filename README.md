## LispASS - Lisp As Syntax Sugar

WEBアプリの開発を「lispのREPLでできたらいいなぁ」という発想で作りました.

### なにができるか

##### REPLが使える

websockを使って、REPLを動かします.
HTML画面をみながら、動作を確認しつつコードを書き足すような、
インターアクティブなことができます. DOM操作の結果がすぐ見られます.

websockのサーバーになる簡単なプログラム(python)とEmacs側のクライアントプログラム(python)を
使って、Emacsの\*inferior-lisp\*モードからプログラムが書けます.


##### lispでイベントハンドラーが書ける

例えばlispの関数でボタンのイベントハンドラーを書けます.
jQueryと一緒に使うと便利です.

```
(defun f (this e)
  (prln "pushed"))

(setq b ($ "button[name='new']"))

(b :on "click" (wrap f))
```

その後、fの定義をREPLから変更すれば、以降は変更後の関数が呼び出されます.
HTMLの再読み込みはいりません.

##### javascriptのオブジェクトを呼び出せる

javascriptのオブジェクトは関数として扱われ、プロパティにアクセスしたり、メソッドを呼び出すことができます.

```
=> ("abc" :length>)
3

=> ("abcd" :indexOf "c")
2
```

逆にjavascriptからlispの関数を呼び出すこともできます.

##### マクロが使える

Emacsのe-lisp程度のマクロが使えます.
バッククオート、コンマ、コンマ@などが使えます.

### 特徴

関数と変数は同じ名前空間を使います.

deep bindingでdynamicスコーピングです. ただしトップの環境とパッケージはhashです.

javascriptのスタックフレームを使います.

GCもjavascript頼みです.

テイルコール最適化していないので、再帰を深くしすぎるとコケます.
