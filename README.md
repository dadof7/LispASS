## LispASS - Lisp As Syntax Sugar for javascript

WEBアプリの開発を「lispのREPLでできたらいいなぁ」という発想で作りました.
ブラウザでlispを動かし、別プロセスからwebsock経由で送り込んだ
S式を評価します. 結果もまたwebsock経由で返されます.

インタープリタは、吉田裕美さんの「ちょっと変わったLisp入門」をベースにしています.

### なにができるか

#### REPLが使える

websockを使って、REPLと対話します.
HTML画面をみながら、動作を確認しつつコードを書き足すような、
インターアクティブなことができます. DOM操作の結果も確認できます.

websockのサーバーになる簡単なプログラム(websock_chat.py)とEmacs側のクライアントプログラム(client.py)を
使って、Emacsの\*inferior-lisp\*モードからプログラムが書けます. 

<img src="system.png">

websock_chat.py経由ではありますが、Emacsから外部リスプを起動しているような立て付けになります.

ポート番号は**5003**で、変更する場合はconfig.js, websock_chat.py, client.pyの冒頭部分を直します.

#### lispでイベントハンドラーが書ける

例えばHTMLにこんなボタンがある場合、

```
<button type="button" name="new" class="btn btn-sm btn-primary">新規</button>
```

lispの関数でclickイベントハンドラーを書くと以下のようになります.
jQueryと一緒に使うと便利です.

```
(defun f (this e)
  (prln "pushed"))

(setq b ($ "button[name='new']"))

(b :on "click" (wrap f))
```

その後、fの定義をREPLから変更すれば、以降は**変更後の関数**が呼び出されます.
HTMLの再読み込みはいりません.

#### javascriptのオブジェクトを呼び出せる

javascriptのオブジェクトは関数として扱われ、プロパティにアクセスしたり、メソッドを呼び出すことができます.

```
=> ("abc" :length>)
3

=> ("abcd" :indexOf "c")
2

=> (setq a (js-eval "[]"))
[]

=> (a :push 3)
1 ; the number of the elements in a

=> a
[3]
```

逆にjavascriptからlispの関数を呼び出すこともできます.

```
<script>
 jQuery(function() {
   _G.funcall("lisp-init"); 
 });     
</script>
```

#### マクロが使える

Emacsのe-lisp程度のマクロが使えます.
バッククオート、コンマ、コンマ@などが使えます.

```
(defmacro += (x &rest args) `(setq ,x (funcall + ,x ,@args)))
```

### 特徴

関数と変数は同じ名前空間を使います.

deep bindingでdynamicスコーピングです. ただしトップの環境とパッケージはhashです.

javascriptのスタックフレームを使います.

GCもjavascript頼みです.

テイルコール最適化していないので、再帰を深くしすぎるとコケます.

演算子は基本的にjavascriptと同じものです.

速くはないですが、最近のjavascriptの性能のおかげで、
ヒューマンインターフェース周りを書くなら(私は)問題無いように感じます.

### 使い方

まずpython3でwebsockサーバーである、websock_chat.pyをコマンドラインから起動します.
必要なライブラリが無ければメッセージが出るのでpipで入れてください.

```
python websock_chat.py
```

次にChromeなどのブラウザでsample.htmlを開いてください.
websock_chat.pyとつながるはずです. 開発用の窓を出しておくと
色々な情報が表示されて便利です.

エディタはEmacsを使うのが前提です. 設定ファイルに
```
;;; websockのlisp環境
(setq *websock* nil)

(defun websock-lisp-mode-hook ()
  (defun lisp-eval-region (start end &optional and-go)
    (interactive "r\nP")
    (comint-send-region (inferior-lisp-proc) start end)
    (comint-send-string (inferior-lisp-proc) (if *websock* "\000\n" "\n"))
    (if and-go (switch-to-lisp t)))
  )

(add-hook 'inferior-lisp-mode-hook 'websock-lisp-mode-hook)

;; (bind-keys :map lisp-mode-map 
;; 	   ("C-j" . lisp-eval-last-sexp))

```
を加えてください. lispに送る**S式の最後に0を付加**する必要があります.

次にEmacsを起動し、a.lを開いてください. 別窓で、Ctrl-u ESC-x run-lispでリスプを起動します.
リスププログラムにはpython client.pyを指定してください.
リスプファイルの先頭には、

```
;; -*- *websock*:t; -*-
```
と書いて置く必要があります.  これにより設定ファイルに書いたフックを
有効にします.

websock_chat.pyを通じてブラウザとEmacsが噛み合っていれば、a.lのバッファで、
lis-eval-last-sexpなどでEmacsからS式が送れるようになります.
どれかのキーにバインドしておくとお手軽です.

いろいろ試しながら関数を書き、うまく動くことが分かったら、関数単位で
初期化ファイルに移します. Makefileを見てください.
リスプの初期化ファイルは、HTML起動時にjavascriptのコメントとして読み込むようになっています.

