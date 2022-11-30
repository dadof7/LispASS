;; -*- *websock*:t; -*-
;; basic.l

(defun bp-system-alert (sign mess)
  (let1 this-alert
      ($ (+ "<div class='alert alert-danger alert-dismissible system-alert' role='alert'>"
	    (format "<span class='badge badge-danger mr-2' style='font-size:1rem;'>{0}</span>" sign) mess
	    "<button type='button' class='close' data-dismiss='alert' aria-label='閉じる'>"
	    "<span aria-hidden='true'>&times;</span>"
	    "</button></div>"))
    (($"#system-err-mess-area") :append this-alert)
    (scroll-to-jq-elem ($"#mess-end"))
    ;;(delay (* 20 1000) (this-alert :alert "close")) ; 自分だけ閉じる
    nil
    ))

(defun bp-custom-alert (sign html)
  (($"#system-err-mess-area") :append
   (+ "<div class='alert alert-info alert-dismissible system-alert' role='alert'>"
      (format "<span class='badge badge-info mr-2' style='font-size:1rem;'>{0}</span>" sign) html
      "<button type='button' class='close' data-dismiss='alert' aria-label='閉じる'>"
      "<span aria-hidden='true'>&times;</span>"
      "</button></div>"))
  (scroll-to-jq-elem ($"#mess-end"))
  nil
  )

(defun bp-alert-close ()
  (($".user-alert") :alert "close"))

(defun bp-alert (sign mess &optional close-other)
  (if close-other (bp-alert-close))

  (let* ((type (if (or (= "エラー" sign)
		       (= "警告" sign))
		   "badge-warning" "badge-primary"))
	 (div-alert
	  ($ (+ "<div class='alert alert-warning alert-dismissible user-alert' role='alert'>"
		(format "<span class='badge {1} mr-2' style='font-size:1rem;'>{0}</span>" sign type)
		mess
		"<button type='button' class='close' data-dismiss='alert' aria-label='閉じる'>"
		"<span aria-hidden='true'>&times;</span>"
		"</button></div>")))
	 )
    (($"#err-mess-area") :append div-alert)
    (scroll-to-jq-elem ($"#mess-end"))
    (delay 4000 (div-alert :alert "close")) ; 自分だけ閉じる
    nil ; promiseを返さないように
    ))

(defun int< (x)
  (js-window :parseInt (num< x)))

(defun mk-h:m (m)
  ;; => (mk-h:m 222)
  ;; ("03" ":" "42")
  (let1 x (num< m)
    (list ((+ "00" (int< (/ x 60))) :slice -2)
	  ":"
	  ((+ "00" (% x 60)) :slice -2))))

(defun m2hh:mm (min)
  ;; => (m2hh:mm 193)
  ;; "03:13"
  (reduce + (mk-h:m min) ""))

(defun list< (jsa)
  (js-foreach x jsa x))


;;; ********************************************************************************
(defun assert (condition mess)
  (if (not condition) (prln "*** assertion: " mess)))
;; (defun assert (condition mess &optional line-no file-name)
;;   (if (not condition) (prln (format "*** assertion failed: {2} in line {0} of file {1}" line-no file-name mess))))
;; (assert nil "aaa" :line :file)
;; (assert t "aaa" :line :file)

;; ====================================================================


(defun listify (a)
  ;; => (listify (js-safe-eval "[1,2,3]"))
  ;; (1 2 3)
  (if (js-isArray a)
      (js-foreach i a i)
    (throw (+ "listify: failed to make a list out of: " a))))

(defun who (conn)
  ;; jsonサーバーにだれがログインしているか問い合わせる
  (let1 p (query conn "who")
    (then p
      (let* ((mess (value :mess>))
	     (rc (mess :0>))
	     (dic (mess :1>))
	     )
	(prln mess)
	(and (= "WHO" rc)
	     (table-set
	      (js-foreach k (Object :keys dic)
		`(("ユーザ名" .	 ,k)
		  ("connection ID" . ,((dic : k) :0>))
		  ("inact. sec" . ,((dic : k) :1>))
		  ;;("in tarn. sec" . ,((dic : k) :2>))
		  ))
	      ))))))
;; (then (who) (prln (today)))

(defun scroll-to-jq-elem (jq-elem)
  (let1 pos ((jq-elem :offset) :top>)
    (($ "body,html") :animate
     (mk-struct `((scrollTop . ,pos)))
     400 "swing")))

(defmacro let-cols-in-a-row (row &rest exprs)
  ;; (macroexpand '(let~ (a b c) '(1 2 3) (* a 2)))
  (let1 L (gensym)
    `(let
	 ,(let1 L (eval row)
	    (foreach-enum* i x L
			   (list (intern (format "q-{0}" (1+ i))) x)))
       ,@exprs)))


;; ================================================================
;; エラー関係

(defmacro bind-vars-and-errs  (tr vars-names &rest exprs)
  ;; tr: tree root where search starts
  ;; var-names: lis of node names
  ;; trからはじめて、var-names中のnameを持つjq要素のvalueが
  ;; let*式でバインドされる. さらにそのnodeに関連付けられた
  ;; err-messクラスを持つ要素もname-errにバインドされる.
  (list 'let*
	(append
	 (foreach i vars-names
	   (let1 selector (format "[name='{0}']" (string< i))
	     `(,i (($ ,tr) :find ,selector)))
	   )
	 ;;
	 (foreach i vars-names
	   (let1 err (intern (+ (string< i) "-err"))
	     `(,err (my-err ,i))))
	 )
	`(progn ,@exprs)))

;;; ================================================================
;; <input>フィールドは以下のようになっている前提
;; .err-groupと.err-messの関係に注目すること
;;
;; <div id="test" class="err-group">
;;   <input type="text">
;;   <div class="err-mess" style="color:red;">
;;     err
;;   </div>
;; </div>

;; (defun my-err (me)
;;   ;; meはjQelement
;;   ;; meの属する.err-goupの中で.err-messクラスのjQelemを探す
;;   (let1 err ((me :closest ".err-group") :find ".err-mess")
;;     (if (= 0 (length err)) (throw "my-err: エラーを表示するクラス .err-mess が見つからない")
;;       err)))

;; (defun set-caption ($me mess)
;;   (if (/= mess "")
;;       ($me :addClass "err-background")
;;     ($me :removeClass "err-background"))

;;   (if (=== ($me :prop "tooltip-initialized") void0) ; 未初期化であれば初期化
;;       (progn
;; 	($me :tooltip)
;; 	($me :prop "tooltip-initialized" 1) ; 初期化した印を付ける
;; 	))
;;   ($me :attr "data-original-title" mess))

;; (defun set-err (jq-elem err-str val)
;;   ;; エラーメッセージをbootstrap4のtooltipにて付加する
;;   ;; 各種エラーメッセージは個別に保持できるようにbit-vectorになっていて
;;   ;; 対応するシンボルに対しval(tもしくはnil)をセットする. t=有効, nil=無効
;;   (let1 html-elem (jq-elem :0>)
;;     (if (=== (html-elem :err-vector>) void0) (html-elem :err-vector< (js-safe-eval "{}")))
;;     (let1 vector (html-elem :err-vector>)
;;       (vector (intern (+ ":" err-str "<")) val)
;;       (let1 all-err-str ""
;; 	(js-foreach k (Object :keys vector)
;; 	  (if (vector : k) (+= all-err-str k " ")))
;; 	(set-caption jq-elem all-err-str)))))

;; (defun set-err-x (jq-elem err-str val)
;;   ;; jq要素の下のベアなhtmlエレメントに
;;   ;; エラーメッセージを付加する
;;   ;; エラーメッセージは個別に保持できるようにbit-vectorになっていて
;;   ;; 対応するシンボルに対しval(tもしくはnil)をセットする
;;   (let1 html-elem (jq-elem :0>)
;;     (if (=== (html-elem :err-vector>) void0) (html-elem :err-vector< (js-safe-eval "{}")))
;;     (let1 vector (html-elem :err-vector>)
;;       (vector (intern (+ ":" err-str "<")) val)
;;       (let1 err (my-err jq-elem)
;; 	(timeout 0 (err :text ""))
;; 	(js-foreach k (Object :keys vector)
;; 	  (if (vector : k) (timeout 0 (err :text (+ (err :text) k " "))))
;; 	  ;;(prln k ":" (vector : k))
;; 	  )))))

(defun my-err (jq-me)
  (jq-me :next ".err-mess"))

(defun set-err (jq-elem err-str set-f)
  :desc "jq要素の下のベアなhtmlエレメントにエラーメッセージを付加する.
         set-f=nil リセット, o.w. エラーメッセージをセット"
  (let1 html-elem (jq-elem :0>)
    (if (=== (html-elem :err-vector>) void0) (html-elem :err-vector< (js-eval "x={}")))
    (let1 vector (html-elem :err-vector>)
      (vector (intern (+ ":" err-str "<")) set-f)
      (let1 err (my-err jq-elem)
	;;(timeout 0 (err :text "")) ; なぜtimeoutを使ったか不明なので除去 2021/7/9
	(err :text "")
	(js-foreach k (Object :keys vector)
	  (if (vector : k)
	      ;; (timeout 0 ; 除去、上に同じ
	      (err :text (+ (err :text) k " "))
	    ;; )
	    )
	  ;;(prln k ":" (vector : k))
	  )))))

;;; ================================================================
(defun mk-dt (&optional dt-str)
  (apply js-new (:Date>) (if dt-str `(,dt-str))))
;; (mk-dt "2020/1/3")

(defun now () (mk-dt))

(defun dt-format (dt &optional fmt)
  ;; 今日を自分好みのフォーマットで文字列にする
  (let1 dx (if (null dt) (js-new (:Date>)) dt)
    (let ((yyyy (dx :getFullYear))
	  (mm ((+ "0" (1+ (dx :getMonth))) :slice -2))
	  (dd ((+ "0" (dx :getDate)) :slice -2))
	  (h ((+ "0" (dx :getHours)) :slice -2))
	  (m ((+ "0" (dx :getMinutes)) :slice -2))
	  (s ((+ "0" (dx :getSeconds)) :slice -2))
	  )
      (format (if fmt fmt "{0}/{1}/{2}") yyyy mm dd h m s))))
;;(dt-format (now))
;;(dt-format (now) "{0}年{1}月{2}日 {3}時{4}分{5}秒")

(defun dt-1 (dt)
  ;; dtから月初を求める
  (let1 dd (js-new (:Date>) dt)
    (dd :setDate 1)
    dd))

(defun dt-31 (dt)
  ;; dtから月末を求める
  (let1 dd (dt-1 dt)
    (dd :setMonth (1+ (dd :getMonth)))
    (dd :setDate 0)
    dd))

(defun dt+ (dt &optional (x 1))
  ;; 翌日 or x日後
  (let1 dd (js-new (:Date>) dt)
    (dd :setDate (+ x (dd :getDate)))
    dd))

(defun dt++ (dt)
  ;; 破壊的翌日
  (dt :setDate (1+ (dt :getDate)))
  dt)

(defun dt- (dt &optional (x 1))
  ;; 前日 or x日前
  (let1 dd (js-new (:Date>) dt)
    (dd :setDate (- (dd :getDate) x))
    dd))

(defun dt-- (dt)
  ;; 破壊的前日
  (dt :setDate (1- (dt :getDate)))
  dt)

(defun dt-str (dt) (dt-format dt))
;; (dt-str (now))

;; 月曜日を0とする関数、日曜日が6
(defun getDay (dt)
  (let1 x (1- (dt :getDay))
    (if (< x 0) 6 x)))

;; 月から始まるdt週の最初の日
(defun 1st-day-of-week (dt)
  (let1 x (js-new (:Date>) dt)
    (x :setDate (- (x :getDate) (getDay x)))
    x))

;; 日で終わるdt週の最後の日
(defun last-day-of-week (dt)
  (let1 x (js-new (:Date>) dt)
    (x :setDate (+ (- (x :getDate) (getDay x)) 6))
    x))

;; dt1からdt2までの日をリストで返す
(defun days-between (dt1 dt2)
  (if (> dt1 dt2) nil
    (cons dt1 (days-between (dt+ dt1) dt2))))

;; dtが含まれる一週間のリストを返す
(defun days-in-week (dt)
  (let2 dt1 (1st-day-of-week dt)
	dt2 (last-day-of-week dt)
    (days-between dt1 dt2)))
;; (days-in-week (now))

;; dtの含まれる一週間のリストを返すが、dtと同じ月に無い日はnilに置き換える
(defun days-in-week-belonging-to-given-month (dt)
  (let1 m (dt :getMonth)
    (foreach i (days-in-week dt)
      (if (= (i :getMonth) m) i))))
;; (days-in-week-belonging-to-given-month (dt-1 (now)))
;; => (nil nil nil Thu Oct 01 2020 21:46:22 GMT+0900 (日本標準時) Fri Oct 02 2020 21:46:22 GMT+0900 (日本標準時) Sat Oct 03 2020 21:46:22 GMT+0900 (日本標準時) Sun Oct 04 2020 21:46:22 GMT+0900 (日本標準時))

;; dtが何週目の日か返す 1から始まる
(defun nth-week (dt)
  ((:Math:floor>) (/ (+ (- (dt :getDate) (getDay dt)) 12) 7)))

;; yyyy/mm月のn週目の日を(日 . n)のリストにして返す
(defun days-in-nth-week (yyyy mm n)
  (let3* dt1 (mk-dt (cat yyyy "/" mm "/1"))
	 dt2 (dt-31 dt1)
	 days (days-between dt1 dt2)
    (grep i (foreach j days
	      (cons j (nth-week j)))
	  (= (cdr i) n))))
;; (days-in-nth-week 2020 10 1)
;; => ((Thu Oct 01 2020 00:00:00 GMT+0900 (日本標準時) . 1) (Fri Oct 02 2020 00:00:00 GMT+0900 (日本標準時) . 1) (Sat Oct 03 2020 00:00:00 GMT+0900 (日本標準時) . 1) (Sun Oct 04 2020 00:00:00 GMT+0900 (日本標準時) . 1))

(setq *DAYS* (js-safe-eval "['日','月','火','水','木','金','土']"))

(defun days-in-ym (yyyy mm)
  (let* ((d1 (mk-dt (+ yyyy "/" mm "/" 1)))
	 (d31 (dt-31 d1))
	 g
	 )
    (defun g (d1 d31)
      (if (> d1 d31) nil
	(cons d1 (g (dt+ d1) d31))))

    (foreach i	(g d1 d31)
      (list
       (i :getDate)
       (*DAYS* : (i :getDay))
       ))))
;;(days-in-ym 2020 7) => ((1 "水") (2 "木") (3 "金") (4 "土") (5 "日") ....)
;;(av 3 (days-in-ym 2020 7)) => ("金")

;;; ================================================================
;; トランザクション関連

(defun promise1 () ; 常に成功し、1を返すpromise
  (promise (resolve 1)))

(defun conn-reset* (conn)
  ;; コネクションのpromiseをリセットし、connを正常に戻す
  ;; ただしconnのpromiseベースのシーケンスは失われるので注意
  (conn :p< (promise1)))

(defmacro sql* (conn stmt &rest args) ;; connが正常の場合のみsqlを実行
  ;; sqlとの違いは以下のquery*を参照
  `(,conn :p< (check-query (,sql ,conn ,stmt ,@args))))

(defmacro query* (conn stmt &rest args) ;; connが正常の場合のみqueryを実行
  ;; stmtの結果をチェックしてくれるqueryのちょっといいバージョン
  ;; 返り値はpromiseでstmtが成功した場合、queryの結果と変わらない
  ;; 新しいpromiseはconnにセットされる、これもqueryと変わらない
  `(,conn :p< (check-query (query ,conn ,stmt ,@args))))

(defmacro if-rejected* (conn &rest exprs) ;; connが失敗している時の処理
  ;; connにはcatchによって作られた新しいpromiseがセットされるので
  ;; connがリセットされたことになる => 以後connの成功に依存する処理も実行される.
  `(,conn :p<
	  (catch (,conn :p>) ,@exprs)))

(defmacro if-resolved* (conn &rest exprs) ;; connが成功している時の処理
  `(,conn :p<
	  (then (,conn :p>)
	    (let ((mess (value :mess>))
		  (rows (value :rows>))
		  (output (value :output>))
		  (limited (= js-true (value :limited>)))
		  )
	      ,@exprs)))) ; 最後exprがundefinedだとparingに失敗する

(defun begin* (conn) ;; connが正常の場合のみトランザクション開始
  (conn :p< (check-query (query conn "begin"))))

(defmacro commit* (conn &rest exprs)
  ;; connが正常な場合のみcommitする
  ;; 以前のsql*が失敗していればこのcommit*ではcommitできないので注意
  `(,conn :p< (then (check-query (query ,conn "commit")) ,@exprs)))

(defmacro rollback* (conn &rest exprs)
  ;; connが以前のsql*で失敗している場合のみrollbackできることに注意
  ;; このrollback*では、正常に終わった処理のrollbackはできない(普通しないから)
  ;; connはcatchで作られたpromiseがセットされるので、正常に戻る
  (let1 p (gensym)
    `(,conn :p<
	    (let1 ,p (,conn :p>)
	      (catch ,p ; connの中のpromiseが失敗した時には、
		(conn-reset* ,conn) ; connを正常に戻し、rollbackが成功すればexprsを実行
		(then (check-query (query ,conn "rollback")) ,@exprs))))))


(defun load (conn fname)
  ;; サーバーのsrc/にあるプログラムをロードする
  ;; ex. src/x.lをロードして評価 (load conn "x.l")
  (let1 p (query conn "load>" fname)
    (then p
      (let ((mess (value :mess>))
	    (output (value :output>))
	    )
	(if (/= "OK" (mess :0>))
	    (prln "load: '" fname "' " (mess :2>))
	  (eat output)
	  (prln "load: '" fname "' OK")
	  )))))


;; -------------
(defun check-query (p &optional file line)
  ;; エラーがあれば失敗するpromiseが返る
  ;; なければ成功するpromiseが返る
  (then p
    (let1 rc (check-dberror value file line)
      (if (/= :OK rc) (throw "check-query: failed")
	;; クエリに問題なし
	;; p ; 本来valueを返すべきと思うが、pでも機能するみたい評価されるとpromiseを返すpromiseは畳まれる?
	value
	))))

(defun check-dberror (value &optional file line)
  ;; messはqueryから返ってきた (value :mess>)の配列
  ;; エラーがなければ "OK"を返す
  ;; あればメッセージを表示し、"OK"以外を返す
  ;;(prln value)
  (let1 mess (value :mess>)
    (if (/= "OK" (mess :0>))
	(let1 err-mess (parse-dberror mess)
	  (if err-mess
	      (if (stringp err-mess)
		  (bp-system-alert "エラー"
				   (format "{0}{1}{2}" err-mess
					   (if file (+ " in " (string< file)) "")
					   (if line (+ " on " (string< line)) "")
					   ))
		;; err-messがstringではない場合は
		;; 既にエラーに対して適切な処理がなされたとしてなにもしない
		)
	    ))
      ;; これ以下mess == "OK"の場合
      (if (= js-true  (value :limited>)) ; 未定義ならundefinedが返る
	  (bp-alert "警告" "結果がを200行を超えたため、途中で打ち切られました."))
      :OK)
    ))


(defun parse-dberror (mess)
  (let1 str (reduce (lambda (x y) (+ x " " y))
		    (js-foreach i mess i) "") ; js配列に入っているメッセージ群を1つの文字列に合体
    (cond
     ;;(((js-safe-eval "/ERROR: duplicate key value violates unique constraint/") :test str)
     ;;"すでにデータがあるので、登録できませんでした.")

     ;;(((js-safe-eval "/ERROR: update or delete on table .+ violates foreign key constraint/") :test str)
     ;;"この行は参照されているため削除できません.")

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;; ここからPostgresサーバー ;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;(((js-safe-eval "/ERROR: 重複キーが一意性制約.+に違反しています/") :test str)
     ;;"重複するレコードが既に存在するため、登録できませんでした.")

     ;;(((js-safe-eval "/ERROR: テーブル.+外部キー制約.+に違反します/") :test str)
     ;;"このレコードは、他から参照されているので削除できません.")

     ;;(((js-safe-eval "/ERROR:.+NULL制約違反です/") :test str)
     ;;"値が必要な項目があります.")

     ;;(((js-safe-eval "/No results.*Previous SQL was not a query/") :test str)
     ;;"他のユーザが、対応するレコードを削除した可能性があります.")

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;; ここからMS-SQLサーバー ;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;(((js-safe-eval "/SQL Server(.*)Communication link failure/") :test str)
     ;;"データベースとの接続不良が発生しました.")

     ;;(((js-safe-eval "/Unexpected EOF from the server/") :test str)
     ;;"データベースとの接続が途切れました.")

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ここからはARCの掲示板用のファイル入出力 ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (((js-regex "/2 No such file or directory/") :test str)
      (if (and (av '*arc-no-alert* (env)) *arc-no-alert*) nil
	(bp-alert "確認" "該当レコードは存在しません."))
      )

     ;; default
     (t str) ; まだ補足していないエラー
     )))


;;; ================================================================


;; 環境envの中でメソッドを定義
;;(w/ env (defclj g (n) (* a (f b)))) と書くよりも
;;(defmethod @env:g (n) ...) こう書きたい
(defmacro defmethod (m &rest parms-args-body)
  (let* ((nm (string< m))
	 (ef (nm :match (js-regex "/^@([^:]+):([^:]+)$/")))
	 )
    (if (= js-null ef) (throw "defmethod: 1st arg must be in form of '@env:f'"))
    (let ((e (intern (ef :1>)))
	  (f (intern (ef :2>)))
	  )
      `(if (null (assoc ',f ,e)) (throw (+ "defmethod: can't find an appropriate definition of " ,nm))
	 (w/ ,e (defclj ,f ,@parms-args-body))))))
;;(macroexpand '(defmethod @env:g (n) (* a b)))
;;(defmethod @env:g (n) (* a a b))

;; @で始まるシンボルの置き換えマクロ
(defmacro *at-hook-handler* (symbol)
  (let1 nm (string< symbol)
    (cond
     ;; @0, @1, ... @n エラー時の環境
     ((nm :match (js-eval "/^@([0-9]+)$/"))
      (let1 idx (nm :substr 1) (list '*err : idx)))
     ;; デフォルト
     (t (throw (+ "*at-hook-handler*: " nm " is not implemented"))))))

;;; ================================================================
;;; サブテーブルを作成する仕組み

(defun mk-table-str (data)
  (let ((buff "")
	(pr (lambda (x) (+= buff x)))
	(prln (lambda (x) (+= buff x "\n")))
	(headers (foreach i (av 'row-headers data) ((i :split ":") :0>)))
	(widths (foreach i (av 'row-headers data) ((i :split ":") :1>)))
	(style "
    <style type='text/css'>
     {0} {
	 width: {1};
     }
     {0} th,
     {0} td {
	 width: 10rem;
	 height: 2rem;
	 vertical-align: middle;
	 padding: 2px 5px;
	 border: 1px solid #ccc;
     }
     {0} th:last-child,
     {0} td:last-child {
	 visibility: hidden;
	 width: initial;
     }
     {0} .fixed01,
     {0} .fixed02{
	 position: sticky;
	 top: 0;
	 left: 0;
	 // color: #fff;
	 background:#E2EFDA;
     }
     {0} thead .fixed01,
     {0} thead .fixed02 {
	 background:#d2f0c0;
     }
     {0} .fixed01::before,
     {0} .fixed02::before {
	 content: '';
	 position: absolute;
	 top: -1px;
	 left: -1px;
	 width: 100%;
	 height: 100%;
	 border-style: solid;
	 border-color: #ccc;
	 border-width: 1px 0 0 0;
     }
     {0} tr th {
	 font-size:11pt;
     }
     {0} tbody tr td {
	 font-size:.875rem;
	 font-weight:400;
     }
     {0} .fixed01{
	 z-index: 2;
     }
     {0} .fixed02{
	 z-index: 1;
     }
    </style>")
	)

    (prln (format "<div style='height:{0};overflow:scroll;'>" (or (av 'table-height data) "30rem")))
    (prln (format style (cat "table#" (av 'table-id data) "." (av 'table-class data)) (av 'table-width data)))

    (prln "<style>")
    (foreach w (enum-dot widths 1)
      (pr (format "  table{1} td:nth-child({0}),table{1} th:nth-child({0}) { " (car w) (cat "#" (av 'table-id data) "." (av 'table-class data))))
      (pr (format "width:{0};" (cdr w)))
      (prln " }")
      )
    (prln "</style>")

    (prln (format "<table id='{0}' class='{1}'>" (av 'table-id data) (av 'table-class data)))

    (prln "<thead><tr>")
    (foreach i (enum-dot headers)
      (prln (format "  <th class='fixed0{1} h-{2}'>{0}</th>"
	      (cdr i) ; ヘッダタイトル
	      (if (= 0 (car i)) 1 2)
	      (car i) ; 連番
	      )))
    (prln "  <th class='fixed02'>DUMMY</th>")
    (prln "</tr></thead>")

    (prln "<tbody>")
    (foreach i (range 200)
      (prln "<tr>")
      (foreach j (enum-dot headers)
	(if (= (car j) 0)
	    (prln (format "  <th class='fixed02 f-0'>列見出し{0}</th>" i))
	  (prln (format "  <td class='f-{0}'>フィールド{0}</td>" (car j))))
	)
      (prln "<td></td>")
      (prln "</tr>")
      )
    (prln "</tbody>")
    (prln "</table>")
    (prln "</div>")
    ))

(defun mk-table$ (data)
  ($ (mk-table-str data)))

(defun clone-template (jtemp) ; jQuery template elem => cloned jQuery elem
  (($ (js-document :importNode ((jtemp :0>) :content>) js-true)) :children))

(defmacro js-map (i v a &rest exprs)
  `(($ :makeArray ,a) :map (wrap (fun (this ,v ,i) ,@exprs))))

(defun last-elem (l) ; リストの最後の要素
  (if (null (cdr l)) (car l)
    (last-elem (cdr l))))

(defun non-last-elems (l) ; リストの最後の要素以外の要素
  (if (null (cdr l)) nil
    (cons (car l) (non-last-elems (cdr l)))))

;; nilを返す要素を除外, iはindexではなく生DOM要素
(defmacro js-filter (i a &rest exprs)
  `(($ :makeArray ,a) :filter (wrap (fun (this ,i)
				      ,@(non-last-elems exprs)
				      (if (null ,(last-elem exprs)) js-false)))))

(defmacro prln-buff (&rest args)
  `(+= buff (cat ,@args "\n")))

(defmacro pr-buff (&rest args)
  `(+= buff (cat ,@args)))

(defun $cat (&rest args)
  ($ (apply cat args)))

(defun $car (x)
  ($ (car x)))

(defun $cadr (x)
  ($ (cadr x)))

(defun $cat (&rest args)
  ($ (apply cat args)))

(defun zen2han (str)
  (:zenkana2Hankana str))

(defun expatch (l nn x)
  ;; リストlの中のn番目の要素をxに入れ替える.
  ;; ただしnは(n1 n2 n3 ... n)のようなリストnnで与える.
  ;; これはn1番目の中のn2番目のn3番目の中の...n番目を入れ替えるという意味になる.
  (cond ((null l) l)
	((null nn) l)
	((and (null (cdr nn)) (= (car nn) 1))
	 (cons x (cdr l)))
	((= (car nn) 1)
	 (cons (expatch (car l) (cdr nn) x) (cdr l)))
	(t
	 (cons (car l) (expatch (cdr l) (cons (1- (car nn)) (cdr nn)) x)))
	))

;; 画面いっぱいのちょっとお待ちくださいoverlayを出す/消す
(defun please-wait-full-overlay (show)
  (if show
      ((($"#fullOverlay") :css "display" "block") :css "z-index" 2147483647) ; 表示
    ((($"#fullOverlay") :css "display" "none") :css "z-index" -1) ; 非表示
    ))


;;(in-package :base)
