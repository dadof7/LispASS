;; -*- *websock*:t; -*-
;; init.l

(setq defmacro
  '(macro (m params &rest body)
     (progn
       (check-fp params)
       (list 'progn (list 'setq m (list 'quote (list 'macro params (if (< 1 (length body)) (cons 'list (cons ''progn body)) (car body)))))
	     (list 'quote m)))))

;; (defmacro defun (f params &rest body)
;;   (let ((n (length body))
;; 	(test (check-fp params))
;; 	)
;;     (list 'progn (list 'setq f (list 'quote (list 'lambda f params (if (< 1 n) (cons 'progn body) (car body)))))
;; 	  (list 'quote f))))
(defmacro defun (f params &rest body)
  (check-fp params)
  (if (and (= :desc (car body)) (stringp (car (cdr body))))
      `(progn (setq ,f (lambda ,(car (cdr body)) ,params ,@(cdr (cdr body)))) (quote ,f))
    `(progn (setq ,f (lambda ,(string< f) ,params ,@body)) (quote ,f))
    ))

;; (defmacro defclj (f &rest args)
;;   ;; defunのようにcljを定義する
;;   ;; ((defclj f (n) (* n n)) 3) => 9
;;   (let ((tmp-f (gensym)))
;;     `(progn
;;        (defun ,tmp-f ,@args)
;;        (setq ,f (clj ,tmp-f)))))
(defmacro defclj (f params &rest body)
  (check-fp params)
  (if (and (= :desc (car body)) (stringp (car (cdr body))))
      `(progn (setq ,f (clj (lambda ,(car (cdr body)) ,params ,@(cdr (cdr body))))) (quote ,f))
    `(progn (setq ,f (clj (lambda ,(string< f) ,params ,@body))) (quote ,f))
    ))

(defmacro modclj (f &rest args)
  "cljの関数部分だけを置き換える ex. (modclj clj-f (n) (* n n))"
  `(dot ,f :expr< (fun ,@args)))

;; default @マクロハンドラー
(defmacro *at-hook-handler* (&rest a)
  (throw "*at-hook-handler* not implemented")
  )

(defmacro labels (defs &rest args)
  (cons 'let
        (cons
         (foreach i defs
           (list (car i) (cons 'lambda (cons (car (cdr i)) (cdr (cdr i)))))
           )
         args)))

(defmacro let1 (var expr &rest exprs) (list 'let (list (list var expr)) (cons 'progn exprs)))

(defmacro let2 (var expr var1 expr1 &rest exprs)
  (list 'let (list (list var expr) (list var1 expr1))
	(cons 'progn exprs)))

(defmacro let2* (var expr var1 expr1 &rest exprs)
  (list 'let* (list (list var expr) (list var1 expr1))
	(cons 'progn exprs)))

(defmacro let3 (var expr var1 expr1 var2 expr2 &rest exprs)
  (list 'let (list (list var expr) (list var1 expr1) (list var2 expr2))
	(cons 'progn exprs)))

(defmacro let3* (var expr var1 expr1 var2 expr2 &rest exprs)
  (list 'let* (list (list var expr) (list var1 expr1) (list var2 expr2))
	(cons 'progn exprs)))

(defmacro letp1 (var p &rest exprs)
  ;; the value of promise p is bound to var while evaluating exprs
  `((async 
     (lambda (,var)
       ,@exprs)
     ,p)))
;; (letp1 v (query *conn* "select 1") (prln (v :mess>)))

(defmacro letp+ (pairs &rest exprs)
  ;; paris is a list of binding pairs as in let
  ;; if the first pair is (var promise), then the resolved value of p is bound to var
  `((async 
     (lambda (,(car (car pairs)))
       (let* ,(cdr pairs)
	 ,@exprs))
     ,(cadr (car pairs)))))

(defmacro letp* (pairs &rest exprs)
  ;; like letp+, but pairs can contain (v promise) pairs
  ;; promises are evaluated in turn
  (if (null (cdr pairs))
      `(letp+ ,pairs
	 ,@exprs)
    `(letp+ (,(car pairs))
       (letp* ,(cdr pairs) ,@exprs))))

(defun reject (&optional x) ; rejectするpromiseを返す
  (Promise :reject x))

;; :Promise:allはpromiseの配列を受け取り、新しいpromise Pを返す.
;; 全部成功した場合、Pのresultは結果の配列となる.
;; 例: result = [3,2]
;; どれか1つでもrejectされた場合、Pのreasonはrejectの値となる.配列ではない.
;; 例: rason = -1
(defun promise-all (&rest pexprs)
  (let1 a (js-array< pexprs)
    (:Promise:all a)))

(defmacro fun (params &rest body)
  (let ((n (length body))
	(test (check-fp params))
	)
    (if (<= n 1)
	`(lambda ,params ,@body)
      `(lambda ,params (progn ,@body)))))

(defmacro when (b &rest y) (list 'if b (cons 'progn y) nil))

(defmacro unless (b &rest y) (list 'if b nil (cons 'progn y)))

(defun endp (x)
  (if (null x) t
    (if (consp x) nil
      (throw "endp failed to detect list-end"))))

(defun /=== (a b)
  (not (=== a b)))

(setq js-object (js-eval "Object"))

(defun js-null? (x) ; undefined or null in js
  (= js-null x))

;; lispのt/nilをjs-true/js-falseに変換
(defun js-tf (p)
  (if p js-true js-false))

(setq *regex-pool* nil)
(defun js-regex (x)
  (or (av x *regex-pool*)
      (let1 regex (js-safe-eval x)
	(setq *regex-pool* (cons (cons x regex) *regex-pool*))
	regex)))

(defmacro js-foreach (i a &rest expr) `(foreach ,i (range (,a :length>)) (let ((,i (,a : ,i))) ,@expr)))
(setq foreach-a js-foreach)

(defmacro js-foreach-hash (k v h &rest exprs)
  `(js-foreach ,k (Object :keys ,h)
     (let1 ,v (,h : ,k)
       ,@exprs)))

(defmacro foreach* (i L &rest exprs)
  "L can be a list or js-array"
  (let1 LL (gensym)
    `(let1 ,LL ,L
       (if (js-isArray ,LL)
	   (js-foreach ,i ,LL ,@exprs)
	 (foreach ,i ,LL ,@exprs)))))

;; (a b c)のような変数の並びにjsのrow配列を順番にlet代入する
;; 例: (let-row (a b c) row (prln a) (prln b) (prln c))
(defmacro let-row (vars row &rest args)
    `(let ,(foreach i (enum-zip vars) (list (cadr i) (list row : (car i))))
       ,@args
       ))

;; dbのクエリ結果をループするのに便利
(defmacro foreach-row (vars rows &rest args)
  (let1 row (gensym)
    `(foreach ,row ,rows (let ,(foreach i (enum-zip vars) (list (cadr i) (list row : (car i))))
			  ,@args
			  ))))

(defmacro foreach-enum* (i x L &rest exprs)
  ;; i index starts from 0
  ;; x item in L
  ;; L list or js-array
  `(let1 ,i -1
     (foreach* ,x ,L
	       (1++ ,i)
	       nil
	       ,@exprs
	       )))

(defmacro foreach-pair (a aL b bL &rest exprs)
  (let1 tmpL (gensym)
    `(let1 ,tmpL ,aL
       (if (not (listp ,tmpL))
	   (throw "foreach-pair: aL must be a list")
	 (foreach* ,b ,bL
	   (let1 ,a (car ,tmpL)
	     (setq ,tmpL (cdr ,tmpL))
	     ,@exprs))))))

(defun js-hash< (alist) (let ((h (js-eval "{}"))) (foreach pair alist (h : (string< (car pair)) (cdr pair))) h))

(defun js-array< (l) (let1 a (js-eval "[]") (foreach i l (a :push i)) a))

(defmacro promise (&rest args)
  ;; (promise expr...)
  ;; inside expr.., (resolve value) or (reject reason) can be used
  `(js-new Promise (wrap (fun (this resolve reject) ,@args))))

;; *** 現在の実装の都合上promiseの返す値valueは２度評価されてしまう
;; よってlispオブジェクトは避けて、javascriptの世界のものを返すように
;; これは直したので問題なくなったような気がする
(defmacro then (p &rest args)
  `(,p :then (wrap (fun (this value) ,@args))))

(defmacro catch (p &rest args)
  `(,p :catch (wrap (fun (this reason) ,@args))))

(defmacro finally (p &rest args)
  `(,p :finally (wrap (fun (this) ,@args))))

(defmacro catch-fainally (finally-exp catch-exp p)
  `(finally
       (catch ,p
	 ,catch-exp)
     ,finally-exp))

(defun table-set (tbl &optional (pretty-print nil))
  (let ((N (length tbl))
	hd
	width-alist
	acc-alist
	2str
	)
    (defun 2str (x)
      (cond ((= js-null x) "#NULL#")
	    ((null x) "nil")
	    (t (string< x))))

    (setq hd (foreach i (car tbl)
	       (car i)))

    (setq width-alist ; ヘッダ文字列長に初期化
      (foreach i hd
	(cons i (sjis-len (2str i)))))

    (setq acc-alist ; 合計リスト初期化
      (foreach i hd
	(cons i 0)))

    (foreach row tbl
      (foreach col row
	(let* ((fs (car col))
	       (fv (cdr col))
	       (fvstr (2str fv))
	       (c1 ((2str fs) :0>))
	       (sz (if (and pretty-print (= "$" c1))
		       (sjis-len (-> (num< fvstr) :toLocaleString))
		     (sjis-len fvstr)))
	       (maxsz (av fs width-alist))
	       )
	  (if (< maxsz sz)
	      (cdr! (assoc fs width-alist) sz)))))

    (prln)
    (foreach i hd
      (let* ((n (av i width-alist))
	     (sz (sjis-len (2str i)))
	     (sps (- n sz))
	     (hdstr (2str i))
	     )
	(pr hdstr)
	(foreach j (range sps)
	  (pr " "))
	(pr "|")))

    (prln)
    (foreach i hd
      (let* ((n (av i width-alist))
	     (c1 (i :0>))
	     (moneyf (and pretty-print (= "$" c1)))
	     (rjustf (and pretty-print (= ">" c1)))
	     )
	(if (or moneyf rjustf)
	    (pr ("-" :repeat (1- n)) ":")
	  (pr ("-" :repeat n)))
	(pr "|"))) ; +
    (prln)
    (foreach row tbl
      (foreach col row
	(let* ((fs (car col))
	       (fv (cdr col))
	       (c1 ((2str fs) :0>))
	       (maxsz (av fs width-alist))
	       (moneyf (and pretty-print (= "$" c1)))
	       (fvstr (if moneyf
			  (-> (num< fv) :toLocaleString)
			(2str fv)))
	       (sz (sjis-len fvstr))
	       (spaces (" " :repeat (- maxsz sz)))
	       )
	  (if moneyf
	      (progn
		(pr spaces fvstr)
		(cdr! (assoc fs acc-alist) (+ (num< fv) (av fs acc-alist)))
		)
	    (pr fvstr spaces))
	  (pr "|")
	  ))
      (prln))
    (if *SQL-STAT* nil (prln))
    (pr (+ "<!-- EOT:" N "件"))
    
    (foreach i acc-alist ; 合計表示
      (let* ((fs (car i))
	     (fv (cdr i))
	     (c1 (fs :0>))
	     (moneyf (and pretty-print (= "$" c1)))
	     )
	(if moneyf (pr " " fs "=" (-> (num< fv) :toLocaleString))) ; 金額のみ合計表示
	))
    (prln " -->")
    ))

(defmacro for (i a b &rest expr)
  `(foreach ,i (range ,a ,b)
     ,@expr))

(defun sha-256 (txt &rest args) ; many times
  (let1 sha (js-safe-eval "new jsSHA(\"SHA-256\",\"TEXT\")")
    (sha :update txt)
    (foreach a args
      (sha :update (string< a)))
    (sha :getHash "HEX")))

(defun append (a b)
  (if (null a) b
    (cons (car a) (append (cdr a) b))))

(defun mapcar (f ll)
  (let1 l (if (js-isArray ll) (foreach i ll i) ll)    
    (if l (cons (f (car l)) (mapcar f (cdr l))))))

(defun filter-out (ll &optional (predf null))
  :desc "リストlをスキャンし、predfがtになるエレメントは除いたlistを返す"
  (let1 l (if (js-isArray ll) (foreach i ll i) ll)    
    (if (null l) nil
      (if (predf (car l)) (filter-out (cdr l) predf)
	(cons (car l) (filter-out (cdr l) predf))))))

(defun findf (ll f)
  :desc "llを走査し、(f x)がnon-nilになったら、ll中のx以降の要素を返す"
  (let1 l (if (js-isArray ll) (foreach i ll i) ll)  
    (if (null l) nil
      (if (f (car l)) l
	(findf (cdr l) f)))))
;;(test (= 2 (car (findf '(1 2 3) (fun (x) (= 2 x))))))
;;(test (= nil (findf '(1 2 3) (fun (x) (= 33 x)))))

(defmacro find (i l &rest exprs)
  `(findf ,l (lambda (,i) ,@exprs)))

(defun grepf (ll f) 
  (let1 l (if (js-isArray ll) (foreach i ll i) ll)
    (if (null l) nil
      (if (f (car l)) (cons (car l) (grepf (cdr l) f))
	(grepf (cdr l) f)))))
;;(test (= "(1 2 3)" (prstr (grepf '(1 2 3 4 5) (fun (x) (<= x 3))))))
;;(test (= nil (grepf '(1 2 3 4 5) (fun (x) (= x 33)))))

(defmacro grep (i l &rest exprs)
  `(grepf ,l (lambda (,i) ,@exprs)))

(defun reg-match (s p)
  :desc "sの中のpにマッチした文字列もしくはnilを返す"
  (let1 m (s :match (js-eval (cat "/" p "/")))
    (if (= js-null m) nil (m :0>))))

(defun alist-guess+ (x)
  (and (listp x) (listp (car x)) (symp (car (car x)))))

(defun mk-struct (x &optional context)
  (if (null context)
      (cond ((null x) (js-safe-eval "[]"))
	    ((and (listp x) (= (length x) 1) (null (car x))) (js-safe-eval "{}"))
	    ((symp x) (js-log x) (throw "symbol can't be included"))
	    ((= (js-typeof x) "undefined") js-null)
	    ((atom x) x)
	    ((alist-guess+ x) (mk-struct x (js-safe-eval "{}")))
	    ((listp x) (mk-struct x (js-safe-eval "[]")))
	    (t (js-log x) (throw "unknown structure"))
	    )
    (cond ((null x) context)
	  ((js-isArray context)
	   (context :push (mk-struct (car x)))
	   (mk-struct (cdr x) context))
	  ((alist-guess+ x)
	   (context (intern (+ ":" (string< (car (car x))) "<")) (mk-struct (cdr (car x))))
	   (mk-struct (cdr x) context))
	  (t (throw "unknown structure"))
	  )))


(defun json-test ()
  (let ((x (js-safe-eval "{}"))
	(header ())
	f-done
	f-fail)
    (x :url< (+ *json-url* "/page0"))
    (x :type< "POST")
    (x :data< (mk-struct `((jsonData . ,(json-string< (mk-struct '("これは" "テスト" "です" ".")))))))
    (x :contentType< "application/JSON")
    (x :dataType< "json")
    (x :scriptCharset< "utf-8")
    (defun f-fail (this)
      (prln "ajax失敗"))
    (defun f-done (this data)
      (foreach-a row data
	(prln)
	(foreach-a col row
	  (pr col ",")))
      (prln))
    ((($ :ajax x) :done (wrap f-done)) :fail (wrap f-fail))))


(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))
(defmacro += (x &rest args) `(setq ,x (funcall + ,x ,@args)))

(defun int-part (x) ; 整数部を返す
  (if (> x 0) (:Math:floor x) (:Math:ceil x)))

(defun frac-part (x) ; 小数部を文字列で返す
  (let1 fraction
      (((string< x) :split ".") :1>)
    (if (=== fraction void0) "0" fraction)))

(defun $0 (id) (($ id) :0>))

(defun jqueryp (obj)
  (js-instanceof obj (js-window :jQuery>)))

(defmacro 1-- (x)
  `(setq ,x (- ,x 1)))

(defmacro 1++ (x)
  `(setq ,x (+ ,x 1)))

(defun format (patt &rest args)
  (let1 n 0
    (foreach arg args
      (setq patt (patt :replace (js-regex (+ "/\\{" n "\\}/g")) arg))
      (1++ n)
      ))
  (if ((js-regex "/\\{\\d+\\}/") :test patt)
      (throw (+ "format: incomplete format: '" patt "'"))
    patt))

(defun reverse (l &optional result)
  (if (null l) result
    (reverse (cdr l) (cons (car l) result))))

(defun triml (str)
  (str :substr 1))

(defun trimr (str)
  (str :slice 0 -1))

(defun contains-nil? (l)
  ;; もしリストlにnilが含まれていればt
  (if (null l) nil
    (if (null (car l)) t
      (contains-nil? (cdr l)))))

(defun zip (&rest args)
  ;; argsに含まれるそれぞれのリストの先頭から1つずつ取って新たなリストを作る.
  ;; 返されるリストは、args中で1番短いリストの長さとなる.
  ;; => (zip '(a b c) '(x y z) '(1 2))
  ;; ((a x 1) (b y 2))
  (if (null args) nil
    (let1 x (mapcar car args)
      (if (contains-nil? x) nil ; run out any list in args?
	(cons x (apply zip (mapcar cdr args)))))))

(defun enum-zip (&rest args)
  ;; zipと同じだが、結果リストの先頭は0から始まる連番を入れる.
  ;; => (enum-zip '(a b c) '(x y z) '(1 2))
  ;; ((0 a x 1) (1 b y 2))
  (let1 r (range (length (car args)))
    (apply zip (cons r args))))

;; *** zipのおかげで、以下のほとんどのpair,zip関数は要らなくなるハズ ***

(defun pair (l) ; 先頭から2つずつlistにする
  (if (null l) nil
    (cons (list (car l) (car (cdr l))) (pair (cdr (cdr l))))))
;; (pair '(1 2 3))
;; => ((1 2) (3 nil))

(defun pair-dot (l) ; 先頭から2つずつdot-pairにする
  (if (null l) nil
    (cons (cons (car l) (car (cdr l))) (pair-dot (cdr (cdr l))))))
;; (pair-dot '(1 2 3))
;; => ((1 . 2) (3))

(defun zip-dot (l1 l2)
  ;; 2つのリストの先頭からdot-pairにする
  ;; できるリストの長さは短い方に合わせる
  (if (and l1 l2)
      (cons (cons (car l1) (car l2)) (zip-dot (cdr l1) (cdr l2)))
    nil))
;; (zip-dot '(1 2 3) '(a b c))
;; => ((1 . a) (2 . b) (3 . c))

(defun enum-dot (l &optional (i 0)) ; 数字とのdot-pairを作る
  (if (null l) nil
    (cons (cons i (car l)) (enum-dot (cdr l) (1+ i)))))
;; (test (= "((0 . a) (1 . b) (2 . c))" (prstr (enum-dot '(a b c)))))
;; (enum-dot '(a b c) 3)

;; enum-dotをjs-arrayでもjqueryオブジェクトでも効くように
(defun enum-dot$ (l1 &optional (i 0))
  (enum-dot (if (jQueryp l1) (foreach i l1 ($ i)) (if (js-isArray l1) (foreach i l1 i) l1)) i))
;;(enum-dot* (js-safe-eval "[1,2,3]"))
;;=> ((0 . 1) (1 . 2) (2 . 3))

;; zip-dotをjs-arrayでもjqueryオブジェクトでも効くように
(defun zip-dot$ (l1 l2)
  (zip-dot (if (jQueryp l1) (foreach i l1 ($ i)) (if (js-isArray l1) (foreach i l1 i) l1))
	   (if (jQueryp l2) (foreach i l2 ($ i)) (if (js-isArray l2) (foreach i l2 i) l2))
	   ))

;; foreachをjqueryオブジェクトでも効くように
(defmacro foreach$ (i l &rest exprs)
  ;;(let1 idx (gensym) ; シンボルテーブルを食いすぎるので
  (let1 idx (intern (+ "#{foreach$}:" i))
    `(foreach ,idx ,l
       (let1 ,i (if (jQueryp ,idx) ,idx ($ ,idx))
	 ,@exprs))))

(defun nth0 (i l) (nth (1+ i) l))
;;(nth0 0 '(a b c))

(defun cadr (l) (car (cdr l)))

(defun caar (l) (car (car l)))

(defun cddr (l) (cdr (cdr l)))

(defun caddr (l) (car (cdr (cdr l))))

(defun cadddr (l) (car (cdr (cdr (cdr l)))))

(defun reduce (f l &optional (initial-value nil p))
  (let1 ll (if p (cons initial-value l) l)
    (if (null (cddr ll)) (apply f ll) ; 引数が2つ以下なら
      (reduce f (cons (f (car ll) (cadr ll)) (cddr ll))))))


;;; ================
;;; DB関連
;;; ================
;; ログインシーケンス
;; login> => ワンタイム値を得る => db-set-hashでpasswdのハッシュ値をセット =>　db-update-hashでワンタイム値を加える
(defun db-connect (uid)
  ;; queryやsqlで使うdb connecitonを返す
  (let2 user-id uid
	hpass "anything will do" ;  ログイン時にはハッシュ値をチェックしない
    (js-new (js-window :DbConn>) user-id hpass)
    ;;(js-new (:_G:DbConn>) user-id hpass)
    ))

;; connにパスワードのハッシュ値をセットする
(defun db-set-hash (conn pass)
  (conn :hashed_pass< (sha-256 pass))
  )

;; ログイン後サーバーが返すone-time-valをハッシュに加える
(defun db-update-hash (conn one-time-val)
  (let1 new-hash (sha-256 (cat (*conn* :hashed_pass>) one-time-val))
    (conn :hashed_pass< new-hash)))

(defun query (conn stmt &rest parms)
  ;; パラメータ付きクエリを非同期実行し、結果のpromiseを返す
  (let ((x (js-safe-eval "{}"))
	f-done
	f-fail
	(user-id (conn :id_str>))
	(hpass (conn :hashed_pass>))
	)
    (x :url< (+ *json-url* "/sql"))
    (x :type< "POST")
    
    (let* ((jstr (json-string< (mk-struct `( ; (user . ,user-id) ; jsonの外で渡すことにした
					    (parms . ,parms)
					    (sql . ,stmt)))))
	   (hash (sha-256 hpass jstr))
	   )
      (x :data< (js-eval "{jsonData:ARGS[1],hash:ARGS[2],user:ARGS[3]}" jstr hash user-id)))
    
    (x :contentType< "application/JSON")
    (x :dataType< "json")
    (x :async< js-true)
    (x :timeout< *json-timeout*) ; ms
    (x :scriptCharset< "utf-8")
    
    (defun f-fail (this)
      (prln "<!-- ajax failed -->")
      (reject "ajax失敗"))
    (defun f-done (this data)
      (prln "<!-- ajax succeeded -->")
      (resolve data))

    ;; connのプロミスを書き換えて、promiseを返す
    ;; ここでのプロミスの挙動: pがプロミスだと仮定する
    ;; (catch p ...) ; pがfailしていなければ渡されたpがそのまま返る
    ;; (then p expr) ; pが成功していれば、新しいプロミスが作られ、そのvalueはexprとなる
    (conn :p<
	  (then (conn :p>)
	    (let1 p (promise ((($ :ajax x) :done (wrap f-done)) :fail (wrap f-fail)))
	      (catch p (if (assoc 'bp-system-alert (env))
			   (bp-system-alert "エラー" (format "{0}: ネットワークの障害が発生した可能性があります.ネットワーク接続、WEBサーバーの状態を確認してください." reason))))
	      )))
    ))

(setq *SQL-STAT* t) ; 統計的な情報を出力するかどうかのスイッチ
(defun sql (conn stmt &rest parms)
  ;; SQL実行例
  ;; (sql conn "select * from table where id=?" 3)
  ;; 文頭に!で全行フェッチ、無い場合は最初の201行のフェッチ
  ;; 結果はpromiseで、thenで受けた時そのvalueはjavascriptの辞書型データで
  ;; sql文の内容によって異なる
  ;; 1) 通常の表が返ってくるsql文では
  ;; {mess: ["OK"], rows: Array, columns: ["id", "code", "shimei", "priority", "seikankbn"], types: ["<class 'int'>", "<class 'str'>", "<class 'str'>", "<class 'int'>", "<class 'int'>"], byte_sizes: [10, 16, 64, 10, 10], …}
  ;; 2) begin、rollbackなど
  ;; {mess: "[BEGIN]"}
  ;; {mess: "[ROLLBACK]"}
  ;; 3) 表の返らないdelete、update、insertなど
  ;; {mess: "[OK]"}
  ;; 4) DBエラーの場合、mess配列にエラーメッセージが入る
  ;; {mess: ["<class 'pyodbc.ProgrammingError'>", "42P01", "[42P01] ERROR: relation \"workerss\" does not exist;…or while executing the query (1) (SQLExecDirectW)"]}
  ;; 5) DB以外のエラーが起こるとconnectionは一旦切断される
  (let (p
	typ-str
	a-list
	b-list
	)
    (defun typ-str (x)
      (cond ((= "<class 'decimal.Decimal'>" x) "DEC")
	    ((= "<class 'datetime.datetime'>" x) "DTTM")
	    ((= "<class 'datetime.date'>" x) "DT")	    
	    ((= "<class 'str'>" x) "STR")
	    ((= "<class 'float'>" x) "FLOAT")
	    ((= "<class 'int'>" x) "INT")
	    (t x)))
    (setq p (apply query conn stmt parms))
    (then p
      (if (=== (value :rows>) void0)
	  (progn
	    (prln (value :mess>))
	    value
	    )
	(setq *ROWS* (value :rows>)) ; 開発用にテンポラリに保存
	(setq a-list (foreach-a row (value :rows>)
		       (for i 0 (row :length>)
			 (cons ((value :columns>) : i) (row : i)))))
	(setq b-list
	  (append
	   (foreach col '(":types>")
	     (cons (cons 'attr/col col)
		   (for i 0 (length (value :columns>))
		     (cons ((value :columns>) : i) (typ-str ((value (intern col)) : i))))))
	   (foreach col '(":byte_sizes>" ":precisions>" ":scales>" ":nullables>")
	     (cons (cons 'attr/col col)
		   (for i 0 (length (value :columns>))
		     (cons ((value :columns>) : i) ((value (intern col)) : i)))))))
	(table-set a-list t)
	
	(if *SQL-STAT*
	    (prln "* ROWS are limited: " (value :limited>))
	  (prln "<div style='page-break-before:always'></div>"))

	(if *SQL-STAT*
	    (let* ((index-row-0
		    (cons (car (car (car b-list))) ":index>"))
		   (index-row-rest
		    (foreach i (enum-dot (cdr (car b-list)))
		      (cons (nth 2 i) (1+ (nth 1 i)))))
		   (index-row (cons index-row-0 index-row-rest))
		   )
	      (table-set (cons index-row b-list))
	      (prln "* Current package " (pwp))))
	value
	))))


(defmacro timeout (msec &rest exprs)
  `((js-window :setTimeout>) (wrap (fun () ,@exprs)) ,msec))

(defmacro delay (ms &rest exprs)
  ;; (delay 1000 (prln "hoi"))
  `(let1 f (fun (this) (resolve (progn ,@exprs)))
     (promise (js-window :setTimeout (wrap f) ,ms))))

;; fがENV:fの形である前提で、(defun f (parms) (w/ ENV:env body))のように
;; 環境付きで評価されるように展開される
(defmacro defun* (f parms &rest body)
  (let1 m ((string< f) :match (js-regex "/^([^:]+):[^:]+$/"))
    (if (= m js-null) (throw (+ "defun*: " (string< f) " not in ENV:" (string< f) " format"))
      `(defun ,f ,parms (w/ ,(intern (+ (m :1>) ":env")) ,@body)))))


(defun chain/pp (o &rest operators)
  ;; (chain/pp dset :rows> :1> :2> :length>)
  ;; js propertyのチェーン評価
  (foreach i operators
    (setq o (o i)))
  o)


(defmacro defun-in-environment (e f params &rest exprs)
  ;; 応用する時の注意
  ;; (with *env* (wrap f)) ; 正
  ;; (wrap (with *env* f)) ; 誤
  (if (assoc f (eval e))
      `(with ,e (defun ,f ,params ,@exprs))
    `(progn
       (setq ,e (cons '(,f) ,e))
       (with ,e (defun ,f ,params ,@exprs)))))

(defmacro setq-in-environment (e f &optional expr)
  (if (assoc f (eval e))
      `(with ,e (setq ,f ,expr))
    `(progn
       (setq ,e (cons '(,f) ,e))
       (with ,e (setq ,f ,expr)))))

(defmacro defun/e (f parms &rest exprs)
  (let1 jsa ((string< f) :split ":")
    (if (/= 2 (length jsa)) (throw (+ "defun/e: wrong function name format: " (string< f)))
      (let ((ee (intern (jsa :0>)))
	    (ff (intern (jsa :1>))))
	`(defun-in-environment ,ee ,ff ,parms ,@exprs)))))

(defmacro setq/e (var &optional val)
  (let1 jsa ((string< var) :split ":")
    (if (/= 2 (length jsa)) (throw (+ "setq//e: wrong symbol name format: " (string< var)))
      (let ((ee (intern (jsa :0>)))
	    (vname (intern (jsa :1>))))
	`(setq-in-environment ,ee ,vname ,val)))))

(defmacro let~ (symL valExprL &rest exprs)
  ;; (macroexpand '(let~ (a b c) '(1 2 3) (* a 2)))
  (append
   (list 'let
	 (let1 vals (eval valExprL)
	   (foreach i symL
	     (let1 x (list i (car vals))
	       (setq vals (cdr vals))
	       x))))
   exprs))


(defmacro setq~ (symL valExprL)
  ;; (macroexpand '(setq~ (a b c) '(1 2 3)))
  ;; 展開後 (progn (setq a 1)... となり
  ;; 1,2,3はそれぞれ評価されてしまうので注意
  (cons 'progn
	(let1 vals (eval valExprL)
	  (foreach i symL
	    (let1 x (list 'setq i (car vals))
	      (setq vals (cdr vals))
	      x)))))


(defmacro w// (aa &rest exprs)
  ;; 環境継承用のw/
  ;; このlispにはクラスの継承は無いが、環境の連鎖で真似できる.
  ;; w/で環境は1つしか書けないが、w//では環境のリストaaを取る.
  ;; aaは通常環境を表すシンボルのリストで各要素は実行時に評価されalistにならなければならない.
  ;; (w// aa exprs...) where aa is in form of (symbol1 symbol2 ...)
  ;; 例:
  ;; (setq A '((a . 1) (b . 2)))
  ;; (setq B '((x . 5) (y . 3)))
  ;; (setq aa '(A B)) ; Bが親でAが子
  ;; (w// aa (* x a)) => 5
  `(w/ (reduce (fun (a b) (append a (eval b))) ,aa nil) ,@exprs))

(defun test (should-be-true)
  (if should-be-true 'test-OK
    (throw "test: failed")))

;; 全角を2, 半角を1として文字数をカウント
(defun sjis-len (ss)
  (let3* s (ss :replace (js-eval "/[−]/g") "x") ; 1スペースのみの非半角文字を半角xに置き換え
	 len (s :length>)
	 zenlen ((s :replace (js-eval "/[\\uFF65-\\uFF9F\\x01-\\x7E\\xA1-\\xDF]/g") "") :length>)
    (+ (* 2 zenlen) (- len zenlen))))
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
;; -*- *websock*:t; -*-

(defun lisp-init ()
  (js-log "lisp-init is called")
  )


