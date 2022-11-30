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
