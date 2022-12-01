;; -*- *websock*:t; -*-
;; init.l

(setq defmacro
  '(macro (m params &rest body)
     (progn
       (check-fp params)
       (list 'progn (list 'setq m (list 'quote (list 'macro params (if (< 1 (length body)) (cons 'list (cons ''progn body)) (car body)))))
	     (list 'quote m)))))

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

(defmacro for (i a b &rest expr)
  `(foreach ,i (range ,a ,b)
     ,@expr))

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

;; 全角を2, 半角を1として文字数をカウント
(defun sjis-len (ss)
  (let3* s (ss :replace (js-eval "/[−]/g") "x") ; 1スペースのみの非半角文字を半角xに置き換え
	 len (s :length>)
	 zenlen ((s :replace (js-eval "/[\\uFF65-\\uFF9F\\x01-\\x7E\\xA1-\\xDF]/g") "") :length>)
    (+ (* 2 zenlen) (- len zenlen))))
;; -*- *websock*:t; -*-
;; basic.l

(defun int< (x)
  (js-window :parseInt (num< x)))

(defun scroll-to-jq-elem (jq-elem)
  (let1 pos ((jq-elem :offset) :top>)
    (($ "body,html") :animate
     (mk-struct `((scrollTop . ,pos)))
     400 "swing")))

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

(defun promise1 () ; 常に成功し、1を返すpromise
  (promise (resolve 1)))

;; @で始まるシンボルの置き換えマクロ
(defmacro *at-hook-handler* (symbol)
  (let1 nm (string< symbol)
    (cond
     ;; @0, @1, ... @n エラー時の環境
     ((nm :match (js-eval "/^@([0-9]+)$/"))
      (let1 idx (nm :substr 1) (list '*err : idx)))
     ;; デフォルト
     (t (throw (+ "*at-hook-handler*: " nm " is not implemented"))))))

(defun clone-template (jtemp) ; jQuery template elem => cloned jQuery elem
  (($ (js-document :importNode ((jtemp :0>) :content>) js-true)) :children))

(defun $cat (&rest args)
  ($ (apply cat args)))

(defun zen2han (str)
  (:zenkana2Hankana str))
;; -*- *websock*:t; -*-

(defun lisp-init ()
  (js-log "*** lisp-init is called ***")
  )
