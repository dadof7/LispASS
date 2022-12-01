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
