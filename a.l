;; -*- *websock*:t; -*-

(defun f (this e)
  (prln "pushed"))

(setq b ($ "button[name='new']"))

(b :on "click" (wrap f))

(defun f (this e)
  (($ this) :css "color" "red"))
