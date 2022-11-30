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
