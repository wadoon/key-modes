;;; key-modes.el -
;;;
;;; Commentary
;;; Author: Alexander Weigl <weigl@kit.edu>

;;; Code:


;;;;
(setq key-mode-hook '())




;;;; Proof Scripts

(defconst KEY_BUFFER_NAME "*KeY*")

(defvar *key-server-buffer* nil)
(defvar *key-server-process* nil)

(defun connect-to-key ()
  (unless *key-server-buffer*
    (setq *key-server-buffer*
	  (get-buffer-create KEY_BUFFER_NAME)))

  (when *key-server-process*
      (delete-process *key-server-process*))

  (setq *key-server-process*
	(open-network-stream "keyserver"
			     *key-server-buffer*
			     "localhost" 61534
			     :type 'plain)))


(defun send-to-key (region)
  (interactive "p")
  (beginning-of-line)
  (push-mark)
  (search-forward ";")

  (let ((command (buffer-substring (region-beginning) (region-end))))
    (process-send-string *key-server-process* (concat command "\n"))))


(defvar key-prove-script-keywords
  '())

(defvar key-prove-script-commands
  '("rule" "auto" "cut" "exit" "instantiate" "macro" "script" "select" "smt" "tryclose" "leave"))

(defvar key-prove-script-macros
  '("autopilot" "autopilot-prep" "split-prop" "upd-simpl" "heap-simpl"))

(defvar key-prove-script-rules
  '("eqSymm" "andLeft" "andRight" "close" "impRight"))

(setq key-prove-script-commands-regex
      (concat "^[ ]*" (regexp-opt key-prove-script-commands :words)))

(setq key-prove-script-macros-regex
      (concat "macro[ ]+\\(" (regexp-opt key-prove-script-macros :words) "\\)" ))

(setq key-prove-script-rules-regex
      (concat (regexp-opt key-prove-script-rules :words)))

(setq key-prove-script-param-regex
      (rx (one-or-more (any "a-zA-Z0-9\"")) "[ ]*=[ ]*" (one-or-more  (any "\"a-zA-Z0-9"))))


(defvar key-prove-script-font-lock-defaults '())

(setq key-prove-script-font-lock-defaults
      `((
	 ("#.*$" . font-lock-comment-face)
	 ;;; regex => font-lock-face
	 (,key-prove-script-param-regex    . font-lock-constant-face)
	 (,key-prove-script-commands-regex 1 font-lock-command-face)
	 (,key-prove-script-macros-regex   1 font-lock-constant-face)
	 (,key-prove-script-rules-regex    . font-lock-keyword-face)
	 )))

(defvar key-prove-script-syntax-table
  )

(define-derived-mode key-prove-script-mode
  fundamental-mode
  "KeY Proof Script"
  ;;"Major Mode for editing Key Proof Scripts files
  ;; See http://key-project.org/proofscripts (not avaiable yet)"

  (bind-key (kbd "C-c C-c") #'send-to-key)


  (setq font-lock-defaults key-prove-script-font-lock-defaults)

  (setq comment-start "#")
  (setq comment-end "")

  ;;(modify-syntax-entry ?# "< b" key-prove-script-syntax-table)
  )

(provide 'key-modes)
;;; key-modes.el ends here
