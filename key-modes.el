;;; key-modes.el -
;;;
;;; Commentary
;;; Author: Alexander Weigl <weigl@kit.edu>

;;; Code:


;;;;
(setq key-mode-hook '())




;;;; Proof Scripts

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

  (setq font-lock-defaults key-prove-script-font-lock-defaults)

  (setq comment-start "#")
  (setq comment-end "")

  ;;(modify-syntax-entry ?# "< b" key-prove-script-syntax-table)
  )

(provide 'key-modes)
;;; key-modes.el ends here
