;;; jml-mode --- Minor Mode for editing JML

;;; Summary:

;;; Commentary:

;;; Code:

(defface jml-basic-comment-face
  '((t :background "darkseagreen2")
       :inherit font-lock-comment-face)
  ""
  :group 'jml-faces)



(defface jml-comment-delimiter-face
  '((t :background "gray"
       :inherit jml-basic-comment-face))
  ""
  :group 'jml-faces)


(defface jml-jml-keyword-face
  '((t :background "yellow"
       :inherit (jml-basic-comment-face font-lock-keyword-face)))
  ""
  :group 'jml-faces)

(defface jml-key-keyword-face
  '((t :background "purple"
       :inherit (jml-basic-comment-face font-lock-keyword-face)))
  ""
  :group 'jml-faces)


(defvar jml-basic-comment-face 'jml-basic-comment-face)
(defvar jml-key-keyword-face 'jml-key-keyword-face)
(defvar jml-jml-keyword-face 'jml-jml-keyword-face)
(defvar jml-comment-delimiter-face  'jml-comment-delimiter-face)


(define-minor-mode  jml-mode
  "Editing for JML-Annotations TODO"
  nil   ;;init value
  "JML" ;; lighter
  ()    ;; keymap

  (if jml-mode
      (jml--add-font-lock-keywords)
    (jml--remove-font-lock-keywords))

  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(defvar jml--java-keywords
  '("boolean" "byte"  "false" "instanceof" "int" "long" "new" "null" "short" "super" "this" "true" "void" ))


(setq jml--jml-keywords
      (sort '("accessible" "assignable" "breaks" "continues"
	      "decreases" "depends" "ensures" "ensures_free"
	      "loop_determines" "non_null"
	      "nullable" "represents" "requires" "requires_free"
	      "signals" "signals_only" "\\bigint" "\\duration"
	      "\\elemtype" "\\empty"
	      "\\everything" "\\exception" "\\exists" "\\forall" "\\fresh"
	      "\\index" "\\into" "\\invariant_for" "\\is_initialized" "\\lblneg"
	      "\\lblpos" "\\lockset" "\\max" "\\measured_by" "\\min"
	      "\\nonnullelements" "\\nothing" "\\not_assigned" "\\not_modified"
	      "\\not_specified" "\\num_of" "\\old" "\\permission" "\\pre"
	      "\\product" "\\reach" "\\real" "\\result" "\\same" "\\space"
	      "\\subset" "\\such_that" "\\sum" "\\TYPE" "\\typeof" "\\type"
	      "\\values" "\\working_space")  #'string<))

;; Bad keyword "loop_separates"  //KeY extension, deprecated"

(setq jml--key-keywords
      (sort '("\\all_fields"
	      "\\all_objects" "\\backup" "\\bsum" "\\by" "\\declassifies" "\\disjoint" "\\domain_implies_created" "determines"
	      "model_method_axiom" "\\erases" "\\free" "\\seq_indexOf" "\\intersect" "\\inv" "\\in_domain" "\\is_finite" "\\itself" "\\locset"
	      "\\map" "\\map_empty"
	      "\\map_get" "\\map_override"
	      "\\map_remove"
	      "\\map_singleton"
	      "\\map_size"
	      "\\map_update"
	      "\\new_elems_fresh"
	      "\\new_objects" "\\reachLocs" "\\seq" "\\seq_2_map"
	      "\\seq_concat" "\\seq_def" "\\seq_empty"
	      "\\seq_get" "\\seq_put" "\\seq_reverse"
	      "\\seq_singleton" "\\seq_sub" "\\set_minus"
	      "\\singleton" "\\static_invariant_for" "\\strictly_nothing"
	      "\\string_equal" "returns"
	      "join_proc" "separates" "\\set_union" "\\infinite_union"
	      "\\transactionUpdated" "\\transient") #'string<))

(setq jml--single-comment-start "//@[ ]*")

(setq jml-font-lock-keywords
      (list
       ;;'("/\\*@\\|@\\|@\\*/" . jml-comment-delimiter-face)
       ;; JML in single comment
       `( "//@" (0 jml-comment-delimiter-face t)
	  (,(regexp-opt jml--jml-keywords t)
	   nil nil
	   (1 jml-jml-keyword-face t)))

       ;; KeY in single comment
       `( "//@" (0 jml-comment-delimiter-face)
	  (,(regexp-opt jml--key-keywords t)
	   nil nil
	   (1 jml-key-keyword-face t)))

       ;; KeY in single comment
       `( "/\\*@" (0 jml-comment-delimiter-face)
	  (,(regexp-opt jml--key-keywords t)
	   (save-excursion (search-forward-regexp "@\\*/")) ;; bad style
	   nil
	   (1 jml-key-keyword-face t)))))


(defun within-jml-comment ()
  (interactive)
  (save-excursion
    (let ((p (point))
	  (prev_open (search-backward-regexp "/\\*@" (point-min) t))
	  next_close)
      (when prev_open
	(setq next_close (search-forward-regexp "@\\*/" p t))
	(not next_close)))))

(defun jml-hl-keywords (limit)
  (save-excursion))


(defun jml--add-font-lock-keywords ()
  "Add extra font-lock keywords to JAVA."

;;  (when (local-variable-p 'lisp-extra-font-lock--installed-keywords)
;;    (font-lock-remove-keywords nil lisp-extra-font-lock--installed-keywords))
;;  (let ((keywords (jml-font-lock-keywords)))
    ;(set (make-local-variable 'lisp-extra-font-lock--installed-keywords)
					;     keywords)
;;  (modify-syntax-entry "\\</\\*@\\>" ">")
  (font-lock-add-keywords nil jml-font-lock-keywords 'append))

(defun jml--remove-font-lock-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil jml-font-lock-keywords))


(defun fk (limit)
  (save-match-data

  (match-start)
  (match-end)

  (set-match-data)
  )

(provide 'jml-mode)
;;; jml-mode.el ends here
