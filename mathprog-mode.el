(defvar mathprog-tab-width 4 "Width of a tab in MATHPROG mode")

(defvar mathprog-mode-hook nil)
(defvar mathprog-basic-offset)

(defvar mathprog-mode-map
  (let ((mathprog-mode-map (make-keymap)))
    (define-key mathprog-mode-map "\C-j" 'newline-and-indent)
    mathprog-mode-map)
  "Keymap for Mathprog/AMPL mode")

(add-to-list 'auto-mode-alist '("\\.mod'" . mathprog-mode))
(add-to-list 'auto-mode-alist '("\\.dat'" . mathprog-mode))

;;; font-lock

(defconst mathprog-declarations
  '("maximize"
    "minimize"
    "var"
    "param"
    "set"
    "subject to")
  "Keywords that can precede a named declaration.")

(defconst mathprog-reserved-words
  '("Current"
    "IN"
    "INOUT"
    "Infinity"
    "Initial"
    "LOCAL"
    "OUT"
    "all"
    "binary"
    "by"
    "check"
    "complements"
    "contains"
    "default"
    "dimen"
    "div"
    "else"
    "environ"
    "exists"
    "forall"
    "if"
    "in"
    "integer"
    "less"
    "logical"
    "max"
    "min"
    "option"
    "setof"
    "shell_exitcode"
    "solve_exitcode"
    "solve_message"
    "solve_result"
    "solve_result_num"
    "subject"
    "sufix"
    "sum"
    "symbolic"
    "table"
    "then"
    "to"
    "union"
    "until"
    "while"
    "within")
  "The list of reserved words in MathProg/AMPL.")

;; A list of built-in arithmetic functions
(defconst mathprog-arithmetic-functions
  '(
    "abs"
    "acos"
    "acosh"
    "alias"
    "asin"
    "asinh"
    "atan"
    "atan2"
    "atanh"
    "ceil"
    "ctime"
    "cos"
    "exp"
    "floor"
    "log"
    "log10"
    "max"
    "min"
    "precision"
    "round"
    "sin"
    "sinh"
    "sqrt"
    "tan"
    "tanh"
    "time"
    "trunc"
    )
  "A list of built-in functions in MathProg/AMPL.")

;; We create optimized regular expressions for the reserved words and
;; arithmetic expressions.
(setq mathprog-declarations-regexp
      (regexp-opt mathprog-declarations 'words))
(setq mathprog-reserved-words-regexp
      (regexp-opt mathprog-reserved-words 'words))
(setq mathprog-arithmetic-regexp
      (regexp-opt mathprog-arithmetic-functions 'words))

(setq mathprog-name-regexp
      "\\<[a-zA-Z_][a-zA-Z_0-9]*")

(defconst mathprog-font-lock-keywords-1
  `(
    (,mathprog-reserved-words-regexp . font-lock-builtin-face)
    (,mathprog-arithmetic-regexp . font-lock-function-name-face))
  "Handles basic keyword highlighting")

(defun mathprog-declaration-pre-match ()
  "Search until the end of a match for a name declaration"
  (save-excursion
    (search-forward-regexp mathprog-name-regexp)))

(defun mathprog-test ()
  (save-excursion (search-forward-regexp "\\(variable,\\)*variable'")))

(defconst mathprog-name-matcher 
  `(,mathprog-declarations-regexp
    (0 font-lock-keyword-face t) 
    (,mathprog-name-regexp 
     (save-excursion
       (search-forward-regexp mathprog-name-regexp))
     nil 
     (0 font-lock-variable-name-face t)))
  "Matches a name declaration.

A name is declared after a var, param, set, minimize, maximize,
or subjec to declaration.

For example,

  var AVariableName;
  param AParameterName;
  set ASetName;
  minimize TheObjective: ...;
  subject to AConstraintName: ...;

")

(defconst mathprog-font-lock-keywords-2
  (append 
   mathprog-font-lock-keywords-1
   (list
    mathprog-name-matcher)))

(defconst mathprog-font-lock-keywords mathprog-font-lock-keywords-2)

;;; Indentation

;; We use the following rules for indentation.
;;
;; 1. If we are at the beginning of the buffer, indent to column 0.
;; 2. If we are inside a set, param, var, objective, or constraint
;;    definition, indent by 2 relative to the parent line
;; 3. If we are inside a brace block, indent to the start of the brace
;;    block
;;

;; We define a block as one of the following.
;;
;; 1. A full expression, optionally terminated by a semicolon
;; 2. A set of expressions inside a pair of curly braces
;; 3. Anything inside a pair of square braces
;;
;; We indent as follows:
;;
;; 1. At the beginning of the buffer we indent to 0.
;; 2. If we are on the second line of a block, we indent relative to
;;   the block.

(defun mathprog-indent-line ()
  "Indent the current line"
  (interactive)
  (let ((indentation-level 0))
    (save-excursion
      (previous-line)
      (end-of-line)
      (setq indentation-level (current-indentation))
      (if (looking-back ":[ ]*$")
          (setq indentation-level
                (+ mathprog-basic-offset indentation-level)))
      (if (looking-back ";[ ]*$")
          (setq indentation-level 0)))
    (save-excursion
      (indent-line-to indentation-level))
    (if (< (current-column) indentation-level)
        (move-to-column indentation-level))))

;;; syntax table

(defvar mathprog-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)     ; Underscores can be in words

    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?# "< b" st)     ; start single-line comments
    (modify-syntax-entry ?\n "> b" st)

    ;; (modify-syntax-entry ?/ ". 124b" st)  ; / begins and ends /*
    ;;                                     ; */-style comments
    ;; (modify-syntax-entry ?\n "> b" st)    ; end single-line comments
    ;; (modify-syntax-entry ?* ". 23" st) ; * is the second and first
    ;;                                     ; character in /* */-style
    ;;                                     ; comment delimters
    (modify-syntax-entry ?' "\"" st)    ; single-quoted strings
    st)
  "Syntax table for `mathprog-mode'.")

(define-derived-mode mathprog-mode prog-mode "MathProg"
  "Major mode for editing GNU MathProg or AMPL code."
  (set-syntax-table mathprog-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '((mathprog-font-lock-keywords)))
  (set (make-local-variable 'mathprog-basic-offset) 4)
  (set (make-local-variable 'indent-line-function) 'mathprog-indent-line)
  ;(set (make-local-variable 'comment-start) "# "))
  )

(defalias 'ampl-mode 'mathprog-mode)
(provide 'ampl-mode)
(provide 'mathprog-mode)
