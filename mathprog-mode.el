(defvar mathprog-tab-width 4 "Width of a tab in MATHPROG mode")

(defvar mathprog-mode-hook nil)
(defvar mathprog-basic-offset)

(add-to-list 'auto-mode-alist '("\\.mod'" . mathprog-mode))
(add-to-list 'auto-mode-alist '("\\.dat'" . mathprog-mode))

(defvar mathprog-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?# "<" table) ; start single-line comments
    (modify-syntax-entry ?\n ">" table) ; end single-line comments
    (modify-syntax-entry ?/ "< 1b" table)
    (modify-syntax-entry ?* "< 2b" table)
    (modify-syntax-entry ?* "> 3b" table)
    (modify-syntax-entry ?/ "> 4b" table)
    (modify-syntax-entry ?' "\"" table) ; single-quoted strings
    table)
  "Syntax table for `mathprog-mode'.")

(defvar mathprog-reserved-words
  '(
    "Current"
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
    "sufix"
    "sum"
    "symbolic"
    "table"
    "then"
    "union"
    "until"
    "while"
    "within"
    "subject"
    "to"
    "maximize"
    "minimize"
    "var"
    "param"
    "set"
    ))

(defvar mathprog-arithmetic-functions
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
    ))

(setq mathprog-reserved-words-regexp (regexp-opt mathprog-reserved-words 'words))
(setq mathprog-arithmetic-regexp (regexp-opt mathprog-arithmetic-functions 'words))

(setq mathprog-font-lock-keywords
      `(
        (,mathprog-reserved-words-regexp . font-lock-builtin-face)
        (,mathprog-arithmetic-regexp . font-lock-function-name-face)))

(define-derived-mode mathprog-mode prog-mode "MATHPROG"
  "Major mode for editing MATHPROG code."
  (set-syntax-table mathprog-mode-syntax-table)
  (setq font-lock-defaults '((mathprog-font-lock-keywords)))
  (set (make-local-variable 'mathprog-basic-offset) 4)
  (set (make-local-variable 'comment-start) "# "))

(defalias 'ampl-mode 'mathprog-mode)
(provide 'ampl-mode)
(provide 'mathprog-mode)
