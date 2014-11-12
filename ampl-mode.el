(defvar ampl-tab-width 4 "Width of a tab in AMPL mode")

(defvar ampl-mode-hook nil)
(defvar ampl-basic-offset)

(add-to-list 'auto-mode-alist '("\\.mod'" . ampl-mode))

(defvar ampl-mode-syntax-table
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
  "Syntax table for `ampl-mode'.")

(defvar ampl-reserved-words
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

(defvar ampl-arithmetic-functions
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

(setq ampl-reserved-words-regexp (regexp-opt ampl-reserved-words 'words))
(setq ampl-arithmetic-regexp (regexp-opt ampl-arithmetic-functions 'words))

(setq ampl-font-lock-keywords
      `(
        (,ampl-reserved-words-regexp . font-lock-builtin-face)
        (,ampl-arithmetic-regexp . font-lock-function-name-face)))

(define-derived-mode ampl-mode prog-mode "AMPL"
  "Major mode for editing AMPL code"
  (set-syntax-table ampl-mode-syntax-table)
  (setq font-lock-defaults '((ampl-font-lock-keywords)))
  (set (make-local-variable 'ampl-basic-offset) 4)
  (set (make-local-variable 'comment-start) "# "))

(provide 'ampl-mode)
