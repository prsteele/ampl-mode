(defvar ampl-tab-width 4 "Width of a tab in AMPL mode")

(defvar ampl-mode-hook nil)
(defvar ampl-basic-offset)

(defvar ampl-mode-map
  (let ((ampl-mode-map (make-keymap)))
    (define-key ampl-mode-map "\C-j" 'newline-and-indent)
    ampl-mode-map)
  "Keymap for Mathprog/AMPL mode")

(add-to-list 'auto-mode-alist '("\\.mod'" . ampl-mode))
(add-to-list 'auto-mode-alist '("\\.dat'" . ampl-mode))

;;; font-lock

(defconst ampl-declarations
  '("maximize"
    "minimize"
    "var"
    "param"
    "set"
    "subject to")
  "Keywords that can precede a named declaration.")

(defconst ampl-reserved-words
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
(defconst ampl-arithmetic-functions
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
(setq ampl-declarations-regexp
      (regexp-opt ampl-declarations 'words))
(setq ampl-reserved-words-regexp
      (regexp-opt ampl-reserved-words 'words))
(setq ampl-arithmetic-regexp
      (regexp-opt ampl-arithmetic-functions 'words))

(setq ampl-name-regexp
      "\\<[a-zA-Z_][a-zA-Z_0-9]*")

(defconst ampl-font-lock-keywords-1
  `(
    (,ampl-reserved-words-regexp . font-lock-builtin-face)
    (,ampl-arithmetic-regexp . font-lock-function-name-face))
  "Handles basic keyword highlighting")

(defun ampl-declaration-pre-match ()
  "Search until the end of a match for a name declaration"
  (save-excursion
    (search-forward-regexp ampl-name-regexp)))

(defun ampl-test ()
  (save-excursion (search-forward-regexp "\\(variable,\\)*variable'")))

(defconst ampl-name-matcher 
  `(,ampl-declarations-regexp
    (0 font-lock-keyword-face t) 
    (,ampl-name-regexp 
     (save-excursion
       (search-forward-regexp ampl-name-regexp))
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

(defconst ampl-font-lock-keywords-2
  (append 
   ampl-font-lock-keywords-1
   (list
    ampl-name-matcher)))

(defconst ampl-font-lock-keywords ampl-font-lock-keywords-2)

;;; Indentation

;; For now we use the following simple rules for indentation.
;;
;; 1. The beginning of the buffer is indented to zero.
;; 2. Lines following a line ending in a colon are indented relative
;;    to that line.
;; 3. Lines following a semicolon are not indented.

(defun ampl-indent-line ()
  "Indent the current line"
  (interactive)
  (let ((indentation-level 0))
    (save-excursion
      (previous-line)
      (end-of-line)
      (setq indentation-level (current-indentation))
      (if (looking-back ":[ ]*$")
          (setq indentation-level
                (+ ampl-basic-offset indentation-level)))
      (if (looking-back ";[ ]*$")
          (setq indentation-level 0)))
    (save-excursion
      (indent-line-to indentation-level))
    (if (< (current-column) indentation-level)
        (move-to-column indentation-level))))

;;; syntax table

(defvar ampl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)    ; Underscores can be in words
    (modify-syntax-entry ?/ ". 14" st) ; Multi-line comments (the / char)
    (modify-syntax-entry ?* ". 23" st) ; Mult-line comments (the * char)
    (modify-syntax-entry ?# "< b" st)  ; Start single-line comments
    (modify-syntax-entry ?\n "> b" st) ; End single-line comments
    (modify-syntax-entry ?' "\"" st)   ; Single-quoted strings
    st)
  "Syntax table for `ampl-mode'.")

(define-derived-mode ampl-mode prog-mode "Ampl"
  "Major mode for editing GNU MathProg or AMPL code."
  (set-syntax-table ampl-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '((ampl-font-lock-keywords)))
  (set (make-local-variable 'ampl-basic-offset) 4)
  (set (make-local-variable 'indent-line-function) 'ampl-indent-line)
  ;(set (make-local-variable 'comment-start) "# "))
  )

(defalias 'mathprog-mode 'ampl-mode)
(provide 'ampl-mode)
(provide 'mathprog-mode)
