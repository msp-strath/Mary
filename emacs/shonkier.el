;; based on:
;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;; https://github.com/bobatkey/sott/blob/master/emacs/sott.el

;; define several class of keywords
(setq shonkier-keywords  '("import" "as"))
(setq shonkier-operators '("->" "@" ";" ":="))
(setq shonkier-warnings  '("TODO" "FIXME"))

;; create the regex string for each class of keywords
(setq shonkier-keywords-regexp  (regexp-opt shonkier-keywords  'words))
(setq shonkier-operators-regexp (regexp-opt shonkier-operators))
(setq shonkier-warnings-regexp (regexp-opt shonkier-warnings))
(setq shonkier-numeric-regexp   "[[:space:](,|>[{;/]\\([[:digit:]]+\\([\./][[:digit:]]+\\)?\\)")
(setq shonkier-function-def-regexp "^\\([[:alpha:]][[:alnum:]]+\\)[[:space:]]*(")
(setq shonkier-strings-regexp      "\\(\\([[:alpha:]][[:alnum:]]*\\)?\\)\".*?\"\\1")

;; clear memory
(setq shonkier-keywords  nil)
(setq shonkier-operators nil)
(setq shonkier-warnings nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq shonkier-font-lock-keywords
  `(
    (,shonkier-keywords-regexp      . font-lock-keyword-face)
    (,shonkier-operators-regexp     . font-lock-builtin-face)
    ;; for warnings we override pre-existing colours (e.g. comment)
    (,shonkier-warnings-regexp      0 font-lock-warning-face t)
    (,"prim[[:alpha:]]+"            . font-lock-builtin-face)
    (,"'[[:alnum:]]+"               . font-lock-constant-face)
    (,shonkier-numeric-regexp       . (1 font-lock-constant-face))
    (,shonkier-function-def-regexp  . (1 font-lock-function-name-face))
    (,shonkier-strings-regexp       0 font-lock-string-face t)
    ))

;; syntax table
(defvar shonkier-syntax-table nil "Syntax table for `shonkier-mode'.")
(setq shonkier-syntax-table
  (let ((st (make-syntax-table)))

  ;; single line & nesting multiple lines
  (modify-syntax-entry ?/ ". 124b" st)
  (modify-syntax-entry ?* ". 23n" st)
  (modify-syntax-entry ?\n "> b" st)

  ;; strings are highlighted separately
  (modify-syntax-entry ?\" "." st)

  st))

(defvar shonkier-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l"	      'shonkier-interpret)
    (define-key map "\C-c\C-c"	      'shonkier-compile-js)
    map)
  "Keymap for shonkier mode.")

(defun shonkier-interpret ()
  "Run shonkier interpreter"
  (interactive)
  (compile (concat "mary shonkier " buffer-file-name)))

(defun shonkier-compile-js ()
  "Run shonkier to Javascript compiler"
  (interactive)
  (compile (concat "mary shonkierjs " buffer-file-name)))


(easy-menu-define shonkier-mode-menu shonkier-mode-map
  "Menu used when shonkier mode is active."
  '("Shonkier"
    ["Interpret"    shonkier-interpret
     :help "Run shonkier interpreter"]
    ["Compile to JS"    shonkier-compile-js
     :help "Run compiler producing Javascript"]))

;; define the mode
(define-derived-mode shonkier-mode prog-mode
  "SHONKIER mode"
  ;; handling comments
  :syntax-table shonkier-syntax-table
  ;; code for syntax highlighting
  ;;  (setq font-lock-keywords-only t)
  (setq font-lock-defaults '((shonkier-font-lock-keywords)))
  (setq mode-name "shonkier")
  ;; add menu
  (easy-menu-add shonkier-mode-menu)
  ;; clear memory
  (setq shonkier-keywords-regexp nil)
  (setq shonkier-operators-regexp nil)
  (setq shonkier-warnings-regexp nil)
  (setq shonkier-numeric-regexp nil)
)

(provide 'shonkier-mode)
