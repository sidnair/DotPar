;; A little mode for editing all our dotpar files

(defvar dotpar-mode-hook nil)

;; might not need this?
(defvar dotpar-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for dotpar major mode")

;; autoload
(add-to-list 'auto-mode-alist '("\\.par\\'" . dotpar-mode))

;; highlighting
(defconst tmp-dotpar-keyword-regex
  (regex-opt
   '("func", "number", "char", "boolean", "void", "nil",
     "if", "else", "elif", "for", "in", "return", "import")
   t))

(defconst dotpar-font-lock-keywords-1
  (list
   '(tmp-dotpar-keyword-regex . font-lock-builtin-face)
   '("\\(\\w*\\)" . font-lock-variable-name-face)
   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face))
  "Highlighting for dotpar")

(defvar dotpar-font-lock-keywords dotpar-font-lock-keywords-1
  "Default highlighting expressions for dotpar mode")

;; 


;; the actual kicker
(defun dotpar-mode ()
  "Major mode for editing dotpar files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dotpar-mode-syntax-table)
  (use-local-map dotpar-mode-map)

(provide 'dotpar-mode)
