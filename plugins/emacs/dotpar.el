;; A little major mode for dotpar

(require 'cc-mode)

;; grab some language constants
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; make dotpar visible to the language constant system
  ;; and fallback to normal c mode if things get hairy
  (c-add-language 'dotpar-mode 'c-mode))

;; change the keywords
(c-lang-defconst
 c-primitive-type-kwds
 dotpar (append '("number")
                (delete '("int" "float" "double")
                        (append
                         (c-lang-const c-primitive-type-kwds)
                         nil))))

;; modifier closest we can get for starting functions with func
(c-lang-defconst
 c-modifier-kwds
 dotpar (cons "func" (c-lang-const c-modifier-kwds)))

(c-lang-defconst
 c-constant-kwds
 dotpar (append '("true" "false")
                (c-lang-const c-constant-kwds)))

(c-lang-defconst
 c-simple-stmt-kwds
 dotpar (append '("println" "each" "map" "reduce")
                (c-lang-const c-simple-stmt-kwds)))


(defgroup dotpar nil "Dotpar mode customizations")

;; extra highlighting stuff
(defcustom dotpar-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in
dotpar mode. Each list item should be a regexp matching a single identifier.")

(defconst dotpar-font-lock-keywords-1 (c-lang-const c-matchers-1 dotpar)
  "Minimal highlighting for dotpar mode.")

(defconst dotpar-font-lock-keywords-2 (c-lang-const c-matchers-2 dotpar)
  "Fast normal highlighting for dotpar mode.")

(defconst dotpar-font-lock-keywords-3 (c-lang-const c-matchers-3 dotpar)
  "Accurate normal highlighting for dotpar mode.")

(defvar dotpar-font-lock-keywords dotpar-font-lock-keywords-3
  "Default expressions to highlight in dotpar mode.")

(defvar dotpar-mode-syntax-table nil
  "Syntax table used in dotpar-mode buffers.")
(or dotpar-mode-syntax-table
    (setq dotpar-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table dotpar))))

(defvar dotpar-mode-map (let ((map (c-make-inherited-keymap)))
		      ;; Add bindings which are only useful for dotpar
		      map)
  "Keymap used in dotpar-mode buffers.")

(defvar dotpar-mode-abbrev-table nil
  "Abbreviation table used in dotpar-mode buffers.")

(c-define-abbrev-table 'dotpar-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)))

(easy-menu-define dotpar-menu dotpar-mode-map "dotpar Mode Commands"
		  ;; Can use `dotpar' as the language for `c-mode-menu'
		  ;; since its definition covers any language.  In
		  ;; this case the language is used to adapt to the
		  ;; nonexistence of a cpp pass and thus removing some
		  ;; irrelevant menu alternatives.
		  (cons "dotpar" (c-lang-const c-mode-menu dotpar)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.par\\'" . dotpar-mode))

(defun dotpar-mode ()
  "Major mode for editing dotpar code

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `dotpar-mode-hook'.

Key bindings:
\\{dotpar-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table dotpar-mode-syntax-table)
  (setq major-mode 'dotpar-mode
	mode-name "dotpar"
	local-abbrev-table dotpar-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars dotpar-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'dotpar-mode)
  (easy-menu-add dotpar-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'dotpar-mode-hook)
  (c-update-modeline)
  ;; and some of my own secret sauce
  (setq c-basic-offset 4)
  )

(provide 'dotpar-mode)
