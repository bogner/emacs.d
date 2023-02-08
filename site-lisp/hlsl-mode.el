;;; hlsl-mode.el --- HLSL Mode using CC Mode

(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'hlsl-mode 'c++-mode))

(c-lang-defconst c-primitive-type-kwds
  hlsl (append '("float2" "float3" "float4")))

(c-lang-defconst c-modifier-kwds
  hlsl (cons "precise" (c-lang-const c-modifier-kwds)))

(defcustom hlsl-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in HLSL mode.")

(defconst hlsl-font-lock-keywords-1 (c-lang-const c-matchers-1 hlsl)
  "Minimal highlighting for HLSL mode.")

(defconst hlsl-font-lock-keywords-2 (c-lang-const c-matchers-2 hlsl)
  "Fast normal highlighting for HLSL mode.")

(defconst hlsl-font-lock-keywords-3 (c-lang-const c-matchers-3 hlsl)
  "Accurate normal highlighting for HLSL mode.")

(defvar hlsl-font-lock-keywords hlsl-font-lock-keywords-3
  "Default expressions to highlight in HLSL mode.")

(defvar hlsl-mode-syntax-table nil
  "Syntax table used in hlsl-mode buffers.")
(or hlsl-mode-syntax-table
    (setq hlsl-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table hlsl))))

; TODO: Can we just derive this from C++?
(defvar hlsl-mode-abbrev-table nil
  "Abbreviation table used in hlsl-mode buffers.")
(c-define-abbrev-table 'hlsl-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar hlsl-mode-map (let ((map (c-make-inherited-keymap)))
		      map)
  "Keymap used in hlsl-mode buffers.")

(easy-menu-define hlsl-menu hlsl-mode-map "HLSL Mode Commands"
  (cons "HLSL" (c-lang-const c-mode-menu hlsl)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . hlsl-mode))

;;;###autoload
(defun hlsl-mode ()
  "Major mode for editing HLSL code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `hlsl-mode-hook'.

Key bindings:
\\{hlsl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table hlsl-mode-syntax-table)
  (setq major-mode 'hlsl-mode
	mode-name "HLSL"
	local-abbrev-table hlsl-mode-abbrev-table
	abbrev-mode t)
  (use-local-map hlsl-mode-map)
  (c-init-language-vars hlsl-mode)
  (c-common-init 'hlsl-mode)
  (easy-menu-add hlsl-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'hlsl-mode-hook)
  (c-update-modeline))

(provide 'hlsl-mode)
