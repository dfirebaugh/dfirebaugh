(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(setq ring-bell-function 'ignore)

(setq catppuccin-flavor 'frappe) ; or 'latte, 'macchiato, or 'mocha
(load-theme 'catppuccin t)

(setq org-directory (expand-file-name "~/org"))
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

(set-face-attribute 'default nil :font "ComicShannsMono Nerd Font" :height 180)

(setq whitespace-style
	'(face
		spaces      ; highlight regular spaces
		space-mark  ; show a symbol for each space
		tabs        ; highlight tabs
		tab-mark    ; show a symbol for each tab
		newline     ; highlight newlines
		newline-mark)) ; show a symbol for end‑of‑line

(setq whitespace-display-mappings
	'(
		;; SPACE (ASCII 32) -> “·”
		(space-mark   ?\   [?·] [?.])
		;; NEWLINE (ASCII 10) -> “↴\uf477” (followed by actual newline)
		(newline-mark ?\n   [?\uF149 ?\n])
		;; TAB (ASCII 9) -> “»”
		(tab-mark     ?\t  [?» ?\t])))

(add-hook 'prog-mode-hook
	(lambda ()
		(tree-sitter-mode)
		(tree-sitter-hl-mode)))

(add-hook 'prog-mode-hook #'whitespace-mode)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Enable LSP for supported languages
(dolist (hook '(go-mode-hook
	lua-mode-hook
	js-mode-hook
	typescript-mode-hook
	sh-mode-hook
	python-mode-hook
	markdown-mode-hook
	c-mode-hook
	c++-mode-hook))

(add-hook hook #'lsp-deferred))

(with-eval-after-load 'lsp-mode
	(setq lsp-headerline-breadcrumb-enable t
		lsp-enable-snippet t
		lsp-enable-symbol-highlighting t
		lsp-enable-indentation nil))

(with-eval-after-load 'evil
	(evil-define-key 'normal prog-mode-map
		(kbd "K") 'lsp-ui-doc-show))

(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; make ";" open the ":" prompt
(general-define-key
	:states '(normal visual operator)
	";" 'evil-ex)

;; Evil leader key setup
(general-create-definer my/leader-keys
	:keymaps '(normal insert visual emacs)
	:prefix "SPC"
	:global-prefix "C-SPC")

(my/leader-keys
	"/" '(comment-line :which-key "comment line")
	"x" '(kill-current-buffer :which-key "kill buffer")
	"f" '(:ignore t :which-key "files")
	"ff" '(find-file :which-key "find file")
	"fs" '(save-buffer :which-key "save file")
	"fr" '(recentf-open-files :which-key "recent files")
	"fw" '(ripgrep-regexp :which-key "ripgrep in dir")
	"p" '(:ignore t :which-key "project")
	"pa" '(projectile-add-known-project :which-key "add project")
	"pp" '(projectile-switch-project :which-key "switch project")
	"pf" '(projectile-find-file :which-key "find file in project")
	"pb" '(projectile-switch-to-buffer :which-key "switch project buffer")
	"ps" '(projectile-ripgrep :which-key "search in project")
	"pt" '(projectile-run-project :which-key "run project")
	"b" '(:ignore t :which-key "buffers")
	"bb" '(switch-to-buffer :which-key "switch buffer")
	"bi" '(ibuffer :which-key "ibuffer")
	"bk" '(kill-current-buffer :which-key "kill buffer")
	"bn" '(next-buffer :which-key "next buffer")
	"bp" '(previous-buffer :which-key "previous buffer")
	"g" '(:ignore t :which-key "git")
	"gs" '(magit-status :which-key "status")
	"gd" '(magit-diff :which-key "diff")
	"gl" '(magit-log-buffer-file :which-key "log current file")
	"h"  '(:ignore t :which-key "help")
	"hf" '(describe-function :which-key "describe function")
	"hv" '(describe-variable :which-key "describe variable")
	"hk" '(describe-key      :which-key "describe key")
	"hm" '(describe-mode     :which-key "describe mode")
	"t" '(:ignore t :which-key "toggle")
	"tt" '(load-theme :which-key "choose theme")
	"tn" '(display-line-numbers-mode :which-key "line numbers")
	"oA" '(org-agenda :which-key "org agenda")
	"op" '(treemacs :which-key "toggle neotree")
	"ot" '(vterm :which-key "vterm")
	"qq" '(save-buffers-kill-terminal :which-key "quit emacs"))
