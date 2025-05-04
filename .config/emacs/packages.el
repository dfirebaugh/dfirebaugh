(use-package evil
	:init
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)
	:config
	(evil-mode 1)
	;; Now safe to define keys
	(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
	(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down))


(use-package evil-collection
	:after evil
	:config
	(evil-collection-init))

(use-package general)
(use-package which-key
	:config (which-key-mode))

(use-package vertico
	:init (vertico-mode))
(use-package orderless
	:custom (completion-styles '(orderless)))
(use-package marginalia
	:init (marginalia-mode))

(use-package corfu
	:init (global-corfu-mode))

(use-package cape)

(use-package doom-themes
	:config (load-theme 'doom-one t))
(use-package doom-modeline
	:init (doom-modeline-mode 1))

(use-package catppuccin-theme)

(use-package hl-todo
	:hook (prog-mode . hl-todo-mode))
(use-package yasnippet
	:config (yas-global-mode 1))

(use-package magit)
(use-package projectile
	:config (projectile-mode))

(use-package treemacs)
(use-package neotree)
(use-package vterm)
(use-package undo-fu)

(use-package flycheck)
(use-package spell-fu)

(use-package lsp-mode)
(use-package lsp-ui)

(use-package go-mode
	:defer t
	:config
	(add-hook 'go-mode-hook 'lsp-deferred)
	(add-hook 'go-mode-hooik
	(lambda ()
	(add-hook 'before--save-hook 'ime-go-before-save))))

(use-package lua-mode)
(use-package markdown-mode)
(use-package js)

(use-package org)

;; Folding support
(use-package hideshow
	:hook ((prog-mode . hs-minor-mode)))

(use-package ripgrep)
(use-package rg)

(use-package diff-hl
	:ensure t
	:hook ((prog-mode . diff-hl-mode)          ; enable in programming buffers
	(text-mode . diff-hl-mode)          ; optionally in text buffers
	(magit-post-refresh . diff-hl-magit-post-refresh)) ; refresh after magit
	:config

;; highlight changes immediately as you type
(diff-hl-flydiff-mode)
	;; if you prefer margin glyphs instead of fringe bitmaps:
	(diff-hl-margin-mode)
)

(use-package org-bullets
	:straight t
	:after org
	:hook (org-mode . org-bullets-mode))
