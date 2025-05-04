(defvar bootstrap-version)
(let ((bootstrap-file
	(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
		(url-retrieve-synchronously
		"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		'silent 'inhibit-cookies)
		(goto-char (point-max))
		(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

;;; ^ Standard straight.el bootstrapping.

(setq package-enable-at-startup nil)
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(load-file (expand-file-name "~/.config/emacs/packages.el" user-emacs-directory))
(load-file (expand-file-name "~/.config/emacs/config.el" user-emacs-directory))

