(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(savehist-mode 1)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(setq column-number-mode t)
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

(add-to-list 'default-frame-alist
	     '(font . "InputMono Thin-12"))


(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)
(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))
(add-to-list 'default-frame-alist '(background-color . "#2e2d2b"))

(use-package exec-path-from-shell
  :ensure t
  :init
 (when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))
 (load (expand-file-name "~/.roswell/helper.el"))
 (load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))
 (setq inferior-lisp-program "ros -Q run"))

(use-package slim-mode
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<down-mouse-1>") nil)
  (setq evil-want-fine-undo t)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>"))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)))

(use-package linum-relative
  :ensure t
  :init
  (setq linum-relative-current-symbol "")
  :config
  (global-linum-mode t)
  (linum-relative-toggle))

(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'ascii)
  :config
  (require 'neotree))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package cl-lib
  :ensure t)

(use-package slime
  :ensure t
  :config
  (slime-setup '(slime-fancy slime-repl slime-fuzzy)))

(use-package smartparens
  :ensure t
  :init
  (add-hook 'lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'slime-repl-mode-hook #'smartparens-mode)
  (show-smartparens-global-mode +1)
  :config
  (require 'smartparens-config)

  (use-package evil-cleverparens
    :ensure t
    :init
    (add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode)
    (setq evil-cleverparens-use-additional-movement-keys nil)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
  :config
  ;;(define-key evil-motion-state-map ":" 'smex)
  (define-key evil-motion-state-map ";" 'evil-ex))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 90)
  (setq fci-rule-width 1)
  (setq fci-rule-color "grey10")
  (add-hook 'after-change-major-mode-hook 'fci-mode))
