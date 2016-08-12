;;;;
;; Packages
;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; Load and activate emacs packages. This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(exec-path-from-shell

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; python
    virtualenvwrapper
    jedi
    flycheck

    linum-relative
    fill-column-indicator

    evil
    evil-visualstar
    evil-leader

    git-gutter))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; general config
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq column-number-mode t)
(recentf-mode 1)
(global-hl-line-mode 1)

(show-paren-mode 1)
(electric-pair-mode 1)

(require 'fill-column-indicator)
(setq-default fci-rule-column 90)
(setq fci-rule-width 1)
(setq fci-rule-color "grey80")
(add-hook 'after-change-major-mode-hook 'fci-mode)

(require 'linum-relative)
(global-linum-mode)
(linum-relative-toggle)

;; evil mode
(require 'evil)
(evil-mode 1)
(global-evil-visualstar-mode)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; rainbow-delimeters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; python
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "/Users/k/.virtualenvs")
(global-flycheck-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; whitespace mode
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")

(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face spaces tabs space-mark tab-mark))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq-default python-indent 8)
            (setq-default tab-width 4)))

(require 'mouse)
(xterm-mouse-mode t)

(global-git-gutter-mode +1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fill-column-indicator virtualenvwrapper rainbow-delimiters linum-relative jedi flycheck exec-path-from-shell evil-visualstar evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
