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

    smartparens

    auto-complete

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; python
    virtualenvwrapper
    jedi
    flycheck

    ;; clojure
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    ac-cider
    clj-refactor

    ;; ruby
    enh-ruby-mode
    robe

    ;; html
    tagedit

    linum-relative
    fill-column-indicator

    evil
    evil-visualstar
    evil-leader
    evil-surround

    smex

    git-gutter

    ag))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; general config
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq column-number-mode t)
(recentf-mode 1)
(global-hl-line-mode 1)

(setq require-final-newline t)

(show-paren-mode 1)
(electric-pair-mode 1)

(require 'fill-column-indicator)
(setq-default fci-rule-column 90)
(setq fci-rule-width 1)
(setq fci-rule-color "grey10")
(add-hook 'after-change-major-mode-hook 'fci-mode)

(require 'linum-relative)
(global-linum-mode)
(linum-relative-toggle)

(advice-add 'linum-relative :filter-return
            (lambda (num)
              (if (not (get-text-property 0 'invisible num))
                  (propertize (replace-regexp-in-string " " "\u2002" num)
                              'face (get-text-property 0 'face num)))))
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

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; whitespace mode
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray10")

(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face spaces tabs space-mark tab-mark))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq-default python-indent 4)
            (setq-default tab-width 4)))

(require 'mouse)
(xterm-mouse-mode t)

(global-git-gutter-mode +1)

(require 'ag)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(ac-config-default)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(require 'smartparens-config)
(add-hook 'clojure-mode #'smartparens-mode)

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(require 'evil-surround)
(global-evil-surround-mode 1)

(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
