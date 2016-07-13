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
  '(;; colorful parenthesis matching
    rainbow-delimiters

    ;; python
    anaconda-mode

    linum-relative

    evil
    evil-visualstar
    evil-leader))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)
(menu-bar-mode -1)

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
(add-hook 'python-mode-hook 'anaconda-mode)

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
(defun track-mouse (e))
(setq mouse-sel-mode)
