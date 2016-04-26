(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(global-evil-visualstar-mode)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(add-to-list 'load-path "~/code/emacs/slime")
(require 'slime-autoloads)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-asdf slime-indentation))
(slime-setup)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(setq show-paren-delay 0)
(setq ring-bell-function 'ignore)
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(require 'whitespace)
(global-whitespace-mode 1)
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
(setq-default indent-tabs-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tsdh-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
