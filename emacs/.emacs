(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; install use-package if not yet installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :init (evil-mode 1))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))

(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn))

(use-package company :ensure t)

(use-package racer :ensure t)

(use-package org)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "kramdown"))

(use-package mmm-mode
  :ensure t)

;; gui
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; when "font" is available
(when (member "Ubuntu Mono" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-13"))
  (set-face-attribute 'default t :font "Ubuntu Mono-13"))

;; utf8 everywhere
(set-locale-environment "en_US.UTF-8")
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (progn
    (set-selection-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8-mac)))
(prefer-coding-system 'utf-8)

(setq company-minimum-prefix-length 1)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(mmm-add-classes
 '((markdown-rust
    :submode rust-mode
    :front "^```rust[\n\r]+"
    :back "^```$")))

(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-rust)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
