;; get the custom-config stuff to go somewhere else, probably don't really want
;; to use this very often
;; done before anything else since lots of things apparently want to write to
;; this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; emacs lisp programming packages
(use-package ht)
(use-package mustache)

(use-package base16-theme
  :init
  (setq base16-theme-256-color-source "base16-shell")
  :config
  (load-theme 'base16-tomorrow-night) ;; FIXME get this from base16-shell

  ;; make line numbers not look terrible
  (set-face-inverse-video 'line-number-current-line nil)
  (set-face-foreground 'line-number-current-line "orange")
  (set-face-background 'line-number-current-line "color-18")

  ;; and the comment delimiters, because for some reason the default
  ;; is to make these invisible
  (set-face-foreground 'font-lock-comment-delimiter-face "brightblack"))

;; FIXME take better advantage of this
(use-package which-key
  :config
  (which-key-mode))

;; almost everything needs to load after evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)   ;; I want this, what do I lose?
  (setq evil-want-C-i-jump nil)   ;; disable C-i jump so I can use TAB key for things
  (setq evil-want-keybinding nil) ;; docs for evil-collection say to do this
  (setq evil-toggle-key "M-SPC")
  :config

  ;; bind the jumping around keys to meta
  ;; since I want to be able to use tab in a terminal
  (define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)

  ;; FIXME S-TAB still doesnt work in terminals, but this is to be
  ;; expected most likely

  (evil-mode))

;; FIXME actually learn to use this better
(use-package evil-org
  :after (org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-magit)

;; highlights whatever the last vim action did
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode))

;; FIXME fix the comment syntax
(use-package evil-commentary
  :after evil
  :demand t
  :config
  (evil-commentary-mode))

;; don't really like this
(use-package projectile)

(use-package magit)

(use-package helm
  :demand t
  :config
  (helm-mode))

(use-package helm-projectile
  :after (helm projectile))

;; (use-package ace-window)

(use-package hydra
  :demand t
  :config
  (defhydra hydra-resize ()
    ("j" evil-window-decrease-height)
    ("k" evil-window-increase-height)
    ("h" evil-window-decrease-width) ;; this is a little wonky
    ("l" evil-window-increase-width)))

(use-package general
  :after (evil hydra)
  :config
  (general-evil-setup) ;; make it easier to write mappings that look like vim

  ;; map jk to escape
  (general-imap "j" (general-key-dispatch 'self-insert-command
		     :timeout 0.25
		     "k" 'evil-normal-state))

  ;; FIXME as usual, learn my own bindings
  (general-define-key
   :states '(normal visual emacs) ;; FIXME these don't seem to kick in everywhere
   :prefix "SPC"
   "TAB" '((lambda () (interactive) (mode-line-other-buffer)) ;; FIXME this doens't actually work
           :which-key "Switch to previous buffer")

   ;; not exactly the same, but close enough
   "SPC w" '(evil-avy-goto-word-0
             :which-key "AVY word")

   "SPC f" '(evil-avy-goto-char
             :which-key "AVY char")

   "SPC j" '(evil-avy-goto-line
             :which-key "AVY line")

   "RET" '(align
           :which-key "Align stuff DWIW")

   ;; 'b' prefix for buffer commands
   "bb" '(helm-projectile-switch-to-buffer
         :which-key "Switch to another open buffer in project")
   "bB" '(helm-buffers-list
         :which-key "Switch to another open buffer")
   "bd" '(evil-delete-buffer
          :which-key "Delete buffer")
   "bw" '(delete-trailing-whitespace
          :which-key "Remove trailing whitespace")

   ;; 'f' for files
   "ff" '(helm-projectile-find-file
          :which-key "Find file in current project")

   ;; 'w' prefix for window management (integrate with tmux?)
   "ws" '(evil-window-split
          :which-key "Split horizontal")
   "wS" '(evil-window-vsplit
          :which-key "Split vertical")
   "wq" '(evil-window-delete
          :which-key "Delete window")
   "wr" '(evil-window-rotate-downwards
          :which-key "Rotate windows")
   ;; FIXME winner mode for rearanging windows?
   "wR" '((lambda () (interactive) (hydra-resize/body))
          :which-key "Enter resize hydra")
   "wA" '(ace-window
          :which-key "ACE window")

   ;; 'g' prefix for git commands
   "gb" '(magit-blame
          :which-key "git blame")

   ;; 'h' (move the help bindings) FIXME put more here
   "hf" '(describe-function
          :which-key "describe function")
   "hv" '(describe-variable
          :which-key "describe variable")
   "hm" '(describe-mode
          :which-key "describe mode")
   "hb"  '((lambda () (interactive) (general-describe-keybindings))
           :which-key "Describe general bindings")
   "hB"  '(describe-bindings
           :which-key "Describe bindings")

   ;; FIXME something to expand selections, run lisp code, etc
   ;; FIXME rip off a bunch of spacemacs bindings
   ))

;; FIXME want C-h C-j C-k C-l movement bindings, but these eat a lot of emacs builtins
;; this is problematic

(use-package vterm)

(use-package org
  :straight org-plus-contrib
  ;;:straight (orgmode :repo "git@gitlab.com:dpzmick/orgmode.git")
  :after (evil general)
  :init
  (setq org-adapt-indentation nil)
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/todoist.org" "~/org/home.org"))
  (setq org-mode-actual-width nil)
  :config

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex  . t)
     (python . t)
     (shell  . t)
     (C      . t)
     (dot    . t))))

;; never got this working
;; (use-package org-ref
;;   :straight t
;;   :after org
;;   :config
;;   (setq reftex-default-bibliography '("/nas/org-ref/references.bib"))
;;   (setq org-ref-bibliography-notes "/nas/org-ref/notes.org"
;;         org-ref-default-bibliography '("/nas/org-ref/references.bib")
;;         org-ref-pdf-directory "/nas/org-ref/pdfs"))

(use-package htmlize
  :straight t)

(use-package rust-mode)

(use-package sclang-extensions
  :config
  (require 'sclang))

(use-package eglot)

(use-package company
  :init
  :config
  (global-company-mode))

;; (use-package company-lsp
;;   :after (company lsp)
;;   :config
;;   (add-to-list 'company-backends 'company-lsp))

(use-package hl-todo ;; FIXME could add some cool keybindings for this
  :demand t          ;; maybe there's a helm mode for these?
  :config
  (global-hl-todo-mode))

;; --- config
(global-display-line-numbers-mode) ;; always display line numbers
(electric-pair-mode t) ;; FIXME ??

;; FIXME put the rest of the tweaks I applied here
(defun my-c-mode-hook ()
  (setq c-backslash-column 80)
  (setq c-default-style "linux"
                c-basic-offset 2)
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'cpp-mode-hook 'my-c-mode-hook)

;; FIXME understand this
(setq-default indent-tabs-mode nil)
(setq show-trailing-whitespace t) ;; always on

;; tweaks to ui
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(xterm-mouse-mode 1)

(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode 1) ;; save minibuffer history
(save-place-mode) ;; save last place in file

(setq
 backup-by-copying t ;; deal with symlinks
 backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(setq
 auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t)))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
(setq mu4e-maildir (expand-file-name "~/mail"))
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder "/Sent")
(setq mu4e-trash-folder "/Trash")
(setq mu4e-view-show-images t)

;; things I want
;; - intelligent autoindent
;; - tabs vs spaces
;; - wrapping and 80 character indicator
;; - search for code in grep (word under cursor)
;; - git tools
;; - good status bar with git branch and additonal info
;; - popup asm in emacs
