;; get the custom-config stuff to go somewhere else, probably don't really want
;; to use this very often
;; done before anything else since lots of things apparently want to write to
;; this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; ---- setup package manager
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

(use-package base16-theme
  :straight t
  ;;:init
  ;;(setq base16-theme-256-color-source "base16-shell")
  :config
  (load-theme 'base16-phd)) ;; FIXME get this from base16-shell

(use-package evil
  :straight t
  :init
  (setq evil-want-C-u-scroll t)   ;; I want this, what do I lose?
  (setq evil-want-C-i-jump nil)   ;; disable C-i jump so I can use TAB key for things
                                  ;; FIXME but I really want C-i jumps since I use that shit
  (setq evil-want-keybinding nil) ;; docs for evil-collection say to do this
  :config

  ;; bind the jumping around keys to meta
  (define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)

  ;; FIXME S-TAB still doesnt work

  (evil-mode)

  ;; loaded inline so this will start after org mode *and* after evil
  (use-package evil-org
    :straight t
    :after org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (use-package evil-magit
    :straight t
    :after magit)

  (use-package expand-region
    :straight t
    :config
    (setq expand-region-contract-fast-key "V"
	  expand-region-reset-fast-key "0")))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda () (evil-org-set-key-theme '(navigation
						 insert
						 textobjects
						 additional
						 calendar)))))

(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode))

(use-package general
  :straight t
  :after evil
  :config
  (general-evil-setup) ;; make it easier to write mappings that look like vim

  ;; map jk to escape
  (general-imap "j" (general-key-dispatch 'self-insert-command
		     :timeout 0.25
		     "k" 'evil-normal-state)))

(use-package org
  :straight org-plus-contrib
  :init
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
     (dot    . t)))

  (use-package org-download
    :straight t))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package todoist
  :straight t
  :init
  (setq todoist-token "57be0f40b4665f41177a9624a797b61a97335e3e")
  (setq todoist-show-all t)
  (setq todoist-backing-buffer "~/org/todoist.org"))

(use-package company
  :straight t
  :config
  (company-mode)
  (add-to-list 'company-backends 'company-irony)) ;; FIXME learn to use this

(use-package indent-guide
  :straight t
  :config
  (indent-guide-mode)) ;; FIXME not being loaded

(use-package magit
  :straight t)

(use-package hl-todo
  :straight t
  :config
  (hl-todo-mode)) ;; FIXME this doesn't appear to be loading

; (use-package helm
;   :straight t
;   :config
;   (helm-mode)) ;; FIXME learn to use this

(use-package flycheck
  :straight t) ;; FIXME learn to use this

(use-package irony
  :straight t)

(use-package company-irony
  :straight t)

(use-package company-irony
  :straight t)

;(add-to-list 'load-path "/home/dpzmick/builds/emacs-libvterm")
;(require 'vterm)

;; FIXME paredit?

;; --- config
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode 1) ;; save minibuffer history

;; (set-face-attribute 'default nil :height 200) ;; 20 point font for UI

;; Make latex biger with the bigger font (need to scale these the same way probably)
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;; attempt to make it possible to use tab and C-i in terminals
;; FIXME is this real
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(setq
 backup-by-copying t ;; deal with symlinks
 backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(setq
 auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t)))

;; some helpful functions

(defun toggle-show-trailing-whitespace ()
  "toggle show trailing whitespace"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; things I want
;; - auto-open files to the last open location
;; - fuzzy finder, buffers, current files
;; - search for code in grep (word under cursor)
;; - good status bar with git branch and additonal info
;; - git tools
;; - easy motion
;; - persistent undo
;; - setup leader key bindings (or find a new way to do bindings)
;; - jump to last open position
;; - intelligent autoindent
;; - tabs vs spaces
;; - fix mouse
;; - easyalign equivilent
;; - easy code commenter
;; - good management of splits (or just use tmux and client/server)
;; - wrapping and 80 character indicator
;; - line numbers?
;; - git changes in gutter (maybe not? that's sort of not useful)
;; - git-timemachine plugin?
;; - learn about emacs-calc or something that will let me do math fast
;; - find a solution to my jupyer notebooks problem/graphics stuff in general
;;   - having more places to do graphics might be neat
;; - convert this file to org mode
;; - maybe abandon this completely and switch to visual-studio-code
;;   - biggest problem with vscode is that window management is terrible
;;   - is tmux really any better? I think yes, but it still isn't very good
;;   - rejoining the session later is a fairly important feature
;;   - the window management component is much less critical that the
;;     running processes, open files, etc.
;; - figure out irc or something
;; - get org-mode to download remote files so I can TRAMP edit babel files
;; - code intelligence that actually works (with tramp, maybe only tramp for org-mode?)
