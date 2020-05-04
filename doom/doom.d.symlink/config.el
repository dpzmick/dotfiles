;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -- my functions
(load! "util.el")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "David Zmick"
      user-mail-address "david@dpzmick.com")

;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq base16-theme-256-color-source "base16-shell")
(setq doom-theme 'base16-tomorrow-night)

;; make line numbers not look terrible
(set-face-inverse-video 'line-number-current-line nil)
(set-face-foreground 'line-number-current-line "orange")
(set-face-background 'line-number-current-line "color-18")

;; make comment delimiters not invisible
(set-face-foreground 'font-lock-comment-delimiter-face "brightblack")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; we'll manage our own install of language server in julia
(setq lsp-julia-package-dir nil)

(setq blink-matching-paren t)

;; doom installs "clippetty" plugin to share the emacs clipboard with the
;; system. this causes strange errors in mutliplexers like tmux (or dvtm):
;; https://github.com/spudlyo/clipetty/issues/15
;; following advice here: https://github.com/hlissner/doom-emacs/issues/2855
(remove-hook 'tty-setup-hook 'doom-init-clipboard-in-tty-emacs-h)

;; don't use a builtin language server for julia lsp
;; instead, we will provide our own by installing it in julia
(setq lsp-julia-package-dir nil) ;; FIXME get this working

;; doom disables both of these indicators in goggles, but I like them
(use-package evil-goggles
  :init
  (setq evil-goggles-enable-delete t
        evil-goggles-enable-change t))

;; tweak a few of the doom defaults
(use-package! evil
  :config
  ;; bind the jumping around keys to meta
  ;; since I want to be able to use tab in a terminal
  (define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)
  (evil-set-toggle-key "M-!"))

(use-package! tmux-pane
  :config
  (tmux-pane-mode)
  (map! :leader
    (:prefix ("v" . "tmux pane")
     :desc "Open hpane" :nv "s" #'tmux-pane-open-horizontal
     :desc "Open vpane" :nv "S" #'tmux-pane-open-vertical
     :desc "Close pane" :nv "c" #'tmux-pane-close
     :desc "Rerun last command" :nv "r" #'tmux-pane-rerun))
  (map! :leader
    (:prefix "t"
     :desc "vpane" :nv "v" #'tmux-pane-toggle-vertical
     :desc "hpane" :nv "h" #'tmux-pane-toggle-horizontal)))

(after! org
  ;; try and disable all of the visual quirks that doom turns on
  (setq org-hide-leading-stars nil)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented nil)

  ;; remove the doom customization hook
  (remove-hook 'org-load-hook '+org-init-appearance-h))

;; augment of doom-emacs default keybindings
(map! :leader
  ;; this overrides the default. was 'bookmark-jump
  :desc "align dwim" "RET" #'dpzmick-align-dwim

  ;; added by me
  (:prefix "w"
   :desc "split horizontally" :nv "S" #'split-window-horizontally)

  (:prefix "S"
   :desc "clear search" :nv "c" #'evil-ex-nohighlight))

;; default aligment mode
;; only does alignment, looking for something that will reindent and align at
;; the same time
(define-key!
  [remap dpzmick-align-dwim] #'align)

;; many modes have rainbows enabled
(defun disable-rainbows () (rainbow-delimiters-mode -1))

(after! yasnippet
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-keymap (kbd "C-n") 'yas-next-field)
  (define-key yas-keymap (kbd "C-p") 'yas-prev-field))

(defun dpzmick-c-mode ()
  (use-package lsp
    :init
    (setq eglot-put-doc-in-help-buffer t
          eglot-auto-display-help-buffer nil
          eglot-confirm-server-initiated-edits t))
  (eglot)
  (disable-rainbows))

(add-hook 'c-mode-hook 'dpzmick-c-mode)
(add-hook 'cpp-mode-hook 'dpzmick-c-mode)

(defun dpzmick-julia-mode ()
  (load! "julia.el")
  (map! :leader
    (:prefix ("m" . "meta commands")
     :desc "run file" :nv "r" #'dpzmick/run-julia-in-tmux
     :desc "run line" :nv "l" #'dpzmick/run-julia-line-in-tmux)))

(add-hook 'julia-mode-hook 'dpzmick-julia-mode)

;; FIXME git blame
;; FIXME overlay mode is awesome, use it more
;; - maybe find a way to quickly pop-up python repl all the time
;; FIXME man pages
;; FIXME show matching parens on insert only
;; FIXME remember last position in file

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
