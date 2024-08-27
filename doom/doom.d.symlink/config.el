;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; -- my functions
;; (load! "util.el")
;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "David Zmick"
      user-mail-address "david@dpzmick.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
(setq doom-font (font-spec :family "Source Code Pro" :size 18))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq base16-theme-256-color-source "colors")
(setq doom-theme 'base16-tomorrow-night)

;; make line numbers not look terrible
(set-face-inverse-video 'line-number-current-line nil)
(set-face-foreground 'line-number-current-line "orange")
(set-face-background 'line-number-current-line "color-18")

;; make comment delimiters not invisible
(set-face-foreground 'font-lock-comment-delimiter-face "brightblack")

(setq blink-matching-paren t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :leader
  ;; this overrides the default. was 'bookmark-jump
  :desc "align dwim" "RET" #'dpzmick-align-dwim

  ;; added by me
  (:prefix "w"
   :desc "split horizontally" :nv "S" #'split-window-horizontally)

  (:prefix "S"
   :desc "clear search" :nv "c" #'evil-ex-nohighlight))

;; doom installs "clippetty" plugin to share the emacs clipboard with the
;; system. this causes strange errors in mutliplexers like tmux (or dvtm):
;; https://github.com/spudlyo/clipetty/issues/15
;; following advice here: https://github.com/hlissner/doom-emacs/issues/2855
(remove-hook 'tty-setup-hook 'doom-init-clipboard-in-tty-emacs-h)

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
  (tmux-pane-mode))

(after! org
  ;; try and disable all of the visual quirks that doom turns on
  (setq org-hide-leading-stars nil)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented nil)

  ;; remove the doom customization hook
  (remove-hook 'org-load-hook '+org-init-appearance-h))

;; default aligment mode
;; only does alignment, looking for something that will reindent and align at
;; the same time
(define-key!
  [remap dpzmick-align-dwim] #'align)

;; many modes have rainbows enabled
(defun disable-rainbows () (rainbow-delimiters-mode -1))

(after! cc-mode
  (setq-default c-basic-offset 2))

(setq +format-with-lsp nil)

(setq org-books-file "~/icloud/org/book-list.org")
(setq org-agenda-files (list "~/icloud/org/org.org"))

(use-package! gptel
 :config
 (setq! gptel-api-key "KEY??"))
