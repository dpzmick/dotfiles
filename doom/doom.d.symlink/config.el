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
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
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
;; (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

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

(defun xah-show-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.

Samples of valid input:

  ffff → 65535
  0xffff → 65535
  #xffff → 65535
  FFFF → 65535
  0xFFFF → 65535
  #xFFFF → 65535

more test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600

URL `http://xahlee.info/emacs/emacs/elisp_converting_hex_decimal.html'
Version 2020-02-17"
  (interactive )
  (let ($inputStr $tempStr $p1 $p2 )
    (if (region-active-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (save-excursion
          (skip-chars-backward "0123456789abcdefABCDEF#x")
          (setq $p1 (point))
          (skip-chars-forward "0123456789abcdefABCDEF#x" )
          (setq $p2 (point)))))
    (setq $inputStr (buffer-substring-no-properties $p1 $p2))
    (let ((case-fold-search nil))
      (setq $tempStr (replace-regexp-in-string "\\`0x" "" $inputStr )) ; C, Perl, …
      (setq $tempStr (replace-regexp-in-string "\\`#x" "" $tempStr )) ; elisp …
      (setq $tempStr (replace-regexp-in-string "\\`#" "" $tempStr )) ; CSS …
      )
    (string-to-number $tempStr 16)))

(defun bitboard-idx (x y)
  (- 63 (+ (* y 8) x)))

(defun bitboard-mask (x y)
  (ash 1 (bitboard-idx x y)))

;; iterate top left to bottom right

(setq max-x 8)
(setq max-y 8)

(defun board-map-x (f acc x y)
  (if (< x max-x)
      (board-map-x f (funcall f acc x y) (+ x 1) y)
    acc))

(defun board-map-y (f acc y)
  (if (< y max-y)
      (board-map-y f (board-map-x f acc 0 y) (+ y 1))
    acc))

(defun board-map (f acc)
  (board-map-y f acc 0))

(defun bitboard-char (bitstring x y)
  (if (not (zerop (logand bitstring (bitboard-mask x y)))) "*" "."))

(defun popup-othello ()
  (interactive)
  (let* ((bs (xah-show-hexadecimal-value))
         (g (lambda (acc x y)
              (if (= x (- max-x 1))
                       (format "%s%s\n" acc (bitboard-char bs x y))
               (format  "%s%s" acc (bitboard-char bs x y))))))
    (popup-tip (board-map g ""))))

(map! :leader
      (:prefix ("m" . "meta commands")
       :desc "do popup" :nv "p" #'popup-othello)
      (:prefix ("m" . "meta commands")
       :desc "do popup" :nv "P" #'xah-show-hexadecimal-value))

;; (defun dpzmick-julia-mode ()
;;   (load! "julia.el")
;;   (map! :leader
;;     (:prefix ("m" . "meta commands")
;;      :desc "run file" :nv "r" #'dpzmick/run-julia-in-tmux
;;      :desc "run line" :nv "l" #'dpzmick/run-julia-line-in-tmux)))
;; 
;; (add-hook 'julia-mode-hook 'dpzmick-julia-mode)

;; a bunch of verilog setttings
(setq verilog-indent-level             2
      verilog-indent-level-module      2
      verilog-indent-level-declaration 2
      verilog-indent-level-behavioral  2
      verilog-indent-level-directive   2
      verilog-case-indent              2
      verilog-tab-always-indent        t
      verilog-auto-endcomments         t
      verilog-indent-begin-after-if    t
      verilog-auto-lineup              'declarations
      verilog-tool                     "iverilog")

;; FIXME git blame
;; FIXME overlay mode is awesome, use it more
;; - maybe find a way to quickly pop-up python repl all the time
;; FIXME man pages
;; FIXME show matching parens on insert only
;; FIXME remember last position in file
