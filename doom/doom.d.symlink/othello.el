
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
