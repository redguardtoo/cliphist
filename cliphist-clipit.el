;;; cliphist-clipit.el --- read parcelllite data file

;; Copyright (C) 2015-2019 Chen Bin

;; Author: Chen Bin <chenin DOT sh AT gmail DOT com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Read "~/.local/share/clipit/history" on Linux

;;; Code:

(defun cliphist-clipit-get-item-size (str len beg)
  "Scan STR whose length is LEN.  Start scanning from position BEG.
If SHORT-INT is t, read 2 bytes.  Or else read 4 bytes."
  (let* (size)
    ;; read 4 bytes in little endian order
    (if (< (+ beg 3) len)
        (setq size (+ (elt str beg)
                      (* 256 (elt str (+ 1 beg)))
                      (+ (* 256 256 (elt str (+ 2 beg)))
                         (* 256 256 256 (elt str (+ 3 beg)))))))
    size))

(defun cliphist-clipit-read-item (str len item)
  "Clipit binary data STR with length LEN is analyzed.
ITEM is the previous item extracted whose data useful for current extraction."
  (let* (rlt
         (beg (if item (+ 12 (nth 1 item))  68))
         (str-beg (+ beg 8))
         ;; read 4 bytes to get the item length
         (size (cliphist-clipit-get-item-size str len beg)))
    ;; read string
    (when (and size (> size 0))
      (setq rlt (list (substring str str-beg (+ str-beg size)) (+ str-beg size))))
    rlt))

(defun cliphist-clipit-read-items (fn-insert)
  "Check save_history defined in ClipIt history.c.
68 bytes rubbish at the beginning of file plus the item list.

For each item in the item list, First 4 bytes specify the size of content.
Then is number 1, content of item, number 4, number 2, and boolean flage is_static.

Please note bytes are stored in little endian way.
Number and boolean flag takes 4 bytes.
Extracted item will be passed to FN-INSERT."
  (let* ((path (file-truename "~/.local/share/clipit/history"))
         str
         str-len
         item
         rlt)
    (when (file-exists-p path)
      (setq str (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (setq buffer-file-coding-system 'binary)
                  (insert-file-contents-literally path)
                  (buffer-substring-no-properties (point-min) (point-max))))
      (setq str-len (length str))
      ;; read clipboard items into cache
      (while (setq item (cliphist-clipit-read-item str str-len item))
        ;; filter out short strings
        (unless (< (length (car item)) 3)
          (funcall fn-insert 'rlt (decode-coding-string (car item) 'utf-8)))))
    rlt))

(provide 'cliphist-clipit)
;;; cliphist-clipit.el ends here
