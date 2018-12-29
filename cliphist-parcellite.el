;;; cliphist-parcellite.el --- read parcelllite data file

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

;; Read "~/.local/share/parcellite/history" on Linux

;;; Code:

(defun cliphist-parcellite-get-item-size (str len beg &optional short-int)
  "Scan STR whose length is LEN.  Start scanning from position BEG.
If SHORT-INT is t, read 2 bytes.  Or else read 4 bytes."
  (let* (size)
    ;; read 4 bytes in little endian order
    (if (< (+ beg 3) len)
        (setq size (+ (elt str beg)
                      (* 256 (elt str (+ 1 beg)))
                      (if short-int 0
                        (+ (* 256 256 (elt str (+ 2 beg)))
                           (* 256 256 256 (elt str (+ 3 beg))))))))
    size))

(defun cliphist-parcellite-read-item (str len item &optional is-new-version)
  "Parcellite binary data STR with length LEN is analyzed.
ITEM is the previous item extracted whose data useful for current extraction.
If IS-NEW-VERSION is t, it's Parcellite v1.0+."
  (let (rlt
        index
        beg
        type
        size)
    (if item (setq beg (nth 1 item))
      (setq beg (if is-new-version 32 0)))
    ;; read 4 bytes to get the item length
    (setq size (cliphist-parcellite-get-item-size str len beg))

    ;; read string
    (if (and size (> size 0))
        (cond
         (is-new-version
          ;; type has two bytes
          (setq type (cliphist-parcellite-get-item-size str len (+ 8 beg) 2))
          ;; 1 means STRING type
          (if (= 1 type)
              ;; Just fetch the content, I don't care about the summary of item
              (setq rlt (list (substring str (+ 36 beg) (+ beg size)) (+ beg size)))))
         (t
          ;; Old version of parcellite is much simpler
          (setq rlt (list (substring str (+ 4 beg) (+ 4 beg size)) (+ 4 beg size))))))
    rlt))

(defun cliphist-parcellite-read-items (fn-insert)
  "For each item, First 4 bytes specify the size of content.
It ends with 4 byte zeroed.  Please note byte are stored in little endian way.
Extracted item will be passed to FN-INSERT."
  (let* ((path (file-truename "~/.local/share/parcellite/history"))
         str
         str-len
         is-new-version
         item
         rlt)
    (when (file-exists-p path)
      (setq str (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (setq buffer-file-coding-system 'binary)
                  (insert-file-contents-literally path)
                  (buffer-substring-no-properties (point-min) (point-max))))
      (setq str-len (length str))
      ;; first 3 characters is "1.0"
      (setq is-new-version (and (= (elt str 0) 49)
                                (= (elt str 1) 46)
                                (= (elt str 2) 48)))
      ;; read clipboard items into cache
      (while (setq item (cliphist-parcellite-read-item str str-len item is-new-version))
        ;; filter out short strings
        (unless (< (length (car item)) 3)
          (funcall fn-insert 'rlt (decode-coding-string (car item) 'utf-8)))))
    rlt))

(provide 'cliphist-parcellite)
;;; cliphist-parcellite.el ends here
