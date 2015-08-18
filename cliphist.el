;;; cliphist.el --- read clipboard history

;; Copyright (C) 2015 Chen Bin
;;
;; Version: 0.0.1
;; Keywords: keyword1 keyword2
;; Author: Chen Bin <chenin DOT sh AT gmail DOT com>
;; URL: http://github.com/redguardtoo/cliphist

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Read clipboard history of parcellite (http://parcellite.sourceforge.net)
;; Other clipboard manager will be supported soon.

;;; Code:

(defvar cliphist-items nil)
(defvar cliphist-current-item-index 0)

(defun cliphist--get-item-size (str str-len beg)
  (let (size)
    ;; read 4 bytes in little endian order
    (if (< (+ beg 3) str-len)
        (setq size (+ (elt str beg)
                      (* 256 (elt str (+ 1 beg)))
                      (* 256 256 (elt str (+ 2 beg)))
                      (* 256 256 256 (elt str (+ 3 beg)))
                      )))
    size))

(defun cliphist--read-item (str str-len is-new-version &optional item)
  (let (rlt index beg size)
    (if item (setq beg (nth 1 item))
      (setq beg 0))
    ;; read 4 bytes to get the item length
    (setq size (cliphist--get-item-size str str-len beg))

    ;; read string
    (if (and size (> size 0))
        (setq rlt (list (substring str (+ 4 beg) (+ 4 beg size)) (+ 4 beg size))))
    rlt))

(defun cliphist-read-items-from-history (path)
  "hexl-mode, Read binary data from PATH.
Return the binary data as unibyte string.
The format of history, first 4 bytes, specify the size of content (the little endian way) , always end with 4 byte zeroed
"
  (let (str v1 item rlt str-len)
    (setq str (with-temp-buffer
                (set-buffer-multibyte nil)
                (setq buffer-file-coding-system 'binary)
                (insert-file-contents-literally path)
                (buffer-substring-no-properties (point-min) (point-max)))
          )
    (setq str-len (length str))
    ;; Is 1.0?
    (setq v1 (and (= (elt str 0) 49)
                  (= (elt str 1) 46) (= (elt str 2) 48)))
    ;; reset cache
    (setq cliphist-items nil)
    ;; read clipboard items into cache
    (while(setq item (cliphist--read-item str str-len v1 item))
      (add-to-list 'rlt (car item)))

    rlt))

(defun cliphist-move (delta)
  (let (b e)
    (when (and cliphist-items
               (> (length cliphist-items) 0))
      (setq b (if (= delta 1) 0 (- (length cliphist-items) 1)))
      (setq e (if (= delta 1) (length cliphist-items) -1))

      ;; move the pointer to the current item in clipboard history
      (setq cliphist-current-item-index (+ delta cliphist-current-item-index))

      ;; rewind if needed
      (if (= cliphist-current-item-index e)
          (setq cliphist-current-item-index b))
      (if (and cliphist-items
               (< cliphist-current-item-index (length cliphist-items)))
          (message "Current clip: %s" (cliphist-get-current-item-content))))
    ))

;;;###autoload
(defun cliphist-get-current-item-content ()
  (decode-coding-string (nth cliphist-current-item-index cliphist-items) 'utf-8))

;;;###autoload
(defun cliphist-next ()
  (interactive)
  (cliphist-move 1))

;;;###autoload
(defun cliphist-previous ()
  (interactive)
  (cliphist-move -1))

;;;###autoload
(defun cliphist-insert-current-item-content ()
  (interactive)
  (if (and cliphist-items
           (< cliphist-current-item-index (length cliphist-items)))
      (insert (cliphist-get-current-item-content))))

(provide 'cliphist)
;;; cliphist.el ends here

