;;; cliphist-greenclip.el --- read parcelllite data file

;; Copyright (C) 2015-2021 Chen Bin

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

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

;;; Code:
(defvar cliphist-greenclip-program "greenclip"
  "The path of greenclip program.")

(defun cliphist-greenclip-read-item (str)
  "Clean up STR and return it as a clipboard item."
  (setq str (replace-regexp-in-string "\u00a0" "\n" str))
  str)

(defun cliphist-greenclip-read-items (fn-insert)
  "Read clipboard items of greenclip.
Extracted item will be passed to FN-INSERT."
  (when (executable-find cliphist-greenclip-program)
    (let* ((cmd (format "%s print" cliphist-greenclip-program))
           (lines (split-string (shell-command-to-string cmd) "[\r\n]+"))
           item
           rlt)
      (when (and lines (> (length lines) 0))
        (dolist (line lines)
          (setq item (cliphist-greenclip-read-item line))
          (unless (< (length item) 3)
            (funcall fn-insert 'rlt (decode-coding-string item 'utf-8)))))
      rlt)))

(provide 'cliphist-greenclip)
;;; cliphist-greenclip.el ends here
