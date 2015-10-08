;;; cliphist-flycut.el --- read flycut data file

;; Copyright (C) 2015 Chen Bin

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

;; Read "~/Library/Application Support/Flycut/com.generalarcade.flycut.plist" on OSX

;;; Code:

(defun cliphist-flycut-decode-xml-string (string)
  "Decode STRING from xml encoded format."
  (let (str)
    (setq str
          (with-temp-buffer
            (insert string)
            (dolist (substitution '(("&" . "&amp;")
                                    ("&lt;" . "<")
                                    ("&gt;" . ">" )
                                    ("&apos;" . "'")
                                    ("&quot;" . "\"")))
              (goto-char (point-min))
              (while (search-forward (car substitution) nil t)
                (replace-match (cdr substitution) t t nil)))
            (buffer-substring-no-properties (point-min) (point-max))))
    (decode-coding-string str 'utf-8)))

(defun cliphist-flycut-read-items (fn-insert)
  "Flycut store the data in xml file.
We use regex to extract the clipboard item.
Then call FN-INSERT to insert the item into the list which returned by this function."
  (let (arr str rlt b e path)
    ;; (setq path (file-truename "~/projs/cliphist/data/flycut/com.generalarcade.flycut.plist")) ; debug
    (setq path (file-truename "~/Library/Application Support/Flycut/com.generalarcade.flycut.plist"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq buffer-file-coding-system 'binary)
      (insert-file-contents-literally path)
      (setq b (re-search-forward "<array>" (point-max) t))
      (setq e (- (re-search-forward "</array>" (point-max) t) 8))
      (setq str (buffer-substring-no-properties b e)))

    ;; hate xml, string matching is ACTUALLY more elegant.
    (setq arr (split-string str "\\(^[\t \r\n]*<dict>[\t \r\n]*\\|[\t \r\n]*</dict>[\t \r\n]*<dict>[\t \r\n]*\\|[\t \r\n]*</dict>[\t \r\n]*$\\)"))
    (dolist (item arr)

      (if (string-match "<string>NSStringPboardType</string>" item)
          (let ((s1 "<key>Contents</key>")
                (s2 "<string>")
                (s3 "</string>")
                b e)
            (setq b (+ (length s1) (string-match s1 item)))
            (setq b (+ (length s2) (string-match s2 item b)))
            (setq e (string-match s3 item b))
            ;; insert item into rlt
            (funcall fn-insert 'rlt (cliphist-flycut-decode-xml-string (substring item b e)))
            )))
    rlt))

(provide 'cliphist-flycut)
;;; cliphist-flycut.el ends here