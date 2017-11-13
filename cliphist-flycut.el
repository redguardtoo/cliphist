;;; cliphist-flycut.el --- read flycut data file

;; Copyright (C) 2015-2017 Chen Bin

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

(require 'xml)

(defun cliphist-find-xml-node-containting-items (items)
  "We need node type is `key' and value is `displayLen'."
  (let* (rlt
         child
         (i 0))
    (while (and (not rlt)
                (< i (length items)))
      (setq child (nth 0 (xml-get-children (nth i items) 'key)))
      (if (and (eq (car child) 'key)
               (string= (nth 2 child) "displayLen"))
          (setq rlt (xml-get-children (car (xml-get-children (nth i items) 'array))
                                      'dict)))
      (setq i (+ i 1)))
    rlt))

(defun cliphist-flycut-guess-preference-path ()
  (let* ((rlt (file-truename "~/Library/Preferences/com.generalarcade.flycut.plist")))
    (unless (file-exists-p rlt)
      (setq rlt (file-truename "~/Library/Application Support/Flycut/com.generalarcade.flycut.plist")))
    rlt))

(defun cliphist-flycut-read-items (&optional fn-insert)
  "Flycut store the data in xml file.
We use regex to extract the clipboard item.
Then call FN-INSERT to insert the item into the list which returned by this function."
  (let* (rlt
         (path (cliphist-flycut-guess-preference-path))
         ;; (path (file-truename "~/projs/cliphist/data/flycut/com.generalarcade.flycut.plist"))
         (xml-tree (cadr (xml-node-children (car (xml-parse-file path)))))
         (arr (cliphist-find-xml-node-containting-items (xml-get-children xml-tree 'dict))))
    (when arr
      (dolist (item arr)
        (let* ((strs (xml-get-children item 'string)))
          ;; strs is two xml node:
          ;;   (string nil calculate)
          ;;   (string nil NSStringPboardType)
          (if (and (string= "NSStringPboardType" (nth 2 (cadr strs)))
                   fn-insert)
              (funcall fn-insert 'rlt (nth 2 (car strs)))))))
    rlt))

(provide 'cliphist-flycut)
;;; cliphist-flycut.el ends here