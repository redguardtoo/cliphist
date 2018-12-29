;;; cliphist-flycut.el --- read flycut data file

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

;; Read "~/Library/Application Support/Flycut/com.generalarcade.flycut.plist" on OSX

;;; Code:

(require 'xml)

(defun cliphist-flycut-get-dict (item)
  "Get dict node from ITEM's array.  First or second element has dict child.
It's decided by Flycut version."
  (let* ((arr (xml-get-children item 'array))
         (rlt (xml-get-children (nth 0 arr) 'dict)))
    (when (and (not rlt)
               (> (length arr) 1))
      (setq rlt (xml-get-children (nth 1 arr) 'dict)))
    rlt))

(defun cliphist-flycut-find-xml-node-containting-items (items)
  "We need node type is `key' and value is `displayLen'."
  (let* (rlt
         child
         (i 0))
    (while (and (not rlt)
                (< i (length items)))
      (setq child (nth 0 (xml-get-children (nth i items) 'key)))
      (when (and (eq (car child) 'key)
                 (string= (nth 2 child) "displayLen"))
        (setq rlt (cliphist-flycut-get-dict (nth i items))))
      (setq i (+ i 1)))
    rlt))

(defun cliphist-flycut-guess-preference-path ()
  (let* ((rlt (file-truename "~/Library/Preferences/com.generalarcade.flycut.plist")))
    (unless (file-exists-p rlt)
      (setq rlt (file-truename "~/Library/Application Support/Flycut/com.generalarcade.flycut.plist")))
    rlt))

(defun cliphist-flycut-is-bplist ()
  (string= (buffer-substring-no-properties (point-min) (+ (point-min) 6))
           "bplist"))

(defun cliphist-flycut-read-plist (plist-file)
  "Read PLIST-FILE which could be in binary format."
  (let* ((plutil-cmd (concat "plutil -convert xml1 -o - " plist-file))
         (content (with-temp-buffer
                    (insert-file-contents plist-file)
                    (when (cliphist-flycut-is-bplist)
                      ;; macOS might use binary plist format
                      (erase-buffer)
                      (insert (shell-command-to-string plutil-cmd)))
                    (xml--parse-buffer nil nil))))
    (cadr (xml-node-children (car content)))))

(defun cliphist-flycut-get-next-value (i dict)
  (let* (out-of-loop (len (length dict)) e rlt)
    (while (and (not out-of-loop) (< i len))
      (setq e (nth i dict))
      (cond
       ((and (listp e) (eq (car e) 'string))
        (setq rlt (nth 2 e))
        (setq out-of-loop t))
       (t
        (setq i (1+ i)))))
    rlt))

(defun cliphist-flycut-get-value-by-key (key-str dict)
  "Get value by KEY-STR from DICT node."
  (let* (out-of-loop (i 0) e val rlt)
    (while (and (not out-of-loop)
                (< i (length dict)))
      (setq e (nth i dict))
      (cond
       ((and (listp e) (eq (car e) 'key))
        (cond
         ((string= key-str (nth 2 e))
          (setq rlt (cliphist-flycut-get-next-value (1+ i) dict))
          (setq out-of-loop t))
         (t
          (setq i (+ 2 i)))))
       (t
        (setq i (1+ i)))))
    rlt))

(defun cliphist-flycut-read-items (&optional fn-insert)
  "Flycut store the data in xml file where item is extracted.
FN-INSERT inserts the item into the list which returned by this function."
  (let* (rlt
         (path (cliphist-flycut-guess-preference-path))
         ;; (path (file-truename "~/projs/cliphist/data/flycut/com.generalarcade.flycut.plist"))
         (xml-tree (cliphist-flycut-read-plist path))
         (arr (cliphist-flycut-find-xml-node-containting-items (xml-get-children xml-tree 'dict))))
    (when arr
      (dolist (item arr)
        (when (string= (cliphist-flycut-get-value-by-key 'Type item) "NSStringPboardType")
          (cond
           (fn-insert
            (funcall fn-insert 'rlt (cliphist-flycut-get-value-by-key "Contents" item)))
           (t
            ;; do nothing
            )))))
    rlt))

(provide 'cliphist-flycut)
;;; cliphist-flycut.el ends here