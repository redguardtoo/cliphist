;;; cliphist.el --- Read clipboard history from Parcellite on Linux and Flycut on OS X

;; Copyright (C) 2015 Chen Bin
;;
;; Version: 0.1.0
;; Package-Requires: ((popup "0.5.0"))
;; Keywords: clipobard manager history
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

;; Read clipboard items from following clipboard managers,
;;   - Parcellite (http://parcellite.sourceforge.net)
;;   - Flycut (https://github.com/TermiT/Flycut)
;;
;; Usage:
;;   Make sure clipboard manager is running.
;;   `M-x cliphist-paste-item' to paste item from history
;;   `M-x cliphist-select-item' to select item
;;   In popup, press `C-n' or `C-p' to navigate, other keys
;;   to filter.
;;
;;   You can customize the behavior of cliphist-select-item,
;;     (setq cliphist-select-item-callback
;;        (lambda (num str) (cliphist-copy-to-clipboard str)))

;;; Code:

(require 'popup)

(defvar cliphist-popup-height 5 "Height of candidates popup.")

(defvar cliphist-item-summary-string-maxlength 32
  "Maximum string length of item summary displayed in menu")

(defvar cliphist-select-item-callback nil
  "The callback of `cliphist-select-item'.
If nil, selected item is copied to clipboard when `cliphist-select-item' called.
Or else the `(funcall cliphist-select-item num item)' will be executed.")

(defvar cliphist-items nil
  "Item list extracted from clipboard manager.")

(autoload 'cliphist-flycut-read-items "cliphist-flycut" nil)
(autoload 'cliphist-parcellite-read-items "cliphist-parcellite" nil)

(defun cliphist-add-item-to-cache (item-list str)
  (let ((stripped (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+$\\)" "" str))
        name item)
    (when (> (length stripped) 0)
      (setq item (popup-make-item (substring-no-properties stripped 0 (min (length stripped) cliphist-item-summary-string-maxlength)) :value str))
      (add-to-list item-list item t)
      )))

;;;###autoload
(defun cliphist-read-items ()
  (interactive)
  (let (rlt)
    (cond
     ((eq system-type 'darwin)
      ;; if nothing in clipboard, avoid purging the cache in Emacs
      (if (setq rlt (cliphist-flycut-read-items 'cliphist-add-item-to-cache))
          (setq cliphist-items rlt)))
     ((or (eq system-type 'gnu/linux) (eq system-type 'linux))
      ;; if nothing in clipboard, avoid purging the cache in Emacs
      (if (setq rlt (cliphist-parcellite-read-items 'cliphist-add-item-to-cache))
          (setq cliphist-items rlt)))
     (t (message "Sorry, only Linux and OS X are supported."))
     )))

;;;###autoload
(defun cliphist-do-item (num fn)
  "Select a item and do something.  Utility used by other commands.
FN do the thing."
  (let (selected-item)
    (cliphist-read-items)
    (if (and cliphist-items
               (> (length cliphist-items) 0))
        (if (setq selected-item
                  (popup-menu* cliphist-items
                               ;; display more content
                               ;; enable search by default
                               :height cliphist-popup-height
                               :isearch t))
            (funcall fn num selected-item))
      (message "Nothing in clipboard yet!"))))

;;;###autoload
(defun cliphist-copy-to-clipboard (str)
  "Copy STR into clipboard."
  (condition-case nil
      (cond
       ((fboundp 'ns-set-pasteboard)
        (ns-set-pasteboard str))
       ((fboundp 'x-set-selection)
        (x-set-selection 'CLIPBOARD str))
       (t
        (error "Clipboard support not available")))
    (error
     (condition-case nil
         (cond
          ((eq system-type 'darwin)
           (with-temp-buffer
             (insert str)
             (call-process-region (point-min) (point-max) "pbcopy")))
          ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
           (with-temp-buffer
             (insert str)
             (if (file-executable-p "xsel") (call-process-region (point-min) (point-max) "xsel" nil nil nil "--clipboard" "--input")
               (call-process-region (point-min) (point-max) "xclip" nil nil nil "-selection clipboard" "--input"))
             ))
          (t
           (error "Clipboard support not available")))
       (error
        (error "Clipboard support not available"))))))

;;;###autoload
(defun cliphist-paste-item ()
  "Paste selected item into current buffer."
  (interactive)
  (cliphist-do-item 1 (lambda (num str) (insert str))))

;;;###autoload
(defun cliphist-select-item (&optional num)
  "Select one item from clipboard history.
NUM is passed to `cliphist-select-item-callback'."
  (interactive "P")
  (cliphist-do-item num (lambda (num str)
                          ;; (message "num=%s str=%s" num str)
                          (if cliphist-select-item-callback
                              (funcall cliphist-select-item-callback num str)
                           (cliphist-copy-to-clipboard str)))))

(provide 'cliphist)
;;; cliphist.el ends here
