;;; cliphist.el --- paste from clipboard managers -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Chen Bin
;;
;; Version: 0.6.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: clipboard manager history
;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
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
;;   - Parcellite (http://parcellite.sourceforge.net) on Linux
;;   - ClipIt (http://clipit.sourceforge.net) on Linux
;;   - Greenclip (https://github.com/erebe/greenclip) on Linux
;;   - Flycut (https://github.com/TermiT/Flycut) on macOS
;;
;; Usage:
;;   Make sure clipboard manager is running.
;;   If you use Flycut on macOS, set up "Preferences > General > Clippings",
;;   so its value is "Save After each clip".
;;   "M-x cliphist-paste-item" to paste item from history
;;   "C-u M-x cliphist-paste-item" rectangle paste item
;;   "M-x cliphist-select-item" to select item
;;
;; You can customize `cliphist-select-item',
;; For example, if you use xclip (https://elpa.gnu.org/packages/xclip.html),
;;
;;   (require 'xclip)
;;   (setq cliphist-select-item-callback
;;      (lambda (num str)
;;         (xclip-set-selection 'clipboard str)))
;;
;; If `cliphist-cc-kill-ring' is true, the selected/pasted string
;; will be inserted into kill-ring.
;;
;; Set `cliphist-linux-clipboard-managers',  `cliphist-macos-clipboard-managers',
;; `cliphist-windows-clipboard-managers' to add your own clipboard managers.
;; Here are steps to support a new clipboard manager named "myclip" on Linux.
;;
;; Step 1, create a file "cliphist-myclip.el" with below content,
;;
;;   (require 'cliphist-sdk)
;;   (defun cliphist-my-read-items ()
;;     (let (rlt
;;           (items '("clip1" "clip2")))
;;       (dolist (item items)
;;         (cliphist-sdk-add-item-to-cache rlt item))
;;       rlt))
;;   (provide 'cliphist-myclip)
;;
;; Step 2, add "(push "myclip" cliphist-linux-clipboard-managers)" into "~/.emacs".
;;
;; Set `cliphist-greenclip-program' if greenclip program is not added into
;; environment variable PATH.
;;

;;; Code:

(defvar cliphist-windows-clipboard-managers
  '()
  "Clipboard managers on Windows.")

(defvar cliphist-macos-clipboard-managers
  '("flycut")
  "Clipboard managers on macOS.")

(defvar cliphist-linux-clipboard-managers
  '("greenclip" "clipit" "parcellite")
  "Clipboard managers on Linux.")

(defvar cliphist-cc-kill-ring nil
  "Copy the selected/pasted item into kill ring.")

(defvar cliphist-select-item-callback nil
  "The callback of `cliphist-select-item'.
If nil, selected item is copied to clipboard when `cliphist-select-item' called.
Or else the `(funcall cliphist-select-item num item)' will be executed.")

(defvar cliphist-items nil
  "Item list extracted from clipboard manager.  Internal variable.")

(defvar cliphist-debug nil
  "Debug flag.")

;;;###autoload
(defun cliphist-version ()
  "Echo package version."
  (message "0.6.0"))

;;;###autoload
(defun cliphist-read-items ()
  "Read clipboard items."
  (interactive)
  (let* ((managers (cond
                    ((eq system-type 'darwin)
                     ;; macOS
                     cliphist-macos-clipboard-managers)

                    ((or (eq system-type 'gnu/linux) (eq system-type 'linux))
                     ;; Linux
                     cliphist-linux-clipboard-managers)

                    ((or (eq system-type 'windows-nt) (eq system-type 'cygwin))
                     cliphist-windows-clipboard-managers))))
    (cond
     (managers
      (let* ((i 0) fn rlt manager)
        (while (and (not rlt)
                    (< i (length managers)))
          (setq manager (nth i managers))
          (setq fn (intern (format "cliphist-%s-read-items" manager)))
          (autoload fn (format "cliphist-%s" manager) nil)
          (setq rlt (funcall fn))
          (when cliphist-debug (message "fn=%s rlt=%s" fn rlt))
          (setq i (1+ i)))

        (when rlt
          (setq cliphist-items rlt))))

     (t
      (message "Sorry, clipboard manager is NOT found.")))))

(defmacro cliphist-do-item (num &optional fn)
  "Select a item with index NUM and call FN to process it.
Utility used by other commands."
  `(let (selected)
     (cliphist-read-items)
     (cond
      ((and cliphist-items (> (length cliphist-items) 0))
       (when (setq selected
                   (completing-read "Clipboard items: " cliphist-items))
         (setq selected (assoc selected cliphist-items))
         (when ,fn (funcall ,fn ,num (cdr selected)))
         (if cliphist-cc-kill-ring (kill-new (cdr selected)))))
      (t
       (message "Nothing in clipboard yet!")))))

(defun cliphist-routine-before-insert ()
  "Make string insertion in `evil-normal-state' work."
  (when (and (functionp 'evil-normal-state-p)
             (functionp 'evil-move-cursor-back)
             (evil-normal-state-p)
             (not (eolp))
             (not (eobp)))
    (forward-char)))

;;;###autoload
(defun cliphist-paste-item (&optional rect-paste)
  "Paste selected item into current buffer.
Rectangle paste the item if arg RECT-PASTE is non-nil."
  (interactive "P")
  (cliphist-do-item 1 `(lambda (num str)
                         ;; evil-mode?
                         (cliphist-routine-before-insert)
                         ;; insert now
                         (if ,rect-paste
                             (insert-rectangle (split-string str "[\r]?\n"))
                           (insert str)))))

;;;###autoload
(defun cliphist-select-item (&optional num)
  "Select one item from clipboard history.
NUM and selected item is passed to `cliphist-select-item-callback'."
  (interactive "P")
  (cliphist-do-item num cliphist-select-item-callback))

(provide 'cliphist)
;;; cliphist.el ends here
