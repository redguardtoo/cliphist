;;; cliphist.el --- Read data from clipboard managers at Linux and Mac

;; Copyright (C) 2015-2019 Chen Bin
;;
;; Version: 0.5.6
;; Package-Requires: ((emacs "24.3") (ivy "0.9.0"))
;; Keywords: clipboard manager history
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
;;   - Parcellite (http://parcellite.sourceforge.net) at Linux
;;   - ClipIt (http://clipit.sourceforge.net) at Linux
;;   - Flycut (https://github.com/TermiT/Flycut) on OSX
;;
;; Usage:
;;   Make sure clipboard manager is running.
;;   `M-x cliphist-paste-item' to paste item from history
;;   `C-u M-x cliphist-paste-item' rectangle paste item
;;   `M-x cliphist-select-item' to select item
;;
;; You can customize the behavior of cliphist-select-item,
;;     (setq cliphist-select-item-callback
;;        (lambda (num str) (cliphist-copy-to-clipboard str)))
;;
;;   If `cliphist-cc-kill-ring' is true, the selected/pasted string
;;   will be inserted into kill-ring
;;
;; You can tweak =cliphist-linux-clipboard-managers= to tell cliphist
;; how to detect clipboard manager:
;;   `(setq cliphist-linux-clipboard-managers '("clipit" "parcellite"))'

;;; Code:

(require 'ivy)

(defvar cliphist-linux-clipboard-managers
  '("parcellite" "clipit")
  "We will try to detect the clipboard manager one by one.")

(defvar cliphist-cc-kill-ring nil
  "Copy the selected/pasted item into kill ring.")

(defvar cliphist-select-item-callback nil
  "The callback of `cliphist-select-item'.
If nil, selected item is copied to clipboard when `cliphist-select-item' called.
Or else the `(funcall cliphist-select-item num item)' will be executed.")

(defvar cliphist-items nil
  "Item list extracted from clipboard manager.  Internal variable.")

(autoload 'cliphist-flycut-read-items "cliphist-flycut" nil)
(autoload 'cliphist-parcellite-read-items "cliphist-parcellite" nil)
(autoload 'cliphist-clipit-read-items "cliphist-clipit" nil)

(defun cliphist--posn-col-row (posn)
  (let* ((col (car (posn-col-row posn)))
         ;; `posn-col-row' doesn't work well with lines of different height.
         ;; `posn-actual-col-row' doesn't handle multiple-width characters.
         (row (cdr (posn-actual-col-row posn))))
    (when (and header-line-format (version< emacs-version "24.3.93.3"))
      ;; http://debbugs.gnu.org/18384
      (cl-decf row))
    (cons (+ col (window-hscroll)) row)))

(defun cliphist--strip (str)
  (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+$\\)" "" str))

(defun cliphist-row (&optional pos)
  "The row position of cursort in current window"
  (interactive)
  (cdr (cliphist--posn-col-row (posn-at-point pos))))

(defun cliphist-create-stripped-summary (str)
  (cliphist-create-summary (cliphist--strip str)))

(defun cliphist-create-summary (stripped)
  (let* ((summary-width (- (frame-width) 3)) ; "summary.."
         (rlt (substring-no-properties stripped 0 (min (length stripped) summary-width)))
         ;; friendly hint in summary that actual value is longer
         (need-hint (< (length rlt) (length stripped))))
    ;; remove cr&lf inside summary
    (setq rlt (replace-regexp-in-string "[ \t\n]+" " " rlt))
    (if need-hint (setq rlt (concat rlt "..")))
    rlt))

(defun cliphist-add-item-to-cache (item-list str)
  (let* ((stripped (cliphist--strip str)))
    ;; don't paste item containing only white spaces
    (if (> (length stripped) 0)
        (add-to-list item-list str t))))

;;;###autoload
(defun cliphist-version ()
  (message "0.5.6"))

;;;###autoload
(defun cliphist-read-items ()
  (interactive)
  (let* (rlt i)
    (cond
     ((eq system-type 'darwin)
      ;; if nothing in clipboard, avoid purging the cache in Emacs
      (if (setq rlt (cliphist-flycut-read-items 'cliphist-add-item-to-cache))
          (setq cliphist-items rlt)))
     ((or (eq system-type 'gnu/linux) (eq system-type 'linux))
      ;; if nothing in clipboard, avoid purging the cache in Emacs
      (setq i 0)
      (while (and (not rlt)
                  (< i (length cliphist-linux-clipboard-managers)))
        (setq rlt (funcall (intern (format "cliphist-%s-read-items"
                                           (nth i cliphist-linux-clipboard-managers)))
                           'cliphist-add-item-to-cache))
        (setq i (+ 1 i)))
      (if rlt (setq cliphist-items rlt)))
     (t (message "Sorry, only Linux and Mac are supported.")))
    rlt))

(defmacro cliphist-do-item (num fn)
  "Select a item and do something.  Utility used by other commands.
FN do the thing."
  `(let (pseudo-height)
    (cliphist-read-items)
    (cond
     ((and cliphist-items (> (length cliphist-items) 0))
      (let* ((ivy-format-function (lambda (cands)
                                    (ivy--format-function-generic
                                     (lambda (str)
                                       (ivy--add-face (cliphist-create-stripped-summary str) 'ivy-current-match))
                                     #'cliphist-create-stripped-summary
                                     cands
                                     "\n"))))
        (ivy-read "Clipboard items:"
                  cliphist-items
                  :action (lambda (item)
                            (funcall ,fn ,num item)
                            (if cliphist-cc-kill-ring (kill-new item))))))
     (t
      (message "Nothing in clipboard yet!")))))

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
             (if (file-executable-p "xsel")
                 (call-process-region (point-min) (point-max) "xsel" nil nil nil "--clipboard" "--input")
               (call-process-region (point-min) (point-max) "xclip" nil nil nil "-selection clipboard" "--input"))
             ))
          (t
           (error "Clipboard support not available")))
       (error
        (error "Clipboard support not available"))))))

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
NUM is passed to `cliphist-select-item-callback'."
  (interactive "P")
  (cliphist-do-item num (lambda (num str)
                          ;; (message "num=%s str=%s" num str)
                          (if cliphist-select-item-callback
                              (funcall cliphist-select-item-callback num str)
                           (cliphist-copy-to-clipboard str)))))

(provide 'cliphist)
;;; cliphist.el ends here
