;;; cliphist.el --- Read clipboard history from Parcellite on Linux and Flycut on OS X

;; Copyright (C) 2015 Chen Bin
;;
;; Version: 0.3.1
;; Package-Requires: ((popup "0.5.0"))
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
;;   By default, we use popup.el to provides candidates window.
;;   But you can use ivy-mode instead by installing Swiper and
;;   `(setq cliphist-use-ivy t)'.
;;
;;   You can customize the behavior of cliphist-select-item,
;;     (setq cliphist-select-item-callback
;;        (lambda (num str) (cliphist-copy-to-clipboard str)))
;;
;;   If `cliphist-cc-kill-ring' is true, the selected/pasted string
;;   will be inserted into kill-ring

;;; Code:

(require 'popup)

(defvar cliphist-cc-kill-ring nil
  "Copy the selected/pasted item into kill ring.")

(defvar cliphist-popup-max-height 9
  "Maximum height of candidates popup.")

(defvar cliphist-item-summary-string-maxlength 32
  "Maximum string length of item summary displayed in popup menu.
If ivy-mode is used, this flag is ignored.")

(defvar cliphist-select-item-callback nil
  "The callback of `cliphist-select-item'.
If nil, selected item is copied to clipboard when `cliphist-select-item' called.
Or else the `(funcall cliphist-select-item num item)' will be executed.")

(defvar cliphist-use-ivy nil
  "Use ivy-mode to display items.  Swiper7+ required.")

(defvar cliphist-items nil
  "Item list extracted from clipboard manager.  Internal variable.")

(autoload 'cliphist-flycut-read-items "cliphist-flycut" nil)
(autoload 'cliphist-parcellite-read-items "cliphist-parcellite" nil)

(defun cliphist--ivy-usable ()
  (and cliphist-use-ivy (fboundp 'ivy-read)))

(defun cliphist--posn-col-row (posn)
  (let ((col (car (posn-col-row posn)))
        ;; `posn-col-row' doesn't work well with lines of different height.
        ;; `posn-actual-col-row' doesn't handle multiple-width characters.
        (row (cdr (posn-actual-col-row posn))))
    (when (and header-line-format (version< emacs-version "24.3.93.3"))
      ;; http://debbugs.gnu.org/18384
      (cl-decf row))
    (cons (+ col (window-hscroll)) row)))

(defun cliphist-row (&optional pos)
  "The row position of cursort in current window"
  (interactive)
  (cdr (cliphist--posn-col-row (posn-at-point pos))))

(defun cliphist-optimized-popup-height ()
  "Calculate the appropriate tooltip height."
  (let* ((lines (cliphist-row))
         (items-length (length cliphist-items))
         (ideal-height (min cliphist-popup-max-height items-length))
         (window-height (if (fboundp 'window-screen-lines)
                            (floor (window-screen-lines))
                          (window-body-height)))
         (below (- window-height 1 lines)))
    (if (and (< below ideal-height)
             (> lines below))
        (- (min lines ideal-height))
      (min below ideal-height))))

(defun cliphist-create-summary (stripped)
  (let (rlt need-hint summary-width)
    (setq summary-width
          (if (cliphist--ivy-usable)
              ;; width of mini-buffer
              (- (frame-width) 3) ; "summary.."
            ;; user defined width
            cliphist-item-summary-string-maxlength))
    (setq rlt (substring-no-properties stripped
                                       0 (min (length stripped) summary-width)))
    ;; friendly hint in summary that actual value is longer
    (setq need-hint (< (length rlt) (length stripped)))
    ;; remove cr&lf inside summary
    (setq rlt (replace-regexp-in-string "[ \t\n]+" " " rlt))
    (if need-hint (setq rlt (concat rlt "..")))
    rlt))

(defun cliphist-popup-position-above-point (height)
  "Height is negative"
  (let (rlt
        (lines-backward (abs height)))
    (save-excursion
      (forward-line (- (1+ lines-backward)))
      (setq rlt (point)))
    rlt))

(defun cliphist-add-item-to-cache (item-list str)
  (let (stripped name)
    ;; trim the summary
    (setq stripped (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+$\\)" "" str))
    ;; don't paste item containing only white spaces
    (when (> (length stripped) 0)
      (add-to-list item-list
                   (if (cliphist--ivy-usable)
                       (cons (cliphist-create-summary stripped) str)
                       (popup-make-item (cliphist-create-summary stripped) :value str))
                   t)
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

(defmacro cliphist-do-item (num fn)
  "Select a item and do something.  Utility used by other commands.
FN do the thing."
  `(let (pseudo-height)
    (cliphist-read-items)
    (cond
     ((and cliphist-items (> (length cliphist-items) 0))
      (cond
       ((cliphist--ivy-usable)
        (ivy-read "Clipboard items:"
                  cliphist-items
                  :action (lambda (item)
                                  (funcall ,fn ,num item)
                                  (if cliphist-cc-kill-ring (kill-new item)))))
       (t
        (setq pseudo-height (cliphist-optimized-popup-height))
        (let ((selected-item
               (popup-menu* cliphist-items
                            :point (if (>= pseudo-height 0) nil (cliphist-popup-position-above-point pseudo-height))
                            ;; popup.el bug, when there is N lines above to show the popup
                            ;; the actual height must be N-1
                            :height (abs pseudo-height)
                            ;; enable search by default
                            :isearch t)))
          (when selected-item
            (funcall ,fn ,num selected-item)
            (if cliphist-cc-kill-ring (kill-new selected-item)))
          ))))
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
  (cliphist-do-item 1 (lambda (num str)
                        ;; evil-mode?
                        (when (and (functionp 'evil-normal-state-p)
                                   (functionp 'evil-move-cursor-back)
                                   (evil-normal-state-p)
                                   (not (eolp))
                                   (not (eobp)))
                          (forward-char))
                        ;; insert now
                        (insert str))))

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