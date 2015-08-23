;;; cliphist.el --- read clipboard history

;; Copyright (C) 2015 Chen Bin
;;
;; Version: 0.0.1
;; Package-Requires: ((popup "0.5.0"))
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

;; Read clipboard itemsfrom following clipboard managers,
;;   - Parcellite (http://parcellite.sourceforge.net)
;;   - Flycut (https://github.com/TermiT/Flycut)
;;
;; You only need `M-x cliphist-paste-item'

;;; Code:

(require 'popup)

(defvar cliphist-items nil
  "Item list extracted from clipboard manager's data")

(autoload 'cliphist-flycut-read-items "cliphist-flycut" nil)
(autoload 'cliphist-parcellite-read-items "cliphist-parcellite" nil)

;;;###autoload
(defun cliphist-read-items ()
  (interactive)
  (let (rlt)
    (cond
     ((eq system-type 'darwin)
      ;; if nothing in clipboard, avoid purging the cache in Emacs
      (if (setq rlt (cliphist-flycut-read-items))
          (setq cliphist-items rlt)))
     ((or (eq system-type 'gnu/linux) (eq system-type 'linux))
      ;; if nothing in clipboard, avoid purging the cache in Emacs
      (if (setq rlt (cliphist-parcellite-read-items))
          (setq cliphist-items rlt)))
     (t (message "Sorry, only Linux and OS X are supported."))
     )))

;;;###autoload
(defun cliphist-paste-item ()
  (interactive)
  (let (selected-item)
    (cliphist-read-items)
    (if (and cliphist-items
               (> (length cliphist-items) 0))
        (if (setq selected-item
                  (popup-menu* cliphist-items
                               ;; display more content
                               ;; enable search by default
                               :isearch t))
          (insert selected-item))
      (message "Nothing in clipboard yet!"))))

(provide 'cliphist)
;;; cliphist.el ends here
