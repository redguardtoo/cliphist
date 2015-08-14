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

(defun test ()
  (interactive)
  (cliphist-read-bytes (file-truename "~/.local/share/parcellite/history"))
  )

(defun cliphist-read-items-from-history (path)
  "hexl-mode, Read binary data from PATH.
Return the binary data as unibyte string.
The format of history, first 4 bytes, specify the size of content (the little endian way) , always end with 4 byte zeroed
"
  ( with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun cliphist-get-item-content ()
  (interactive)
  (let (rlt)
    (setq rlt "hello world")
    (message "rlt=%s" rlt)
    rlt))

(defun cliphist-next-item ()
  (interactive)
  (message "cliphist-next-item called")
  )

(defun cliphist-previous-item ()
  (interactive)
  (message "cliphist-previous-item called")
  )

(provide 'cliphist)
;;; cliphist.el ends here

