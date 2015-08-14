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

;; Read from clipboard history from parcellite (http://parcellite.sourceforge.net)
;; Other clipboard manager will be supported soon.

;;; Code:

(defun cliphist-get-item-content ()
  (interactive "sEnter a string:")
  (let (rlt)
    (setq rlt "hello world")
    rlt))

(provide 'cliphist)
;;; cliphist.el ends here

