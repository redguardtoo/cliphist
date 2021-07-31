;;; cliphist-tests.el ---  unit tests for cliphist -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; Code:

(require 'ert)
(require 'cliphist)

(ert-deftest cliphist-test-generic ()
  (let* ((cliphist-items '(("c1" . "clip1") ("c2" . "clip2")))
         (cliphist-linux-clipboard-managers nil))
    (cliphist-read-items)
    (should (eq 2 (length cliphist-items)))))

(ert-deftest cliphist-test-flycut ()
  (require 'cliphist-flycut)
  (let* ((cliphist-flycut-history-path "data/flycut/com.generalarcade.flycut.plist")
         (rlt (cliphist-flycut-read-items)))
    (should rlt)
    (should (eq (length rlt) 3))
    (should (string= (cdr (nth 0 rlt)) "calculate"))))

(ert-deftest cliphist-test-parcellite ()
  (require 'cliphist-parcellite)
  (let* ((cliphist-parcellite-history-path "data/parcellite1.0/history")
         (cliphist-parcellite-installed-p t)
         (rlt (cliphist-parcellite-read-items)))
    (should (eq (length rlt) 3))
    (should (string= (cdr (nth 0 rlt)) "bvi ~/.local/share/parcellite/history"))))

(ert-deftest cliphist-test-clipit ()
  (require 'cliphist-clipit)
  (let* ((cliphist-clipit-history-path "data/clipit/history")
         (cliphist-clipit-installed-p t)
         (rlt (cliphist-clipit-read-items)))
    (should (eq (length rlt) 5))
    (should (string= (cdr (nth 0 rlt)) " triggers"))))

(ert-run-tests-batch-and-exit)
;;; cliphist-tests.el ends here
