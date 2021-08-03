;;; cliphist-sdk.el --- SDK -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'subr-x)

(defun cliphist-sdk-create-summary (stripped)
  "Create summary from STRIPPED string."
  (let* ((summary-width (- (frame-width) 3)) ; "summary.."
         (rlt (substring-no-properties stripped 0 (min (length stripped) summary-width)))
         ;; friendly hint in summary that actual value is longer
         (need-hint (< (length rlt) (length stripped))))
    ;; remove cr&lf inside summary
    (setq rlt (replace-regexp-in-string "[ \t\n]+" " " rlt))
    (if need-hint (setq rlt (concat rlt "..")))
    rlt))

(defmacro cliphist-sdk-add-item-to-cache (item-list str)
  "Add STR into ITEM-LIST."
  `(let* ((stripped (string-trim ,str)))
     ;; don't paste item containing only white spaces or short strings
     (when (> (length stripped) 3)
       (push (cons (cliphist-sdk-create-summary stripped) ,str)
             ,item-list))))

(defun cliphist-sdk-feed-text-to-cli (text program &rest arguments)
  "Feed TEXT to cli PROGRAM with ARGUMENTS through stdin."
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min)
                         (point-max)
                         program
                         nil
                         nil
                         nil
                         arguments)))

(provide 'cliphist-sdk)
;;; cliphist-sdk.el ends here
