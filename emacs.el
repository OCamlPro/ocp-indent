;; Eval this file to automatically use ocp-indent on tuareg buffers
;;
;; this is a very simple binding that is not recommended for big files at the
;; moment...

(defun ocp-indent-region (start end)
  (interactive nil)
  (let*
      ((pos (point))
       (start-line (line-number-at-pos start))
       (end-line (line-number-at-pos end))
       (cmd (format "ocp-indent --numeric -l %d-%d" start-line end-line))
       (output-buffer (generate-new-buffer "*ocp-indent tmp*")))
    (shell-command-on-region (point-min) (point-max) cmd output-buffer)
    (let*
        ((ret (with-current-buffer output-buffer (buffer-string)))
         (indents (mapcar 'string-to-number (split-string ret "\n"))))
      (save-excursion
        (mapcar* '(lambda (line indent) (goto-line line) (indent-line-to indent))
                 (number-sequence start-line end-line)
                 indents))
      (when (tuareg-in-indentation-p) (back-to-indentation)))
    (kill-buffer output-buffer)))

(defun ocp-indent-line ()
  (interactive nil)
  (ocp-indent-region (point) (point)))

(defun ocp-setup-indent ()
  (interactive nil)
  (set (make-local-variable 'indent-line-function) #'ocp-indent-line)
  (set (make-local-variable 'indent-region-function) #'ocp-indent-region))

(add-hook 'tuareg-mode-hook 'ocp-setup-indent t)
