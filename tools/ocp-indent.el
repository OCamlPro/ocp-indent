;; Eval this file to automatically use ocp-indent on tuareg buffers
;;
;; this is a very simple binding that is not recommended for big files at the
;; moment...

(require 'cl)

(defun ocp-indent-region (start end)
  (interactive nil)
  (let*
      ((pos (point))
       (start-line (line-number-at-pos start))
       (end-line (line-number-at-pos end))
       (cmd (format "ocp-indent --numeric -l %d-%d" start-line end-line))
       (text (buffer-substring (point-min) (point-max)))) ;; todo: only copy from top of phrase
    (let
        ((indents
          (with-temp-buffer
            (insert text)
            (shell-command-on-region (point-min) (point-max) cmd t t
                                     shell-command-default-error-buffer t)
            (mapcar 'string-to-number (split-string (buffer-string) "\n")))))
      (save-excursion
        (mapcar* '(lambda (line indent) (goto-line line) (indent-line-to indent))
                 (number-sequence start-line end-line)
                 indents))
      (when (tuareg-in-indentation-p) (back-to-indentation)))))

(defun ocp-indent-line ()
  (interactive nil)
  (if (and (save-excursion (skip-chars-backward " \t") (bolp))
           (save-excursion (skip-chars-forward " \t") (eolp)))
      ;; a hack to indent the cursor if we are on an empty line
      (progn
        (insert "x")
        (ocp-indent-region (point) (point))
        (delete-char -1))
    (ocp-indent-region (point) (point))))

(defun ocp-setup-indent ()
  (interactive nil)
  (set (make-local-variable 'indent-line-function) #'ocp-indent-line)
  (set (make-local-variable 'indent-region-function) #'ocp-indent-region))

(add-hook 'tuareg-mode-hook 'ocp-setup-indent t)
