;; Eval this file to automatically use ocp-indent on tuareg buffers
;;
;; this is a very simple binding that is not recommended for big files at the
;; moment...

(require 'cl)
(require 'tuareg)

(defcustom ocp-indent-path "ocp-indent"
  "*Path to access the ocp-indent command"
  :group 'tuareg :type '(file))

(defcustom ocp-indent-config nil
  "*Ocp-indent config string, as for its --config option"
  :group 'tuareg
  :type '(choice (const nil) (string)))

(defun ocp-indent-command (start-line end-line)
  (let ((prg (executable-find ocp-indent-path)))
    (if prg
        (format
         "%s --numeric %s --lines %d-%d"
         prg
         (if ocp-indent-config (format "--config %S " ocp-indent-config) "")
         start-line end-line)
      (error "Can't indent: program %S not found" ocp-indent-path))))

(defun ocp-indent-region (start end)
  (interactive nil)
  (let*
      ((pos (point))
       (start-line (line-number-at-pos start))
       (end-line (line-number-at-pos end))
       (cmd (ocp-indent-command start-line end-line))
       (text (buffer-substring (point-min) (point-max)))) ;; todo: only copy from top of phrase
    (let
        ((indents
          (with-temp-buffer
            (insert text)
            (if (/= 0
                    (shell-command-on-region
                     (point-min) (point-max) cmd t t
                     "*ocp-indent-error*" t))
                (error "Can't indent: %s returned failure" cmd))
            (mapcar 'string-to-number (split-string (buffer-string) "\n")))))
      (save-excursion
        (mapcar* '(lambda (line indent)
                    (goto-line line)
                    (indent-line-to indent))
                 (number-sequence start-line end-line)
                 indents))
      (when (tuareg-in-indentation-p) (back-to-indentation)))))

(defun ocp-indent-line ()
  (interactive nil)
  (ocp-indent-region (point) (point)))

(defun ocp-setup-indent ()
  (interactive nil)
  (set (make-local-variable 'indent-line-function) #'ocp-indent-line)
  (set (make-local-variable 'indent-region-function) #'ocp-indent-region))

(add-hook 'tuareg-mode-hook 'ocp-setup-indent t)
