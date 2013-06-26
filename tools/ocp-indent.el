;; Eval this file to automatically use ocp-indent on caml/tuareg buffers
;;
;; this is a very simple binding that is not recommended for big files at the
;; moment...

(require 'cl)

(defgroup ocp-indent nil
  "ocp-indent OCaml indenter binding configuration"
  :group 'languages)

(defcustom ocp-indent-path "ocp-indent"
  "*Path to access the ocp-indent command"
  :group 'ocp-indent :type '(file))

(defcustom ocp-indent-config nil
  "*Ocp-indent config string, as for its --config option.
WARNING: DEPRECATED, this will override any user or project
ocp-indent configuration files"
  :group 'ocp-indent
  :type '(choice (const nil) (string)))

(defcustom ocp-indent-syntax nil
  "*Enabled syntax extensions for ocp-indent (see option --syntax)"
  :group 'ocp-indent
  :type '(repeat (string)))

(defcustom ocp-indent-allow_tabs nil
  "*Allow indent-tabs-mode in ocaml buffers. Not recommended, won't work well."
  :group 'ocp-indent
  :type '(bool))

(defun ocp-in-indentation-p ()
  "Tests whether all characters between beginning of line and point
are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun ocp-indent-command (start-line end-line)
  (let ((prg (executable-find ocp-indent-path)))
    (if prg
        (format
         "%s --numeric %s%s--lines %d-%d"
         prg
         (if ocp-indent-config (format "--config %S " ocp-indent-config) "")
         (reduce
          (lambda (acc syn) (format "%s--syntax %S " acc syn))
          ocp-indent-syntax
          :initial-value "")
         start-line end-line)
      (error "Can't indent: program %S not found" ocp-indent-path))))

(defun ocp-indent-region (start end)
  (interactive "r")
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
            (mapcar 'string-to-number (split-string (buffer-string))))))
      (save-excursion
        (goto-char start)
        (mapcar
         #'(lambda (indent) (indent-line-to indent) (forward-line))
         indents))
      (when (ocp-in-indentation-p) (back-to-indentation)))))

(defun ocp-indent-line ()
  (interactive nil)
  (ocp-indent-region (point) (point)))

(defun ocp-setup-indent ()
  (interactive nil)
  (unless ocp-indent-allow-tabs (set indent-tabs-mode nil))
  (set (make-local-variable 'indent-line-function) #'ocp-indent-line)
  (set (make-local-variable 'indent-region-function) #'ocp-indent-region))

(add-hook 'tuareg-mode-hook 'ocp-setup-indent t)
(add-hook
 'caml-mode-hook
 '(lambda ()
    (ocp-setup-indent)
    (local-unset-key "\t")) ;; caml-mode rebinds TAB !
 t)
