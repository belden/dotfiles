(require 'json)

;; js2 settings
(setq js2-mirror-mode nil)
(setq js2-mode-indent-ignore-first-tab t)
(setq js2-strict-inconsistent-return-warning nil)

;;js settings (for json)
(setq js-indent-level 2)

(defun esk-pp-json ()
  "Pretty-print the json object following point."
  (interactive)
  (let ((json-object (save-excursion (json-read))))
    (switch-to-buffer "*json*")
    (delete-region (point-min) (point-max))
    (insert (pp json-object))
    (goto-char (point-min))))

(defun nb-js-lint ()
  "Run current buffer under jslint"
  (interactive)
  (compile (format "jsl -process %s" (buffer-file-name))))

(defun nb-js-debugger ()
  "Insert a debugger breakpoint"
  (interactive)
  (save-excursion
    (end-of-line)
    (insert "\n\n// XXX\ndebugger;\n")
    (next-line -2)
    (indent-for-tab-command)
    (next-line)
    (indent-for-tab-command)))

(defun nb-js-dumper ()
  "console.log variable under point."
  (interactive)
  (let ((var (word-at-point)))
    (save-excursion
      (end-of-line)
      (insert (format "\n\n// XXX\nconsole.log(JSON.stringify(%s, null, 2));\n" var))
      (next-line -2)
      (indent-for-tab-command)
      (next-line)
      (indent-for-tab-command))))

;; nb js minor mode
(defvar nb-js-mode-map (make-sparse-keymap)
  "Bindings for nb-js-mode with prefix maps")

(define-prefix-command 'nb-js-prefix-map)
(define-key nb-js-prefix-map (kbd "d b") 'nb-js-debugger)
(define-key nb-js-prefix-map (kbd "d d") 'nb-js-dumper)

(define-key nb-js-mode-map (kbd "s-n") 'nb-js-prefix-map)
(define-key nb-js-mode-map (kbd "s-a") 'nb-js-prefix-map)

(define-key nb-js-mode-map (kbd "<f7>") 'nb-js-lint)
(define-key nb-js-mode-map (kbd "<mouse-1>") #'mouse-set-point)

(eval-after-load 'js2-mode
  '(progn
     ;; Fix js2's crazy indentation
     (setq js2-bounce-indent-flag nil
           js2-cleanup-whitespace t
           js2-indent-on-enter-key t)

     (defun js-continued-var-decl-list-p ()
       "Return non-nil if point is inside a continued variable declaration list."
       (interactive)
       (let ((start (save-excursion (js-re-search-backward "\\<var\\>" nil t))))
         (and start
              (save-excursion (re-search-backward "\n" start t))
              (not (save-excursion
                     (js-re-search-backward
                      ";\\|[^, \t][ \t]*\\(/[/*]\\|$\\)" start t))))))

     (defun js-proper-indentation (parse-status)
       "Return the proper indentation for the current line."
       (save-excursion
         (back-to-indentation)
         (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
               (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
               (continued-expr-p (js-continued-expression-p)))
           (cond (ctrl-stmt-indent)
                 ((js-continued-var-decl-list-p)
                  (js-re-search-backward "\\<var\\>" nil t)
                  (+ (current-indentation) js2-basic-offset))
                 ((nth 1 parse-status)
                  (goto-char (nth 1 parse-status))
                  (if (looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
                      (progn
                        (skip-syntax-backward " ")
                        (when (= (char-before) ?\)) (backward-list))
                        (back-to-indentation)
                        (cond (same-indent-p
                               (current-column))
                              (continued-expr-p
                               (+ (current-column) (* 2 js2-basic-offset)))
                              (t
                               (+ (current-column) js2-basic-offset))))
                    (unless same-indent-p
                      (forward-char)
                      (skip-chars-forward " \t"))
                    (current-column)))
                 (continued-expr-p js2-basic-offset)
                 (t 0)))))))

(define-minor-mode nb-js-mode
  "Minor mode for js-specific functions

Commands:
\\{nb-js-mode-map}
"
  :init-value nil
  :keymap nb-js-mode-map)

(add-hook 'js2-mode-hook
          (lambda ()
            (nb-js-mode)
            (when (local-variable-p 'before-save-hook)
              (setq before-save-hook
                    (cons 'js2-before-save (default-value 'before-save-hook))))))

(provide 'nb-js)
