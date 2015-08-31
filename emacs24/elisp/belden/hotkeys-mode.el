(define-minor-mode belden/hotkeys-mode
  "Bind F* keys like Belden likes"
  :lighter " 貝"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<f4>") 'belden-follow)
	    (define-key map (kbd "<f5>") 'compile)  ;; see below for treatment of 'compile-command in 'cperl-mode-hook
	    (define-key map (kbd "<f6>") 'belden-next-error-recenter)
	    (define-key map (kbd "<f7>") 'scott-window-mdi-maximize-restore-toggle)
	    (define-key map (kbd "<f8>") 'belden-hide-this-buffer)
	    (define-key map (kbd "\e[19;5~") 'nav-toggle)            ;; C-<f8>
	    (define-key map (kbd "<f9>") 'belden-perl-debug)
	    (define-key map (kbd "<f10>") 'belden-comment-dwim)
	    (define-key map (kbd "<f11>") 'other-window)
	    (define-key map (kbd "<f12>") 'font-lock-mode)
	    map))

;; for <f5>
(add-hook 'cperl-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "perl " (buffer-file-name)))))


(load "scott-window.el") ;; for <f7>

(setq nav-disable-overager-window-splitting t)
(require 'nav) ;; for <f8>

(provide 'belden/hotkeys-mode)
