(define-minor-mode belden/hotkeys-mode
  "Bind F* keys like Belden likes"
  :lighter " 貝"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<f4>") 'belden-follow)
	    (define-key map (kbd "<f5>") 'compile)  ;; see below for treatment of 'compile-command in 'cperl-mode-hook
	    (define-key map (kbd "<f6>") 'belden/next-error-recenter)
	    (define-key map (kbd "<S-f6>") 'belden/previous-error-recenter)
	    (define-key map (kbd "<f7>") 'belden/hotkeys-mode/delete-or-restore-other-windows-vertically)
	    (define-key map (kbd "<M-f7>") 'scott-window-mdi-maximize-restore-toggle)
	    (define-key map (kbd "<f8>") 'delete-window)
	    (define-key map (kbd "<C-f8>") 'nav-toggle)
	    (define-key map (kbd "<f9>") 'belden-perl-debug)
	    (define-key map (kbd "<f10>") 'belden/hotkeys-mode/comment-dwim)
	    (define-key map (kbd "<f11>") 'other-window)
	    (define-key map (kbd "<f12>") 'font-lock-mode)
	    map))

;; <M-f1>
(defun belden/menu-bar-open ()
  "toggle the menu bar on and activate it"
  (interactive)
  (progn
    (menu-bar-mode 1)
    (menu-bar-open)
    ))
(defadvice belden/menu-bar-open (after belden-deactivate-menu activate)
  "close the menu bar after it gets opened"
  (menu-bar-mode -1))

;; for <f5>
(defun belden/hotkeys-mode/cperl-compilation ()
  (set (make-local-variable 'compile-command)
       (concat "perl " (buffer-file-name))))
(add-hook 'cperl-mode-hook 'belden/hotkeys-mode/cperl-compilation)

;; <f7>
(defun belden/hotkeys-mode/delete-or-restore-other-windows-vertically ()
  "Delete other windows above and below this window. If the last action was to delete other windows, then restore deleted ones."
  (interactive)
  (if (eq last-command 'belden/hotkeys-mode/delete-or-restore-other-windows-vertically)
      (jump-to-register 'B)
    (progn
      (window-configuration-to-register 'B)
      (delete-other-windows-vertically)
      )))

;; for <M-f7>
(load "scott-window.el")

;; for <C-f8>
(setq nav-disable-overager-window-splitting t)
(require 'nav)

;; for <f10>
(defun belden/hotkeys-mode/comment-dwim ()
  "Comment the currently selected region. If no region is selected, comment the current line and advance to the next line."
  (interactive)
  (if (not mark-active)
      (progn
	(comment-region (line-beginning-position) (line-end-position))
	(forward-line 1)
	)
    (comment-dwim nil)))

(provide 'belden/hotkeys-mode)
