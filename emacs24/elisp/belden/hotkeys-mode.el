(provide 'belden/hotkeys-mode)
(define-minor-mode belden/hotkeys-mode
  "Bind F* keys like Belden likes"
  :lighter " 貝"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-#") 'belden/hotkeys-mode/comment-dwim)
	    (define-key map (kbd "<f3>") 'belden/visit-next-interesting-spot)
	    (define-key map (kbd "<f4>") 'belden-follow)
	    (define-key map (kbd "<f5>") 'compile)  ;; see below for treatment of 'compile-command in 'cperl-mode-hook
	    (define-key map (kbd "<f6>") 'belden/next-error-recenter)
	    (define-key map (kbd "<S-f6>") 'belden/previous-error-recenter)
	    (define-key map (kbd "<f7>") 'belden/hotkeys-mode/delete-or-restore-other-windows-vertically)
	    (define-key map (kbd "<C-f7>") 'belden/hotkeys-mode/force-window-restore)
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

;; <f3>
(defun belden/visit-next-interesting-spot ()
  "visit the next 'interesting' spot in a file, for some definition of 'interesting'"
  (interactive)
  (set-mark-command t))

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
      ;; todo: only do this if other windows exist vertically - then doing f7, move, f7 wouldn't blow away 'B
      (window-configuration-to-register 'B)
      (delete-other-windows-vertically)
      )))

(defun belden/hotkeys-mode/force-window-restore ()
  "really restore the last window config, whatever that means"
  (interactive)
  (jump-to-register 'B))

;; for <M-f7>
(load "scott-window.el")

;; for <C-f8>
(setq nav-disable-overager-window-splitting t)
(require 'nav)

;; for <f9>
;;;;;;;;;;;;
;; debugger customizations:
;;    show line numbers in the file that the debugger is visiting
(defadvice gud-find-file (after show-line-numbers activate compile)
  (with-current-buffer (get-file-buffer file)
    (if (string-match "^perl5db.pl\$" (buffer-name))
        'nil
      (setnu-mode 1))))

;;    turn off line numbers when we're done in the debugger
(defadvice gud-sentinel (after remove-line-numbers activate compile)
  (belden-remove-line-numbers))

;;    run the debugger on the current file
(defun belden-perl-debug (perldebug-file)
	"Run the perl debugger"
	(interactive
	 (list (read-string
					"Run perldb as: "
					(format "perl -d %s" (buffer-file-name)))))
	(perldb perldebug-file))

(defun belden-remove-line-numbers ()
  (interactive)
  (loop for buf in (buffer-list) do
        (with-current-buffer buf (if setnu-mode (setnu-mode nil)))))
;;; </f9>


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
