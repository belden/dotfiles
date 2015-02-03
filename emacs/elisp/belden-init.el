(provide 'belden-init)

(if (window-system) (belden-default-font-tiny)) ;; set a smaller font if we can

;; cperl-mode for horrible mason files
(global-set-key [(super a) ?c ?p] 'cperl-mode)

;; set "s-a ts" to toggle the emacs server on and off
(global-set-key [(super a) ?t ?s] 'belden-toggle-server)
(setq belden-toggle-server-piddle 't)
(defun belden-toggle-server ()
	"Toggle the emacs server on and off"
	(interactive)
	(if (equal belden-toggle-server-piddle t)
			(server-force-delete)
		  (server-start))
	(setq belden-toggle-server-piddle (not belden-toggle-server-piddle)))

;; (belden-default-font-tiny)

;; "s-a sp" will set the project
;; (global-set-key [(super a) ?s ?p] 'belden-set-project)
;; (defun belden-set-project (root)
;; 	(interactive "Droot: ")
;; 	(setenv "code_root" root))

;; buffer movement - term-mode sets these up, and I'd like to get in the habit of using them.
;; (global-set-key (kbd "C-c b") 'iswitchb-buffer)
;; (global-set-key (kbd "C-c C-b") 'electric-buffer-list)

;; C-q f11, s-f11 - make this window big/small
;; (global-set-key (kbd "C-q <f11>") 'scott-window-mdi-maximize-restore-toggle)
;; (global-set-key (kbd "s-<f11>") 'scott-window-mdi-maximize-restore-toggle)

(defun belden/add-a-block ()
  "Insert a code block, i.e. { } move point to the first line inside the block"
	(interactive)
	(insert " {\n}")
	(previous-line)
	(end-of-line)
	(insert "\n  "))
(global-unset-key "\M-{")
(global-set-key "\M-{" 'belden/add-a-block)

(require 'remember)
(defun belden/remember ()
	"remember selected text, or open the remember editor. See `M-x info M-m remember' for remember.el info"
	(interactive)
	(if mark-active (remember-region) (remember)))
(global-set-key (kbd "s-a r r") 'belden/remember)

;; M-S-w M-S-{h,j,k,l} do scootch-or-move-window-{left,down,up,right}
(global-set-key "\M-W\M-h" 'belden-scootch-left-or-find-win-left)
(global-set-key "\M-W\M-j" 'belden-scootch-down-or-find-win-down)
(global-set-key "\M-W\M-k" 'belden-scootch-up-or-find-win-up)
(global-set-key "\M-W\M-l" 'belden-scootch-right-or-find-win-right)

(defun belden/move-comment ()
	"move a comment to the next line, eg:

        // foo
        if err != nil {
          ...
        }
        
   to

        if err != nil { // foo
          ...
        }

  "
        
	(interactive) 
	(belden-kill-whole-line) 
	(next-line) 
	(previous-line) 
	(end-of-line) 
	(insert " ") 
	(yank) 
	(delete-char 1))
(global-set-key (kbd "s-a m c") 'belden/move-comment)

(global-set-key "\C-xra" 'append-to-register)          ;; C-x ra : append-to-register
(global-set-key "\C-xK" 'belden-kill-whole-line) ;; C-x K  : kill whole line

(defun belden/compile-thingy (belden/compile-command)
	"compile a thingy"
	(interactive
	 (list (read-string
					"Compile using: "
					(format "gosh %s" (buffer-file-name)))))
	(compile belden/compile-command))
