(provide 'scott-window)
;;;;;;;;;;;;;;;;;;
; fancy delete-other-windows
; from: http://www.cs.berkeley.edu/~smcpeak/elisp/scott.emacs.el
(defvar my-saved-window-config-list nil)
(defun scott-window-mdi-maximize-restore-toggle ()
  "When called in a multi-window frame it will save the window
  configuration by calling `current-window-configuration', then call
  `delete-other-windows'.  When called in a single-window frame it will
  restore the frame configuration by calling `set-window-configuration'."
  (interactive)
  (if (> (count-windows) 1)
      (progn
        (gc-my-window-config-list (selected-frame))
        (setq my-saved-window-config-list
              (cons (list (buffer-name) (current-window-configuration))
                    my-saved-window-config-list))
        (delete-other-windows))
    (restore-applicable-window-configuration my-saved-window-config-list)))
(defvar saved-window-config-subwindow nil)
(make-variable-buffer-local 'saved-window-config-subwindow)
(defun maximize-restore-toggle-with-subbuffer ()
  "This will always minimize the subbuffer, not the one the point is in"
  (interactive)
  (if (> (count-windows) 1)
      (progn
        (when saved-window-config-subwindow
          (if (buffer-file-name) (save-buffer))
          (other-window 1))
        (scott-window-mdi-maximize-restore-toggle)
        )
    (progn
      (scott-window-mdi-maximize-restore-toggle)
      (when (> (count-windows) 1) ; some windows got restored
        (setq saved-window-config-subwindow 'nil)
        (other-window 1)
        (setq saved-window-config-subwindow t)))))

(defun gc-my-window-config-list (frame)
  "Remove any saved configs that apply to deleted frames or to
  the 'frame' argument."
  (setq my-saved-window-config-list
    (filter-list my-saved-window-config-list
      #'(lambda (config)
          (and
            (member (window-configuration-frame (car (cdr config))) (frame-list))
            (not (eq (window-configuration-frame (car (cdr config))) frame))
          ))
    )))
(defun restore-applicable-window-configuration (list)
  "Look through 'list' for a window config that applies to the selected
  frame.  If found, restore via that config.  If not, say so."
  (if (not list)
      (princ "There is no saved window config for this buffer.")
    (let ((bufname (car (car list)))
          (windowconfig (car (cdr (car list)))))
      (if (and (eq (window-configuration-frame windowconfig) (selected-frame))
               (eq bufname (buffer-name)))
          ; restore it
          (set-window-configuration windowconfig)
        ; else, proceed down list
        (restore-applicable-window-configuration (cdr list))))))
(defun filter-list (list predicate)
  "Return a list containing only those elements from 'list' which
  cause 'predicate' to return true."
  (if (not list)
      nil          ; recursion base case
      (if (funcall predicate (car list))
          ; keep the item
          (cons (car list) (filter-list (cdr list) predicate))
          ; else, remove it
          (filter-list (cdr list) predicate)
      )))
(defun delete-window-restorable ()
  (interactive)
  (if (eq (count-windows) 2)
      (progn
        (other-window 1)
        (scott-window-mdi-maximize-restore-toggle))
    (delete-window)))
