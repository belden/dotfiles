(provide 'belden-macros)
(require 'macros)
(require 'edmacro)

(defun belden-toggle-kbd-macro-recording ()
  (interactive)
  (if defining-kbd-macro (end-kbd-macro) (start-kbd-macro 'nil)))

(defun belden-edit-kbd-macro (macro)
  (interactive "CName of macro to edit (last-kbd-macro): ")
  (if (or (not macro) (string= macro "") (string= macro "last"))
      (setq macro "last-kbd-macro"))
  (if (string= macro "last-kbd-macro")
      (edit-kbd-macro 'call-last-kbd-macro)
    (if (or (string= macro "view-lossage") (string= macro "lossage"))
        (edit-kbd-macro 'view-lossage)
      (edit-kbd-macro macro))))

(defun belden-lossage () (interactive) (view-lossage))

(defun belden-assign-to-last-kbd-macro (symbol)
  "Assign a named macro to the last keyboard macro defined."
  (interactive "CName of stored macro to save as last-macro: ")
  (or symbol (error "No keyboard macro defined"))
  (and (fboundp symbol)
       (not (stringp (symbol-function symbol)))
       (not (vectorp (symbol-function symbol)))
       (error "Function %s is already defined and not a keyboard macro"
	      symbol))
  (if (string-equal symbol "") (error "No command name given"))
  (setq last-kbd-macro (symbol-function symbol)))

