(provide 'belden-editor-toggles)

(setq belden-toggle-server-piddle 't)
(defun belden-toggle-server ()
	"Toggle the emacs server on and off; this affects whether the command-line tool 'emacsclient' is able to do anything."
	(interactive)
	(if (equal belden-toggle-server-piddle t)
			(server-force-delete)
		  (server-start))
	(setq belden-toggle-server-piddle (not belden-toggle-server-piddle)))


(setq belden-toggle-hard-tabs-piddle 't)
(defun belden-toggle-tabs ()
	"Toggle between hard and soft tabs; the default is hard tabs."
	(interactive)
	(setq belden-toggle-hard-tabs-piddle (not belden-toggle-hard-tabs-piddle))
	(setq-default indent-tabs-mode belden-toggle-hard-tabs-piddle)
  (if (equal belden-toggle-hard-tabs-piddle t) (message "<tab> sends hard tabs") (message "<tab> is nice and soft")))

(setq belden-toggle-scroll-bars-piddle 't)
(defun belden-toggle-scroll-bars ()
	"Toggle scrollbars on and off; default is on."
	(interactive)
	(setq belden-toggle-scroll-bars-piddle (not belden-toggle-scroll-bars-piddle))
	(scroll-bar-mode belden-toggle-scroll-bars-piddle))

;; (defun belden-toggle-whitespace)
