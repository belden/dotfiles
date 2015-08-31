(require 'cl)

;; cycle among these themes easily using 'C-c C-t'
(setq cycle-themes-theme-list
      '(wombat ample wheatgrass ample-zen afternoon clues))
(require 'cycle-themes)
(cycle-themes-mode)
;; (load-theme 'wombat 't 'nil)   ;; load and enable this theme
;; (load-theme 'wheatgrass 't 't) ;; just load this one
;; (loop for p in cycle-themes-theme-list
;;       (load-theme p 't 't))
;; (load-theme (car cycle-themes-theme-list) 't 'nil)

(defun belden/look-and-feel/load-themes-yo! (l)
  (while l
    (load-theme (car l) 't 't)
    (setq l (cdr l))))

(belden/look-and-feel/load-themes-yo! cycle-themes-theme-list)

;; allow undoing of window splitting
(require 'winner)
(winner-mode 1)
(global-set-key (kbd "C-c <left>") 'winner-undo)
(global-set-key (kbd "C-c M-h") 'winner-undo)

;; colorize compilation buffers, I've wanted this for so long!
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


