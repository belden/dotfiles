(provide 'belden-console)

;; make C-q send s-a
(define-key function-key-map (kbd "C-q") nil)
(define-key function-key-map (kbd "C-q") (kbd "s-a"))

;; this is just convenient: M-] does a blink-paren
(global-set-key (kbd "\e]") 'belden-goto-matching-char)             ;; M-] = blink paren

;; iterm+screen
(global-set-key (kbd "\e[A") '(lambda () (interactive) (previous-line 4))) ;; ctrl+up    = go up by 5 lines
(global-set-key (kbd "\e[B") '(lambda () (interactive) (next-line 4)))     ;; ctrl+down  = down by 5 lines
(global-set-key (kbd "\e[1;5C") 'belden-forward-word)                   ;; ctrl+right = next word
(global-set-key (kbd "\e[1;5D") 'belden-backward-word)                  ;; ctrl+left  = prev word
(global-set-key (kbd "\e[1;10A") 'belden-scootch-up-or-find-win-up)
(global-set-key (kbd "\e[1;10B") 'belden-scootch-down-or-find-win-down)
(global-set-key (kbd "\e[1;10C") 'belden-scootch-right-or-find-win-right)
(global-set-key (kbd "\e[1;10D") 'belden-scootch-left-or-find-win-left)

;; Add vim-style navigation keys: M-{h,j,k,l} do {left,down,up,right}
(global-set-key (kbd "\eh") 'backward-char)
(global-set-key (kbd "\ej") 'next-line)
(global-set-key (kbd "\ek") 'previous-line)
(global-set-key (kbd "\el") 'forward-char)
(global-set-key (kbd "\eH") 'belden-backward-word)
(global-set-key (kbd "\eJ") '(lambda () (interactive) (next-line 4)))
(global-set-key (kbd "\eK") '(lambda () (interactive) (previous-line 4)))
(global-set-key (kbd "\eL") 'belden-forward-word)

;; these are control/alt arrow bindings for gnome-terminal within a screen session
;; control+{up,down,right,left}
(global-set-key (kbd "\e[1;3A") 'belden-goto-matching-char)             ;; alt+up = blink paren
(global-set-key (kbd "\e[1;3B") 'belden-goto-matching-char)             ;; alt+up = blink paren
(global-set-key (kbd "\e[1;4A") 'belden-scootch-up-or-find-win-up)
(global-set-key (kbd "\e[1;4B") 'belden-scootch-down-or-find-win-down)
(global-set-key (kbd "\e[1;4C") 'belden-scootch-right-or-find-win-right)
(global-set-key (kbd "\e[1;4D") 'belden-scootch-left-or-find-win-left)
(global-set-key (kbd "\e[1;5A") '(lambda () (interactive) (previous-line 4))) ;; ctrl+up    = go up by 5 lines
(global-set-key (kbd "\e[1;5B") '(lambda () (interactive) (next-line 4)))     ;; ctrl+down  = down by 5 lines
(global-set-key (kbd "\e[1;5C") 'belden-forward-word)                   ;; ctrl+right = next word
(global-set-key (kbd "\e[1;5D") 'belden-backward-word)                  ;; ctrl+left  = prev word

;; (global-set-key "\C-q " 'set-mark-command)                                    ;; ctrl-q space = highlight
;; (global-set-key "\C-q'" 'belden-toggle-kbd-macro-recording)             ;; ctrl-q '   = start/stop recording macro
;; (global-set-key "\C-qpd" 'cperl-perldoc)
;; (global-set-key "\C-qub" 'belden-update-buffers)

