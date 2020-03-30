;; (define-key input-decode-map (kbd "\e[1;2A]") [S-up])
;; (define-key input-decode-map (kbd "\e[1;3A]") [M-up])
;; (define-key input-decode-map (kbd "\e[1;4A]") [S-M-up])
(define-key input-decode-map (kbd "\e[1;5A]") [C-up])
;; (define-key input-decode-map (kbd "\e[1;6A]") [S-C-up])

;; (define-key input-decode-map (kbd "\e[1;2B]") [S-down])
;; (define-key input-decode-map (kbd "\e[1;3B]") [M-down])
;; (define-key input-decode-map (kbd "\e[1;4B]") [S-M-down])
(define-key input-decode-map (kbd "\e[1;5B]") [C-down])
;; (define-key input-decode-map (kbd "\e[1;6B]") [S-C-down])

;; (define-key input-decode-map (kbd "\e[1;2C]") [S-right])
;; (define-key input-decode-map (kbd "\e[1;3C]") [M-right])
;; (define-key input-decode-map (kbd "\e[1;4C]") [S-M-right])
(define-key input-decode-map (kbd "\e[1;5C]") [C-right])
;; (define-key input-decode-map (kbd "\e[1;6C]") [S-C-right])

;; (define-key input-decode-map (kbd "\e[1;2D]") [S-left])
;; (define-key input-decode-map (kbd "\e[1;3D]") [M-left])
;; (define-key input-decode-map (kbd "\e[1;4D]") [S-M-left])
(define-key input-decode-map (kbd "\e[1;5D]") [C-left])
;; (define-key input-decode-map (kbd "\e[1;6D]") [S-C-left])

(define-key input-decode-map "\e[1;3P" [M-f1])
(define-key input-decode-map "\e[17;2~" [S-f6])
(define-key input-decode-map "\e[18;3~" [M-f7])
(define-key input-decode-map "\e[19;5~" [C-f8])

;; maybe I'll regret this one day: make M-{h,j,k,l} be interpreted as {left,down,up,right}
(define-key input-decode-map (kbd "M-h") [left])
(define-key input-decode-map (kbd "M-j") [down])
(define-key input-decode-map (kbd "M-k") [up])
(define-key input-decode-map (kbd "M-l") [right])

;; Control + F{8,12} for tmux + iterm2
(define-key input-decode-map "\e[19~" [f8])
(define-key input-decode-map "\e[20~" [f9])
(define-key input-decode-map "\e[21~" [f10])
(define-key input-decode-map "\e[23~" [f11])
(define-key input-decode-map "\e[24~" [f11])
