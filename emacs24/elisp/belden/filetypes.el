(autoload 'belden-cperl-mode "belden-cperl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|[tT]\\)\\'" . belden-cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . belden-cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . belden-cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . belden-cperl-mode))

(autoload 'mediamath-go-mode "mediamath-go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . mediamath-go-mode))
