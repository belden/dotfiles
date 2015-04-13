all:
	@bin/deploy

emacs23:
	rsync -zar emacs/emacs ~/.emacs
	rsync -zar emacs/elisp/ ~/.elisp/
