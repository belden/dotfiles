all: deploy tmux vim

deploy:
	@bin/deploy

emacs23:
	rsync -zar emacs/emacs ~/.emacs
	rsync -zar emacs/elisp/ ~/.elisp/

tmux:
	rsync -zar conf/.tmux/ ~/.tmux/

vim:
	vim +PluginInstall +qall
