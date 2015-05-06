all: deploy tmux vim

deploy:
	@bin/deploy

emacs23:
	rsync -zar emacs/emacs ~/.emacs
	rsync -zar emacs/elisp/ ~/.elisp/

tmux:
	rsync -zar conf/.tmux/ ~/.tmux/

vim:
	git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall
