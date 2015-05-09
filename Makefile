all: deploy tmux vim

deploy:
	@bin/deploy

emacs23:
	rsync -zar emacs/emacs ~/.emacs
	rsync -zar emacs/elisp/ ~/.elisp/

emacs24:
	rsync -zar emacs-24/emacs ~/.emacs
	rsync -zar emacs-24/elisp/ ~/.elisp24/

tmux:
	rsync -zar conf/.tmux/ ~/.tmux/

vim:
	#[ ! -d ~/.vim/bundle/Vundle.vim ] && git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall
