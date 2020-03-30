.PHONY: emacs24	

all: deploy tmux vim

deploy:
	@bin/deploy

emacs23:
	rsync -zar emacs/emacs $HOME/.emacs23
	ln -sf $HOME/.emacs23 ~/.emacs
	rsync -zar emacs/elisp/ $HOME/.elisp/

emacs24:
	mkdir -p $HOME/.autosaves
	rsync -zar emacs24/emacs $HOME/.emacs24
	ln -sf $HOME/.emacs24 ~/.emacs
	rsync -zar emacs24/elisp/ $HOME/.elisp24/

uninstall-emacs24:
	\rm -rf $HOME/.emacs24
	\rm -rf $HOME/.elisp24
	\rm -rf $HOME/.emacs.d

reinstall-emacs24: uninstall-emacs24 emacs24

tmux:
	rsync -zar conf/.tmux/ $HOME/.tmux/

vim:
	#[ ! -d $HOME/.vim/bundle/Vundle.vim ] && git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall
