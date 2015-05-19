.PHONY: emacs24	

all: deploy tmux vim

deploy:
	@bin/deploy

emacs23:
	rsync -zar emacs/emacs ~/.emacs23
	ln -sf ~/.emacs24 ~/.emacs
	rsync -zar emacs/elisp/ ~/.elisp/

emacs24:
	mkdir -p ~/.autosaves
	rsync -zar emacs24/emacs ~/.emacs24
	ln -sf ~/.emacs24 ~/.emacs
	rsync -zar emacs24/elisp/ ~/.elisp24/

uninstall-emacs24:
	\rm -rf ~/.emacs24
	\rm -rf ~/.elisp24
	\rm -rf ~/.emacs.d

reinstall-emacs24: uninstall-emacs24 emacs24

tmux:
	rsync -zar conf/.tmux/ ~/.tmux/

vim:
	#[ ! -d ~/.vim/bundle/Vundle.vim ] && git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall
