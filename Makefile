all: subtrees byte-compile

byte-compile:
	(find site-lisp/* -name '*-mac.el'; \
         find site-lisp/* -name '*.el' ! -name '*-mac.el') \
         | sed 's/.*/-eval (byte-compile-file\\ \\"&\\")/' \
	 | (cat -; echo -e save-buffers-kill-emacs) | xargs emacs

subtrees:
	-git remote add use-package https://github.com/jwiegley/use-package.git
	-git remote add yasnippet https://github.com/joaotavora/yasnippet.git
	-git remote add yasnippet-snippets https://github.com/AndreaCrotti/yasnippet-snippets.git
	-git remote add prettier-js https://github.com/prettier/prettier.git
	-git remote add add-node-modules-path https://github.com/codesuki/add-node-modules-path.git
	-git remote add peeve https://github.com/rdparker/peeve.git
	-git remote add packed https://github.com/emacscollective/packed.git
	-git remote add auto-compile https://github.com/emacscollective/auto-compile.git
