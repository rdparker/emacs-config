MACROS=`find site-lisp -name '*-mac.el'`
SRC=`find site-lisp -name '*.el' ! -name '*-mac.el'`

all: subtrees byte-compile

# Emacs' batch mode is not used here so that peeve is properly
# configured and the .elc files are generated in
# Emacs-version-specific directories.
byte-compile:
	emacs --eval \
	      "(progn \
		 (dolist (pkg '(${MACROS}) nil) \
		   (byte-compile-file (symbol-name pkg))) \
		 (kill-emacs))"
	emacs --eval \
	      "(progn \
		 (dolist (pkg '(${SRC}) nil) \
		   (byte-compile-file (symbol-name pkg))) \
		 (kill-emacs))"

subtrees:
	-git remote add use-package https://github.com/jwiegley/use-package.git
	-git remote add yasnippet https://github.com/joaotavora/yasnippet.git
	-git remote add yasnippet-snippets https://github.com/AndreaCrotti/yasnippet-snippets.git
	-git remote add prettier-js https://github.com/prettier/prettier.git
	-git remote add add-node-modules-path https://github.com/codesuki/add-node-modules-path.git
	-git remote add peeve https://github.com/rdparker/peeve.git
	-git remote add packed https://github.com/emacscollective/packed.git
	-git remote add auto-compile https://github.com/emacscollective/auto-compile.git
