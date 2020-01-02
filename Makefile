all: subtrees

subtrees:
	-git remote add use-package https://github.com/jwiegley/use-package.git
	-git remote add yasnippet https://github.com/joaotavora/yasnippet.git
	-git remote add yasnippet-snippets https://github.com/AndreaCrotti/yasnippet-snippets.git
	-git remote add prettier-js https://github.com/prettier/prettier.git
	-git remote add add-node-modules-path https://github.com/codesuki/add-node-modules-path.git
	-git remote add peeve https://github.com/rdparker/peeve.git
