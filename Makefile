all: submodules subtrees

submodules:
	git submodule update --init

subtrees:
	-git remote add yaml-mode https://github.com/yoshiki/yaml-mode.git
	-git remote add switch-window https://github.com/dimitri/switch-window.git
	-git remote add magit https://github.com/magit/magit.git
	-git remote add git-modes https://github.com/magit/git-modes.git
	-git remote add with-editor https://github.com/magit/with-editor.git
	-git remote add auto-complete https://github.com/auto-complete/auto-complete.git

.PHONY: all submodules subtrees
