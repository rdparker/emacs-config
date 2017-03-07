all: submodules subtrees

submodules:
	git submodule update --init

subtrees:
	-git remote add yaml-mode https://github.com/yoshiki/yaml-mode.git
	-git remote add switch-window https://github.com/dimitri/switch-window

.PHONY: all submodules subtrees
