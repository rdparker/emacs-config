all: submodules subtrees

submodules:
	git submodule update --init --recursive --recommend-shallow

subtrees:
	-git remote add yaml-mode https://github.com/yoshiki/yaml-mode.git
	-git remote add switch-window https://github.com/dimitri/switch-window.git
	-git remote add magit https://github.com/magit/magit.git
	-git remote add git-modes https://github.com/magit/git-modes.git
	-git remote add with-editor https://github.com/magit/with-editor.git
	-git remote add auto-complete https://github.com/auto-complete/auto-complete.git
	-git remote add org-mode https://github.com/rdparker/org-mode.git
	-git remote add anaphora https://github.com/rolandwalker/anaphora.git
	-git remote add anything-config http://repo.or.cz/r/anything-config.git
	-git remote add applescript-mode https://github.com/rdparker/applescript-mode.git
	-git remote add c-eldoc https://github.com/nflath/c-eldoc.git

.PHONY: all submodules subtrees
