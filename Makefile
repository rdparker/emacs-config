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
	-git remote add creole-mode https://github.com/nicferrier/creole-mode.git
	-git remote add dash.el https://github.com/magnars/dash.el
	-git remote add ecb https://github.com/emacsmirror/ecb
	-git remote add el-autoyas.el https://github.com/mlf176f2/el-autoyas.el.git
	-git remote add elpy https://github.com/jorgenschaefer/elpy.git
	-git remote add emmet-mode https://github.com/smihica/emmet-mode.git
	-git remote add epl https://github.com/cask/epl.git
	-git remote add evernote-mode https://github.com/pymander/evernote-mode.git
	-git remote add find-file-in-project https://github.com/technomancy/find-file-in-project.git
	-git remote add flx https://github.com/lewang/flx.git
	-git remote add fuzzy-el https://github.com/auto-complete/fuzzy-el.git
	-git remote add graphviz-dot-mode https://github.com/ppareit/graphviz-dot-mode.git
	-git remote add helm https://github.com/rdparker/helm.git
	-git remote add helm-descbinds https://github.com/emacs-helm/helm-descbinds.git
	-git remote add emacs-helm-gtags https://github.com/syohex/emacs-helm-gtags.git
	-git remote add helm-swoop https://github.com/ShingoFukuyama/helm-swoop.git
	-git remote add highlight-symbol.el https://github.com/nschum/highlight-symbol.el.git
	-git remote add emacs-huskie https://github.com/nicferrier/emacs-huskie.git
	-git remote add iedit https://github.com/victorhge/iedit.git
	-git remote add jshint-mode https://github.com/daleharvey/jshint-mode.git
	-git remote add emacs-kv https://github.com/nicferrier/emacs-kv.git
	-git remote add lispstick-el https://github.com/rdparker/lispstick-el
	-git remote add markdown-mode https://github.com/rdparker/markdown-mode.git
	-git remote add p4.el https://github.com/gareth-rees/p4.el.git
	-git remote add paredit https://github.com/rdparker/paredit.git
	-git remote add pkg-info.el https://github.com/lunaryorn/pkg-info.el.git
	-git remote add popup-el git://github.com/auto-complete/popup-el.git
	-git remote add powerline https://github.com/milkypostman/powerline.git
	-git remote add projectile https://github.com/bbatsov/projectile.git
	-git remote add pyvenv https://github.com/jorgenschaefer/pyvenv.git
	-git remote add emacs-epackage--quilt https://github.com/rdparker/emacs-epackage--quilt.git
	-git remote add s.el https://github.com/magnars/s.el.git
	-git remote add session https://github.com/rdparker/session.git
	-git remote add shoes-off https://github.com/rdparker/shoes-off.git
	-git remote add emacs-http-server https://github.com/skeeto/emacs-http-server.git
	-git remote add skewer-mode https://github.com/skeeto/skewer-mode.git
	-git remote add smart-tab https://github.com/sroccaserra/smart-tab.git
	-git remote add use-package https://github.com/jwiegley/use-package.git
	-git remote add emacs-web https://github.com/nicferrier/emacs-web.git
	-git remote add web-beautify https://github.com/yasuyk/web-beautify.git
	-git remote add whitespace-cleanup-mode https://github.com/purcell/whitespace-cleanup-mode.git
	-git remote add window-numbering.el https://github.com/nschum/window-numbering.el.git

.PHONY: all submodules subtrees
