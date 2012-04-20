#! /bin/sh
isprog() {
    which "$1" >/dev/null 2>&1
}

qmkdir() {
    mkdir "$@" 2>/dev/null || true
}

installpkg() {
    $ADMINPROG $PKGPROG install "$@"
}

update() {
    case "$1" in
	"")
	    git fetch
	    git merge --ff-only origin/master || true
	    ;;

	bzr)
	    git bzr pull
	    ;;
    esac
}

gitit() {
    if [ -n "$2" ]; then
	dir=$2
    else
	dir=$(basename "$1" .git)
    fi

    echo $1
    if cd $dir 2>/dev/null; then
	update $SUBCMD
	cd ..
    else
	git $SUBCMD clone "$@"
    fi
}

bzrit() {
    SUBCMD=bzr gitit "$@"
}

for prog in aptitude yum pkg; do
    if isprog $prog; then
	PKGPROG=$prog
	break
    fi
done
if [ -z "$PKGPROG" ]; then
    uname -a >&2
    echo "Could not find a package manager for this system." >&2
    exit 1
fi

for prog in sudo su; do
    if isprog $prog; then
	ADMINPROG=$prog
	break
    fi
done

# From this point on, exit on error
set -e

# and report the error
trap cleanup EXIT
cleanup() {
    retval=$?
    trap - EXIT
    if [ $retval -eq 1 ]; then
	echo Error running \"$_\" >&2
	exit 2
    fi
}

isprog git || installpkg git || true
isprog git || installpkg git-core

qmkdir ~/.dotfiles
cd ~/.dotfiles

gitit git://github.com/rdparker/emacs-config.git
ln -sf .dotfiles/emacs-config/.emacs ~

qmkdir ~/src
cd ~/src

gitit git://github.com/termie/git-bzr-ng.git
ln -sf ../src/git-bzr-ng/git-bzr ~/bin

qmkdir -p ~/lib/lisp/el
cd ~/lib/lisp/el

bzrit bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk cedet
if isprog make; then
    (cd cedet && find -name Makefile -exec touch '{}' \; && make)
else
    isprog emacs || installpkg emacs
    emacs -q --no-site-file -l cedet-build.el -f cedet-build.el
fi