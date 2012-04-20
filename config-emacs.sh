#! /bin/sh
isprog() {
    which "$1" 2>&1 >/dev/null
}

qmkdir() {
    mkdir "$@" 2>/dev/null || true
}

installpkg() {
    $ADMINPROG $PKGPROG install "$@"
}

updateclone() {
    dir=$(basename "$1" .git)
    if cd $dir; then
	git fetch
	git merge origin/master || true
	cd ..
    else
	git clone "$1"
    fi
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

updateclone git://github.com/rdparker/emacs-config.git
ln -sf ~/.dotfiles/emacs-config/.emacs ~

qmkdir -p ~/lib/lisp/el
cd ~/lib/lisp/el