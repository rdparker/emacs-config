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
	    git bzr pull $(basename "$2")
	    ;;
    esac
}

bzrit() {
    if [ -n "$2" ]; then
	dir=$2
    else
	dir=$(basename "$1" | sed 's/^lp://')
    fi

    echo $1
    if cd $dir 2>/dev/null; then
	bzr update
	cd ..
    else
	bzr branch "$@"
    fi
}

gitit() {
    if [ -n "$2" ]; then
	dir=$2
    else
	dir=$(basename "$1" .git)
    fi

    echo $1
    if cd $dir 2>/dev/null; then
	update $SUBCMD "$@"
	cd ..
    else
	git $SUBCMD clone "$@"
    fi
}

gitbzrit() {
    SUBCMD=bzr gitit "$@"
}

install_bazaar() {
    case $(uname -r) in
	*.el5.*)
	    $ADMINPROG rpm -Uvh http://dl.fedoraproject.org/pub/epel/5/i386/epel-release-5-4.noarch.rpm
	    ;;
    esac

    installpkg bzr
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
    if [ $retval -eq 1 ]
    then
	echo Error running \"$_\" >&2
	exit 2
    fi
}

isprog ssh || installpkg ssh || true
isprog ssh || installpkg openssh
test -n "$SSH_AGENT_PID" || eval `ssh-agent`

isprog git || installpkg git || true
isprog git || installpkg git-core

qmkdir ~/.dotfiles
cd ~/.dotfiles

gitit git://github.com/rdparker/emacs-config.git
ln -sf .dotfiles/emacs-config/.emacs ~

qmkdir ~/src
cd ~/src

gitit git://github.com/termie/git-bzr-ng.git
qmkdir ~/bin
ln -sf ../src/git-bzr-ng/git-bzr ~/bin
if ! isprog git-bzr; then
    PATH=~/bin:$PATH
    export PATH
    echo WARNING:  Add ~/bin to your PATH for git-bzr. >&2
fi

qmkdir -p ~/lib/lisp/el

isprog bzr || install_bazaar
[ -e ~/.bazaar/plugins/bzr-fastimport ] && rm ~/.bazaar/plugins/bzr-fastimport
if bzr fast-import 2>&1 | grep -q "unknown command"; then
    installpkg bzr-fastimport
fi
if bzr fast-import 2>&1 | grep -q "unknown command"; then
    bzrit lp:bzr-fastimport fastimport
    qmkdir -p ~/.bazaar/plugins
    ln -sf ~/src/fastimport ~/.bazaar/plugins
fi

cd ~/lib/lisp/el
# Remove cedit if it is corrupt or was not checked out with git-bzr
[ -d cedet ] && [ ! -d cedet/.git/bzr -o ! -f cedet/.git/bzr/map/master.git ] \
    && rm -rf cedet
gitbzrit bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk cedet
if isprog make; then
    (cd cedet && find -name Makefile -exec touch '{}' \; && make)
else
    isprog emacs || installpkg emacs
    emacs -q --no-site-file -l cedet-build.el -f cedet-build.el
fi