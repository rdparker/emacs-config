#! /bin/sh
PYTHON=python
EASY_INSTALL=easy_install

isprog() {
    which "$1" >/dev/null 2>&1
}

qmkdir() {
    mkdir "$@" 2>/dev/null || true
}

installpkg() {
    $ADMINPROG $PKGPROG install "$@"
}

removepkg() {
    $ADMINPROG $PKGPROG remove "$@"
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

# bzr fast-import will require Python 2.6+ make a new enough bzr is loaded
check_bzr() {
    epelrpm=http://dl.fedoraproject.org/pub/epel/5/i386/epel-release-5-4.noarch.rpm
    if ! isprog bzr || bzr --version | grep -q 'python.2\.4'; then
	# Remove any bzr using python 2.4
	isprog bzr && removepkg bzr
	# Red Hat and CentOS 5 do not have a bzr package.  Get it from EPEL
	case $(uname -r) in
	    *.el5.*)
		[ -f /etc/yum.repos.d/epel.repo ] || $ADMINPROG rpm -Uvh $epelrpm
		isprog python26 || installpkg python26
		isprog easy_install-2.6 || installpkg python26-distribute
		[ -f /usr/include/python2.6/compile.h ] || installpkg python26-devel
		$ADMINPROG easy_install-2.6 bzr
		;;

	    *)
		installpkg bzr
		;;

	esac

    fi
    case $(uname -r) in
	*.el5.*)
	    PYTHON=python2.6
	    EASY_INSTALL=easy_install-2.6
	    ;;
    esac
}

check_bzr_fastimport() {
    check_fastimport
    # Remove mis-installed plugin
    [ -e ~/.bazaar/plugins/bzr-fastimport ] && \
	rm ~/.bazaar/plugins/bzr-fastimport

    if bzr fast-import 2>&1 | grep -q "unknown command"; then
	installpkg bzr-fastimport
    fi
    if bzr fast-import 2>&1 | grep -q "unknown command"; then
	qmkdir -p ~/.bazaar/plugins
	cd ~/.bazaar/plugins
	rm fastimport || true
	bzrit lp:bzr-fastimport fastimport
    fi

    # Make sure it works
    check_testtools		# dependency
}

check_fastimport() {
    if ! $PYTHON -c 'import fastimport'; then
	installpkg python-fastimport
	# Can't rely on the exit code of installpkg, so retest
	$PYTHON -c 'import fastimport' || $ADMINPROG $EASY_INSTALL fastimport
    fi
}

check_testtools() {
    if ! $PYTHON -c 'import testtools'; then
	installpkg python-testtools
	# Can't rely on the exit code of installpkg, so retest
	$PYTHON -c 'import testtools' || $ADMINPROG $EASY_INSTALL testtools
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
    if [ $retval == 1 ]
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


check_bzr
check_bzr_fastimport

# Required making CEDET
isprog makeinfo || installpkg texinfo

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