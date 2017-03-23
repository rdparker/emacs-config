#! /bin/bash

set -e
# set -x

awkscript='
BEGIN{found=0}
/remote/{found=1}
/^$/&&found==1{printf "\t-git remote add %s %s\n", mod, origin;found=0}
{print}'

# PATH=$PATH:$(git --exec-path)
# . git-sh-setup

# require_work_tree

toplevel=$(pwd)

function die ()
{
	die_with_status 1 "$@"
}

function die_with_status ()
{
	status=$1
	shift
	printf >&2 '%s\n' "$*"
	exit "$status"
}

# This is similar to git subtree, but it cheats and fetches the entire
# repository so that a raw commit id may be added as a subtree. The
# git subtree command only supports branches and tags unless the
# remote had raw commit reference support turned on.
function subtree_add()
{
    subdir=$1
    mod=$2
    ref=$3

    git fetch $mod || die "Cannot fetch $mod"
    git read-tree --prefix=$subdir $ref || die "Cannot place $ref into $subdir"
    git checkout -- $subdir

    child=$(git write-tree) || exit $?
    head=$(git rev-parse HEAD) || exit $?
    tree=$(git log -1 --pretty=format:%T $ref) || exit $?
    short=$(git rev-parse --short $tree) || exit $?
    squash=$(git commit-tree $tree <<EOF
Squashed '$subdir/' content from commit $short

git-subtree-dir: $subdir
git-subtree-split: $ref
EOF
	   ) || exit $?

    merge=$(git commit-tree $child -p $head -p $squash <<EOF
Merge commit '$squash' as '$subdir'
EOF
	 ) || exit $?

    git reset $merge || die "Cannot reset to $merge"
}

function process_submodule()
{
    local subdir=$1

    local orig_branch=$(git rev-parse --abbrev-ref HEAD) || exit $?

    # Get info about the submodule
    local ref=$(cat .git/modules/$subdir/HEAD)
    cd $subdir
    local origin=$(git remote -v | awk 'NR==1{print $2}') || exit $?
    local mod=$(basename $origin .git) || exit $?

    ###
    ### Remove the submodule
    ###
    cd - >/dev/null

    # Do all the work for a module and tree on a branch
    git checkout -B update-$mod || die "Cannot checkout update-$mod"

    # Deinitialize the submodule
    git submodule deinit $subdir || die "Cannot deinit $subdir"

    # Remove it from git's record of things
    git rm -r --cached $subdir || die "Cannot remove cached $subdir"

    # Remove the content
    rm -rf $subdir

    # Remove the module cache
    rm -rf .git/modules/$subdir

    # Remove the entry from any .gitmodules which reference it.
    # sed -i "/$(echo $subdir|sed 's=/=\\/=g')\"[]]\$/,+2d" .gitmodules
    for modfile in $(find -name .gitmodules); do
	modfiledir=$(dirname $modfile | sed 's=^\./==')
	relpath=$(echo $subdir | sed s=^$modfiledir/==)
	sed -i "/$(echo $relpath|sed 's=/=\\/=g')\"[]]\$/,+2d" $modfile
    done

    # If .gitmodules is empty, git remove it. Otherwise git add it
    if [ -s .gitmodules ]; then
	git add .gitmodules || die "Cannot add .gitmodules"
    else
	git rm .gitmodules || die "Cannot remove .gitmodules"
    fi

    # Commit the changes
    git commit -m "Remove the $mod submodule" || die "Error committing changes"

    ###
    ### Add a remote for the subtree
    ###
    awk -v mod=$mod -v origin=$origin "$awkscript" Makefile > Makefile.$$
    mv -f Makefile.$$ Makefile
    git add Makefile || die "Error adding Makefile"
    if ( echo $mod | grep -q '^[aeiou]'); then
	git commit -m "Add a remote to support an $mod subtree" \
	     || die "Error committing changes"
    else
	git commit -m "Add a remote to support a $mod subtree" \
	     || die "Error committing changes"
    fi
    git remote add $mod $origin || \
	die "If you restarted mod2tree please remove the $mod remote and try again."

    ###
    ### Create the subtree
    ###
    subtree_add $subdir $mod $ref

    ###
    ### Merge the update branch back into master and remove the branch
    ###
    git checkout $orig_branch || die "Cannot checkout $orig_branch"
    git merge --no-edit --no-ff update-$mod || die "Cannot merge update-$mod"
    git branch -d update-$mod || die "Cannot delete update-$mod"
    # There are times like with merging in yasnippet where this will
    # leave you with a dirty checkout. So do a reset so that only
    # submodules which are truly missing are hit in the next section.
    git reset --hard || exit $?

    ###
    ### Restore any missing nested submodules that may have been
    ### deleted when their parent was made into a subtree above.
    ###
    git status -s | while read status dir
    do
	if [ "$status" != "D" ]; then
	    echo "Unexpected git status" >&2
	    git status >&2
	    exit 1
	fi
	git submodule update --init "$dir" || \
	    die "Error updating submodule $dir"
    done
}

function add_nested_submodules()
{
    for x in $(find -name .gitmodules | sed 1d); do
	dir=$(dirname $x | sed 's=^\./==') || exit $?
	escaped_dir=$(echo $dir| sed s=/=\\\\/=g) || exit $?
	paths=$(awk '/path = /{print $3}' $x | sed s/^/$escaped_dir\\//) || \
	    exit $?

	for path in $paths; do
	    if ! grep -q "path = $path" .gitmodules; then
		cat $x | sed s=submodule\ \"=\&$escaped_dir/= | \
		    sed s/path\ =\ /\&$escaped_dir\\// | tee -a .gitmodules || \
		    exit $?
	    fi
	done
    done

    git status -s
    if [ $(git status -s | wc -l) -ne 0 ]; then
	git add .gitmodules || die "Error adding .gitmodules"
	git commit -m 'Add nested submodules at top level' || \
	    die "Error committing changes"
    fi

    make submodules
}


initial_branch=$(git rev-parse --abbrev-ref HEAD) || exit $?
git checkout -b submodules-to-subtrees || exit $?

add_nested_submodules

for submod in $(git submodule | awk '{print $2}'); do
    process_submodule $submod
    echo "If Emacs does not automatically exit, eval '(kill-emacs 1)'."
    emacs --debug-init -e kill-emacs || \
	die "Submodule $submod broke Emacs cleanly exiting."
done

git checkout "$initial_branch" || exit $?
git merge --no-edit --no-ff submodules-to-subtrees || exit $?
git branch -d submodules-to-subtrees
