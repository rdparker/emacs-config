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

# This is similar to git subtree, but it cheats and fetches the entire
# repository so that a raw commit id may be added as a subtree. The
# git subtree command only supports branches and tags unless the
# remote had raw commit reference support turned on.
function subtree_add()
{
    subdir=$1
    mod=$2
    ref=$3

    git fetch $mod
    git read-tree --prefix=$subdir $ref
    git checkout -- $subdir

    child=$(git write-tree)
    head=$(git rev-parse HEAD)
    tree=$(git log -1 --pretty=format:%T $ref)
    short=$(git rev-parse --short $tree)
    squash=$(git commit-tree $tree <<EOF
Squashed '$subdir/' content from commit $short

git-subtree-dir: $subdir
git-subtree-split: $ref
EOF
	   )

    merge=$(git commit-tree $child -p $head -p $squash <<EOF
Merge commit '$squash' as '$subdir'
EOF
	 )

    git reset $merge || (echo git reset $merge failed >&2 && exit 1)
}

function process_submodule()
{
    local subdir=$1

    local orig_branch=$(git rev-parse --abbrev-ref HEAD)

    # Get info about the submodule
    cd $subdir
    local origin=$(git remote -v | awk 'NR==1{print $2}')
    local ref=$(git show-ref HEAD |awk '{print $1}')
    local mod=$(basename $origin .git)

    ###
    ### Remove the submodule
    ###
    cd -

    # Do all the work for a module and tree on a branch
    git checkout -B update-$mod

    # Deinitialize the submodule
    git submodule deinit $subdir

    # Remove it from git's record of things
    git rm -r --cached $subdir

    # Remove the content
    rm -rf $subdir

    # Remove the module cache
    rm -rf .git/modules/$subdir

    # Remove the entry from .gitmodules
    sed -i "/$(echo $subdir|sed 's=/=\\/=g')\"[]]\$/,+2d" .gitmodules

    # If .gitmodules is empty, git remove it. Otherwise git add it
    if [ -s .gitmodules ]; then
	git add .gitmodules
    else
	git rm .gitmodules
    fi

    # Commit the changes
    git commit -m "Remove the $mod submodule"

    ###
    ### Add a remote for the subtree
    ###
    awk -v mod=$mod -v origin=$origin "$awkscript" Makefile > Makefile.$$
    mv -f Makefile.$$ Makefile
    git add Makefile
    if ( echo $mod | grep -q '^[aeiou]'); then
	git commit -m "Add a remote to support an $mod subtree"
    else
	git commit -m "Add a remote to support a $mod subtree"
    fi
    git remote add $mod $origin || ( echo If you restarted mod2tree please remove the $mod remote and try again. >&2 && exit 1)

    ###
    ### Create the subtree
    ###
    subtree_add $subdir $mod $ref

    ###
    ### Merge the update branch back into master and remove the branch
    ###
    git checkout $orig_branch
    git merge --no-edit --no-ff update-$mod
    git branch -d update-$mod
    # There are times like with merging in yasnippet where this will
    # leave you with a dirty checkout. So do a reset so that only
    # submodules which are truly missing are hit in the next section.
    git reset --hard

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
	git submodule update --init "$dir"
    done
}

function add_nested_submodules()
{
    for x in $(find -name .gitmodules | sed 1d); do
	dir=$(dirname $x | sed 's=^\./==')
	escaped_dir=$(echo $dir| sed s=/=\\\\/=g)
	paths=$(awk '/path = /{print $3}' $x | sed s/^/$escaped_dir\\//)
	for path in $paths; do
	    if ! grep -q "path = $path" .gitmodules; then
		cat $x | sed s=submodule\ \"=\&$escaped_dir/= | \
		    sed s/path\ =\ /\&$escaped_dir\\// | tee -a .gitmodules
	    fi
	done
    done

    git status -s
    if [ $(git status -s | wc -l) -ne 0 ]; then
	git add .gitmodules
	git commit -m 'Add nested submodules at top level'
    fi

    make submodules
}


initial_branch=$(git rev-parse --abbrev-ref HEAD)
git checkout -b submodules-to-subtrees

add_nested_submodules

for submod in $(git submodule | awk '{print $2}'); do
    process_submodule $submod
done

git checkout "$initial_branch"
git merge --no-ff submodules-to-subtrees
git branch -d submodules-to-subtrees
