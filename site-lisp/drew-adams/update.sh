#! /bin/sh

# Update emacs-init.el.
wget https://www.emacswiki.org/emacs/download/emacs-init.el
sed 's="~/drews-lisp-20"="~/.emacs.d/site-lisp/drew-adams"=' emacs-init.el.1 > emacs-init.el
rm emacs-init.el.1

files=$(sed -n '
/Features that might be required by this library:/,/;;;;;;;;;;;;;;;;;;;;;;;;/{
  /;;   /{
    s/[;`,.]//g;p
  }
}' emacs-init.el | sed "s/'//g")

for f in $files; do
    # Append the .el extension
    f=$f.el

    # Download it
    wget https://www.emacswiki.org/emacs/download/$f

    # If the file was downloaded with a .1 extension because it
    # already existed replace the original with it.
    [ -e $f.1 ] && mv $f.1 $f

    # If Drew is not the Author or Maintainer then delete the
    # file. This handles the odd dependency that is not written by
    # Drew, but which may be downloadable from Emacswiki and the
    # zero-length files that are created when no file is found
    # to download.
    egrep -q ';; (Author|Maintainer): Drew Adams' $f || rm $f
done

# Update any other files that are in this directory but are not listed
# in emacs-init.el.
others=$(for f in *.el; do
	     found=0;
	     for g in $files; do
		 if [ $g.el == $f ]; then
		     found=1;
		     break;
		 fi;
	     done;
	     [ $found -eq 0 ] && echo $f;
	 done)

# Extract the URL's from all the elisp files in this directory. Assume
# they are emacswiki links, convert them to the appropriate download
# link, and download the file.
grep ';; URL:' $others | \
    sed 's/.*URL: //;s/%2b/+/g;s=org/=&emacs/download/=' | \
    xargs -n 1 wget

# The downloads will have .1 appended to their names. Replace the
# originals with the new files.
for f in *.el.1; do mv $f $(basename $f .1); done
