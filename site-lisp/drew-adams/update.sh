#! /bin/sh
# Extract the URL's from all the elisp files in this directory. Assume
# they are emacswiki links, convert them to the appropriate download
# link, and download the file.
grep ';; URL:' *.el | sed 's/.*URL: //;s/%2b/+/g;s=org/=&emacs/download/=' | xargs -n 1 wget

# The downloads will have .1 appended to their names. Replace the
# originals with the new files.
for f in *.el.1; do mv $f $(basename $f .1); done
