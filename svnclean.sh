#!/usr/bin/env bash

# http://stackoverflow.com/questions/5474732/how-can-i-add-a-help-method-to-a-shell-script
usage="$(basename "$0") [-h] [-l] [-p]

Remove files that are not controlled by svn,

where:
    -h  show this help text
    -p  prompt before every removal (default: force, i.e. never ask)
    -l  only list the files to be removed"

listfiles=0
rmprompt=0
# http://stackoverflow.com/questions/4882349/parsing-shell-script-arguments
while [[ $1 == -* ]]; do
    case "$1" in
	-h) echo "$usage"; exit;;
	-l) listfiles=1; shift;;
	-p) rmprompt=1; shift;;
	-*) echo "invalid option: $1" 1>&2; exit 1;;
    esac
done

# http://stackoverflow.com/questions/4515586/clean-an-svn-checkout-remove-non-svn-files
if [ $listfiles -eq 1 ]; then
    svn status --no-ignore | grep '^[?I]' | sed "s/^[?I] //" | xargs -I{} echo "{}"
elif [ $rmprompt -eq 1 ]; then
    svn status --no-ignore | grep '^[?I]' | sed "s/^[?I] //" | xargs -p -I{} rm -rf "{}"
else
    svn status --no-ignore | grep '^[?I]' | sed "s/^[?I] //" | xargs -I{} rm -rf "{}"
fi
