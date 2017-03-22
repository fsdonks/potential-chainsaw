#!/bin/sh
#
# Perform a `git pull` from each of the git working directories
# underneath the current working directory.
#
# Usage/Example
#
# $ ./pull-all.sh
# >>>>  figdemo  <<<<
# Already up-to-date.
# >>>>  marathon  <<<<
# Already up-to-date.
# >>>>  mardoc  <<<<
# Already up-to-date.
# >>>>  potential-chainsaw  <<<<
# Already up-to-date.
# >>>>  proc  <<<<
# Already up-to-date.

find . -type d -name .git | awk '{sub(/^.\//,"");sub(/\/.git$/,"")}1' |
    while read d; do
        echo '>>>> ' ${d} ' <<<<' >&2
        (cd "$d"; git pull)
    done
