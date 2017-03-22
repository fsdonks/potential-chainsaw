#!/bin/sh
#
# Make .sha1 files for any artifacts in a Maven repository that are
# versioned as `SNAPSHOT` and are missing these files.  This is
# useful, e.g., when you zip/tar up your local .m2 repository,
# "sneaker net" it to an "air gapped" network, and use it as a *local
# mirror* for others on that same network to pull dependencies from.
#
# Usage
#
# $ make-sha1-files.sh rootdir
#
# where you replace `rootdir` with the directory name of the root of a
# Maven repository.
#
# Example
#
# Say the directory `m2repo` is a copy -- or a copy of a subset -- of
# your local .m2 that you are going to turn into a local mirror for
# somewhere else.
#
# $ ls -ld m2repo
# drwxr-xr-x 1 fs-rick 1049089 0 Mar 22 11:03 m2repo/
#
# Now, use this script to generate the (missing) .sha1 files for the
# SNAPSHOT dependencies.
#
# $ make-sha1-files.sh m2repo
# Checking proc/proc/0.2.1-SNAPSHOT/proc-0.2.1-SNAPSHOT.jar.sha1 . . . not found (cutting a .sha1)
# Checking proc/proc/0.2.1-SNAPSHOT/proc-0.2.1-SNAPSHOT.pom.sha1 . . . not found (cutting a .sha1)
# Checking spork/spork/0.1.9.9-SNAPSHOT/spork-0.1.9.9-SNAPSHOT.jar.sha1 . . . not found (cutting a .sha1)
# Checking spork/spork/0.1.9.9-SNAPSHOT/spork-0.1.9.9-SNAPSHOT.pom.sha1 . . . not found (cutting a .sha1)
#
# Use the same invocation to spot-check that it worked, if you're the
# "belt and suspenders" type.  No worries, because the script is
# functionally idempotent.
#
# $ make-sha1-files.sh m2repo
# Checking proc/proc/0.2.1-SNAPSHOT/proc-0.2.1-SNAPSHOT.jar.sha1 . . . ok
# Checking proc/proc/0.2.1-SNAPSHOT/proc-0.2.1-SNAPSHOT.pom.sha1 . . . ok
# Checking spork/spork/0.1.9.9-SNAPSHOT/spork-0.1.9.9-SNAPSHOT.jar.sha1 . . . ok
# Checking spork/spork/0.1.9.9-SNAPSHOT/spork-0.1.9.9-SNAPSHOT.pom.sha1 . . . ok

test $# -gt 0 || { echo "Usage: ${0} rootdir" >&2; exit 2; }

rootdir="$1"
test -d "${rootdir}" || {
    echo "Directory ${rootdir} DNE.  Try again." >&2
    exit 3; }

SHA1SUM="${SHA1SUM:-sha1sum}"
type "${SHA1SUM}" >/dev/null 2>&1 || {
    echo "Cannot find the sha1sum program.  Try again." >&2
    exit 4; }

find ${rootdir} -type d -name \*SNAPSHOT 2>/dev/null |
    while read d; do
        (cd "${d}"
         ppd="${d/${rootdir}/}"; ppd="${ppd#*/}"
         for f in $(ls -1 *SNAPSHOT.{pom,jar} 2>/dev/null); do
             s="${f}.sha1"
             echo -n "Checking ${ppd}/${s} . . . " >&2 
             if [ -r "${s}" ]; then
                 echo "ok" >&2
             else
                 echo "not found (cutting a .sha1)" >&2
                 ${SHA1SUM} "${f}" | awk '{print $1}' > "${s}"
             fi
         done)
    done
