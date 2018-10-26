#!/bin/sh -e

basedir=$(realpath "$(dirname "$0")"/..)
#tmptmpl=$(basename "$0")
#tmpd=$(mktemp -t "$tmptmpl")
tmpd=$basedir/_build/stage
manifest=$tmpd/+MANIFEST
rootdir=$tmpd/rootdir


trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$rootdir"/usr/local/sbin \
         "$rootdir"/usr/local/libexec/albatross \
         "$rootdir"/usr/local/etc/rc.d

# stage service scripts
for f in albatross_log \
	 albatross_stat \
	 albatross_console \
	 albatross_daemon \
	 albatross_x
do install -U $basedir/packaging/rc.d/$f $rootdir/usr/local/etc/rc.d/$f; done

# stage albatross app binaries
for f in vmmd vmmd_log vmmd_console vmmd_stats; do
    install -U $basedir/_build/app/$f.native \
	 $rootdir/usr/local/libexec/albatross/$f; done

install -U $basedir/_build/app/vmmc_local.native \
	$rootdir/usr/local/sbin/vmmc_local

# create +MANIFEST
flatsize=$(find "$rootdir" -type f -exec stat -f %z {} + |
               awk 'BEGIN {s=0} {s+=$1} END {print s}')

gitver=$(git rev-parse --short HEAD)

sed -e "s:%%GITVER%%:${gitver}:" -e "s:%%FLATSIZE%%:${flatsize}:" \
    "$basedir/packaging/MANIFEST" > "$manifest"

{
    printf '\nfiles {\n'
    find "$rootdir" -type f -exec sha256 -r {} + |
        awk '{print "    " $2 ": \"" $1 "\"," }'
    find "$rootdir" -type l |
        awk "{print \"    \"\$1 \": -,\"}"
    printf '}\n'
} | sed -e "s:${rootdir}::" >> "$manifest"

pkg create -r "$rootdir" -M "$manifest" -o $basedir/_build/
