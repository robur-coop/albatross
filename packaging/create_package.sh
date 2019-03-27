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
	 albatross_influx \
	 albatross_tls \
	 albatross_x
do install -U $basedir/packaging/rc.d/$f $rootdir/usr/local/etc/rc.d/$f; done

# stage albatross app binaries
for f in albatrossd albatross_log albatross_console albatross_influx; do
    install -U $basedir/_build/default/daemon/$f.exe \
	 $rootdir/usr/local/libexec/albatross/$f; done

for f in albatross_tls_endpoint albatross_tls_inetd; do
    install -U $basedir/_build/default/tls/$f.exe \
	 $rootdir/usr/local/libexec/albatross/$f; done

install -U $basedir/_build/default/stats/albatross_stats.exe \
	 $rootdir/usr/local/libexec/albatross/albatross_stats

install -U $basedir/_build/default/stats/albatross_stat_client.exe \
	 $rootdir/usr/local/sbin/albatross_stat_client

for f in albatross_client_local albatross_client_remote_tls albatross_client_bistro; do
    install -U $basedir/_build/default/client/$f.exe \
	$rootdir/usr/local/sbin/$f; done

for f in albatross_provision_ca albatross_provision_request; do
    install -U $basedir/_build/default/provision/$f.exe \
	$rootdir/usr/local/sbin/$f; done

# create +MANIFEST
flatsize=$(find "$rootdir" -type f -exec stat -f %z {} + |
               awk 'BEGIN {s=0} {s+=$1} END {print s}')

gitver=$(git rev-parse --short HEAD)

sed -e "s:%%GITVER%%:${gitver}:" -e "s:%%FLATSIZE%%:${flatsize}:" \
    "$basedir/packaging/MANIFEST" > "$manifest"

{
    printf '\nfiles {\n'
    find "$rootdir" -type f -exec sha256 -r {} + | sort |
        awk '{print "    " $2 ": \"" $1 "\"," }'
    find "$rootdir" -type l | sort |
        awk "{print \"    \"\$1 \": -,\"}"
    printf '}\n'
} | sed -e "s:${rootdir}::" >> "$manifest"

export SOURCE_DATE_EPOCH=$(git log -1 --pretty=format:%ct)
pkg create -r "$rootdir" -M "$manifest" -o $basedir/_build/
