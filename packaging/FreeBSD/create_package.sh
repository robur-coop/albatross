#!/bin/sh -e

basedir=$(realpath "$(dirname "$0")"/../..)
pdir=$basedir/packaging/FreeBSD
bdir=$basedir/_build/default
#tmptmpl=$(basename "$0")
#tmpd=$(mktemp -t "$tmptmpl")
tmpd=$basedir/_build/stage
manifest=$tmpd/+MANIFEST
rootdir=$tmpd/rootdir
sbindir=$rootdir/usr/local/sbin
rcdir=$rootdir/usr/local/etc/rc.d
libexecdir=$rootdir/usr/local/libexec/albatross

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
	 albatross_tls
do install -U $pdir/rc.d/$f $rcdir/$f; done

# stage albatross app binaries
for f in albatrossd albatross_log albatross_console albatross_influx; do
    install -U $bdir/daemon/$f.exe $libexecdir/$f;
done

for f in albatross_tls_endpoint albatross_tls_inetd; do
    install -U $bdir/tls/$f.exe $libexecdir/$f;
done

install -U $bdir/stats/albatross_stats.exe $libexecdir/albatross_stats

install -U $bdir/stats/albatross_stat_client.exe $sbindir/albatross_stat_client

for f in albatross_client_local \
             albatross_client_remote_tls \
             albatross_client_bistro \
             albatross_client_inspect_dump
do install -U $bdir/client/$f.exe $sbindir/$f; done

for f in albatross_provision_ca albatross_provision_request; do
    install -U $bdir/provision/$f.exe $sbindir/$f;
done

# create +MANIFEST
flatsize=$(find "$rootdir" -type f -exec stat -f %z {} + |
               awk 'BEGIN {s=0} {s+=$1} END {print s}')

gitver=$(git rev-parse --short HEAD)

sed -e "s:%%GITVER%%:${gitver}:" -e "s:%%FLATSIZE%%:${flatsize}:" \
    "$pdir/MANIFEST" > "$manifest"

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
