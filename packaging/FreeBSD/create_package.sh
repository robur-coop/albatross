#!/bin/sh -e

basedir=$(realpath "$(dirname "$0")"/../..)
pdir=$basedir/packaging/FreeBSD
bdir=$basedir/_build/install/default/bin
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
for f in albatross_stat \
	 albatross_console \
	 albatross_daemon \
	 albatross_influx \
	 albatross_tls
do install -U $pdir/rc.d/$f $rcdir/$f; done

# stage albatross app binaries
for f in albatrossd \
             albatross-console \
             albatross-influx \
             albatross-tls-endpoint \
             albatross-tls-inetd \
             albatross-stats
do install -U $bdir/$f $libexecdir/$f; done

for f in albatross-stat-client \
             albatross-client-local \
             albatross-client-remote-tls \
             albatross-client-bistro \
             albatross-client-inspect-dump \
             albatross-provision-ca \
             albatross-provision-request
do install -U $bdir/$f $sbindir/$f; done

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
