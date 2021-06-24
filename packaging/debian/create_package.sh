#!/bin/sh -e

# only execute anything if either
# - running under orb with package = albatross
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "albatross" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
rootdir=$tmpd/rootdir
sbindir=$rootdir/usr/sbin
bindir=$rootdir/usr/bin
systemddir=$rootdir/usr/lib/systemd/system
debiandir=$rootdir/DEBIAN

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$sbindir" "$bindir" "$debiandir" "$systemddir"

# stage daemon binaries
for f in albatrossd \
             albatross-console \
             albatross-influx \
             albatross-tls-endpoint \
             albatross-tls-inetd \
             albatross-stats
do install $bdir/$f $sbindir/$f; done

# stage client binaries
for f in albatross-stat-client \
             albatross-client-local \
             albatross-client-remote-tls \
             albatross-client-bistro \
             albatross-client-inspect-dump \
             albatross-provision-ca \
             albatross-provision-request
do install $bdir/$f $bindir/$f; done

# stage systemd scripts
for f in albatross_stats \
	 albatross_console \
	 albatross_daemon
do
    install $basedir/packaging/Linux/$f.service $systemddir/$f.service;
    install $basedir/packaging/Linux/$f.socket $systemddir/$f.socket;
done

# install debian metadata
install $basedir/packaging/debian/control $debiandir/control
install $basedir/packaging/debian/changelog $debiandir/changelog
install $basedir/packaging/debian/copyright $debiandir/copyright
install $basedir/packaging/debian/postinst $debiandir/postinst

dpkg-deb --build $rootdir $basedir/albatross.deb
echo 'bin: [ "albatross.deb" ]' > $basedir/albatross.install
