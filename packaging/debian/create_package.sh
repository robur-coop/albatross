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
libexecdir=$rootdir/usr/libexec/albatross
bindir=$rootdir/usr/bin
systemddir=$rootdir/usr/lib/systemd/system
examplesdir=$rootdir/usr/share/doc/albatross/examples
debiandir=$rootdir/DEBIAN

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$libexecdir" "$bindir" "$debiandir" "$systemddir" "$examplesdir"

# stage daemon binaries
for f in albatrossd \
             albatross-console \
             albatross-influx \
             albatross-tls-endpoint \
             albatross-stats
do install $bdir/$f $libexecdir/$f; done

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
    install -m 0644 $basedir/packaging/Linux/$f.service $systemddir/$f.service;
    install -m 0644 $basedir/packaging/Linux/$f.socket $systemddir/$f.socket;
done
install -m 0644 $basedir/packaging/Linux/albatross_influx.service \
        $systemddir/albatross_influx.service
install -m 0644 $basedir/packaging/Linux/albatross_tls_endpoint.service \
	$examplesdir/albatross_tls_endpoint.service
install -m 0644 $basedir/packaging/Linux/albatross_tls_endpoint.socket \
	$examplesdir/albatross_tls_endpoint.socket

# install debian metadata
install -m 0644 $basedir/packaging/debian/control $debiandir/control
install -m 0644 $basedir/packaging/debian/changelog $debiandir/changelog
install -m 0644 $basedir/packaging/debian/copyright $debiandir/copyright
install $basedir/packaging/debian/postinst $debiandir/postinst

ARCH=$(dpkg-architecture -q DEB_TARGET_ARCH)
sed -i -e "s/^Architecture:.*/Architecture: ${ARCH}/" $debiandir/control

dpkg-deb --build $rootdir $basedir/albatross.deb
echo 'bin: [ "albatross.deb" ]' > $basedir/albatross.install
echo 'doc: [ "README.md" ]' >> $basedir/albatross.install
