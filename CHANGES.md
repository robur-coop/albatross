# v2.1.0 (2024-08-15)

* BUGFIX install tls_endpoint as systemd service (not only as example), fix typo
  (#183 @PizieDust)
* BUGFIX albatross-influx: use "cow_faults" (fixed typo in #182) as unsigned
  integer (b9695dd9c267f5e59f18154a632adba0481f9d72, @hannesm)
* BUGFIX albatross-influx: avoid empty measurement (#182 @hannesm)
* BUGFIX tls-endpoint: don't fail if a bad client connects (#180 @hannesm)
* FEATURE tls-endpoint: add syslog support (esp. useful for inetd) (fixes #176,
  #185 @hannesm)
* FEATURE Update to FreeBSD 14 (#172, @hannesm)
* BUGFIX update command: make usable with local client (#184, @reynir)
* BUGFIX update command: handle HTTP not found explicitly
  (fixes #147, #171 @hannesm)
* FEATURE Albatross: record start timestamp via info (fixes #168, #169 @hannesm)
* BUGFIX Albatross: store timestamp as generalized time instead of utc time
  (#167 @hannesm @reynir, adjusted by #181 for backwards compatibility
   @PizieDust)
* BUGFIX FreeBSD: restart services when they terminate
  (@hannesm, 64f28fbd88504ec33d6bfde5211684e0ba1bc193)
* BUGFIX packaging: install albatross-client as albatross-client
  (@hannesm, 23acb8b3edbe0153e1bd24a1736b80e73a27e33f)
* update nix inputs (#165, #175, #179 @Julow)
* use ohex instead of hex (#174 @hannesm)
* fix README (#177 @PizieDust, 2a1c3d898b586946ce7c4f3171ea5eb856f4ade8,
  107c235eb94e83d6a077c42ee8b527c207af4ba7)
* fix opam file (@hannesm 2f316d2e49866fe08b9e12c12194062bbbaa2329)
* update cirrus (@hannesm 6e9e45fe397f48468f1c154379d728233673a115
  24adebe6c3456c8910cb6c9561db68cc0ad3e033
  ec8a7017c03bdccbb996a4ecfcdd88a00de7ec80
  40360d00df2e0b95bf7b6a71958b2f9fd451df25)

# v2.0.0 (2023-05-14)

This is a breaking release since the binaries to be installed have been revised
and merged. The `albatross-client-local` is now `albatross-client [--socket]`,
`albatross-provision-ca` is now `albatross-client sign` or `albatross-client
generate`. The `albatross-client-remote` is now `albatross-client certificate`.
The `albatross-client-bistro` is now `albatross-client <command>
--destination <host> --ca ca.pem --ca-key ca.pem --server-ca cacert.pem`.

And finally, `albatross-tls-inetd` is now `albatross-tls-endpoint --inetd`.

- Document TLS usage (#155, #156 @TheLortex @hannesm)
- Improve TLS experience by providing more reasonable error messages and apply
  more checks before establishing a TLS session (#157 @hannesm @reynir)
- Slim down binaries:
  - remove albatross-stat-client binary (#161 @hannesm)
  - move X509 parts out of Albatross_cli (#158 @reynir)
  - merge albatross-tls-inetd with albatross-tls-endpoint (#160 @hannesm)
  - merge albatross-client-inspect-dump into albatross-client (#161 @hannesm)
  - merge albatross-provision-ca and albatross-provision-request into
    albatross-client (#159 @reynir @hannesm)
  - merge albatross-client-remote and albatross-client-bistro and
    albatross-client-local into albatross-client (#162 @hannesm)
- Check solo5 ABI tender version, allow multiple solo5 tenders to exist (named
  as solo5-{s,h}vt.<ABI> (#163 @hannesm)
- Improve documentation and manpages (#164 @hannesm)

# v1.5.6 (2023-02-19)

- Update to tls 0.16 packaging (#154 @hannesm)

# v1.5.5 (2023-02-16)

- Systemd scripts: default to less verbose logging (#151 @dinosaure @reynir)
- Add a command to restart unikernels (#148 @hannesm @reynir)
- Split code depending on unix into albatross.unix to allow usage of albatross
  in environments without unix - thus MirageOS (#148 @hannesm @reynir)

# v1.5.4 (2022-12-09)

- Improve error messages when socket binding fails, and when albatross-console
  is not running (#139, inspired by #137, @hannesm @samoht)
- Debian packages: add gmp and libnl package dependencies (#141 #142 @reynir
  @hannesm)
- albatross-tls-endpoint: use Unix.inet6_addr_any, also add a command line
  argument to specify the listening address (#144 #145 @reynir @hannesm,
  reported in #143 by @palainp)
- command line: allow multiple ":" in the hostname (to support IPv6 addresses)
  (#146 @hannesm)

# v1.5.3 (2022-11-16)

- Support --block-sector-size (solo5 0.7.4) (#134 @hannesm @reynir)
- Invert communication between albatross-stats and albatross-daemon (#131 #133 @hannesm @reynir)
- Cleanups (avoid catch-all, remove migrate_name support, #130 @hannesm)
- Add minimal support for macOS (#128 @samoht)
- Upgrade to http-lwt-client 0.2.0 (#127 @hannesm)
- Remove unnecessary includes (#126 @reynir)

# v1.5.2 (2022-10-25)

- BUGFIX policy (vmm_resources): when inserting a policy, check policies above,
  but not the same one (@hannesm)
- tls-endpoint: listen on systemd socket, add systemd example (#119 @Julow
  @reynir)
- albatross-stats systemd service: allow AF_NETLINK to gather network interface
  statistics (@reynir)
- BUGFIX albatross-stats: use if_nametoindex, simplify code (#125 @dinosaure
  @reynir @hannesm)
- Add deployment scripts for nixos (#120 @Julow)

# v1.5.1 (2022-08-31)

- Albatross_influx: drop leading ':' if path should be dropped (#114 @hannesm)
- FreeBSD packaging: rename albatross_stat to albatross_stats (#113 @hannesm)
- Refactor albatross BHyve stat collection (to avoid exception in List.combine)
  (#116 @reynir @hannesm)
- Albatross-client-update: adapt URLs to current builder-web deployment (#117
  @hannesm)
- Albatross-client: allow passing a mac address explicitly to create for each
  network interface (#107 @reynir)

# v1.5.0 (2022-05-16)

- Revise Name.t to use ':' as path separation, and allow '.' in labels.
  Previously the path was built by the common name in the X.509 certificate
  chain and the leaf certificate was appended (i.e. chain certificate "foo",
  chain certificate "bar", leaf certificate "my.unikernel" lead to the name
  "foo.bar.my.unikernel" -- and chain certificate "foo", leaf "bar.my.unikernel"
  lead to the identical name). Since the holder of the certificate and private
  key "foo" could issue at any point another intermediate certificate for "bar",
  this is not security critical -- but for resource management this was
  confusing and lead to some issues (policy could be violated).
  Now, the path separator is ':' (i.e. "foo:bar:my.unikernel" and
  "foo:bar.my.unikernel").
  In addition, various test cases have been added, for vmm_trie, vmm_resources
  and also for old and new wire versions (albatross daemon state, command
  execution) to ensure that old clients continue to work with new server
  components. The wire version has been bumped to WV5, since the Name.t encoding
  was changed. (#111, @hannesm @reynir)
- systemd: fifo are created by albatross_daemon (not albatross_console) (#106,
  @reynir)
- systemd: cleanup, use group albatross, (#108, fixes #105, @reynir)
- documentation: remove solo5-elftool requirement -- since 1.4.0
  ocaml-solo5-elftool is used (#109 @hannesm)
- CI execute tests (#112, @hannesm)
- fix URL to builder-web (https://builds.robur.coop) which dropped the
  opam-switch postfix in the URL (#113 @hannesm)
- albatross-client-local, albatross-client-bistro: support remote (socket/host)
  '-' to output the command as hexdump (PEM file) on standard output (@hannesm)

# v1.4.3 (2022-03-15)

- Debian packaging: set architecture to DEB_TARGET_ARCH (@reynir)
- FreeBSD packaging: normalize version number (. instead of -) (@hannesm)
- Add systemd service script for albatross_influx (@hannesm)
- Update to cmdliner 1.1.0 (#104 @hannesm)
- Support IPv6 in daemon (albatross_tls_endpoint) and influx (#104 @hannesm)

# v1.4.2 (2022-02-04)

- fix issues "use OCaml solo5-elftool instead of binary" where the compressed
  unikernel image was passed to the tool (if albatross-provision-request was
  used, and in albatross-daemon) (#101 by @palainp, fixed in #102 by @hannesm)

# v1.4.1 (2022-01-31)

- Debian packaging fix typo in lib_exec_dir (create_package.sh, #100 @dinosaure)

# v1.4.0 (2022-01-28)

- albatross-provision-ca: support signing of server certificates
- use solo5-elftool (developed in OCaml) instead of binary, avoids solo5
  dependency for albatross-client-* (#94 @reynir, fixes #93), removes jsonm
  dependency
- albatross-influx: reconnect TCP to influx (telegraf) host (#97 @hannesm,
  fixes #69)
- by default, do not print argv in unikernel_info (and pp_wire). only if the
  logging level is verbose (#96 @hannesm, fixes @92)
- avoid file descriptor leak on Linux in albatross-stats when reading
  /proc/<pid>/status (#99 @hannesm)
- remove astring dependency (#99 @hannesm)
- Debian packaging
  - install metadata and service scripts with 0644 permissions (@reynir)
  - postinst: do not use sudo (@reynir)
  - postinst: set ownerhip of /var/lib/albatross and /var/lib/albatross/block
    to $ALBATROSS_USER (@reynir)
  - postinst: create group and user only if they do not exist yet (@reynir)
  - install daemons into /usr/libexec/albatross to avoid accidental invocation
    (@hannesm #95, fixes #91)
  - add network.target and NetworkManager-wait-online.service to dependencies
    of albatross_daemon to ensure bridges already exists (#98 @reynir,
    fixes #90)

# v1.3.1 (2021-10-30)

- Linux: install binaries into /usr/sbin, adjust systemd scripts
  (#89 by @reynir, fixed by @hannesm)
- remove rresult dependency (@hannesm)
- remove deprecated Fmt functions (@dinosaure)

# v1.3.0 (2021-09-14)

- provide arguments for public-key-type and bits, the default is now
  Ed25519 (used to be RSA) (@hannesm)
- use happy-eyeballs for name resolution and connection setup (@hannesm)
- converge albatross-client-* semantics (@hannesm)
- Implement block_set and block_dump subcommands, also block_add is extended
  to include the block device data (@hannesm, inspired by @dinosaure)
- Implement update subcommand for albatross-client-{local,bistro}, which
  (a) retrieves the digest of a running unikernel (b) looks up that hash
  on http://builds.robur.coop (a repository of reproducible built unikernels)
  (c) does a unikernel update (with same arguments and configuration)
  (@reynir @hannesm)
- Debian and FreeBSD packaging via orb, available on https://builds.robur.coop
  (#80 @hannesm @reynir)
- Linux: add albatross user and group, as done on FreeBSD (#79 #81 @dinosaure)
- Fixes to README (#78 @yomimono)

# v1.2.0 (2021-06-08)

- linux packaging albatross_stat -> albatross_stats (#73 @smorimoto)
- albatross_stats: report runtime in microseconds on linux (#74 @hannesm)
- use metrics-rusage for albatross processes (to report host system usage)
  (#76 @hannesm)
- remove albatross_log, instead log to stdandard output (#75 @hannesm)

# v1.1.1 (2021-04-23)

- upgrade to decompress 1.3 (#71 @dinosaure)
- upgrade to x509 0.13.0 (#72 @hannesm)

# v1.1.0 (2021-01-25)

- fix and improve block device handling (#67 @hannesm)
- unikernel_info now includes a SHA256 digest of the unikernel image (fixes #61,
  #64 @hannesm)
- unikernel_get returns only the image, and a boolean value whether it is
  compressed or not (#64 @hannesm)
- allow naming of block devices (#64 @hannesm) similar to network devices
- albatrossd: wait 3 seconds between socket connection retries, default to 2
  retries (previously was 0) (#64 @hannesm)
- infer dbdir and tmpdir from uname before processing (#60 @hannesm)
- statistics: adapt to FreeBSD 13 (#62 @sg2342)
- use binaries with '-' instead of '_' in FreeBSD packaging (to be the same as
  on Linux)
- extend PATH with /usr/local/bin and /usr/local/sbin in FreeBSD RC scripts,
  allowing solo5 utilities installed in PATH (#64 @hannesm)
- use latest version of decompress (>= 1.2.0 #68 @dinosaure)
- add test for ASN grammar (#65 @reynir)

# v1.0.1 (2020-12-02)

* Use the conf-libnl3 opam package on Linux (created by @reynir, #57 @hannesm)
* Avoid using sysctl.h on Linux (#58 @reynir @kit-ty-kate)

# v1.0.0 (2020-12-01)

Initial release
