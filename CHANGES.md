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
