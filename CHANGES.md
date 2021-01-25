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
