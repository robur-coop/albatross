# systemd service scripts

these are preliminary and just here to let people play with `solo5-spt`, the seccomp-enabled backend for [Solo5](https://github.com/Solo5/solo5) on Linux.
Note: The socket paths are hardcoded relative to the RuntimeDirectory (tmpdir).
If you modify `Vmm_core.socket_path` you must modify the corresponding `.socket` file(s) in this directory.

The debian packaging installs the executables and these systemd scripts accordingly.