# systemd service scripts

these are preliminary and just here to let people play with `solo5-spt`, the seccomp-enabled backend for [Solo5](https://github.com/Solo5/solo5) on Linux.
Note: The socket paths are hardcoded relative to the RuntimeDirectory (tmpdir).
If you modify `Vmm_core.socket_path` you must modify the corresponding `.socket` file(s) in this directory.

1) You need to build the `albatross` tooling in this repository
2) To run unikernels, you need to build and install solo5-elftool and at least one of the tenders: solo5-hvt and solo5-spt. They can be installed somewhere in PATH or in /var/lib/albatross/.
3) See [`install.sh`](./install.sh) for commands required to deploy it.
4) `sudo journalctl -fu albatross'*'.service`
5) ideally, once the services are up and running, you would be able to issue this command to deploy a unikernel:
   `sudo albatross-client-local create helloworld /path/to/hello_world.spt`
