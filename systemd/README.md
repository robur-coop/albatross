# systemd service scripts

these are preliminary and just here to let people play with `solo5-spt`, the seccomp-enabled backend for [Solo5](https://github.com/Solo5/solo5) on Linux.

1) You need to build the `vmm` tooling in this repository
2) You need to build a `spt`-compatible binary to deploy.
3) See [`install.sh`](./install.sh) for commands required to deploy it.
4) `sudo journalctl -fu albatross'*'.service`
5) ideally, once the services are up and running, you would be able to issue this command to deploy a unikernel:
   `sudo vmmc_local.native -t spt-amd64 --compression0 helloworld /path/to/hello_world.spt`
