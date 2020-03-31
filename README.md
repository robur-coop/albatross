# Albatross: orchestrate and manage MirageOS unikernels with Solo5

[![Build Status](https://travis-ci.org/hannesm/albatross.svg?branch=master)](https://travis-ci.org/hannesm/albatross)

The goal of albatross is robust deployment of [MirageOS](https://mirage.io)
unikernels using [Solo5](https://github.com/solo5/solo5). Resources managed
by albatross are network interfaces of kind `tap`, which are connected to
already existing bridges, block devices, memory, and CPU. Each unikernel is
pinned (`cpuset` / `taskset`) to a specific core.

Albatross allows remote management, to deploy or destroy a unikernel, no shell
access is necessary. The remote channel is a mutually authenticated (with X.509
certificates) TLS connection. Console output of the unikernels is stored in
memory in a ring buffer, and accessible from remote. Monitoring data (CPU and
memory usage) of the unikernels can be collected as well, and pushed into a
Influx time series database.

Albatross consists of multiple processes, each running with the least
privileges. Albatross can be run next to other orchestration systems, it does
not assume to be the single instance on a dom0 which creates and destroys
virtual machines. Resource policies can be dynamically configured for each
administrative domain (similar to DNS, a hierarchical naming scheme), and is
statically checked (to decrease while going down the tree) and dynamically when
a new unikernel is to be deployed.

When a unikernel was deployed on albatross, it tries the best to keep this
running, even when the physical hardware reboots, or albatross is restarted.
When the unikernel exits, depending on configuration and its exit code, it is
re-started. The current set of running unikernels is persisted on disk, though
there is no dependency or order how to restart them.

The scope of albatross is to provide a minimal orchestration system that avoids
the need of shell access on the dom0. This leads to mostly immutable - or only
mutable via albatross which writes a log for every administrative change -
infrastructure. Further dissemination of albatross into virtual machines, and
a communication interface for deploying and destroying unikernels, is being
researched on.

## Components

Albatross consists of a set of binaries. Several daemons, which communicate in a
request-response style over Unix domain sockets, are run in the host system:
- `albatrossd`: privileged to create and destroy unikernels
- `albatross_console`: reads the console output of unikernels
- `albatross_log`: event log
- `albatross_stats`: statistics gathering (rusage, ifstat, BHyve debug counters)
- `albatross_tls_inetd`: remote deployment via TLS and inetd (an alternative is `albatross_tls_endpoint`)
- `albatross_influx`: statistic reporting from `albatross_stats` to influx

The main daemon is the privileged `albatrossd`, which supervises unikernels. It opens
a listening Unix domain socket, reads the persisted unikernel configuration,
starts these unikernels, and awaits commands. Access can be regulated by Unix
file permissions, only those users who can write to that socket can send
commands.

`Albatross_console` does not keep any persistent state, but a ring buffer of console
output from each unikernel. These messages can be retrieved by a client, as a
stream of messages (history, and whenever a new line is output, it is send to
the interested client). Each unikernel output can only be read by a single
client, to avoid amplification of traffic if lots of clients are connected.
`Albatrossd` sends a message to `albatross_console` whenever a new unikernel is started,
upon reception `albatross_console` opens and reads the fifo which the unikernel will
write their standard output to.

`Albatross_log` keeps a persistent event log for albatross, can be read by clients.

`Albatross_stats` gathers periodically statistics (memory, CPU, network, hypervisor)
from all running unikernels.

`Albatross_tls_inetd` is executed via inetd (socket activation), and proxy
requests from remote clients to the respective daemons described above. It
enforce client authentication, and use the commen names of the client
certificate chain as administrative domain. The policies are embedded in CA
certificates, the command is embedded in the leaf certificate. The
`albatross_tls_endpoint` is an alternative, which listen on a TCP port and
executes an asynchronous task for each incoming request.

The following command-line applications for local and remote management are provided:
- `albatross_client_local`: sends a command locally to the Unix domain sockets
- `albatross_client_remote_tls`: connects to a remote TLS endpoint and sends a command
- `albatross_provision_request`: creates a certificate signing request containing a command
- `albatross_provision_ca`: certificate authority operations: sign, generate, and revoke (NYI)
- `albatross_client_bistro`: command line utility to execute a command remotely: request, sign, remote (do not use in production, requires CA key locally)

## Installation

To install Albatross, run `opam pin add albatross
https://github.com/hannesm/albatross`.

Init scripts for FreeBSD are provided in the `packaging/FreeBSD/rc.d`
subdirectory, and a script to create a FreeBSD package
`packaging/FreeBSD/create_package.sh`.
For Linux, systemd service scripts are available in `packaging/Linux`.

It may help to read [the _outdated_ blog article](https://hannes.nqsb.io/Posts/VMM)
for motivation of albatross and an overview over its functionality.
