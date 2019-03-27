# Albatross: orchestrate and manage MirageOS unikernels with Solo5

[![Build Status](https://travis-ci.org/hannesm/albatross.svg?branch=master)](https://travis-ci.org/hannesm/albatross)

The goal of albatross is robust deployment of [MirageOS](https://mirage.io)
unikernels using [Solo5](https://github.com/solo5/solo5), including precise
error handling of failures. The code running under superuser privileges is
minimimal. Albatross is supposed to be run on a machine in the dom0, next to the
hypervisor. Albatross keeps track of unikernel resource usage (memory, CPUs,
bridges, block storage and active block devices). Policies restricting these
resources for administrative domains are available. Local and remote deployments
are supported, remote ones are authenticated and encrypted via a mutually
authenticated TLS connection using X.509 client certificates.  Multi-tenancy
deployments are possible, tenants do not need any other access to the machine:
console output and statistics gathered by the host are accessible via TLS.
Albatross keeps the information of running unikernels persistently, and starts
these unikernels when the albatross daemon is started. This means that whenever
a unikernel was started, it keeps running until it crashes or an explicit
destroy command is issued.

The administrative domain is similar to DNS: each unikernel has a name (e.g.
`foo.hello`), which consists of labels separated by dots. Policies and
access is done on a name basis - if access to `foo` is granted, `foo.hello`,
`foo.bar.hello`, etc. can be accessed, but not `bar` or `bar.hello`.

## Components

Albatross consists of a set of binaries. Several daemons, which communicate in a
request-response style over Unix domain sockets, are run in the host system:
- `albatrossd`: privileged to create and destroy unikernels
- `albatross_console`: reads the console output of unikernels
- `albatross_log`: event log
- `albatross_stats`: statistics gathering (rusage, ifstat, BHyve debug counters)
- `albatross_tls_endpoint`: remote deployment via TLS with client certificate, and proxies to local daemons
- `albatross_tls_inetd`: remote deployment via TLS and inetd (alternative to `albatross_tls_endpoint`)
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

`Albatross_tls_endpoint` and `albatross_tls_inetd` listen on a TCP port, and proxy requests from
remote clients to the respective daemons described above. They enforce client
authentication, and use the commen names of the client certificate chain as
administrative domain. The policies are embedded in CA certificates, the command
is embedded in the leaf certificate.

The following command-line applications for local and remote management are provided:
- `albatross_client_local`: sends a command locally to the Unix domain sockets
- `albatross_client_remote_tls`: connects to a remote TLS endpoint and sends a command
- `albatross_provision_request`: creates a certificate signing request containing a command
- `albatross_provision_ca`: certificate authority operations: sign, generate, and revoke (NYI)
- `albatross_client_bistro`: command line utility to execute a command remotely: request, sign, remote (do not use in production, requires CA key locally)

## Installation

To install Albatross, run `opam pin add albatross
https://github.com/hannesm/albatross`.

Init scripts for FreeBSD are provided in the `packaging/rc.d` subdirectory.

TODO: from here on, this documentation is not up to date.

It may help to read [the _outdated_ blog article](https://hannes.nqsb.io/Posts/VMM)
for motivation of albatross and an overview over its functionality.
