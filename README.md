# Albatross: orchestrate and manage MirageOS unikernels with Solo5

The goal of albatross is robust deployment of [MirageOS](https://mirage.io)
unikernels using [Solo5](https://github.com/solo5/solo5). Resources managed
by albatross are network interfaces of kind `tap`, which are connected to
already existing bridges, block devices, memory, and CPU. Each unikernel is
pinned (`cpuset` / `taskset`) to a specific core.

Albatross allows remote management. To deploy or destroy a unikernel, no shell
access is necessary. The remote channel is a mutually authenticated (with X.509
certificates) TLS connection. Console output of the unikernels is stored in
memory in a ring buffer, and accessible remotely. Monitoring data (CPU and
memory usage) of the unikernels can be collected as well, and pushed into an
Influx time series database.

Albatross consists of multiple processes, each running with the least
privileges. Albatross can be run next to other orchestration systems; it does
not assume to be the single instance on a dom0 which creates and destroys
virtual machines. Resource policies can be dynamically configured for each
administrative domain (similar to DNS, a hierarchical naming scheme), and are
checked statically (to decrease while going down the tree)
and dynamically when a new unikernel is to be deployed.

When a unikernel is deployed, albatross tries its best to keep it
running, even when the physical hardware reboots, or albatross is restarted.
When the unikernel exits, depending on configuration and its exit code, it is
re-started. The current set of running unikernels is persisted on disk, though
there is no dependency or order for restarting them.

The scope of albatross is to provide a minimal orchestration system that avoids
the need for shell access on the dom0. This leads to mostly immutable - or only
mutable via albatross - infrastructure. Further dissemination of albatross into
virtual machines, and a communication interface for deploying and destroying
unikernels, is being researched on.

## Components

Albatross consists of a set of binaries. Several daemons, which communicate in a
request-response style over Unix domain sockets, are run in the host system:
- `albatrossd`: privileged to create and destroy unikernels
- `albatross-console`: reads the console output of unikernels
- `albatross-stats`: statistics gathering (rusage, ifstat, BHyve debug counters)
- `albatross-tls-endpoint`: remote deployment via TLS (and possibly inetd)
- `albatross-influx`: statistic reporting from `albatross-stats` to influx

The main daemon is the privileged `albatrossd`, which supervises unikernels. It opens
a listening Unix domain socket, reads the persisted unikernel configuration,
starts these unikernels, and awaits commands. Access can be regulated by Unix
file permissions -- only those users who can write to that socket can send
commands.

`Albatross-console` does not keep any persistent state, but a ring buffer of console
output from each unikernel. These messages can be retrieved by a client as a
stream of messages (history, and whenever a new line is output, it is sent to
the interested client). Each unikernel output can only be read by a single
client, to avoid amplification of traffic if lots of clients are connected.
`Albatrossd` sends a message to `albatross-console` whenever a new unikernel is started,
upon reception `albatross-console` opens and reads the fifo which the unikernel will
write their standard output to.

`Albatross-stats` periodically gathers statistics (memory, CPU, network, hypervisor)
from all running unikernels.

`Albatross-tls-endpoint` listens on a TCP port, or uses systemd socket activation, or
via inetd, and proxies requests from remote clients to the respective daemons described
above. It enforces client authentication, and uses the common names of the client
certificate chain as the administrative domain. The policies are embedded in CA
certificates, and the command is embedded in the leaf certificate.

The following command-line applications for local and remote management are provided:
- `albatross-client-local`: sends a command locally to the Unix domain sockets
- `albatross-client-remote-tls`: connects to a remote TLS endpoint and sends a command
- `albatross-provision-request`: creates a certificate signing request containing a command
- `albatross-provision-ca`: certificate authority operations: sign, generate, and revoke (NYI)
- `albatross-client-bistro`: command line utility to execute a command remotely: request, sign, remote (do not use in production, requires CA key locally)

## Albatross over TLS

Albatross uses PKI to authenticate both client and server. Requests are signed
by a certificate authority (CA) that is trusted by the server. CA can delegate
resources using policies, which happens by creating an intermediate CA.
Revokation is not implemented, as delegation happens without the server knowing
about it.


This example shows how one can delegate part of the resources to a user. There
are 4 entities:
- **server**: runs the albatross TLS endpoint
- **CA**: root certificate authority
- **intermediate CA**: user certificate authority
- **client**: requests initiator

_Note_: there are 4 entities but depending on the security model some can exist
on the same machine. For example, when **client** and **intermediate CA** can be
combined, requests are automatically signed using `albatross-client-bistro` (see step 8).

## Setup

This step-by-step guide shows how files are generated and to which entity they
belong. Filename is in **bold** when it's created by the current step.

1. Generate the root CA certificate and server keypair

```
albatross-provision-ca generate ca db
```


| description           | | server         |  CA            | intermediate CA | client |
| --------------------- |-| -------------- | -------------- | --------------- | ------ |
| _private key_         | | **server.key** | **ca.key**     |                 |        |
| _public certificate_  | | **server.pem** | **cacert.pem** |                 |        |

2. **server:** start the endpoint using the server keypair and the root CA certificate

```
albatross-tls-endpoint cacert.pem server.pem server.key
```

3. **intermediate CA:** we want to delegate part of the resources to a given
user. The user generates a signing request to allow a memory of 1024MB to run
16 unikernels on CPU IDs 0 and 1.

```
albatross-provision-request add_policy user 16 --mem 1024 --cpu 0 --cpu 1
```

| description                   | | server     |  CA        | intermediate CA | client |
| ----------------------------- |-| ---------- | ---------- | --------------- | ------ |
| _private key_                 | | server.key | ca.key     | **user.key**    |        |
| _public certificate_          | | server.pem | cacert.pem |                 |        |
| _certificate signing request_ | |            |            | **user.req**    |        |

4. **CA:** CA signs the user's request, which generates an intermediate CA
certificate containing the restriction policies (limited memory, cpu), which in
turn will be used to sign user requests.

```
albatross-provision-ca sign cacert.pem db ca.key user.req
```

| description                   | | server     |  CA        | intermediate CA | client |
| ----------------------------- |-| ---------- | ---------- | --------------- | ------ |
| _private key_                 | | server.key | ca.key     | user.key        |        |
| _public certificate_          | | server.pem | cacert.pem | **user.pem**    |        |
| _certificate signing request_ | |            |            | user.req        |        |

5. **client:** the client wants to create an unikernel, instead of using the
albatross-client-local command, it has to wrap the request in a
certificate signing request which will be submitted to the intermediate CA.

```
albatross-provision-request create hello hello-key.hvt [--arg='--hello=albatross-hi'] [--cpu=1]
```

| description                   | | server     |  CA        | intermediate CA | client        |
| ----------------------------- |-| ---------- | ---------- | --------------- | ------------- |
| _private key_                 | | server.key | ca.key     | user.key        | **hello.key** |
| _public certificate_          | | server.pem | cacert.pem | user.pem        |               |
| _certificate signing request_ | |            |            | user.req        | **hello.req** |

6. **intermediate CA:** the intermediate CA signs the request

```
albatross-provision-ca sign user.pem db user.key hello.req
```

| description                   | | server     |  CA        | intermediate CA | client        |
| ----------------------------- |-| ---------- | ---------- | --------------- | ------------- |
| _private key_                 | | server.key | ca.key     | user.key        | hello.key     |
| _public certificate_          | | server.pem | cacert.pem | user.pem        | **hello.pem** |
| _certificate signing request_ | |            |            | user.req        | hello.req     |

7. **client:** client sends the signed request to the server,
`albatross-provision-ca` appended the intermediate CA certificate to `hello.pem`
to form the full chain.

```
albatross-client-remote-tls cacert.pem hello.pem hello.key <REMOTE_IP:PORT>`
```

8. Steps 5, 6, and 7 can be done in a single command - if there's no requirement
to retain the signing request and certificate, and the user keys are on the
local machine.

```
albatross-client-bistro create hello hello.hvt --ca=user.pem --ca-key=user.pem --server-ca=cacert.pem <REMOTE_IP:PORT> [--arg='--hello=albatross-hi'] [--cpu=1]
```

## Installation

Binary packages are available for Debian, Ubuntu and FreeBSD. [How to install](https://robur.coop/Projects/Reproducible_builds).

For other operating systems / distributions, run `opam install albatross`.

Also read [the blog article](https://hannes.nqsb.io/Posts/Albatross)
for the motivation behind albatross and an overview of its functionality.
