# Albatross: Managing virtual machines

[![Build Status](https://travis-ci.org/hannesm/albatross.svg?branch=master)](https://travis-ci.org/hannesm/albatross)

A set of binaries to manage, provision, and deploy virtual machine images.  This
is very much work in progress, don't expect anything stable.

Please read [the blog article](https://hannes.nqsb.io/Posts/VMM) for motivation
and an overview.

The implementation uses explicit errors (no exceptions), and make mostly use of
the (blocking!) Bos library for operating system commands.  A thin layer of Lwt
is used on top to (more gracefully) handle multiple connection, and to have a
watching thread (in `waitpid(2)`) for every virtual machine started by vmmd.

To install Albatross, run `opam pin add albatross
https://github.com/hannesm/albatross`.  On FreeBSD, `opam pin add
solo5-kernel-ukvm --dev` is needed as well.

The following elaborates on how to get the software up and running, following by
provisioning and deploying some unikernels.  There is a *server* (`SRV`)
component which needs six binaries: vmm_console, vmm_log, vmm_stats_lwt, vmmd,
ukvm-bin.none, and ukvm-bin.net; a `CA` machine (which should be air-gapped, or
at least use some hardware token) for provisioning which needs vmm_sign, and
vmm_gen_ca; and a *development* (`DEV`) machine which has a fully featured OCaml
and MirageOS environment.  Each step is prefixed with the machine it is supposed
to be executed on.  Of course you can conflate everything into a single
development system or your server, all up to you and your security scenario.

Exact file transfer operations between these machines is not in scope of this
document, but kept abstract as `COPY`.  Some commands require superuser
privileges (use `sudo`, `su`, or `doas`), I prefixed them with `#`.

File endings used in this document:
- `.db` for CA databases
- `.pem` for (PEM-encoded) signed certificates
- `.key` for (PEM-encoded) private keys
- `.req` for (PEM-encoded) certificate signing requests

## Setup a certificate authority

The first step is to setup a certificate authority (private key and CA
certificate).  The CA private key can sign and revoke everything, you should
better keep it in a safe place (air-gapped machine etc.) - not on the server!

```
CA> vmm_gen_ca ca ca.db [--days 3650] [--server "server"] [--server-days 365]
```

This generated five files:
- `ca.key` which is the CA private key
- `cacert.pem` which is the CA certificate
- `ca.db` which contains a map between serial number and name of issued certificates
- `server.pem` is the server certificate
- `server.key` is the private key of the server

## Server setup

If you have installed this package on your development machine, follow some more
steps to produce the remaining required binaries:

```
CA> COPY cacert.pem server.pem server.key SRV:
DEV> git clone https://github.com/mirage/mirage-skeleton.git
DEV> cd mirage-skeleton/tutorial/hello
DEV> mirage configure -t ukvm
DEV> mirage build
DEV> mv ukvm-bin /tmp/ukvm-bin.none
DEV> cd ../device-usage/network
DEV> mirage configure -t ukvm
DEV> mirage build
DEV> mv ukvm-bin /tmp/ukvm-bin.net
DEV> cd ../../..
DEV> COPY /tmp/ukvm-bin.none /tmp/ukvm-bin.net SRV:
DEV> COPY vmm_console vmm_log vmm_stats_lwt vmmd SRV:
```

```
SRV> vmm_console -vv cons.sock &
SRV> vmm_log -vv log.out log.sock &
SRV> vmm_stats_lwt -vv stat.sock & #optional
SRV# vmmd -vv . cacert.pem server.pem server.key
```

Some setup for network interfaces is needed, depending on your operating system.
You can also add NAT to allow your virtual machines to talk to the outside
world, or add your external interface to the bridge directly, or just keep your
VMs local.

```
# FreeBSD
SRV# ifconfig bridge create #should output bridge0
SRV# ifconfig bridge0 name ext
SRV# sysctl net.link.tap.up_on_open=1
# Linux
SRV# brctl addbr ext
```

## Provision our first virtual machine

We will delegate some resource to a certificate and key we keep on our
development machine.

```
DEV> vmm_req_delegation dev 2 1024 --cpu 1 --bridge ext/10.0.0.2/10.0.0.5/10.0.0.1/24
DEV> COPY dev.req CA:
```

This produced two files, dev.req and dev.key.  Keep the key in a safe place!

```
CA> vmm_sign ca.db cacert.pem ca.key dev.req [--days 10]
CA> COPY dev.pem DEV:
```

Now, our DEV machine can use its delegation certificate for issuing other
certificates.  We'll create a certificate for interactive use, and one
containing the hello unikernel.

```
DEV> vmm_req_permissions admin --permission all
DEV> vmm_sign dev.db dev.pem dev.key admin.req
```

This produced in the first step two files, `admin.req` and `admin.key`, and in
the second step two more files, `dev.db` and `admin.pem`.

```
DEV> vmm_req_vm hello mirage-skeleton/tutorial/hello/hello.ukvm 12 1
DEV> vmm_sign dev.db dev.pem dev.key hello.req
```

This generates a private key `hello.key` and a certificate signing request named
`hello.req` including the virtual machine image `hello.ukvm`, which gets 12MB
memory and CPU id 1.  The second command used the `dev.key` to sign the signing
request and output a `hello.pem`.

The flag `--force` can be passed to `vmm_req_vm`.  This means: if there already
exists a running virtual machine with the same name, kill it and start the new
one provided in the certificate.

To actually deploy anything, the server needs the chain (i.e. the vm certificate
and the delegation certificate).  Our client needs the main CA certificate to
authenticate the server itself.

```
CA> COPY cacert.pem DEV:
DEV> cat admin.pem dev.pem > admin.bundle
DEV> cat hello.pem dev.pem > hello.bundle
```

And deploying (watch the output of the processes started on the server above!):

```
DEV> vmm_client cacert.pem hello.bundle hello.key SRV:1025
DEV> vmm_client cacert.pem admin.bundle admin.key SRV:1025 --db dev.db
```

Commands are at the moment `info`, `statistics`, `destroy`, `attach`, `detach`,
and `log`.
