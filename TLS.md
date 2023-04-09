# Albatross over TLS

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
combined, requests are automatically signed using `albatross-client-bistro`.

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
albatross-provision-request create hello hello-key.hvt
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
