---
date: 2015-06-12T10:38:52+02:00
description: Technical Design Document
menu: main
title: Core State
weight: 50
---

## Overview

Within PPSP, there are 4 main types of state we are interested in tracking:

1. what [swarms]({{< relref "supervision.md#swarm-worker" >}}) are we participating in?
1. which [peers]({{< relref "supervision.md#peer-worker" >}}) are we in contact with?
1. which [channels]({{< relref "supervision.md#channel-worker" >}}) are active?
1. what UDP [ports]({{< relref "supervision.md#peer-worker" >}}) are open?

The Core is a set of 4 OTP applications that provide managed supervisor and
worker functionality to manage these states per unique channel, peer, port,
and swarm, underneath the overall swirl application. These top-level OTP
apps are not started when swirl itself is started, but only when the
relevant component is required - for example, the `peer_worker` itself is
not instantiated until a UDP port is required for sending or receiving data,
and a `swarm_worker` is only started when a specific swarm (file or content)
is to be shared.

## Description

The Core is a logical grouping of these 4 types of state into separate OTP
applications, as seen below.

![OTP supervision trees](../../assets/img/otp_structure.png)

This document covers the design & implementation of the workers that
encapsulate each type of state.

The main feature of note is the choice to group swarms, channels, and peers
separately, under their own supervisors, rather than a structure where a
single swarm, related channels, and peers, all share a common supervisor.
Further information on the structure itself is found under
[Supervision trees]({{< relref "supervision.md" >}}).

The reason for this is that within PPSPP a peer instance, with a
corresponding port, can host multiple swarms, and that a swarm is not
necessarily bound to a specific UDP port, but could use multiple ports and
network interfaces. Only a specific channel must be unique between two
peers, and so channels are used as the definitive association between a peer
and a given swarm. Within Swirl, a channel will be uniquely assigned, i.e.
there will be no duplicates.

This leaves open the option of spreading swarms, peers, and channels across
potentially different erlang nodes in future to optimise for certain
architectures or performance requirements.

## Implementation

## Lookups and the [gproc] Registry

As a consequence of separating peers, channels, and swarms, all processes
must use a registry, [gproc], a third-party Erlang/OTP app, widely used in
the community to look up process ids (pids), or request whatever key/value
data is needed, such as the transport address (IP + UDP port) for a channel,
or chunk addressing scheme used within a particular swarm.

`gproc` provides an in-memory key-value store where keys are linked to their
owning processes. In the event of a process terminating (whether gracefully
or not), `gproc` automatically removes the invalid entries. This makes
overall management of state within Swirl very simple - cleaning up and
restarting a given worker is as easy as terminating itself and restarting,
whereupon it will pick up the minimal state already provided at startup, and
acquire any further state through valid transactions with other peers or
erlang processes.

This approach is a key part of Erlang application design, often called the
`error kernel`, where only the minimum amount of state required to bootstrap
the required functionality is carefully preserved between potential crashes
or restarts.

For example, in the case of a PPSPP swarm, the minimal set of information
required is the complete set of [PPSPP options], including the root hash and
hashing algorithm, and whatever transport address (IP address and UDP port),
that is being advertised. When the swarm restarts, and existing peer will
continue to contact the swarm on the same IP and channel, and these will
simply be re-created as packets are received. Some additional traffic will
no doubt be incurred as out peer re-establishes its list of peers, and their
chunk status, but this state clearly is easily recovered simply by
participating in the swarm, and does not need special management or
persistence.

## [peer_worker]({{< relref "peer_worker.md" >}})

### Responsibilities

The `peer_worker` is a single module that provides a local instance of a
PPSPP Peer:

- spawning and managing a UDP port
- handling incoming UDP datagrams
- routing them to a separate process for processing

The worker process is registered using `gproc` as outlined above, with
the UDP port as the key, and optionally swarm parameters if supplied at
time of `gen_server` startup.

In the even of a failure, for example, UDP port is closed, the supervising
process will restart the `peer_worker` automatically, with the minimum state
again, and re-using the same UDP port to register on.

### Lifecycle

The `peer_worker` is instantiated when a UDP port is requested, typically
to host a swarm, or join one.

It continues receiving UDP datagrams and sends them to a `channel_worker`
for processing, and stays active until explicitly shut down.

### Tests

- start the worker & confirm it is active and registered
- stop the worker & confirm clean termination and has been unregistered
- receive any UDP datagram & ensure the server process is still live

### Generated Module Docs

- [Data Types]({{< relref "peer_worker.md" >}}#types)
- [Function Index]({{< relref "peer_worker.md" >}}#index)
- [Function Details]({{< relref "peer_worker.md" >}}#functions)

### [swarm_worker]({{< relref "swarm_worker.md" >}})

### Responsibilities

The `swarm_worker` is a single module that provides a local instance of a
PPSPP Swarm:

- starting and stopping a unique registered instance of a swarm within swirl
- given the swarm id, locate the managing swarm_worker process
- given the swarm id, retrieving associated options for the swarm

The worker process is registered using `gproc` as outlined above, with
the `swarm_id` as the key, and the additional PPSPP options as swarm
parameters.

Each `swarm_worker` process need only look up the `swarm_id` and
`port_number` in the registry to find a suitable `peer_worker`.

### Lifecycle

The `swarm_worker` is instantiated when a new swarm is required.

It provides a single location to locate and retrieve PPSPP data chunks for
a given swarm, even if these are stored in an external file or an
in-memory cache.

The `swarm_worker` stays active until explicitly shut down.

### Tests

- start the worker & confirm it is active and registered
- look up PPSPP options for the worker and confirm they match initial state
- stop the worker & confirm clean termination and has been unregistered
- retrieve the type of storage associated with this swarm

### Generated Module Docs

- [Data Types]({{< relref "swarm_worker.md" >}}#types)
- [Function Index]({{< relref "swarm_worker.md" >}}#index)
- [Function Details]({{< relref "swarm_worker.md" >}}#functions)

## [channel_worker]({{< relref "channel_worker.md" >}})

- represents a remote swarm participant
- does the dirty work of decoding datagrams, handling messages, and
sending responses via new encoded datagrams
- maintains information about remote peer

The `channel_worker` is a module that provides a unique instance of a PPSPP
Channel. It maintains state for a specific remote peer, and registers both
the channel and the transport address for that peer in the registry.

When PPSPP peers connect, they contact each other on the already-known IP and
UDP port transport address, on the generic `channel 0`. Each peer supplies
its own local randomly generated `channel_id` to be used in future
callbacks. This allows PPSPP to support multiplexing many remote peers into
a single UDP port. Note that this id is assigned separately by each peer,
i.e. a peer will have a local channel id that is most likely different from
the remote id that it shared with another peer. This is often a source of
confusion!

The channel worker is significant in that it binds a unique transport
address (IP address + UDP port) together with a local randomly generated
64-bit UUID for that swarm. These keys are registered with `gproc` and allow
other workers to identify what channels are involved in a given swarm, or
which peers to contact for chunks, for example.

Thus, also within swirl, a given remote peer may appear multiple times in
the registry, each with a different `channel_id`, if that remote peer is
participating in multiple swarms.

### Responsibilities

It receives incoming datagrams from `peer_worker` processes, and parses
them. Once successfully parsed, the channel_worker will handle any received
messages, including deciding on further actions if any to be taken, such as
sending additional chunks, updating local state, or simply closing down, if
for example, a timeout has passed -- PPSPP typically assumes a maximum 3
minute lifespan for a channel / peer without an explicit response or
heartbeat.

Within swirl, this same id is used to identify the remote peer that we are
communicating with, and for a particular swarm on a remote peer, there will
only be a single matching `channel_worker`. If swirl is participating
in multiple swarms with that remote peer, then there will be multiple
different channels present in swirl, each one representing the channel.

- starting and stopping a unique registered instance of a channel within swirl
- given the swarm id, locate the managing channel_worker process
- given the swarm id, retrieving associated options for the swarm

The worker process is registered using `gproc` as outlined above, with
the `channel_id' as the key, and the additional PPSPP options as swarm
parameters.

Each `channel_worker` process need only look up the `channel_id` and
`port_number` in the registry to find a suitable `peer_worker` to route
outgoing datagrams to.

### Lifecycle

The `channel_worker` is created using a 
### Tests

- start the worker & confirm it is active and registered
- look up PPSPP options for the worker and confirm they match initial state
- stop the worker & confirm clean termination and has been unregistered

### Generated Module Docs

- [Data Types]({{< relref "channel_worker.md" >}}#types)
- [Function Index]({{< relref "channel_worker.md" >}}#index)
- [Function Details]({{< relref "channel_worker.md" >}}#functions)

The `channel_worker` will maintain its own channel state, and only needs to know how to look up the outgoing swarm and peer `pid` to send datagrams.


[gproc]: https://github.com/uwiger/gproc
[PPSPP options]: https://tools.ietf.org/html/rfc7574#section-7
