---
title: OTP Supervision Structure
date: 2014-04-29
tags:
    - development
    - erlang
menu: main
weight: 20
---

## Overview

The Erlang VM, or BEAM runtime, as it is more commonly known, manages a set of
independent, concurrent processes across many CPU cores. These processes are
structured into supervision trees, incorporating reliability and robustness
functionality by specifying restart policies, notification of failures within
the system, and responses to that -- for example, a typical process may be
configured to be restarted automatically up to 10 times within a given 5 minute
window. If this window is exceeded, the parent process, a supervisor, will also
be terminated. At each level of restarts, increasingly more internal state is
discarded, until either the input or function that causes the error is disposed
of, or is eliminated from the system. This approach is what gives the BEAM its
well-known reliability and robustness.

This section outlines the high level supervisor structure for the swirl app,
running within the BEAM VM.

## Major Components

The application swirl is a top-level OTP app, and comprises a number of separate
modules that run as individual supervisors underneath it, managed
hierarchically.

![Swirl OTP Structure](../../assets/img/otp_structure.png)

The blue box outlines the 4 OTP supervisors that represent the overall swirl
application, and the pairs of coloured dots and associated right parenthesis
show that each supervisor application has five associated workers below it. The
swirl app itself is shown with a purple dot, and has no associated workers, but
will manage restarting the supervisors themselves in the event of a failure of
an entire subsystem.

### swirl_app

As the top level container for the entire swirl OTP application, it manages the
supervisors for all peers, swarms, and channels underneath it. Starting and
stopping the swirl application effectively launches and halts all our running
processes in the BEAM VM.

- `peer_sup`, `swarm_sup`, and `channel_sup` are supervisors that manage pools
  of peers, swarms, and channels, for the main application
- each supervisor has again a corresponding OTP worker process that maps
  one-to-one for each peer, swarm, or channel that is active in the system

### peer_worker

This process manages a single UDP port, accepting inbound and outbound datagrams
for processing or delivery. Almost all work is done outside this process, in
channel or swarm workers. Inbound packets are scanned minimally to route them to
the correct `channel_worker`, where parsing and further handling is done.

While currently each peer_worker manages a single IP/port, it is
straightforwards to run multiple peer_workers sharing the same socket for
increased throughput, and indeed multiple peer_workers across different IP &
port ranges. This can be further distributed to other erlang nodes, and push
processing of received packets out elsewhere in the network.

If the peer_worker process crashes, it is automatically restarted, picking up
the port number and any related options from the supervisor. There is no
significant transient data managed or held by the peer_worker.

### swarm_worker

It owns the state for a given swarm - what the options are, how to retrieve
relevant data chunks from a cache or disk store, tracking which chunks should be
requested from the network, or are already present.

The swarm worker is a simple `gen_server` based loop bound to a specific
channel, which has a one-to-one correspondence with a specific swarm id. As the
swarm id is based on the merkle root hash of the associated data, this also
means that the chunk size, hash function, and similar swarm parameters are also
unique for this swarm/channel. The state kept by a swarm is slightly larger than
a given peer worker, as it includes the active peers within the swarm. At this
stage it is not clear if it will be necessary, for performance & stability, to
have an associated process per remote peer in the swarm, to track information
such as downloaded / requested chunks, choke status etc. The initial
implementation contains only the set of active peers and their channels, within
the process state itself, and the full swarm state within a related ets table,
per swarm. In the event of a swarm worker needing to be restarted, the data
contained in the ets table can be retrieved lazily as the swarm re-establishes
itself.

If the swarm_worker process crashes, it is automatically restarted, picking up
the swarm options from the supervisor. Information about which chunks are held
by which peers is stored on a per-channel basis, so there is no significant
impact should the swarm_worker die for any reason.

### channel_worker

A `channel_worker` represents a one-for-one mapping with an active, external
peer. It tracks the network location of a peer, the assigned channel, and what
chunks are available, or are required by, that remote peer. It coordinates the
bulk of the work for its own peer, relying only on the other modules when
actually writing data to the network, or updating the swarm_worker on
significant changes. Loss of a single channel (or external peer) should have
little to no effect on the swarm as a whole, or the available UDP ports.

If the channel_worker process crashes, it is automatically restarted, picking up
the channel options from the supervisor. Information about this specific peer is
lost, other than its transport address, but no other channels, peers nor swarms
are impacted. Information about this specific remote peer is effectively rebuilt
from the swarm, as messages such as `HAVE` and `REQUEST` are sent regularly.

### gproc

The [gproc] erlang application is a 3rd party library, included in swirl. It is
heavily used across the Erlang community. It acts as a distributed information
store, allowing registration and lookups of such queries as "which channels are
a member of this swarm", or "which peers have a given chunk, or range of
chunks". It is based on an erlang in-memory database [ets] table, and provides a
cache of data associated with each worker process listed above. If a given
worker terminates, [gproc] automatically expires the relevant entries, and any
processes that rely on this information are informed of that termination.

If [gproc] crashes, the entire swirl application, including all peers, channels,
ports, swarms and workers, will also terminate. This is relatively unlikely due
to both the extensive use of [gproc] within the erlang community, and due to
separation of the [gproc] application from performing user-defined
functionality. [gproc] does support a multi-server distributed mode, which can
be more resilient, however the cost is that some queries would become global,
which is significantly slower, and netsplits become a concern. Alternatives
include using a library such as [riak_core] or similar distributed consistency
libraries and running multiple copies of swarm_workers across an erlang cluster,
to allow handling recovery from erlang node restarts or application outages.

### other Apps

All other functionality runs from within these supervisor trees, triggered by
messages between proceses, and external packets coming from the network, or used
as a library application from within one of the above supervisors and workers.

[gproc]: https://github.com/uwiger/gproc [ets]:
http://erlang.org/doc/man/ets.html [riak_core]:
https://github.com/basho/riak_core
