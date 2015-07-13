---
categories:
- blog
- design
date: 2015-07-13T21:46:45+02:00
tags:
- concepts
- ppspp
title: Swarms, Peers, and Channels
description: Understanding the difference between Swarms, Peers, and Channels
weight: 0
---

The most important components within PPSPP are Swarms, Peers, and Channels.
It wasn't until I started implementing that I understood how best these
can be decoupled. The PPSPP [RFC7574] could perhaps do a better job of
articulating the difference between these concepts, and perhaps I could
have contributed it.

## The Swarm

I think of a swarm as two things, one spatial, and one temporal, linked by
a common set of options. Of most importance, is the choice of hashing
algorithm and chunk size, and these control what the root hash must be of
the Merkle tree.

The spatial component is easy - the raw data on disk, or being shared within
the swarm, which is effectively a list, or set, of chunks, or ranges of
chunks of the content.

The temporal component can be thought of as the current active set of active
peers -- essentially a list of related network transport addresses.

In terms of state, then, a swarm can be represented as the intersection of
a set of chunks (possibly simplified through use of ranges), and a set of
currently active peers.

This allows us to ask and answer questions such as "where can I get chunks
X & Y", or "does anybody else in the swarm still need chunk Z"?

## Peers

I believe peers are where people get confused, at least in hindsight it was
where I got confused, which is not the same thing. Anyway. Peers are
compared to bittorrent nodes, but this is not correct.

The defining characteristic of a peer is a network transport address
comprising an `IP address`, and a `UDP Port`. Nothing else. PPSP allows
multiplexing many swarms through a single endpoint, to help support the very
common use cases of mobile networks (running phones on IPv6 and bridging
IPv4 in the telco's datacentre) and home / office routers (IPv4 NAT).

In the swirl model, all the peers are managed separately. They are very very
simple wrappers around a UDP port, and simply use the first 4 bytes of each
incoming packet to decide which channel to route this to &mdash; more on
that below.

Thus the peer's state really only consists of an assigned UDP port, and what
ever IP address is appropriate for the network - either a NATted IPv4
address, IPv6 or a direct IPv4 connection.

It is possible to keep a list of active channels and remote peers within the
Peer state, but it actually makes more sense to keep these with each channel
that is associated with the remote peer, and if necessary, register these
within Swirl so that we can answer questions like "how many active Swirl
peers are in this swarm?" or "which channels are participating?".

In the Swirl model, this complexity is left to the channel to manage. Let's
look at them next.

## Channels

The channel then, binds all of these together. A channel in PPSP is loosely
described as:

> A logical connection between two peers. The channel concept allows peers
> to use the same transport address for communicating with different peers.
> -- [RFC7574]

The thing that relates all three components, channels, swarms, and peers, is
that they share the same swarm options. This is a slight inaccuracy as it's
possible for swarms to *not* use Merkle hash trees, and just shuttle chunks
and ranges around, but you might as well use HTTP in that case, so we'll
conveniently ignore it.

The swarm is a logical entity, so let's assume that our mighty swarm
comprises two peers: IP/UDP endpoints. Because each endpoint might be part
of many other swarms, each datagram must include a tag to ensure we can
match packets to their appropriate swarms. This tag is always the first 4
bytes in big endian network order. For the first communication between peers
they send their `handshake` requests to the special `channel zero`, and the
subsequent `handshake` reply provides the remote peer with a new channel for
subsequent communications.

> [3.11. Channels]
>
> It is increasingly complex for peers to enable communication between
> each other due to NATs and firewalls.  Therefore, PPSPP uses a
> multiplexing scheme, called channels, to allow multiple swarms to use
> the same transport address.  Channels loosely correspond to TCP
> connections and each channel belongs to a single swarm, as
> illustrated in Figure 1.  As with TCP connections, a channel is
> identified by a unique identifier local to the peer at each end of
> the connection (cf.  TCP port), which MUST be randomly chosen.  In
> other words, the two peers connected by a channel use different IDs
> to denote the same channel.  The IDs are different and random for
> security reasons, see Section 12.1.
> 
> In the PPSP-over-UDP encapsulation (Section 8.3), when a Channel C
> has been established between Peer A and Peer B, the datagrams
> containing messages from Peer A to Peer B are prefixed with the four-
> byte channel ID allocated by Peer B, and vice versa for datagrams
> from Peer B to A.  The channel IDs used are exchanged as part of the
> handshake procedure, see Section 8.4.  In that procedure, the channel
> ID with value 0 is used for the datagram that initiates the
> handshake.  PPSPP can be used in combination with Session Traversal
> Utilities for NAT (STUN) [RFC5389].

In Swirl, a channel represents the main place to capture and store state
that is specific to a given remote peer, in a unique swarm. The
`channel_worker` needs to track what chunks are available at each peer,
what data is requested by a remote peer, its network address, and any
special constraints such as if the peer only supports a subset of message
types to converse with.

The consequence of binding this complex state into a `channel_worker` is
that, even if a particular swarm participant has issues, we do not lose the
state of the entire swarm, nor our active UDP ports, but only that
individual peer. The only important state that a `channel_worker` needs to
have is the remote endpoint (IP address, UDP port, & channel id) it
communicates with, and the `swarm id` that it is also part of. In the event
a worker is restarted, it can very quickly retrieve the remaining state from
its peer in subsequent `have` and `request` messages.

## Summary

By keeping these components separated, we can dramatically increase the
reliability of both the Swirl application, and also the availability of the
overall swarm service for all participants.

[RFC7574]: https://tools.ietf.org/html/rfc7574
[RFC5389]: https://tools.ietf.org/html/rfc5389
[3.11. Channels]: https://tools.ietf.org/html/rfc7574#section-3.11
