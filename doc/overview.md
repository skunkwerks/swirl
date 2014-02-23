# PPSP Overview

PPSP is a transport protocol — it transfers a stream of opaque binary data from one location to another. It is unique amongst transport protocols as it is a many-to-many transfer protocol, that is, there is no single master server or endpoint that manages the data transfer.

A swarm is a set of peers that are sharing (receiving and/or transferring) the same data, as a set of small chunks, which is identified by a unique cryptographic recursive hash of the data, called the Root Hash.

## Chunks and Datagrams

The chunks are transferred within UDP datagrams, each comprising one or more
idempotent messages, each of which, in turn, forms part of a conversation
between several peers.

Data may be obtained from any connected peer, and by design, the integrity of any given chunk of data can be verified efficiently using the cryptographic hash of that data chunk, and the preceding chunks or hashes thereof.

The data is hashed in a tree structure, which enables efficient validation of both the integrity of the data, and also of other nearby chunks within the tree.

## The Peer-to-Peer Streaming Peer Protocol, in Brief

The protocol itself is quite straightforwards:

- peers locate each other initially via a separate [tracker] protocol
- they send handshake messages, selecting a common set of protocol options, including the  root hash of the content that the swarm is managing
- subsequently they exchange messages including:
    - chunks requested from other peers
    - an updated list of chunks that other peers now have
    - any new peers that have joined the scheme
    - performance information, including choke and unchoke messages
- once sufficient chunks have been retrieved, the peer may disconnect, or remain connected to help share the data within the swarm
- peers that repeatedly send invalid chunks are eventually ignored either
faulty or malicious, and are subsequently ignored

See the PPSPP draft, [section 2] for a longer introduction.

## Live Streaming Support

PPSPP includes the ability to manage a transfer of data that is only just beginning to be created. For example, [live streaming] video or audio can be shared immediately to other peers, before the full broadcast has even finished. A sliding window root hash is cryptographically signed with public key, so that each peer can be sure that the new root hash has come from the trusted originator of the content.

## Further Reading

The [PPSPP] draft and related [tracker] specifications are excellent resources. In particular, for the core PPSP protocol, read [section 3] 3.1 - 3.9, skip the PEX* stuff, then jump to [section 8], 8.1 - 8.8.
there is a summary on [merkle trees](merkle.md) within this documentation.

[section 2]: http://tools.ietf.org/html/draft-ietf-ppsp-peer-protocol#section-2
[section 3]: http://tools.ietf.org/html/draft-ietf-ppsp-peer-protocol#section-3
[section 8]: http://tools.ietf.org/html/draft-ietf-ppsp-peer-protocol#section-8

# PPSPP Terminology

### Peer

An application that listens on a given network address (e.g. IP address an UDP port) for PPSPP datagrams, and dispatches those to the appropriate PPSP channel within that peer.

A peer may belong to multiple swarms, with different parameters, depending on the implementation model. For example, a single peer may host a live video download in several different quality or performance types. Each of these video streams must map to a different swarm ID, as the underlying data being transferred will be cryptographically unique.

### Swarm

A group of peers that are sharing & transferring a given stream of binary data, with certain specific parameters common to the swarm, including the Root Hash, Chunk Size, and number of related parameters. All the peers participating in a given swarm will be using the same parameters for this specific swarm. Note that a peer may be participating in many swarms; channels are used to manage this.

### Channels

A channel is a unique, per-peer identifier, that enables multiplexing many individual swarms onto a single IP address / UDP port pair. Note that the channel is unique also for remote peers too.

### Hash

A cryptographic function that reduces a large chunk to a small digest. In PPSP, the hash is usually [SHA1]. By providing hashes of data at the same time as transferring the chunk itself, the received may determine if the data or hash was corrupted in transit, and if necessary discard and retrieve the data again.

### Hash Trees

PPSP uses a more generalised hashing scheme, called a Merkle signature scheme. The hash tree is simple, shown with this graphic from the [tribler] project. Note that the data chunks are laid out along the bottom row of the graphic, and concatenated hashes occupy the tree's roots:

![merkle hash tree](https://github.com/skunkwerks/swirl/wiki/images/merkletree-v4.png)

This final hash is known as the Root Hash in PPSPP, and the root hash, along with a select set of hashes as specific nodes further down the tree, is what enables a given peer to determine, without having the entire data stream already downloaded, whether the data sent by the other peer is both valid, and present in this specific swarm's data.

For more details, refer to this project's [merkle] hash tree documentation.

[SHA1]: http://tools.ietf.org/html/rfc3174
[tribler]: http://www.tribler.org/
[tracker]: http://tools.ietf.org/html/draft-ietf-ppsp-base-tracker-protocol
[PPSPP]: https://datatracker.ietf.org/doc/draft-ietf-ppsp-peer-protocol
[live streaming]: http://tools.ietf.org/html/draft-ietf-ppsp-peer-protocol#section-6