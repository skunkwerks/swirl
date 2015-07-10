---
title: Swirl Implementation
description: Overview of the project implementation phases
date: 2014-04-29
menu: main
weight: 10
tags:
    - overview
    - funding
    - development
---

The Swirl project aims to deliver a fully functional implementation of the
new [IETF] Transport Protocol [Peer-to-Peer Streaming Peer Protocol], known
as `PPSP`, in a number of languages and frameworks.

The first implementation is being written in [Erlang/OTP], which is a
functional programming language combined with a robust set of application
libraries designed and tested for writing scalable and reliable telco and
internet services. Erlang is widely used in mission-critical applications
today, from military, financial, telecoms, medical, as well as
internet-scale games, databases, social networks, and messaging services.

## Approach

The chosen approach is to implement swirl in phases, and funding is being
provided generously by [NLNet] for the first stages:

1. a [decoder]({{< relref "decoder.md" >}}), taking internet / wire format
    to internal parsed format
2. an [encoder]({{< relref "encoder.md" >}}), performing the reverse step
    from internal format to the wire
3. a [storage]({{< relref "storage.md" >}}) layer comprising merkle tree,
    binmap lookups and key-value storage
4. simple [deployment]({{< relref "deployment.md" >}}) including FreeBSD and
    Linux/docker support

Further information on timeframes can be found in the high level
[planning]({{< relref "planning.md">}}) guide.

## Understanding the Codebase

To make the most sense of the application itself, it's recommended to read
in order:

- high-level code [layout]({{< relref "layout.md" >}}) introducing the
  general structure of the modules
- the [supervision]({{< relref "supervision.md" >}}) tree (application
  hierarchy) implemented in Erlang/OTP
- specific information per [module](../api/)

## Future Plans

Further phases are planned, currently:

- live [streaming]({{< relref "streaming.md" >}}) support
- [zeroconf]({{< relref "zeroconf.md" >}}) / MDNS support (also known as
  bonjour protocol) for identifying local peers
- NAT [traversal]({{< relref "traversal.md" >}}) support, via STUN/TURN/ICE
  etc, depending on the outcome of the IETF standardisation process
- [browser]({{< relref "browser.md" >}}) support
- native [mobile]({{< relref "mobile.md" >}}) device support
- [embedded]({{< relref "embedded.md" >}}) systems such as desktop boxes and
  digital television
- support for [billing]({{< relref "billing.md" >}}) functionality
- enabling [traffic]({{< relref "traffic.md" >}}) management and shaping
  functionality and interfaces

[Erlang/OTP]: http://www.erlang.org/
[NLNet]: http://nlnet.nl/news/2013/20130901-awards.html
[IETF]: https://www.ietf.org/
[Peer-to-Peer Streaming Peer Protocol]: https://datatracker.ietf.org/doc/rfc7574
