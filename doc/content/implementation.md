---
title: Swirl Implementation
date: 2014-04-29
menu: main
weight: 5
tags:
    - overview
    - funding
    - development
---

The Swirl project aims to deliver a fully functional implementation of the PPSP
protocol in a number of languages and frameworks.

The first implementation is being written in [Erlang/OTP], which is a functional
programming language combined with a robust set of application libraries
designed and tested for writing scalable and reliable telco and internet
services. Erlang is widely used in mission-critical applications today, from
military, financial, telecoms, medical, as well as internet-scale games,
databases, social networks, and messaging services.

## Approach

The chosen approach is to implement swirl in phases, and funding is being
provided generously by [NLNet] for the first stages:

1. a [decoder], taking internet / wire format to internal parsed format
2. an [encoder], performing the reverse step from internal format to the wire
3. a [storage] layer comprising merkle tree, binmap lookups and key-value storage
4. simple [deployment] including FreeBSD and Linux/docker support

To make the most sense of the application itself, it's recommended to read in orer:

- high-level code [layout] introducing the general structure of the modules
- the [supervision] tree (application hierarchy) implemented in Erlang/OTP
- further sections per module as you like

## Future Plans

Further phases are planned, currently:

- live [streaming] support
- [zeroconf] / MDNS support (also known as bonjour protocol) for identifying local peers
- NAT [traversal] support, via STUN/TURN/ICE etc, depending on the outcome of
   the IETF standardisation process
- [browser] support
- native [mobile] device support
- [embedded] systems such as desktop boxes and digital television
- support for [billing] functionality
- enabling [traffic] management and shaping functionality and interfaces

[Erlang/OTP]: http://www.erlang.org/
[NLNet]: http://nlnet.nl/news/2013/20130901-awards.html

[billing]: ../billing
[browser]: ../browser
[decoder]: ../decoder
[deployment]: ../deployment
[embedded]: ../embedded
[encoder]: ../encoder
[layout]: ../layout
[merkle]: ../merkle
[mobile]: ../mobile
[overview]: ../overview
[storage]: ../storage
[streaming]: ../streaming
[supervision]: ../supervision
[traffic]: ../traffic
[traversal]: ../traversal
[usage]: ../usage
[zeroconf]: ../zeroconf
