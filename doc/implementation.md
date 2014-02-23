# Implementation

The Swirl project aims to deliver a fully functional implementation of the PPSP protocol in a number of languages and frameworks.

The first implementation is being written in [Erlang/OTP], which is a functional programming language combined with a robust set of application libraries designed and tested for writing scalable and reliable telco and internet services. Erlang is widely used in mission-critical applications today, from military, financial, telecoms, medical, as well as internet-scale games, databases, social networks, and messaging services.

## Approach

The chosen approach is to implement swirl in phases, and funding is being provided generously by [NLNet] for the first stages:

1. a [decoder], taking internet / wire format -> internal parsed format
2. an [encoder], performing the reverse step from internal format to the wire
3. [storage] layer comprising merkle tree, binmap lookups and key-value storage
4. simple [deployment] including FreeBSD & Linux/docker support
 
However, none of this can be implemented effectively, unless the overall framework of the project is in place. Work on the decoder in particular became blocked, as the simplistic approach used initially to manager the UDP port handlers, got in the way of passing the appropriate swarm and peer information through to the right package parsers.

This is in the process of being ripped out now, and as a bonus of this work, the [supervision] tree (the OTP structure of applications including peers and swarms) is being cleaned up in the [peer-supervisor] branch.

## Future Plans

Further phases are planned, currently:

1. live [streaming] support
2. [zeroconf] / MDNS support (also known as bonjour protocol) for identifying local peers
3. NAT [traversal] support, via STUN/TURN/ICE etc, depending on the outcome of the IETF standardisation process
4. [browser] support
5. [mobile] device support
6. [embedded] systems


[Erlang/OTP]: http://www.erlang.org/
[NLNet]: http://nlnet.nl/news/2013/20130901-awards.html
[peer-supervisor]: https://github.com/skunkwerks/swirl/blob/feature/add-peer-supervisor/src/swirl_sup.erl

[decoder]: decoder.md
[encoder]: encoder.md
[supervision]: supervision.md
[overview]: overview.md
[merkle]: merkle.md
[deployment]: deployment.md
[storage]: storage.md
[streaming]: streaming.md
[zeroconf]: zeroconf.md
[traversal]: traversal.md
[browser]: browser.md
[mobile]: mobile.md
[embedded]: embedded.md
