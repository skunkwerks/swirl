---
categories:
- blog
- design
- erlang
date: 2015-06-23T14:18:56+02:00
tags:
- update
- erlang
- pattern-matching
title: The Design Document Drop
---
The last week or so has been taken up with bringing a lot of documentation
up to date. While the focus was on what I refer to as "the Core", a lot
of other pieces needed tweaking along the way.

The docs are all online now, and a good starting point is
[Usage]({{< relref "usage.md" >}}), then [Core]({{< relref "core.md" >}}).
Only some of this example API works yet, I'm trying to keep the habit of
updating the design document and adding tests before jumping into coding.

## Last Week

So it turns out that my use of [edoc] notation for generating docs direct
from the code had some errors, and in fact the tools I use to post-process
those into markdown [edown] also had some errors. This resulted in a steady
stream of small commits to most files, reviewing the code comments, function
notes, and small fixes as I noted mistakes along the way. Rebasing this to
`develop` branch took a bit of work due to the age of the `add-channels`
branch.

## Next Up

I'm planning to continue working on a couple of features in Core, namely
reworking the `channel_worker` to become the same place that packets get
sent to, after minimal parsing in the `peer_worker`. This has a couple of
key consequences:

- All processes are properly registered under a known supervisor, no more
  spawning off `proc_lib/spawn/3` but sending a message directly to the
  channel itself.
- The `peer_worker` needs to do a little more work, as it needs to work
  out whether this is a request on `channel zero`, where there will not
  yet be a registered process to handle these requests. It might be better
  to create a special `channel zero` worker, to handle all of these requests
  directly, and then the peer_worker stays focused on pushing packets, not
  parsing and processing.

The other major piece planned is to write up the specification for the
packet [Decoder]({{< relref "decoder.md" >}}). While the majority of the
actual code is already written, this is pretty straightforwards both from
a code perspective and from documentation side. The guts of it is a
recursive function that uses Erlang's powerful bit-level pattern-matching
to extract swarm options, message types and the rest, directly from the
binary UDP datagram. This is some of the first code I wrote for Swirl, in
fact before the project had a name, and it's one of the pieces I enjoyed the
most.

If you're not aware of how bit-level pattern matching works in Erlang, read
[LYSE](http://learnyousomeerlang.com/starting-out-for-real) `Bit Syntax`, 
[here](http://css.dzone.com/articles/erlang-binaries-and-bitstrings), and
the official
[docs](http://www.erlang.org/doc/programming_examples/bit_syntax.html#id66107).

The two most important capabilities are matching within a function head,
(not a case statement, or nested if clauses!), and the ability to match,
within in the same function head, variables already matched. The canonical
example of this is using the length field in a TCP packet. Erlang's
implementation of this is extremely efficient, and is one of the main
reasons I wanted to implement PPSPP in Erlang.

For more advanced examples and tips, the [Efficiency Guide] is invaluable.

[Efficiency Guide]: http://www.erlang.org/doc/efficiency_guide/binaryhandling.html
[edoc]: http://www.erlang.org/doc/apps/edoc/chapter.html
[edown]: https://github.com/uwiger/edown
