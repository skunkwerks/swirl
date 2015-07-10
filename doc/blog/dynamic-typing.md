---
categories:
- blog
- erlang
date: 2015-07-02T11:09:37+02:00
tags:
- update
- erlang
- dialyzer
- typing
- dynamic
title: The Joys of Dynamic Typing
---

I'd expected to make good progress this week on both the Decoder docs, and
also refactoring the core's `channel_worker` code to match the design doc
done the previous week, but with some family complications, along with
a tricky dialyzer bug, progress has been slow.

Sadly, good references for [dialyzer] and Erlang's [typespecs] are slim, see
`LYSE`'s sections on [types] and [more dialyzer] to get started.

Normally, implementing types in Erlang is straightforwards:

Encapsulate the data structure within a given module using opaque types,
such as this snippet from [ppspp_options.erl]:

```erlang
-export_type([swarm_id/0,
              merkle_tree_hash_function/0,
              content_integrity_protection_method/0,
              options/0,
              option/0]).

%% Matches a subset of types from http://erlang.org/doc/man/crypto.html
%%  digest_type() =  md5 | sha | sha224 | sha256 | sha384 | sha512, and
%%  hash_algorithms() = md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512
-opaque merkle_tree_hash_function() :: sha | sha224 | sha256 | sha384 | sha512.
```

And then add setters and getters to allow outside modules to retrieve their
required data. This feels awfully like Haskell's lenses and Erlang maps would
be an infinitely better solution, by the way.

Dialyzer then ensures that no functions outside the current module are able
to access inside the data structure. As a consequence, every opaque type
inside a module needs to implement setters and getters as the external
modules cannot peer inside anymore.

Now that [OTP-18.0 has landed], I intend to move a large chunk of code
over to using maps, both for speed, and simplicity - pattern matching is now
available within maps in function heads, instead of proplists being scanned
repeatedly. Having dialyzer keep an eye on things will help enormously.

However one of the complexities is that, when a datagram is parsed, different
modules are required for each part - [ppspp_options.erl] for the `swarm
options`, [ppspp_message.erl] for the actual messages themselves, and all
of these need to be called from [ppspp_datagram.erl] which is where the
parsing is started from.

During a new handshake from a new peer, requesting to join our swarm, we
need to compare the requested swarm options with the actual ones we have
available. This means that the datagram parser needs to pull out nested
options that are inside the message body itself, which it doesn't have direct
access to. As these are `proplist()` & `orddict()` the normal approach is
just to scan through until the correct item is found, however of course
with opaque types, this can only be done inside a given module. And that
module *doesn't* know how to unwrap a datagram, nor ignore for example,
the endpoint.

This caused trouble, lots of it.

I didn't manage to track down the underlying reason, but implementing the
usual tagged tuple `{messages, list(message())}` as an opaque data type
caused `typer`, a tool to generate success types for newly added code, to
run in an endless loop, and [dialyzer] to complain with:

```erlang-repl
ppspp_datagram.erl:171: The call
ppspp_message:handle( Messages::ppspp_message:messages(),
                      Swarm_Options::any(),
                      Endpoint::ppspp_datagram:endpoint())
contains an opaque term as 1st argument when terms of different types are
expected in these positions.
```

Ouch.

Long story short, I ended up debugging these types on paper, lots of paper,
spread out over the table and floor. In the end, I exposed the message type
in a different way, both as individual messages, as a list of messages, as
well as the `{messages, list(message())}` tagged tuple. This works, but
in future it will need to be revisited.

[ppspp_options.erl]: {{< relref "ppspp_options.md" >}}
[ppspp_message.erl]: {{< relref "ppspp_message.md" >}}
[ppspp_datagram.erl]: {{< relref "ppspp_datagram.md" >}}
[OTP-18.0 has landed]: http://www.erlang.org/news/88
[typespecs]: http://erlang.org/doc/reference_manual/typespec.html
[dialyzer]: http://www.erlang.org/doc/man/dialyzer.html
[types]: http://learnyousomeerlang.com/types-or-lack-thereof
[more dialyzer]: http://learnyousomeerlang.com/dialyzer
