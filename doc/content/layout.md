---
title: Code Layout
description: A walk through of the software repository for code and docs
date: 2014-12-18
menu: main
weight: 30
tags:
    - overview
    - code
    - structure
---

The usual incantations for cloning a repo are found in the main [README].

After cloning the git repository, you'll find a bunch of directories. The code
is a typical Erlang/OTP application, with a structure that allows building both
a stand-alone Erlang [application] or a single [escript] which is a single
executable file with a dependency only on the Erlang runtime. The stand-alone
application is built using [relx], which provides a tarball-like version of the
application, ready for deployment.

[application]: http://www.erlang.org/doc/design_principles/applications.html#id71171
[escript]: http://www.erlang.org/doc/man/escript.html
[relx]: https://github.com/erlware/relx


    ├── doc
    │   └── content
    ├── include
    ├── site
    │   ├── archetypes
    │   ├── layouts
    │   │   └── root
    │   ├── static
    │   │   └── assets
    │   └── themes
    │       └── hyde-x
    ├── src
    └── test
            └── data

Documentation is in markdown format, and is generated using [hugo]. More details
can be found in the associated [documentation] within the `doc` folder.

The `/include/` directory, contains [headers] that provide the definitions
and constants extracted from the PPSPP RFC itself, used across both swirl
itself and any other tools or applications that might embed or use it as a
dependency.

The `/site/` structure houses the components for [hugo], including the [hyde-x]
theme, static assets for source code syntax [highlighting], and the core html
for the [project] site itself in `/site/layouts/root/`.

Almost all of the erlang source code can be found in `/src/` apart from the
[gproc] plugin, by Ulf Wiger, an external dependency. It will be available
in `/deps/gproc/` after the first `make` invocation.

Of course tests are found in `/test/` which contains both the test functionality
and fixtures in `/test/data/`. These are built using the erlang [common_test]
framework, to handle testing functionality across and between modules.

[headers]: https://github.com/skunkwerks/swirl/tree/master/include
[source]: https://github.com/skunkwerks/swirl/tree/master/src
[documentation]: https://github.com/skunkwerks/swirl/tree/master/doc/README.md
[hugo]: https://gohugo.io/
[hyde-x]: https://github.com/zyro/hyde-x
[highlighting]: http://highlightjs.org/
[project]: http://www.swirl-project.org/
[gproc]: https://github.com/uwiger/gproc
[common_test]: http://www.erlang.org/doc/man/common_test.html

### After `make`

Note that on the first run of `make`, internet access is required to pull down
the dependencies. Subsequent invocations can be done offline.

After running `make`, a `/deps/` directory is created that contains the external
dependencies, both for test and runtime. An `/ebin/` directory contains the
erlang bytecode, and can be used for testing locally, using the `make console`
shortcut. There will also be a `/swirl` executable escript, which contains a
portable version of the entire application. It requires a compatible version
of the erlang runtime installed on the destination system, but nothing else.
This is appropriate for use as a command-line tool, but is not designed to run
as a long-term daemon.

Erlang code is contained in modules, which contain functions. These are compiled
into beam files of the same name prefix, and it is these files, along with
some configuration files, that makes up an erlang [release] -- a self-contained
package of code that can be transferred to a system with a compatible
architecture for usage.

After building, the complete erlang release, contained in `/_rel/swirl/` can be
transferred using rsync, or packaged up into an RPM, Debian, or FreeBSD pkg
format for installation. It comprises both the erlang runtime, and the swirl
application, has no other external dependencies, and is obviously platform
dependent.

## Module Overview

There are two main types of modules within `swirl`:

- the OTP structure that manages and runs the swarms
- protocol-specific modules that translate between wire format and native
  format, and can handle (process) a specific datatype

Each of these modules will have a corresponding set of tests in the `/test/`
directory, using the `common_test` framework. These are automatically run
during build time.

### Supervisors

The OTP [supervision] structure has its own section, so it's not covered here
in detail. The top level application, `swirl`, contains 3 supervisor modules
that handle the dirty work of managing peers (UDP endpoints), channels (one
instance per connected remote peer to a swarm), and the swarms themselves.

Each of these supervisors has a corresponding [gen_server] [supervisor]
`*_sup.erl`, a worker `*_worker.erl`, and possibly an API library `*.erl` to
expose to other modules, for example:

    swirl_worker.erl
    swirl_sup.erl
    swirl.erl

The `swirl.erl` library is special, in that it also provides a set of
user-accessible functions that manage other modules - starting and stopping
swarms, adding peers, and allowing an operator to work with channels and
peers in the live system directly.

### Protocol Internals

There are dedicated modules to handle parsing PPSPP options, datagrams, and
messages, and each PPSPP [message type] has a dedicated module of its own.

Each module can transform that option, datagram, or message type to and from
that format, in a type-safe way, i.e. the implementation of a given message type
is not accessible from other libraries. Additional processing functionality is
located here too -- each message handler knows how to process its own message,
and none other.

Within each module there are typically a set of common exported functions:

- `unpack/1` takes a binary data structure, for example an incoming packet,
  and transforms it into a more efficient erlang term representation. These
  resulting structures are conveniently very readable during debugging.
- `pack/1` takes an erlang term representation, and turns it into a matching
  binary representation, ready to be sent out on the wire again.
- `handle/1` takes the unpacked data format and knows how to process that
  particular data structure.

For example a `handshake` message has a matching `ppspp_handshake.erl` module,
comprising `pack/1`, `unpack/`, and `handle/1` functions, and calling
channel, swarm, and options modules as needed to parse and process the data it
receives.

## Usage

See the [usage]({{< relref "usage.md" >}}) section for more information on
running [swirl] in practice.

[README]: https://github.com/skunkwerks/swirl/tree/master/README.md

[release]: http://www.erlang.org/doc/design_principles/release_structure.html
[gen_server]: http://www.erlang.org/doc/man/gen_server.html
[supervisor]: http://www.erlang.org/doc/man/supervisor.html
[message type]: https://tools.ietf.org/html/rfc7574#section-3
[swirl]: http://www.swirl-project.org/
