---
title: Using Swirl
description: Swirl UI Design
date: 2015-06-24
menu: main
tags:
  - page
  - ui
draft: false
categories:
  - overview
  - design
weight: 0
---

## Overview

Swirl currently has a very simplistic user interface - the erlang shell. In
future this could be extended with a simple HTTP RESTful API and embedded
HTML UI.

Despite this, it is already possible to do all operations from the erlang
console today: run `swirl:help().` to view supported commands

## Description

The main user activities are:

- join a swarm to retrieve specific content
- create, or validate, the hashes for given content
- start a swarm to share supplied content

```erlang-repl
1> swirl:get("ppspp://swirl.skunkwerks.at/c89800bf").
2> swirl:join("ppspp://swirl.skunkwerks.at:7777/c89800bf").
3> swirl:hash("file:///var/db/swirl/messier74.jpg").
4> swirl:share("file:///var/db/swirl/messier74.jpg.mhash").
```

The [PPSPP URL Specification]({{< relref "url.md" >}}) explains how the
above URLs are assembled.

In most cases, users will simply defer to the default protocol options in
[section 12.1.6], but if required, all the above functions support an
additional `options` parameter that should contain a valid PPSPP options
hash. This can be easily created via `ppspp_options:use_default_options/1`
which returns the following data structure:

```erlang-repl
1> ppspp_options:use_default_options(<<"c39e">>).
{options,[{chunk_addressing_method, chunking_32bit_chunks},
          {chunk_size, 1024},
          {content_integrity_check_method, merkle_hash_tree},
          {merkle_hash_tree_function, sha256},
          {minimum_version, 1},
          {swarm_id,<<"c39e">>},
          {version,1}]}
```

See [swarm options]({{< relref "#swarm-options" >}}) below for details.

## Developer Support

In addition to the user activities above, you can manage
the core components of a swarm directly, namely a `peer`, `swarm`, or a
`channel`. These are exposed for erlang application developer usage, rather
than normal end users.

```erlang-repl
1> swirl:start_swarm("c89800bf").
=INFO REPORT==== 24-Jun-2015::10:54:52 ===
swarm: <0.304.0> started with swarm_id:<<200,152,0,191>>
 and options: {options,[{chunk_addressing_method,chunking_32bit_chunks},
                        {chunk_size,1024},
                        {content_integrity_check_method,merkle_hash_tree},
                        {merkle_hash_tree_function,sha},
                        {minimum_version,1},
                        {swarm_id,<<200,152,0,191>>},
                        {version,1}]}
{ok,<0.304.0>}

2> swirl:start_peer("c89800bf").
peer: <0.306.0> listening on udp:7777
  options: {options,[{chunk_addressing_method,chunking_32bit_chunks},
                     {chunk_size,1024},
                     {content_integrity_check_method,merkle_hash_tree},
                     {merkle_hash_tree_function,sha},
                     {minimum_version,1},
                     {swarm_id,<<200,152,0,191>>},
                     {version,1}]}
{ok,<0.306.0>}

3> swirl:start_channel(4027566846).
channel: <0.309.0> listening on "ppspp://127.0.0.1:60720#0xf00fcafe"
  endpoint: {endpoint,
                [{channel,0},
                 {ip,{127,0,0,1}},
                 {port,60720},
                 {socket,#Port<0.6939>},
                 {transport,udp},
                 {uri,"127.0.0.1:60720#0xf00fcafe"}]}
{ok,<0.306.0>}
```

See the core [swirl]({{< relref "swirl.md" >}}) module for detailed options,
including specifying additional parameters:

- [Data Types]({{< relref "swirl.md" >}}#types)
- [Function Index]({{< relref "swirl.md" >}}#index)
- [Function Details]({{< relref "swirl.md" >}}#functions)

### Joining a PPSPP Swarm

To join a swarm, we require 3 things:

- the root hash
- a transport address (DNS name / IP address + UDP Port) of an existing peer in the swarm
- some optional parameters about the swarm configuration, particularly chunk size, hash function, and chunk addressing methods

The latter options can be skipped if the protocol defaults are used. In most
cases this makes everybody's life easier.

### Swarm Options

All handling of swarm options is handled within the
[ppspp_options]({{< relref "ppspp_options.md" >}}) module. Each of these
functions takes `swarm options` as a parameter. The getters return the requested data, and the setters return a new, updated, `swarm options`.

```erlang
get_chunk_addressing_method/1
get_content_integrity_check_method/1
get_merkle_hash_tree_function/1
get_minimum_version/1
get_swarm_id/1
get_maximum_supported_version/1

set_chunk_addressing_method/1
set_content_integrity_check_method/1
set_merkle_hash_tree_function/1
set_swarm_id/1
```

Note that setting minimum and maximum version protocol is done by swirl
itself, and is therefore not alterable by the user.

Additional details are available below.

## Generated Module Docs

- [Data Types]({{< relref "ppspp_options.md" >}}#types)
- [Function Index]({{< relref "ppspp_options.md" >}}#index)
- [Function Details]({{< relref "ppspp_options.md" >}}#functions)

[section 12.1.6]: https://tools.ietf.org/html/draft-ietf-ppsp-peer-protocol#section-12.1.6
