---
title: PPSPP URL Draft
description: A Proposed URL Specification for the PPSP Protocol
categories:
- overview
- design
date: 2015-06-24T13:28:19+02:00
menu: main
tags:
- page
- url
weight: 100
---

While there is no official PPSPP URL defined yet, the following
simple structure has been used, based off a quick reading of
[Guidelines for new URL Schemes](https://tools.ietf.org/html/rfc2718) and
[URI Generic Syntax](https://tools.ietf.org/html/rfc3986).

- `scheme`: defines the protocol in use, by default `ppspp`. The PPSPP 
  specification declares protocol to be transport agnostic: it may use UDP or
  SCTP or something else. While for now only UDP transport is defined as base 
  one, in future this may change. To handle PPSPP-over-Proto situations, custom
  transport MUST be defined after `+` character, like: `ppspp+sctp` which reads 
  as "use PPSPP protocol over SCTP transport".
  By default UDP protocol is assumed and MUST be used, while `ppspp+udp` is 
  still valid scheme.

- `authority`: the IP address and port `example.net:7777`
- `path`: the root hash in use, or requested filename `c89800bf` or `messier74.jpg`
- optional `query` string: supports passing swarm options such as: `content_integrity_protection_method=merkle_hash_tree &merkle_hash_tree_function=sha-256`

The keys and values used for the optional query string are the lower-case
versions, with whitespace replaced by underscores, of those referred to in
[PPSPP section 7] (https://tools.ietf.org/html/rfc7574-12#section-7) and the canonical [PPSPP IANA registry](https://www.iana.org/assignments/ppspp/ppspp.xhtml).

Some examples are:

- using DNS name and IP Port transport address as authority, with root hash:  [ppspp://example.net:7777/c89800bf](ppspp://example.net:7777/c89800bf)
- same root hash with IPv6 address and UDP port:  [ppspp://[2a01:4f8:200:12cf::2]:7777/c89800bf](ppspp://[2a01:4f8:200:12cf::2]:7777/c89800bf)
- requesting a filename with complex swarm options: [ppspp://example.net:7777/messier74.jpg?content_integrity_protection_method=merkle_hash_tree&merkle_hash_tree_function=sha-256&chunk_addressing_method=32-bit_bins](ppspp://example.net:7777/messier74.jpg?content_integrity_protection_method=merkle_hash_tree&merkle_hash_tree_function=sha-256&chunk_addressing_method=32-bit_bins)

Comments and suggestions to improve this are welcomed.
