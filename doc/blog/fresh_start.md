---
categories:
- blog
date: 2015-06-02T23:34:09+02:00
tags:
- update
- musing
title: A Fresh Start
---

The last few months have been pretty well occupied with personal stuff --
a long trip back to New Zealand, and the arrival of a new baby in the
family. Not surprisingly, `swirl` has been on the back burner during this
time.

## Refocus

The break gave me ample time to think about what I'm hoping to achieve with
the project, and how best to move things along, as progress has been slower
than I would like in and around life and family.

Pragmatically I'm getting around 3-4 hours a day of focused work time in, and
this has changed the timelines quite a bit. To that end I will be completing
the design documents first and then organising some additional Erlang help
to move things along.

## This Week

Most of this week has been occupied with new baby related things, but in and
around that, I've integrated the excellent [edown](https://github.com/uwiger/edown)
documentation library into the project, and set it up to produce the very
pretty [API]({{< relref "api/index.md" >}}) pages. These are generated
directly from the Erlang code, and there are still quite a few rough edges
in there to clean up due to missing or invalid tagging on my part. I'm
really pleased with the results however, as they include both `data types`
and `function docs` in a very readable fashion.

## Current State

Work to date has been around the overall structure of the application, in
particular what I think of now as the "Core" of swirl - the OTP processes
or daemons that own and manage the state for the given peers, swarms, and
the corresponding channels that weave them together.

This is largely complete, although some changes will inevitably be required
as the other modules are done, the overall structure should remain the same.
See the [OTP supervision guide]({{< relref "supervision.md" >}}) for the
full story.

Unfortunately this work isn't specifically in the original NLNet grant,
however doing it has really crystallised my thinking around what data and
state *needs* to be managed, and what can be maintained or used within a
given process.

## Next Up

This and next week's focus is to take the current codebase and turn it into
a design document for the other components to build off and refer to.
