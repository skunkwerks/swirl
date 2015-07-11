# The Swirl Project

![Swirl Mascot, Messier Spiral Galaxy M74, also known as NGC 628][swirl_m74_small]

Swirl is an implementation of the IETF [PPSP] protocol, `RFC7574` in
Erlang/OTP, and released under the [ALv2] license. It is currently incomplete.

Until the project's `alpha` status is removed, expect this repo to change
quickly and without respecting any version numbers. Bugs are expected, but
any reports or even patches would make my day/week/year.

## Requirements

- A modern UNIX system, although `swirl` will likely run on Windows, the
    compile-time dependencies are more complicated. Please contact the
    project if you want to build or run on Windows.
- The most recent release of `Erlang/OTP` available. This is `OTP 18.0` at
    time of writing, and is available from [ESL] for most platforms. This
    is strongly advised, instead of using a possibly inconsistent version from
    your OS packaging system. Swirl will work with `OTP 17.x` but performance
    will not be as good.
- A reasonable level of Erlang and PPSPP knowledge is anticipated.
- If you are developing on / with `swirl` you will need a pre-built dialyzer
    plt. If you don't know what this is, just run `make distclean plt` each
    time you install/upgrade to a new Erlang/OTP release.

## Usage

- Use `make` as usual. It will retrieve dependencies, so internet is needed
  at least for the first run.
- After building, a stand-alone command, `./swirl` starts a simple peer
    on `localhost:7777`, without console access.
- `make console` loads the full erlang application but you'll need to enter
  either `swirl:start_peer().` for a single instance, or read the
  console help via `swirl:help().` to see how to start multiple peers.

## Developers

- `make distcheck` cleans all build files out, compiles, and runs all tests,
  including common_test, and dialyzer.
- `make reindent` uses vim to ensure your indentation is consistent.
- more information is available in [contributing.md]

## Example Usage

From a clean git checkout of `git://github.com/skunkwerks/swirl`,
just run, supplying your root hash:

    make all console
    # erlang console is automatically launched
    swirl:start("c89800bfc82ed01ed6e3bfd5408c51274491f7d4").
    swirl:quit().

This peer should now be active, and able to participate in PPSPP swarms.

Here's what a sample session looks like from the above commands:

```
🌈  dch@akai % git clone git://github.com/skunkwerks/swirl && cd swirl
🌈  dch@akai % make console

 APP    gproc.app.src
 ERLC   ppspp_channel.erl ... swarm_worker.erl
 APP    swirl.app.src

Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [smp:4:4] [async-threads:64] [hipe] [kernel-poll:true] [dtrace]

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.59.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.60.0>},
                       {id,overload},
                       {mfargs,{overload,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.57.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.61.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
         application: sasl
          started_at: swirl@akai

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
          supervisor: {local,swirl_sup}
             started: [{pid,<0.67.0>},
                       {id,peer_sup},
                       {mfargs,{peer_sup,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
          supervisor: {local,swirl_sup}
             started: [{pid,<0.70.0>},
                       {id,swarm_sup},
                       {mfargs,{swarm_sup,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
          supervisor: {local,swirl_sup}
             started: [{pid,<0.71.0>},
                       {id,channel_sup},
                       {mfargs,{channel_sup,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 10-Jul-2015::13:45:17 ===
         application: swirl
          started_at: swirl@akai

=INFO REPORT==== 10-Jul-2015::13:45:17 ===
swirl: protocol rfc7574-20150510

=INFO REPORT==== 10-Jul-2015::13:45:17 ===
swirl: version #b4d09d0


swirl: online help {"rfc7574-20150710","b4d09d0"}
use any of these commands, prefixed by `swirl:` to run:

help().                    these help notes
start().                   starts the swirl application, but no peers or swarms
stop().                    stops the swirl application and all peers and swarms
start(Options | Id).       starts a new swarm and peer on OS selected random port
start_swarm(Options | Id). starts a swarm using given options or swarm id
stop_swarm(Options | Id).  stops a swarm using given options or swarm id
start_peer().              starts a peer with default port
start_peer(Port).          starts a peer with given port
stop_peer().               stops a single peer on the default port
stop_peer(Port).           stops a single peer on the given port
start_pool(First, Last).   starts peers on consecutive ports from First to Last
stop_pool(First, Last).    stops peers on consecutive ports from First to Last
quit().                    immediately terminates the entire BEAM vm

e.g. swirl:start("c89800bfc82ed01ed6e3bfd5408c51274491f7d4").
use ^c twice to exit, or type `swirl:quit().` for a graceful shutdown.
Eshell V7.0.2  (abort with ^G)

(swirl@continuity)1> swirl:start("c89800bfc82ed01ed6e3bfd5408c51274491f7d4").

=INFO REPORT==== 10-Jul-2015::13:59:30 ===
swarm: <0.73.0> started with swarm_id:<<200,152,0,191,200,46,208,30,214,227,
                                        191,213,64,140,81,39,68,145,247,212>>
 and options: {options,[{chunk_addressing_method,chunking_32bit_chunks},
                        {chunk_size,1024},
                        {content_integrity_check_method,merkle_hash_tree},
                        {merkle_hash_tree_function,sha},
                        {minimum_version,1},
                        {swarm_id,<<200,152,0,191,200,46,208,30,214,227,191,
                                    213,64,140,81,39,68,145,247,212>>},
                        {version,1}]}

=INFO REPORT==== 10-Jul-2015::13:59:30 ===
peer: <0.74.0> listening on udp:60421
  options: {options,[{chunk_addressing_method,chunking_32bit_chunks},
                     {chunk_size,1024},
                     {content_integrity_check_method,merkle_hash_tree},
                     {merkle_hash_tree_function,sha},
                     {minimum_version,1},
                     {swarm_id,<<200,152,0,191,200,46,208,30,214,227,191,213,
                                 64,140,81,39,68,145,247,212>>},
                     {version,1}]}

=INFO REPORT==== 10-Jul-2015::13:59:30 ===
swirl: started swarm <0.73.0> and peer <0.74.0> on port 60421
{ok,<0.73.0>,<0.74.0>,60421}

(swirl@continuity)2> swirl:stop().

=INFO REPORT==== 10-Jul-2015::13:59:37 ===
swarm: <0.73.0> terminating swarm <<200,152,0,191,200,46,208,30,214,227,191,
                                    213,64,140,81,39,68,145,247,212>>, using 2928 bytes, due to reason: shutdown
  with state {state,
                 <<200,152,0,191,200,46,208,30,214,227,191,213,64,140,81,39,68,
                   145,247,212>>,
                 {options,
                     [{chunk_addressing_method,chunking_32bit_chunks},
                      {chunk_size,1024},
                      {content_integrity_check_method,merkle_hash_tree},
                      {merkle_hash_tree_function,sha},
                      {minimum_version,1},
                      {swarm_id,
                          <<200,152,0,191,200,46,208,30,214,227,191,213,64,
                            140,81,39,68,145,247,212>>},
                      {version,1}]}}

=INFO REPORT==== 10-Jul-2015::13:59:37 ===
peer: <0.74.0> terminating port 60421, using 7080 bytes, due to reason: shutdown

=INFO REPORT==== 10-Jul-2015::13:59:37 ===
    application: swirl
    exited: stopped
    type: temporary
ok
(swirl@continuity)3> swirl:quit().
ok

🌈  dch@akai %
```

## Images

The [Swirl Project] uses a number of stunning images of the Messier M74
Spiral Galaxy, also known as NGC 628. These images are in the [public domain],
and you can see more of them at the [hubblesite], and on their [mobile] site.

![Swirl Mascot, Messier Spiral Galaxy M74, also known as NGC 628][swirl_m74_large]

[ALv2]: http://www.apache.org/licenses/LICENSE-2.0.html
[PPSP]: https://datatracker.ietf.org/doc/rfc7574/
[ESL]: https://www.erlang-solutions.com/downloads/download-erlang-otp
[Swirl Project]: http://www.swirl-project.org/
[public domain]: http://hubblesite.org/about_us/copyright.php
[hubblesite]: http://hubblesite.org/gallery/wallpaper/pr2007041a/
[mobile]: http://m.hubblesite.org/vote/pr2007041a
[swirl_m74_small]: https://raw.github.com/wiki/skunkwerks/swirl/logo/hs-2007-41-a-thumb.jpg
[swirl_m74_large]: https://raw.github.com/wiki/skunkwerks/swirl/logo/hs-2007-41-a-web.jpg
[download]: https://raw.github.com/wiki/skunkwerks/swirl/tools/rebar
[contributing.md]: https://github.com/skunkwerks/swirl/blob/develop/CONTRIBUTING.md

