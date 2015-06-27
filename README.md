# The Swirl Project

![Swirl Mascot, Messier Spiral Galaxy M74, also known as NGC 628][swirl_m74_small]

Swirl is an Erlang/OTP implementation of the draft IETF [PPSP] protocol,
under the [ALv2] license. It is currently incomplete.

Until the IETF draft status is removed, expect this repo to change quickly
and without respecting any version numbers. Bugs are expected, but any reports
or even patches would make my day/week/year.

## Requirements

- A modern UNIX system, although `swirl` will likely run on Windows the
    compile-time dependencies are more complicated. Please contact the
    project if you want to build or run on windows support.
- The most recent release of `Erlang/OTP` available. This is `OTP 18.0` at
    time of writing, and is available from [ESL] for most platforms. This
    is strongly advised, instead of using a possibly inconsistent version from
    your OS packaging system. Swirl will work with `OTP 17.x` but performance
    will not be as good.
- A reasonable level of Erlang and PPSPP knowledge is anticipated.
- If you are developing on / with `swirl` you will need a pre-built dialyzer
    plt. If you don't know what this is, just run `make plt` each
    time you install/upgrade to a new Erlang/OTP release.

## Usage

- Use `make` as usual. It will retrieve depedencies, so internet is needed.
- After building, a stand-alone command, `./swirl` starts a simple peer
    on `localhost:7777`, without console access.
- `make console` loads the full erlang application but you'll need to enter
  either `swirl:start_peer().` for a single instance, or read the
  console help via `swirl:help().` to see how to start multiple peers.

## Developers

- `make distcheck` cleans all build files out, compiles, and runs all tests,
  including eunit, common_test, and dialyzer.
- `make reindent` uses vim to ensure your indentation is consistent.
- more information is available in [contributing.md]

## Example Usage

From a clean git checkout of `git://github.com/skunkwerks/swirl.git`,
just run:

    make all console
    # erlang console is automatically launched
    swirl:start_peer().
    swirl:quit().

And here's what a sample session looks like from the above commands:

```
🌈  dch@akai % git clone  && cd swirl
🌈  dch@akai % make all console
rebar clean
==> git (clean)
rebar get-deps update-deps
==> git (get-deps)
==> git (update-deps)

rebar compile escriptize
==> git (compile)
Compiled src/peer_sup.erl
Compiled src/ppspp_datagram.erl
Compiled src/ppspp_options.erl
Compiled src/swirl_app.erl
Compiled src/swirl.erl
Compiled src/peer_worker.erl
Compiled src/swirl_sup.erl
Compiled src/convert.erl
Compiled src/ppspp_message.erl
Compiled src/swarm_sup.erl

==> git (escriptize)
dialyzer -pa ./ebin -I ./include -r ebin \
		-Werror_handling -Wrace_conditions
  Checking whether the PLT /Users/dch/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis... done in 0m1.02s
done (passed successfully)

erl -pa ./ebin -I ./include -s crypto -smp \
		-setcookie swirl -sname swirl \
		+K true +A 16 \
		-s swirl -s swirl help

Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:64] [hipe] [kernel-poll:true] [dtrace]
Eshell V6.2  (abort with ^G)
(swirl@akai)1>

=INFO REPORT==== 27-Oct-2014::23:34:28 ===
swirl: protocol d-10

=INFO REPORT==== 27-Oct-2014::23:34:28 ===
swirl: version #4e775b9
swirl: online help
use any of these commands, prefixed by `swirl:` to run:

 help().                    these help notes
 start().                   starts the swirl application, but no peers or swarms
 stop().                    stops the swirl application, active peers and swarms
 start_peer().              starts a peer with default port & options
 start_peer(Hash).          starts a peer with defaults and given hash
 start_pool(First, Last).   starts peers on consecutive ports from First to Last
 start_peer(Port, Options). starts a peer with supplied port and options
 stop_peer().               stops a single peer on the default port
 stop_peer(Port).           stops a single peer on the given port
 stop_pool(First, Last).    stops peers on consecutive ports from First to Last
 quit().                    terminates *immediately* the entire BEAM vm


 e.g. swirl:start_peer("c89800bfc82ed01ed6e3bfd5408c51274491f7d4").

 use ^c twice to exit, or type `swirl:quit().` for a graceful shutdown.

(swirl@akai)1> swirl:start_peer().

=INFO REPORT==== 27-Oct-2014::23:34:56 ===
peer: <0.55.0> listening on udp:7777
  options: {options,[{chunk_addressing_method,chunking_32bit_chunks},
                     {chunk_size,1024},
                     {content_integrity_check_method,merkle_hash_tree},
                     {merkle_hash_tree_function,sha},
                     {minimum_version,1},
                     {swarm_id,<<>>},
                     {version,1}]}
{ok,<0.55.0>}

(swirl@akai)2> swirl:quit().

=INFO REPORT==== 27-Oct-2014::23:35:09 ===
peer: <0.55.0> terminating port 7777, using 5936 bytes, due to reason: shutdown

=INFO REPORT==== 27-Oct-2014::23:35:09 ===
    application: swirl
    exited: stopped
    type: temporary
ok
(swirl@akai)3>
🌈  dch@akai %
```

## Images

The [Swirl Project] uses a number of stunning images of the Messier M74
Spiral Galaxy, also known as NGC 628. These images are in the [public domain],
and you can see more of them at the [hubblesite], and on their [mobile] site.

![Swirl Mascot, Messier Spiral Galaxy M74, also known as NGC 628][swirl_m74_large]

[ALv2]: http://www.apache.org/licenses/LICENSE-2.0.html
[PPSP]: http://datatracker.ietf.org/doc/draft-ietf-ppsp-peer-protocol/
[ESL]: https://www.erlang-solutions.com/downloads/download-erlang-otp
[Swirl Project]: http://www.swirl-project.org/
[public domain]: http://hubblesite.org/about_us/copyright.php
[hubblesite]: http://hubblesite.org/gallery/wallpaper/pr2007041a/
[mobile]: http://m.hubblesite.org/vote/pr2007041a
[swirl_m74_small]: https://raw.github.com/wiki/skunkwerks/swirl/logo/hs-2007-41-a-thumb.jpg
[swirl_m74_large]: https://raw.github.com/wiki/skunkwerks/swirl/logo/hs-2007-41-a-web.jpg
[download]: https://raw.github.com/wiki/skunkwerks/swirl/tools/rebar
[contributing.md]: https://github.com/skunkwerks/swirl/blob/develop/CONTRIBUTING.md

