# The Swirl Project

![Swirl Logo in Colour][swirl_logo_colour]
 
Swirl is an Erlang/OTP implementation of the draft IETF [PPSP] protocol,
under the [ALv2] license.

Until the IETF draft status is removed, expect this repo to change quickly
and without respecting any version numbers. Bugs are expected, but any reports
or even patches would make my day/week/year.

## Requirements

- a modern UNIX system, although `swirl` will likely run on Windows the
    compile-time dependencies are more complicated. Consider using cygwin
    or mingw shells to provide `make` support
- the most recent release of `Erlang/OTP` available. This is `R16B03` at
    time of writing, and is available from [ESL] for most platforms. This
    is preferred to using a possibly inconsistent version from your OS
    packaging system.
- Build and install Erlang's [rebar] build tool from source
- A reasonable level of Erlang and PPSPP knowledge is anticipated.

## Usage

- use `make` to build, and `make run` to start a default implementation
    on `localhost:7777`.
- `make dev` loads appropriate erlang apps but you'll need to specify
    either `swirl_app:start().` for a single instance, or use
    `tests:start_farm(Nodes).` where `Nodes` will be the additional ports
    > 4000 to run a PPPSPP peer farm.
- `ctrl-G q` to quit as usual.

![Swirl Logo in Black and White][swirl_logo_bw]

[ALv2]: http://www.apache.org/licenses/LICENSE-2.0.html
[PPSP]: http://datatracker.ietf.org/doc/draft-ietf-ppsp-peer-protocol/
[rebar]: https://github.com/rebar/rebar
[ESL]: https://www.erlang-solutions.com/downloads/download-erlang-otp
[swirl_logo_colour]: https://raw.github.com/wiki/skunkwerks/swirl/logo/swirl_colour.png "Colour Logo"
[swirl_logo_bw]: https://raw.github.com/wiki/skunkwerks/swirl/logo/swirl_bw.png "B&W Logo"
