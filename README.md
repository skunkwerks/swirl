# The Swirl Project

![Swirl Mascot, Messier Spiral Galaxy M74, also known as NGC 628][swirl_m74_small]

Swirl is an Erlang/OTP implementation of the draft IETF [PPSP] protocol,
under the [ALv2] license.

Until the IETF draft status is removed, expect this repo to change quickly
and without respecting any version numbers. Bugs are expected, but any reports
or even patches would make my day/week/year.

## Requirements

- A modern UNIX system, although `swirl` will likely run on Windows the
    compile-time dependencies are more complicated. Please contact the
    project if you want to build or run on windows support.
- The most recent release of `Erlang/OTP` available. This is `R16B03-1` at
    time of writing, and is available from [ESL] for most platforms. This
    is preferred to using a possibly inconsistent version from your OS
    packaging system.
- Development will use the OTP 17.0 release as soon as it's available
- Either build and install Erlang's [rebar] build tool from source, or
    [download] it and put in your path.
- A reasonable level of Erlang and PPSPP knowledge is anticipated.

## Usage

- Use `make` to build, and `make run` to start a default implementation
    on `localhost:7777`.
- `make dev` loads appropriate erlang apps but you'll need to specify
    either `swirl_app:start().` for a single instance, or use
    `tests:start_farm(Nodes).` where `Nodes` will be the additional ports
    > 4000 to run a PPPSPP peer farm.
- `ctrl-G q` to quit as usual.

## Images

The [Swirl Project] uses a number of stunning images of the Messier M74
Spiral Galaxy, also known as NGC 628. These images are in the [public domain],
and you can see more of them at the [hubblesite], or on its [mobile] version.

![Swirl Mascot, Messier Spiral Galaxy M74, also known as NGC 628][swirl_m74_large]

[ALv2]: http://www.apache.org/licenses/LICENSE-2.0.html
[PPSP]: http://datatracker.ietf.org/doc/draft-ietf-ppsp-peer-protocol/
[rebar]: https://github.com/rebar/rebar
[ESL]: https://www.erlang-solutions.com/downloads/download-erlang-otp
[Swirl Project]: http://swirl-project.org/
[public domain]: http://hubblesite.org/about_us/copyright.php
[hubblesite]: http://hubblesite.org/gallery/wallpaper/pr2007041a/
[mobile]: http://m.hubblesite.org/vote/pr2007041a
[swirl_m74_small]: https://raw.github.com/wiki/skunkwerks/swirl/logo/hs-2007-41-a-thumb.jpg
[swirl_m74_large]: https://raw.github.com/wiki/skunkwerks/swirl/logo/hs-2007-41-a-web.jpg
[download]: https://raw.github.com/wiki/skunkwerks/swirl/tools/rebar
