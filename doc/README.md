# Swirl Documentation

The [documentation] is written in markdown. The [raw pages] are located in
`/doc/content/`. We are using [Hugo] to generate the both the website and the
documentation directly from markdown source.

The HTML for the [website] itself is stored in `/site/layouts/root/`, and the
standard hugo [config] file `config.yaml` and the theme can be found in
`/site/` directly.

You can either build hugo from source, or more conveniently, download a static
pre-built binary. Both are covered in Hugo's [installation guide].

[Hugo]: http://gohugo.io/
[installation guide]: http://gohugo.io/overview/installing/

To build, or re-build the documentation, simply run `make doc` or `make publish`
to regenerate the docs. `publish` requires permissions to update the google
storage site, if you don't have this, simply send a pull request to the project
and we'll rebuild it when merging your contribution.

[documentation]: http://www.swirl-project.org/content/
[raw pages]: https://github.com/skunkwerks/swirl/tree/master/doc/content/
[website]: https://github.com/skunkwerks/swirl/tree/master/site/layouts/root
[config]: https://github.com/skunkwerks/swirl/blob/master/site/config.yaml
