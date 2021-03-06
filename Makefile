PROJECT   = swirl

DEPS      = gproc

ERLC_OPTS = +debug_info
PLT_APPS += crypto public_key compiler asn1 inets tools

DOC_DEPS  = edown
EDOC_OPTS = {dir, "doc/api"}, \
		{application, ["swirl"]}, \
		{doclet, edown_doclet}, \
		{subpackages, false}, \
		{todo, false}, \
		{top_level_readme, \
			{"doc/api/index.md", "http://www.swirl-project.org/api"}}, \
		{report_missing_types, false}, \
		{title, "Swirl API documentation"}, \
		{pretty_printer, erl_pp}
escript::

include erlang.mk

.PHONY : draftdoc doc publish quick console reindent

distcheck: distclean all plt dialyze tests escript
	@echo "*** check indentation before git push ***"

quick: escript
	@echo quick: a fast start app
	./swirl

dev: SWIRL_CONSOLE_OPTS ?= -s observer
dev: console

console: deps app
	@erl -pa ./ebin -pz ./deps/*/ebin -pz ./test/ \
			-I ./include -s crypto -smp \
			-setcookie swirl -sname swirl \
			+K true +A 16 \
			-s swirl -s swirl help $(SWIRL_CONSOLE_OPTS)

clean:: doc-clean

doc-clean:
	@echo " GEN    clean-doc"
	@rm -rf public doc/api

doc: doc-clean hackdoc gohugo

hackdoc: edoc
	@echo doc: hacking up doc/api/index.md file
	@mv doc/api/README.md doc/api/index.md
	@perl -pi -e 's!href="(\w+)\.md"!href="\1"!g' doc/api/index.md
	$(eval GITVERSION := $(shell git describe --dirty --abbrev=7 --tags --always --first-parent 2>/dev/null || true))
	@echo doc: splicing in doc build details $(GITVERSION)
	@perl -pi -e 's!GITVERSION!$(GITVERSION)!g' doc/api/index.md
	@echo doc: hacking up doc/api/\*.md typespec links from edown for pretty urls
	@perl -pi -e 's!href="(\w+)\.md(#type-\w+)"!href="../\1\2"!g' doc/api/*.md
	@echo doc: building site in public/

gohugo: hackdoc
	@(cd site && hugo --verbose )

draftdoc: doc-clean hackdoc
	@(cd site && hugo --verbose --buildDrafts)

newpost:
	@echo doc: creating ./doc/blog/$(name).md
	@(cd site && hugo new --kind=post blog/$(name).md )

newpage:
	@echo doc: creating ./doc/content/$(name).md
	@(cd site && hugo new --kind=page content/$(name).md )

newddoc:
	@echo doc: creating ./doc/content/$(name).md
	@(cd site && hugo new --kind=design content/$(name).md )

watch:
	@echo doc: watching for changes
	@(cd site && hugo server --verbose --watch )

publish: doc
	@echo publish: shipping site from public/ to gs://www.swirl-project.org/
	@gsutil -m rm -R gs://www.swirl-project.org/**
	@gsutil -m cp -R -z html,md,css,xml,js,svg  public/* gs://www.swirl-project.org/

reindent:
	@# requires either vim 7.4, or github.com/vim-erlang/vim-erlang-runtime
	@# this should indent the same as emacs erlang major mode or it's a bug
	@# add -c ':set runtimepath^=~/v/.vim/bundle/vim-erlang-runtime/' if less
	vim -ENn -u NONE \
			-c 'filetype plugin indent on' \
			-c 'set expandtab shiftwidth=4' \
			-c 'args src/*.?rl test/*.?rl' \
			-c 'argdo silent execute "normal gg=G" | update' \
			-c q
