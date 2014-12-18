PROJECT   = swirl

DEPS      = gproc
TEST_DEPS = proper
dep_gproc = git git://github.com/uwiger/gproc.git master

ERLC_OPTS = +debug_info
PLT_APPS += crypto public_key compiler asn1 inets tools

escript::

include erlang.mk

.PHONY : doc publish run console reindent

distcheck: distclean all plt dialyze tests escript
	@echo "*** check indentation before git push ***"

run: escript
	./swirl

dev: SWIRL_CONSOLE_OPTS ?= -s observer
dev: console

console:
	@erl -pa ./ebin -pz deps/*/ebin \
			-I ./include -s crypto -smp \
			-setcookie swirl -sname swirl \
			+K true +A 16 \
			-s swirl -s swirl help $(SWIRL_CONSOLE_OPTS)

clean:: doc-clean

doc-clean:
	@echo " GEN    clean-doc"
	@rm -rf public

doc: clean
	@echo doc: building site in public/
	@(cd site && hugo --config=config.yaml --destination=../public -v)

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
