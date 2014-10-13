all: distclean compile

deps:
	rebar get-deps update-deps compile

clean:
	rebar clean skip_deps=true

distclean:
	git clean -fdxe .dialyzer.plt
	git reset --hard

compile: clean
	rebar compile escriptize skip_deps=true

distcheck: distclean check
	@echo "*** check indentation before git push ***"

check: clean compile eunit ct dialyze

ct:
		rebar ct skip_deps=true

eunit:
		rebar eunit skip_deps=true

dialyze: .dialyzer.plt
	dialyzer --plt .dialyzer.plt \
		-I ./include \
		--src -r ./src \
		--fullpath \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		; [ $$? -ne 1 ]  # ignore warning (2) or ok (0) but not error (1)

.dialyzer.plt:
	@echo "*** dialyzer plt not found -- build takes a wee while ***"
	dialyzer --build_plt --output_plt .dialyzer.plt --apps \
		erts kernel stdlib crypto \
		sasl common_test eunit compiler \
		| fgrep -v dialyzer.ignore

reindent:
	@# requires either vim 7.4, or github.com/vim-erlang/vim-erlang-runtime
	@# this should indent the same as emacs erlang major mode or it's a bug
	@# add -c ':set runtimepath^=~/v/.vim/bundle/vim-erlang-runtime/' if less
	vim -E -N --noplugin -u /dev/null -c ':filetype plugin indent on' \
		-c ':args src/*.?rl' \
		-c 'argdo silent execute "normal gg=G"' \
		-c 'update' -c q

dev:
	erl -pa ./ebin -I ./include -s crypto -smp \
		-setcookie swirl -sname swirl \
		+K true +A 16 \
		-s swirl -s swirl help

console:
	erl -pa ./ebin -I ./include -s crypto -smp \
		-setcookie swirl -sname console \
		+K true +A 16 \
		-s swirl -s swirl help

run:
	./swirl

.PHONY : doc publish

doc:
	@rm -rf public
	@echo doc: building site in public/
	@(cd site && hugo --config=config.yaml --destination=../public -v)

publish: doc
	@echo publish: shipping site from public/ to gs://www.swirl-project.org/
	gsutil -m rm -R gs://www.swirl-project.org/**
	gsutil -m cp -R -z html,md,css,xml,js,svg  public/* gs://www.swirl-project.org/
