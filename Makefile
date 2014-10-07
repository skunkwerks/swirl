all: distclean compile

deps:
	rebar get-deps update-deps

clean:
	rebar clean

distclean:
	git clean -fdxe .dialyzer.plt
	git reset --hard

compile: clean
	rebar compile escriptize

commit: distclean check
	@echo "*** check indentation before git push ***"

check: compile eunit ct dialyze

ct:
		rebar skip_deps=true ct

eunit:
		rebar skip_deps=true eunit

dialyze: .dialyzer.plt
	dialyzer --plt .dialyzer.plt \
		-I ./include \
		--src -r ./src \
		--fullpath \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wunderspecs \
		; [ $$? -ne 1 ]

.dialyzer.plt:
	@echo *** dialyzer plt not found -- this takes a wee while ***
	dialyzer --build_plt --output_plt .dialyzer.plt --apps \
		erts kernel stdlib crypto \
		sasl common_test eunit compiler \
		| fgrep -v dialyzer.ignore

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
