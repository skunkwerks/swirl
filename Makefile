all: clean compile

deps:
	rebar get-deps update-deps

clean:
	rebar clean

distclean:
	git clean -fdx
	git reset --hard

compile: clean deps
	rebar compile escriptize

commit: distclean compile check
	@echo "*** check indentation before git push ***"

check: eunit dialyze

eunit:
		rebar clean eunit

dialyze: clean
	dialyzer -I ./include --src -r ./src \
		-Werror_handling -Wrace_conditions -Wunderspecs

dialyzer-setup:
	dialyzer --build_plt --apps erts kernel stdlib crypto \
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

.PHONY : doc
doc:
	@rm -rf public
	@echo doc: building site in public/
	@(cd site && hugo --config=config.yaml --destination=../public -v)
