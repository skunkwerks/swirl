all:
	rebar compile escriptize

get-deps:
	rebar get-deps

clean:
	rebar clean

dev:
	erl -pa ./ebin -I ./include -s crypto -smp -setcookie swirl -s sync \
		-sname swirl +K true +A 16 -s swirl_app

run:
	erl -pa ./ebin -I ./include -s crypto -smp -setcookie swirl \
		-sname swirl +K true +A 16 -s swirl_app
