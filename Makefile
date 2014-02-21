all: clean compile dialyze

get-deps:
	rebar get-deps

clean:
	rebar clean

compile:
	rebar compile escriptize

dialyze:
	dialyzer -pa ./ebin -I ./include -r ebin \
		-Werror_handling -Wrace_conditions

dialyzer-setup:
	dialyzer --build_plt --apps erts kernel stdlib crypto mnesia \
		sasl common_test eunit compiler

dev: all
	erl -pa ./ebin -I ./include -s crypto -smp -setcookie swirl -s sync \
        	-sname swirl +K true +A 16 -s swirl

run: all
	erl -pa ./ebin -I ./include -s crypto -smp -setcookie swirl \
		-sname swirl +K true +A 16 -s swirl -noshell -noinput

