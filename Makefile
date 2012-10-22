all: compile start

compile:
	./rebar compile

start:
	erl -pa ebin -boot start_sasl -s shoutcast -noshell
	
