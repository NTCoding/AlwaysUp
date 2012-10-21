all: compile start

compile:
	./rebar compile

start:
	erl -boot start_sasl
	application:load(shoutcast).
	application:start(shoutcast).
