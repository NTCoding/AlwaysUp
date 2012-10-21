all: compile start

compile:
	./rebar compile

start:
	erl -s application load shoutcast -s application start shoutcast
	
