-module(shoutcast).

-export([start/0]).


start() ->
	application:start(shoutcast),
	ok.

