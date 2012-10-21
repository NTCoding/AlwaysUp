-module(shoutcast).

-export([start/0]).

-define(CHUNKSIZE, 122880). %% Config setting

-include_lib("kernel/include/file.hrl").

start() ->
	playlist:start(),
	player:play(),
	spawn(fun() -> 
					listen(),
					io:format("Ready to stream - connect on port 3000~n"),
					timer:sleep(infinity) 
		  end).


listen() ->
	{ok, Listen} = gen_tcp:listen(3000, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
	spawn(fun() -> wait_for_connection(Listen) end).


wait_for_connection(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> wait_for_connection(Listen) end),
	inet:setopts(Socket, [{packet, 0}, binary, {nodelay, true}, {active, true}]),
	wait_for_request_to(Socket).


wait_for_request_to(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			stream_to(Socket)
	end.
	

stream_to(Socket) ->
	io:format("Sending response headers to client~n"),
	gen_tcp:send(Socket, [icy_response_headers()]),
	player:join_stream(Socket).

icy_response_headers() ->
	["ICY 200 OK\r\n",
	 "icy-notice1: <BR>This stream requires <a href=\"http://www.winamp.com/\">Winamp</a><BR>\r\n",
	 "icy-notice2: Erlang Shoutcast server<BR>\r\n",
	 "icy-name: Erlang mix\r\n",
	 "icy-genre: Erlang specials\r\n",
	 "icy-url: http://localhost:3000\r\n",
	 "content-type: audio/mpeg\r\n",
	 "icy-pub: 1\r\n",
	 "icy-metaint: ", integer_to_list(?CHUNKSIZE), "\r\n",
	 "icy-br:96\r\n\r\n"].
