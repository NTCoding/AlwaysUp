-module(shoutcast).

-export([start/0]).

-define(CHUNKSIZE, 122880). 

-define(FILENAME, "song.mp3").

-include_lib("kernel/include/file.hrl").

start() ->
	spawn(fun() -> 
					listen(),
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
	end,
	wait_for_request_to(Socket).


stream_to(Socket) ->
	io:format("Sending response headers to client~n"),
	gen_tcp:send(Socket, [icy_response_headers()]),
	send_data_to(Socket, 0).

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


send_data_to(Socket, CurrentSongBytesSoFar) ->
	{Chunk, Size} = get_chunk(CurrentSongBytesSoFar),
	gen_tcp:send(Socket, Chunk),
	Metadata = get_metadata(),
	gen_tcp:send(Socket, Metadata),
	send_data_to(Socket, CurrentSongBytesSoFar + Size + 1).
		

get_chunk(BytesSoFar) ->
	StartByte = BytesSoFar,
	EndByte = BytesSoFar + ?CHUNKSIZE,
	FileSize = get_file_size(),
	if
		EndByte > FileSize ->
			io:format("restarting song~n"),
			chunk_end_and_start_together(StartByte, FileSize);

		true ->
			get_chunk(StartByte, ?CHUNKSIZE)
	end.


get_chunk(Start, Length) ->
	{ok, Stream} = file:open(?FILENAME, [read, binary, raw]),
	io:format("Reading chunks ~p to ~p~n", [Start, Start + Length]),	
	{ok, Chunk} = file:pread(Stream, Start, Length),
	file:close(Stream),
	{Chunk, ?CHUNKSIZE}.


get_file_size() ->
	{ok, FileInfo} = file:read_file_info(?FILENAME),
	#file_info{size=Size} = FileInfo,
	Size.


chunk_end_and_start_together(Start, FileSize) ->
	{ok, Stream} = file:open(?FILENAME, [read, binary, raw]),
	{ok, EndChunk} = file:pread(Stream, Start, (FileSize - Start)),
	{ok, StartChunk} = file:pread(Stream, 0, ?CHUNKSIZE - size(EndChunk)),
	file:close(Stream),
	{[EndChunk, StartChunk], ?CHUNKSIZE}.


get_metadata() ->
	Data = ["StreamTitle='wickedsong';StreamUrl='http://localhost:3000'"],
	Binary = list_to_binary(Data),
	NBlocks = ((size(Binary) - 1) div 16) + 1,
	PaddingSize = NBlocks*16 - size(Binary),
	Padding = lists:duplicate(PaddingSize, 0),
	Header = list_to_binary([NBlocks, Binary, Padding]),
	Header.
    
	
	
	
	
	
	
