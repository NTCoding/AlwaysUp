-module(shoutcast).

-export([start/0]).

-define(CHUNKSIZE, 122880). 

-include_lib("kernel/include/file.hrl").

start() ->
	playlist:start(),
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
	case get_chunk(CurrentSongBytesSoFar) of
		{continue, Chunk, Size} ->
			gen_tcp:send(Socket, Chunk),
			Metadata = get_metadata(),
			gen_tcp:send(Socket, Metadata),
			send_data_to(Socket, CurrentSongBytesSoFar + Size + 1);

		{boundary, Chunks, Size1} ->
			gen_tcp:send(Socket, lists:nth(1, Chunks)),
			gen_tcp:send(Socket, lists:nth(2, Chunks)),
			Metadata = get_metadata(),
			gen_tcp:send(Socket, Metadata),
			Size2 = size(lists:nth(2, Chunks)),
			send_data_to(Socket, Size2 + 1)
	end.

		

get_chunk(BytesSoFar) ->
	StartByte = BytesSoFar,
	EndByte = BytesSoFar + ?CHUNKSIZE,
	FileName = playlist:current_song(),
	io:format("Streaming: ~p~n", [FileName]),
	FileSize = get_file_size(FileName),
	if
		EndByte > FileSize ->
			io:format("Reached end of ~p~n", [FileName]),
			{Chunks, Size} = chunk_end_and_start_together(FileName, StartByte, FileSize - StartByte),
			{boundary, Chunks, Size};

		true ->
			{Chunk, Size1} = get_chunk(FileName, StartByte, ?CHUNKSIZE),
			{continue, Chunk, Size1}
	end.


get_chunk(FileName, Start, Length) ->
	{ok, Stream} = file:open(FileName, [read, binary, raw]),
	io:format("Sending bytes ~p to ~p~n", [Start, Start + Length]),	
	{ok, Chunk} = file:pread(Stream, Start, Length),
	file:close(Stream),
	{Chunk, ?CHUNKSIZE}.


get_file_size(FileName) ->
	{ok, FileInfo} = file:read_file_info(FileName),
	#file_info{size=Size} = FileInfo,
	Size.


chunk_end_and_start_together(FileName, Start, Length) ->
	EndChunk = read_chunk_from(FileName, Start, Length),
	{ok, NewFileName} = playlist:advance_song(),
	StartChunk = read_chunk_from(NewFileName, 0, ?CHUNKSIZE - size(EndChunk)),
	{[EndChunk, StartChunk], ?CHUNKSIZE}.


read_chunk_from(FileName, Start, Length) ->
	{ok, Stream} = file:open(FileName, [read, binary, raw]),
	{ok, Chunk} = file:pread(Stream, Start, Length),
	file:close(Stream),
	Chunk.


get_metadata() ->
	Data = ["StreamTitle='wickedsong';StreamUrl='http://localhost:3000'"],
	Binary = list_to_binary(Data),
	NBlocks = ((size(Binary) - 1) div 16) + 1,
	PaddingSize = NBlocks*16 - size(Binary),
	Padding = lists:duplicate(PaddingSize, 0),
	Header = list_to_binary([NBlocks, Binary, Padding]),
	Header.
    
	
	
	
	
	
	
