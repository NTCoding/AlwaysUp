-module(player).

-export([play/0, join_stream/1]).

-define(CHUNKSIZE, 122880). %% Config setting

-include_lib("kernel/include/file.hrl").

play() ->
	Pid = spawn(fun() -> loop([]) end),
	register(player, Pid),
	spawn(fun() -> 
					play_current_song(0),
					timer:sleep(infinity) 
		  end).


loop(Clients) ->
	receive 
		{Pid, clients} ->
			Pid ! {player, Clients},
			loop(Clients);

		{join, Who, Socket} ->
			Who ! {joined, ok},
			%% io:format("New client joined the stream~n"),
			Updated = lists:append(Clients, [Socket]),
			loop(Updated)

	after 10 ->
		loop(Clients)
	end.	
	

play_current_song(CurrentSongBytesSoFar) ->
	timer:sleep(2000), %% Config setting? Intelligent adjustment?
	case get_chunk(CurrentSongBytesSoFar) of
			{continue, Chunk, Size} ->
				publish(Chunk),
				publish(get_metadata()),
				play_current_song(CurrentSongBytesSoFar + Size + 1);

			{boundary, Chunks, Size1} ->
				publish(lists:nth(1, Chunks)),
				publish(lists:nth(2, Chunks)),
				publish(get_metadata()),
				Size2 = size(lists:nth(2, Chunks)),
				play_current_song(Size2 + 1)
		
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
	Data = ["StreamTitle='" ++ playlist:current_song() ++"';StreamUrl='http://localhost:3000'"],
	Binary = list_to_binary(Data),
	NBlocks = ((size(Binary) - 1) div 16) + 1,
	PaddingSize = NBlocks*16 - size(Binary),
	Padding = lists:duplicate(PaddingSize, 0),
	list_to_binary([NBlocks, Binary, Padding]).


publish(Data) ->
	Sockets = get_clients(),
	lists:foreach(fun(Socket) -> gen_tcp:send(Socket, Data) end, Sockets).
	

get_clients() ->
	player ! {self(), clients},
	receive
		{player, Sockets} ->
			Sockets
	end,
	Sockets.


join_stream(Socket) ->
	player ! {join, self(), Socket},
	receive
		{join, ok} ->
			ok
	end.
	
