-module(playlist_provider).

-export([start/0, stream/1]).


start() ->
	io:format("Starting playlist provider~n"),
	Pid = spawn(fun() -> 
					Playlist = playlist_generator:generate(3),
					io:format("Intitial playlist: ~p~n", [Playlist]),					
					loop({Playlist ,[], []}) 
		  		 end),
	register(playlist, Pid).


loop({Playlist, Connections, LastPublishTime}) ->
	
	case is_time_to_publish_next_chunk(LastPublishTime) of
		true -> 
			playlist ! {publish};

		false -> []
	end,
	receive
		{connect, Pid} ->
			io:format("Connecting client: ~p to playlist provider~n", [Pid]),
			loop({Playlist, lists:append([Pid], Connections), LastPublishTime});

		{publish} ->
			publish_song(Playlist, Connections),
			[CurrentSong|QueuedSongs] = Playlist,
			NewPlaylist = Playlist, %% append another song here
			io:format("New playlist: ~p~n", [NewPlaylist]),
			loop({NewPlaylist, Connections, now()});

		{Pid, connections} ->
			Pid ! {ok, Connections},
			loop({Playlist, Connections, LastPublishTime})
	after (1000 * 2) ->
		loop({Playlist, Connections, LastPublishTime})
	end.


is_time_to_publish_next_chunk([]) ->
	true;

is_time_to_publish_next_chunk(Date) ->
	Diff = timer:now_diff(now(), Date),
	Milliseconds = Diff / 1000,
	Seconds = Milliseconds / 1000,
	io:format("Time to next publish: ~p seconds~n", [2 - Seconds]),
	Seconds > 2 .


publish_song(Playlist, []) ->
	io:format("No clients connected. Publish aborted~n");

publish_song(Playlist, Connections) ->
	[Filename|Tail] = Playlist,
	io:format("About to publish: ~p~n", [Filename]),
	{ok, Mp3Data} = file:read_file("songs/" ++ Filename),
	lists:foreach(fun(C) -> C ! {chunk, Mp3Data} end, Connections).


stream_blah(Pid) ->
	case is_connected(Pid) of
		false -> playlist ! {connect, Pid};
		true -> []
	end,
	receive
		{chunk, Mp3Data} -> []					
	end,
	%% Might need to remove Pid from connection here?
	spawn(fun() -> io:format("Streaming chunk to client:~p~n", [Mp3Data]) end), 	
	Mp3Data.

stream(Pid) ->
	case is_connected(Pid) of
		false -> playlist ! {connect, Pid};
		true -> []
	end,
	playlist ! {Pid, connections},
	receive
		{ok, Connections} -> []
	end,
	lists:foreach(fun(C) -> send_song(C) end, Connections),
	receive
		{song, Binary} -> []
	end,
	Binary.


send_song(Pid) ->
	{ok, Mp3Data} = file:read_file("songs/song2.mp3"),
	Pid ! {song, Mp3Data}.

is_connected(Pid) ->
	playlist ! {Pid, connections},
	receive
		{ok, Connections} -> []
	end,
	lists:member(Pid, Connections).


