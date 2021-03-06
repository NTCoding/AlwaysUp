-module(playlist).

-export([start/0, current_song/0, advance_song/0]).


start() ->
	io:format("Starting playlist~n"),
	Pid = spawn(fun() -> loop(get_random_song()) end),
	register(playlist, Pid).


current_song() ->
	playlist ! {self(), current_song},
	receive
		{ok, Song} ->
			Song
	end.

advance_song() ->
	playlist ! {self(), advance_song},
	receive
		{ok, NewFileName} -> 
			{ok, NewFileName}
	end.

loop(Song) ->
	receive
		{Pid, current_song} ->
			Pid ! {ok, Song},
			loop(Song);

		{Pid, advance_song} ->
			NewSong = get_random_song(),
			Pid ! {ok, NewSong},
			io:format("Playlist updated. Current song: ~p~n", [NewSong]),
			loop(NewSong)
	after 10 ->
		loop(Song)
	end.



get_random_song() ->
	{ok, Filenames} = file:list_dir("songs"),
	Mp3Files = lists:filter(fun(F) -> lists:suffix(".mp3", F) end, Filenames),
	select_randomly(Mp3Files).


select_randomly(Filenames) ->
	random:seed(now()),
	Index = random:uniform(length(Filenames)),
	lists:nth(Index, Filenames).
	
