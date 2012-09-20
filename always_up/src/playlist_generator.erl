-module(playlist_generator).

-export([generate/1, get_random_song/0]).


generate(Number) ->
	build(Number, []).


build(Number, Playlist) ->

	case length(Playlist) of

		3 -> Playlist;
		
		Any -> 
			build(Number, [get_random_song() | Playlist])

	end.
	

get_random_song() ->
	{ok, Filenames} = file:list_dir("songs"),
	Mp3Files = lists:filter(fun(F) -> lists:suffix(".mp3", F) end, Filenames),
	select_randomly(Mp3Files).


select_randomly(Filenames) ->
	random:seed(now()),
	Index = random:uniform(length(Filenames)),
	lists:nth(Index, Filenames).
