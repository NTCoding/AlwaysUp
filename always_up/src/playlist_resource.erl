-module(playlist_resource).

-export([init/1, content_types_provided/2, to_mp3/2]).

-include_lib("webmachine/include/webmachine.hrl").


init([]) ->
	{ok, undefined}.


content_types_provided(ReqData, State) ->
	{[{"audio/mpeg3", to_mp3}], ReqData, State}.


to_mp3(ReqData, State) ->
	File = get_random_song_file(),
	{ok, Content} = file:read_file("songs/" ++ File),
	{Content, ReqData, State}.


get_random_song_file() ->
	{ok, Filenames} = file:list_dir("songs"),
	select_randomly(Filenames).


select_randomly(Filenames) ->
	random:seed(now()),
	Index = random:uniform(length(Filenames)),
	lists:nth(Index, Filenames).
		

	



	
