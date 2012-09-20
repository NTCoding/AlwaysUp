-module(playlist_resource).

-export([init/1, content_types_provided/2, to_mp3/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
	{ok, undefined}.


content_types_provided(ReqData, State) ->
	{[{"audio/mpeg3", to_mp3}], ReqData, State}.


to_mp3(ReqData, State) ->
	File = "song.mp3",
	{ok, Content} = file:read_file(File),
	{Content, ReqData, State}.

	



	