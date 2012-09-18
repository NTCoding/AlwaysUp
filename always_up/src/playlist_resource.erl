-module(playlist_resource).

-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
	{ok, undefined}.


to_html(ReqData, State) ->
	Content = "<head><body><h3>Playlistio</h3></body></head>",
	{Content, ReqData, State}.
