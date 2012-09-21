-module(playlist_provider).

-behaviour(gen_server).

%% API
-export([start/0, stream/1]).


%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start() ->
	io:format("Starting playlist provider~n"),
	gen_server:start_link({local, playlist}, ?MODULE, [], []).


stream(Pid) ->
	gen_server:call(playlist, {stream, Pid}).



	

%% Gen server
init([]) -> 
	Playlist = playlist_generator:generate(3),
	io:format("Initial playlist: ~p~n", [Playlist]),
	State = {Playlist, [], []},
	{ok, State}.

handle_call({stream, Pid}, From, State) ->
	{Playlist, _, _} = State,
	Filename = 	lists:nth(1, Playlist),
	{ok, Mp3Data} = file:read_file("songs/" ++ Filename),
	{reply, Mp3Data, State};

handle_call(_Request, _From, State) ->
	{reply, [], State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	ok.

code_change(_OldVsn, State, Extra) ->
	{ok, State}.
	
