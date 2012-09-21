-module(playlist).

-export([start/0]).
-import(lists, [map/2, reverse/1]).

-define(CHUNKSIZE, 24576).


start() -> 
	spawn(fun() ->
				start_parallel_server(3000),
				sleep(infinity)
		  end).
	
			

sleep(T) ->
    receive
    after T ->
       true
    end.
