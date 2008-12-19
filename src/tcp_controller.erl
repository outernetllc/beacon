-module(tcp_controller).
-export([handle_event/1]).
		
handle_event(too_many_users) ->
	io:format("Stop letting these freeloaders in.~n");
handle_event(X) ->
	io:format("~w ignored event: ~p~n", [?MODULE, X]).