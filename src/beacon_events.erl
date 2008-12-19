-module(beacon_events).
-export([create/1, add_handler/2, event/2]).

create(Name) ->
	register(Name, spawn(fun() -> handler(fun no_op/1) end)).
	
add_handler(Name, Fun) -> Name ! {add, Fun}.

event(Name, X) -> Name ! {event, X}.

handler(Fun) ->
	receive
		{add, Fun1} ->
			handler(Fun1);
		{event, Any} ->
			(catch Fun(Any)),
			handler(Fun)
	end.
	
no_op(_) -> void.