-module(beacon).
-behavior(application).
-export([start/2, stop/1, init/1, start_client/0]).

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(DEF_PORT, 2222).

start_client() ->
	supervisor:start_child(tcp_client_supervisor, []).
	
start(_Type, _Args) ->
	ListenPort = get_app_env(listen_port, ?DEF_PORT),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, tcp_fsm]).
	
stop(_S) ->
	ok.
	
init([Port, Module]) ->
	{ok,
		{_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
			[
				{tcp_server_supervisor, {tcp_server, start_link, [Port, Module]}, permanent, 2000, worker, [tcp_server]},
				{tcp_client_supervisor, {supervisor, start_link, [{local, tcp_client_supervisor}, ?MODULE, [Module]]}, permanent, infinity, supervisor, []}
			]
		}
	};
	
init([Module]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
			[
				{undefined, {Module, start_link, []}, temporary, 2000, worker, []}
			]
		}
	}.
	
get_app_env(Opt, Default) ->
	case application:get_env(application:get_application(), Opt) of
		{ok, Val} ->
			Val;
		_ ->
			case init:get_argument(Opt) of
				[[Val | _]] ->
					Val;
				error ->
					Default
			end
	end.