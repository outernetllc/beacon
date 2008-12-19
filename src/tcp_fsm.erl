-module(tcp_fsm).
-behavior(gen_fsm).

-export([start_link/0, set_socket/2, init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export(['WAIT_FOR_SOCKET'/2, 'WAIT_FOR_DATA'/2]).

-define(TIMEOUT, 120000).

-record(state, {socket, addr}).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).
	
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
	gen_fsm:send_event(Pid, {socket_ready, Socket}).
	
init([]) ->
	process_flag(trap_exit, true),
	{ok, 'WAIT_FOR_SOCKET', #state{}}.
	
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
	inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
	{ok, {IP, _Port}} = inet:peername(Socket),
	{next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
	
'WAIT_FOR_SOCKET'(Other, State) ->
	error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p~n", [Other]),
	{next_state, 'WAIT_FOR_SOCKET', State}.
	
'WAIT_FOR_DATA'({data, Data}, #state{socket=S} = State) ->
	ok = gen_tcp:send(S, Data),
	{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
	
'WAIT_FOR_DATA'(timeout, State) ->
	error_logger:error_msg("~p Client Connection timeout - closing.~n", [self()]),
	{stop, normal, State};
	
'WAIT_FOR_DATA'(Data, State) ->
	io:format("~p Ignoring data: ~p~n", [self(), Data]),
	{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.
	
handle_event(Event, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.
	
handle_sync_event(Event, _From, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.
	
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Bin}, StateData);
	
handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=Addr} = StateData) ->
	error_logger:info_msg("~p Client ~p disconnected~n", [self(), Addr]),
	{stop, normal, StateData}.
	
terminate(_Reason, _StateName, #state{socket=Socket}) ->
	(catch gen_tcp:close(Socket)),
		ok.
		
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.