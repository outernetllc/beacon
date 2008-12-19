{application, beacon, [
	{description, "The Beacon Realtime Messaging Server"},
	{vsn, "1.5"},
	{id, "beacon"},
	{modules, [beacon_events, tcp_fsm, tcp_server]},
	{registered,   [tcp_server_supervisor, tcp_server]},
	{applications, [kernel, stdlib]},
	{mod, {beacon, []}},
	{dnv, []}
]}.