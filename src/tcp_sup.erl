-module(tcp_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

start_child() ->
    supervisor:start_child(?SERVER, []).

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}]),
    Server = {tcp_server, {tcp_server, start_link, [LSock]},
	      temporary, brutal_kill, worker, [tcp_server]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    spawn_link(fun() ->		       
		       [start_child() || _ <- lists:seq(1, 5)]
	       end),
    {ok, {RestartStrategy, Children}}.
