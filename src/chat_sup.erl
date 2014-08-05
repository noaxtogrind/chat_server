-module(chat_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
    TCPSup = {tcp_sup, {tcp_sup, start_link, [Port]},
	      permanent, 10500, supervisor, [tcp_sup]},
    Children = [TCPSup],
    RestartStrategy = {one_for_one, 6, 3600},
    {ok, {RestartStrategy, Children}}.
