-module(chat_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(CONTROLLER, chat_controller).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, ?CONTROLLER]).

init([Port, Mod]) ->
    NameServer = {name_server, {name_server, start_link, []},
		  permanent, 10500, worker, [name_server]},
    TCPSup = {tcp_sup, {tcp_sup, start_link, [Port, Mod]},
	      permanent, 10500, supervisor, [tcp_sup]},
    Children = [NameServer, TCPSup],
    RestartStrategy = {one_for_one, 6, 3600},
    {ok, {RestartStrategy, Children}}.
