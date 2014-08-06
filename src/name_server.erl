-module(name_server).

-behaviour(gen_server).

-export([start_link/0]).

%% API

-export([add/2,
	 list/0]).

%% gen_server callbacks

-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 

%% API

add(Name, Pid) ->
    gen_server:call(?SERVER, {add, Name, Pid}).

list() ->
    gen_server:call(?SERVER, list).

%% gen_server callbacks

%% NB server is started with name_server name

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    io:format("Initialising name_server ~p~n", [self()]),
    Users = dict:new(),
    {ok, Users}.

handle_call({add, Name, Pid}, _From, Users) ->
    {Reply, NewUsers} = case dict:is_key(Name, Users) of
			    false ->
				{ok, dict:append(Name, Pid, Users)};
			    true ->
				{{error, "Name exists"}, Users}
			end,
    {reply, Reply, NewUsers};
handle_call(list, _From, Users) ->
    Reply = dict:to_list(Users),
    {reply, Reply, Users}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, Users) ->
    {noreply, Users}.

terminate(_Reason, _Users) ->
    ok.

code_change(_OldVsn, Users, _Extra) ->
    {ok, Users}.

