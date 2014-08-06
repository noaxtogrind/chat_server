-module(name_server).

-behaviour(gen_server).

-export([start_link/0]).

%% API

-export([speak/0]).

%% gen_server callbacks

-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 

%% API

speak() ->
    gen_server:cast(?SERVER, speak).

%% gen_server callbacks

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    io:format("Initialising name_server ~p~n", [self()]),
    Users = dict:new(),
    {ok, Users}.

handle_call(_Request, _From, Users) ->
    Reply = ok,
    {reply, Reply, Users}.

handle_cast(speak, Users) ->
    %% io:format("Hello World from name_server!~n"),
    io:format("~p~n", [dict:to_list(Users)]),
    {noreply, Users}.

handle_info(_Info, Users) ->
    {noreply, Users}.

terminate(_Reason, _Users) ->
    ok.

code_change(_OldVsn, Users, _Extra) ->
    {ok, Users}.

