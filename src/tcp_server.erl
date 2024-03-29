-module(tcp_server).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2, 
	 code_change/3]).

-record(state, {lsock, mod, args}).

start_link(LSock, Mod, Args) ->
    gen_server:start_link(?MODULE, [LSock, Mod, Args], []).

init([LSock, Mod, Args]) ->
    io:format("Initialising tcp_server ~p~n", [self()]),
    erlang:send_after(0, self(), trigger),
    {ok, #state{lsock = LSock, mod = Mod, args = Args}}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData},  #state{mod = Mod, args = Args} = State) ->
    %% io:format("Received '~p' from ~p~n", [RawData, Socket]),
    inet:setopts(Socket, [{active, once}]), 
    NewArgs = Mod:handle(Socket, RawData, Args),
    {noreply, State#state{args = NewArgs}};
handle_info({tcp_closed, _Socket}, State) ->
    %% io:format("~p closed~n", [Socket]),
    {stop, normal, State};
handle_info({tcp_error, _Socket}, State) ->
    %% io:format("~p error; closing~n", [Socket]),
    {stop, normal, State};
handle_info(trigger, #state{lsock = LSock} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    inet:setopts(Socket, [{active, once}]),
    %% io:format("~p accepted~n", [Socket]),
    tcp_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% io:format("~p terminated~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

