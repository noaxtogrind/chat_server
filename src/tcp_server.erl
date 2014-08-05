-module(tcp_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2, 
	 code_change/3]).

-record(state, {lsock}).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    io:format("Initialising ~p~n", [self()]),
    erlang:send_after(0, self(), trigger),
    {ok, #state{lsock = LSock}}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    io:format("Received '~p' from ~p~n", [RawData, Socket]),
    inet:setopts(Socket, [{active, once}]), 
    gen_tcp:send(Socket, io_lib:fwrite("You sent: ~p~n", [RawData])),
    {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
    io:format("~p closed~n", [Socket]),
    {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
    io:format("~p error; closing~n", [Socket]),
    {stop, normal, State};
handle_info(trigger, #state{lsock = LSock} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    inet:setopts(Socket, [{active, once}]),
    io:format("~p accepted~n", [Socket]),
    tcp_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p terminated~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

