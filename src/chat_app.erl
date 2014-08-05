-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 1066).

start(_StartType, _StartArgs) ->
    case chat_sup:start_link(?PORT) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
