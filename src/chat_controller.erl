-module(chat_controller).

-export([handle/1]).

%% simple echo pattern

handle(RawData) ->
    io_lib:fwrite("You sent: ~p~n", [RawData]).
