-module(chat_controller).

-export([handle/2]).

%% simple echo pattern

handle(Socket, Data) ->
    gen_tcp:send(Socket, io_lib:fwrite("You sent: ~p~n", [Data])).
