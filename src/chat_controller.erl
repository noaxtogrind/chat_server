-module(chat_controller).

-export([handle/2]).

-define(BLANK, "\r\n").

-define(CONNECT, <<"CONNECT">>).

-define(QUIT, <<"QUIT">>).

clean_newline(Input) ->
    case re:replace(Input, "\\r?\\n$", "") of
	[Output, []] ->
	    Output;
	_ ->
	    list_to_binary(Input)
    end.

handle(Socket, RawData) ->
    case RawData of
	?BLANK ->
	    void;
	_ ->
	    CleanData=clean_newline(RawData),
	    case re:split(CleanData, "\\:\\s*") of 
		[?CONNECT, Name] ->
		    Resp=io_lib:fwrite("Connected as ~p~n", [binary_to_list(Name)]),
		    gen_tcp:send(Socket, Resp);
		[?QUIT] ->
		    gen_tcp:send(Socket, "Quit!\n");
		_ ->
		    gen_tcp:send(Socket, "Unknown :-(\n")
	    end
    end.
