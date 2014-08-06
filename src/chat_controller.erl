-module(chat_controller).

-export([handle/3]).

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

handle(Socket, RawData, _Args) ->
    case RawData of
	?BLANK ->
	    void;
	_ ->
	    CleanData=clean_newline(RawData),
	    Resp=case re:split(CleanData, "\\:\\s*") of 
		     [?CONNECT, Name] ->
			 io_lib:fwrite("Connected as ~p~n", [binary_to_list(Name)]);
		     [?QUIT] ->
			 "Quit!\n";
		     _ ->
			 "Unknown :-(\n"
		 end,
	    gen_tcp:send(Socket, Resp)
    end.
