-module(chat_controller).

-include("chat.hrl").

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

handle(Socket, RawData, User) ->
    case RawData of
	?BLANK ->
	    User;
	_ ->
	    CleanData=clean_newline(RawData),
	    case re:split(CleanData, "\\:\\s*") of 
		[?CONNECT, Name] ->
		    Resp=io_lib:fwrite("Connected as ~p~n", [binary_to_list(Name)]),
		    gen_tcp:send(Socket, Resp),
		    User#user{name=Name};
		[?QUIT] ->
		    Resp="Quit!\n",
		    gen_tcp:send(Socket, Resp),
		    User#user{name=null};
		_ ->
		    Resp="Unknown :-(\n",
		    gen_tcp:send(Socket, Resp),
		    User
	    end
    end.
