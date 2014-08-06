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
		    case name_server:add(Name, Socket) of
			ok ->
			    gen_tcp:send(Socket, "Connected\n"),
			    User#user{name=Name};
			{error, ErrMsg} ->
			    gen_tcp:send(Socket, ErrMsg++"\n"),
			    User
		    end;
		[?QUIT] ->
		    case User#user.name of
			null ->
			    gen_tcp:send(Socket, "Not connected\n"),
			    User;
			Name ->
			    case name_server:remove(Name) of
				ok ->
				    gen_tcp:send(Socket, "Disconnected\n"),
				    User#user{name=null};
				{error, ErrMsg} ->
				    gen_tcp:send(Socket, ErrMsg++"\n"),
				    User    
			    end
		    end;
		_ ->
		    gen_tcp:send(Socket, "Unhandled\n"),
		    User
	    end
    end.
