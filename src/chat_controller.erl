-module(chat_controller).

-include("chat.hrl").

-export([handle/3]).

-define(CONNECT, <<"CONNECT">>).

-define(QUIT, <<"QUIT">>).

connect(Socket, User, Name) ->
    case User#user.name of
	?NULL ->
	    case name_server:add(Name, Socket) of
		ok ->
		    gen_tcp:send(Socket, "Connected\n"),
		    User#user{name=Name};
		{error, ErrMsg} ->
		    gen_tcp:send(Socket, ErrMsg++"\n"),
		    User
	    end;
	_ ->
	    gen_tcp:send(Socket, "Already connected\n"),
	    User
    end.

quit(Socket, User) ->
    case User#user.name of
	?NULL ->
	    gen_tcp:send(Socket, "Not connected\n"),
	    User;
	Name ->
	    case name_server:remove(Name) of
		ok ->
		    gen_tcp:send(Socket, "Disconnected\n"),
		    User#user{name=?NULL};
		{error, ErrMsg} ->
		    gen_tcp:send(Socket, ErrMsg++"\n"),
		    User    
	    end
    end.

unhandled(Socket, User) ->
    gen_tcp:send(Socket, "Unhandled\n"),
    User.

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
		    connect(Socket, User, Name);
		[?QUIT] ->
		    quit(Socket, User);
		_ ->
		    unhandled(Socket, User)
	    end
    end.
