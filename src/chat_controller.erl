-module(chat_controller).

-include("chat_app.hrl").

-export([handle/3]).

-define(CONNECT, <<"CONNECT">>).

-define(SAY, <<"SAY">>).

-define(PRIV, <<"PRIV">>).

-define(QUIT, <<"QUIT">>).

send(Socket, Msg) ->
    gen_tcp:send(Socket, Msg++"\n").
    
broadcast(Msg) ->
    lists:map(fun({_, [Socket]}) -> send(Socket, Msg) end, name_server:list()).

connect(Socket, User, Name) ->
    case User#user.name of
	?NULL ->
	    case name_server:add(Name, Socket) of
		ok ->
		    broadcast(io_lib:fwrite("~p connected", [binary_to_list(Name)])),
		    User#user{name=Name};
		{error, ErrMsg} ->
		    send(Socket, ErrMsg),
		    User
	    end;
	_ ->
	    send(Socket, "Already connected"),
	    User
    end.

say(Socket, User, Msg) ->
    case User#user.name of
	?NULL ->
	    send(Socket, "Not connected"),
	    User;
	Name ->
	    broadcast(io_lib:fwrite("~p says ~p", [binary_to_list(Name), binary_to_list(Msg)])),   
	    User
    end.

priv(FromSocket, User, ToName, Msg) ->
    case User#user.name of
	?NULL ->
	    send(FromSocket, "Not connected"),
	    User;
	FromName ->
	    case name_server:lookup(ToName) of
		{ok, ToSocket}->
		    send(ToSocket, io_lib:fwrite("~p says ~p", [binary_to_list(FromName), binary_to_list(Msg)])),
		    User;
		{error, ErrMsg} ->
		    send(FromSocket, ErrMsg),
		    User    
	    end
    end.

quit(Socket, User) ->
    case User#user.name of
	?NULL ->
	    send(Socket, "Not connected"),
	    User;
	Name ->
	    case name_server:remove(Name) of
		ok ->
		    broadcast(io_lib:fwrite("~p disconnected", [binary_to_list(Name)])),
		    send(Socket, "Disconnected"),		    
		    User#user{name=?NULL};
		{error, ErrMsg} ->
		    send(Socket, ErrMsg),
		    User    
	    end
    end.

handle(Socket, RawData, User) ->
    case RawData of
	?BLANK ->
	    User;
	_ ->
	    [CleanData, []] = re:replace(RawData, "\\r?\\n$", ""),
	    case re:split(CleanData, "\\:\\s*") of 
		[?CONNECT, Name] ->
		    connect(Socket, User, Name);
		[?SAY, Msg] ->
		    say(Socket, User, Msg);
		[?PRIV, ToName, Msg] ->
		    priv(Socket, User, ToName, Msg);
		[?QUIT] ->
		    quit(Socket, User);
		_ ->
		    send(Socket, "Unhandled"),
		    User
	    end
    end.
