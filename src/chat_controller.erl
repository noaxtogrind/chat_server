-module(chat_controller).

-export([handle/1]).

-define(BLANK, "\r\n").

%% http://stackoverflow.com/questions/853296/remove-whitespace-using-erlang-regex

clean(Input) ->
    case re:replace(Input, "\\r?\\n$", "") of
	[Output, []] ->
	    Output;
	_ ->
	    list_to_binary(Input)
    end.

handle(RawData) ->
    case RawData of
	?BLANK ->
	    "No data received\n";	
	_ ->
	    CleanData = clean(RawData),
	    io_lib:fwrite("You sent: ~p~n", [binary_to_list(CleanData)])
    end.
