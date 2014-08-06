-module(chat_controller).

-export([handle/1]).

%% http://stackoverflow.com/questions/853296/remove-whitespace-using-erlang-regex

clean(Input) ->
    case re:replace(Input, "\\r?\\n$", "") of
	[Output, []] ->
	    binary_to_list(Output);
	_ ->
	    Input
    end.

handle(RawData) ->
    case RawData of
	"\r\n" ->
	    "No data received\n";	
	_ ->
	    CleanData = clean(RawData),
	    io_lib:fwrite("You sent: ~p~n", [CleanData])
    end.
