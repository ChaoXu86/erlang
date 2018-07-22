log(Format,Args) ->
    io:format(readable_current_time()++Format, Args).
log(Messages) ->
    io:format(readable_current_time() ++ Messages).

readable_current_time() ->
    {{Y,M,D},{Hour,Min,Sec}} = calendar:now_to_datetime(os:timestamp()),
    lists:flatten(io_lib:format("[~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w]",[Y,M,D,Hour,Min,Sec])).

