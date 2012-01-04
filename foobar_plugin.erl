-module(foobar_plugin).
-compile(export_all).

-define(BAR_COUNT, filename:join("build", "bar.count")).

foo(_Config, _AppFile) ->
    rebar_log:log(info, "foo!~n", []).

clean(_Config, _AppFile) ->
    case is_base_dir() of
        true -> rebar_file_utils:rm_rf("build");
        false -> ok
    end.

bar(_Config, _AppFile) ->
    case is_base_dir() of
        true -> 
            Count = case filelib:is_regular(?BAR_COUNT) of
                false ->
                    0;
                true ->
                    {ok, Bin} = file:read_file(?BAR_COUNT),
                    list_to_integer(binary_to_list(Bin))
            end,
            NewCount = Count + 1,
            io:format("Bar command has been run ~p times~n", [NewCount]),
            rebar_utils:ensure_dir(?BAR_COUNT),
            file:write_file(?BAR_COUNT,
                        list_to_binary(integer_to_list(NewCount)), [write]);
        false ->
            ok
    end.

is_base_dir() ->
    rebar_utils:get_cwd() == rebar_config:get_global(base_dir, undefined).
