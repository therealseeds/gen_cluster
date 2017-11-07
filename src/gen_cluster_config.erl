-module(gen_cluster_config).

-export([get_opts/2,
         reload_sys/0]).

-spec get_opts(Application :: atom(),
               [Key :: atom() |
                {Key :: atom(), NewKey :: atom()} |
                {Key :: atom(), NewKey :: atom(), Default :: any()}]) ->
                      [{atom(), any()}].
get_opts(App, OptSpec) ->
    get_opts(App, OptSpec, []).
get_opts(App, [{EnvKey, NewKey} | Rest], Done) ->
    get_opts(App, [{EnvKey, NewKey, undefined} | Rest], Done);
get_opts(App, [{EnvKey, NewKey, Default} | Rest], Done) ->
    case application:get_env(App, EnvKey) of
	undefined ->
	    case Default of
		undefined ->
		    get_opts(App, Rest, Done);
		_ ->
		    get_opts(App, Rest, [{NewKey, Default} | Done])
	    end;
	{ok, Value} ->
	    get_opts(App, Rest, [{NewKey, Value} | Done])
    end;
get_opts(App, [EnvKey | Rest], Done) ->
    get_opts(App, [{EnvKey, EnvKey} | Rest], Done);
get_opts(_App, [], Done) ->
    Done.

-spec reload_sys() -> {ok, EnvDiff} | {error, {config_change, EnvDiff, [{error, any()}]}} | {error, any()} when
      EnvDiff :: [{AppName :: atom(),
                   {Changed :: [{atom(), any()}],
                    New :: [{atom(), any()}],
                    Removed :: [atom()]}
                  }].
reload_sys() ->
    Files = case init:get_argument(config) of
                {ok, FoundFiles} -> FoundFiles;
                _ -> []
            end,
    case lists:filter(fun (File) -> filename:basename(File,".config") =:= "sys" end, Files) of
        [SysFile] ->
            case file:consult(filename:join(filename:dirname(SysFile), "sys.config")) of
                {ok, [Config]} ->
                    Appls = [{application, A, element(2, application:get_all_key(A))} ||
                                {A,_,_} <- application:which_applications()],
                    EnvBefore = application_controller:prep_config_change(),
                    %% change_application_data takes care of loading config file references in sys.config
                    case application_controller:change_application_data(Appls, Config) of
                        ok -> Appls;
                        {error, Reason} -> exit({change_appl_data, Reason})
                    end,
                    EnvDiff =
                        lists:flatmap(
                          fun ({_,App,_}) ->
                                  AppEnvBefore = case lists:keyfind(App, 1, EnvBefore) of
                                                     {App, FoundAppEnvBefore} -> FoundAppEnvBefore;
                                                     false -> []
                                                 end,
                                  AppEnvNow = lists:sort(application:get_all_env(App)),
                                  case application_controller:do_config_diff(AppEnvNow, AppEnvBefore) of
                                      {[],[],[]} -> [];
                                      AppEnvDiff -> [{App, AppEnvDiff}]
                                  end
                          end, Appls),
                    error_logger:warning_report([{"sys.config reloaded", EnvDiff}]),
                    case application_controller:config_change(EnvBefore) of
                        ok ->
                            {ok, EnvDiff};
                        {error, ConfigChangeErrs} ->
                            error_logger:error_report([{"sys.config reloaded", EnvDiff},
                                                       {"with config_change errors", ConfigChangeErrs}]),
                            {error, {config_change, EnvDiff, ConfigChangeErrs}}
                    end;
                {ok, []} ->
                    {error, sys_file_empty};
                {error, Err} ->
                    {error, Err}
            end;
        [_|_] ->
            {error, multiple_sys_files};
        [] ->
            {error, no_sys_file}
    end.
