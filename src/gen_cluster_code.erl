-module(gen_cluster_code).

-export([changed/0,
         is_changed/1,
         load_all/0,
         update_erts_lib_path/1,
         update_code_paths/0]).

-spec changed() -> [module()].
changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M) =:= true].

-spec is_changed(module()) -> boolean() | 'error'.
is_changed(Mod) ->
    case code:get_object_code(Mod) of
        {Mod, Beam, _Filename} ->
            case beam_lib:version(Beam) of
                {ok, {Mod, DiskVsn}} ->
                    {value, {_, LoadedVsn}} =
                        lists:keysearch(vsn, 1, Mod:module_info(attributes)),
                    LoadedVsn =/= DiskVsn;
                {error, beam_lib, _} ->
                    error
            end;
        error ->
            error
    end.

-spec load_all() -> {'ok', [module()]} | {'error', [{module(), any()}]}.
load_all() ->
    update_code_paths(),
    Mods = changed(),
    [code:purge(Mod) || Mod <- Mods],
    case code:atomic_load(Mods) of
        ok ->
            error_logger:warning_report([{"code reloaded", Mods}]),
            {ok, Mods};
        {error, Errs} ->
            error_logger:warning_report([{"code reloaded", Mods}, {"with errors", Errs}]),
            {error, Errs}
    end.

update_erts_lib_path(Name) ->
    BasePath = filename:join(lists:droplast(filename:split(code:lib_dir(stdlib)))),
    EbinWildcard = gen_cluster_util:to_str(filename:flatten(filename:join([BasePath, "*", "ebin"]))),
    Ebins = filelib:wildcard(EbinWildcard),
    Bs = lists:sort([create_bundle_from_path(Ebin) || Ebin <- Ebins]),
    case lists:keysearch(gen_cluster_util:to_str(Name), 1, Bs) of
        {value, Bundle} ->
            case update_code_paths(BasePath, [Bundle]) of
                [UpdateRes] -> {ok, UpdateRes};
                [] -> ok
            end;
        false ->
            {error, bad_name}
    end.

-spec update_code_paths() -> {'ok', [{string(), file:filename()}]}.
update_code_paths() ->
    BasePath = code:lib_dir(),
    EbinWildcard = gen_cluster_util:to_str(filename:flatten(filename:join([BasePath, "*", "ebin"]))),
    Ebins = filelib:wildcard(EbinWildcard),
    Bs = lists:ukeysort(1, lists:sort([create_bundle_from_path(Ebin) || Ebin <- Ebins])),
    {ok, update_code_paths(BasePath, Bs)}.
update_code_paths(BasePath, [{_, _, FullName}=Bundle | Rest]) ->
    Path = gen_cluster_util:to_str(filename:flatten(filename:join([BasePath, FullName, "ebin"]))),
    %% split dir name into app name and vsn
    {NameStr, [$- | VsnStr]} =
        case string:chr(FullName, $-) of
            0 ->
                {FullName, "-"};
            FirstDashIdx ->
                lists:split(FirstDashIdx-1, FullName)
        end,
    %% try to convert to atom in order to call lib_dir. atom should already exist if app is already loaded
    try list_to_existing_atom(NameStr) of
        Name ->
            case code:lib_dir(Name) of
                {error, bad_name} ->
                    true = code:add_pathz(Path),
                    [{NameStr, VsnStr} | update_code_paths(BasePath, Rest)];
                Path ->
                    update_code_paths(BasePath, Rest);
                ExistingPath ->
                    case filelib:is_dir(ExistingPath) andalso
                        create_bundle(filename:basename(ExistingPath)) of
                        ExistingBundle when is_tuple(ExistingBundle), ExistingBundle >= Bundle ->
                            update_code_paths(BasePath, Rest);
                        _ ->
                            true = code:replace_path(Name, Path),
                            [{NameStr, VsnStr} | update_code_paths(BasePath, Rest)]
                    end
            end
    catch
        _:badarg ->
            true = code:add_pathz(Path),
            [{NameStr, VsnStr} | update_code_paths(BasePath, Rest)]
    end;
update_code_paths(_, []) ->
    [].

create_bundle_from_path(Path) ->
    Basename = gen_cluster_util:to_str(filename:basename(Path)),
    UseBasename = case Basename of
                      "ebin" ->
                          gen_cluster_util:to_str(filename:basename(filename:dirname(Path)));
                      _ ->
                          Basename
                  end,
    create_bundle(UseBasename).
    
%% modified from kernel/code_server.erl
create_bundle(FullName) ->
    case split_base(FullName) of
	{Name, VsnStr} ->
	    case vsn_to_num(VsnStr) of
		{ok, VsnNum} ->
		    {Name,VsnNum,FullName};
		false ->
		    {FullName,[0],FullName}
	    end;
	_ ->
	    {FullName,[0],FullName}
    end.

split_base(BaseName) ->
    case string:rchr(BaseName, $-) of
        0 ->
            BaseName;
	LastDashIdx ->
	    {Name, [$- | Vsn]} = lists:split(LastDashIdx-1, BaseName),
            {Name, Vsn}
    end.
vsn_to_num(Vsn) ->
    Vsns = string:tokens(Vsn, "."),
    case lists:all(fun is_numstr/1, Vsns) of
	true ->
	    {ok, [list_to_integer(S) || S <- Vsns]};
	_  ->
	    false
    end.
is_numstr(Cs) ->
    lists:all(fun (C) when $0 =< C, C =< $9 -> true; 
		  (_)                       -> false
	      end, Cs).

