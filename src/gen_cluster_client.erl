-module(gen_cluster_client).

%% public api: gen_server equivalents without monitors
-export([call/2, call/3, cast/2]).

%% OTP gen_* process registry callbacks
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).

-type role() :: primary | fallback.
-type service_name() :: atom().
-type island_name() :: {Service :: service_name(), IslandNo :: pos_integer()}.
-type group_name() :: {Service :: service_name(), IslandNo :: pos_integer(), Role :: role()}.
-export_type([role/0, service_name/0, island_name/0, group_name/0]).

-define(DEFAULT_TIMEOUT, 5000).

%%
%% public api: gen_server equivalents without monitors
%%

-spec call(ToIsland :: island_name(), Msg :: any()) -> Reply :: any().
call(ToIsland, Msg) ->
    call(ToIsland, Msg, ?DEFAULT_TIMEOUT).

-spec call(ToIsland, Msg, TimeoutMs) -> Reply :: any() when
      ToIsland :: island_name(),
      Msg :: any(),
      TimeoutMs :: non_neg_integer().
call(ToIsland, Msg, Timeout) ->
    Tag = make_ref(),
    case whereis_name(ToIsland) of
	Pid when is_pid(Pid) ->
	    Pid ! {'$gen_call', {self(), Tag}, Msg};
	undefined ->
	    exit({noproc, {?MODULE, call, [ToIsland, Msg]}})
    end,
    receive
        {Tag, Reply} ->
            Reply
    after Timeout ->
            exit({timeout, {?MODULE, call, [ToIsland, Msg]}})
    end.

-spec cast(ToIsland :: island_name(), Msg :: any()) -> ok.
cast(ToIsland, Msg) ->
    gen_server:cast({via, ?MODULE, ToIsland}, Msg).

%%
%% OTP gen_* process registry callbacks
%%

-spec register_name(Group :: group_name(), Pid :: pid()) -> yes | no.
register_name({Service, IslandNo, Role}=Group, Pid) ->
    try register(Service, Pid) of
        true ->
            try
                ok = pg2:create(Group),
                ok = pg2:join(Group, Pid),
                error_logger:info_msg("node ~s pid ~p joined service ~s island ~B as ~s",
                                      [node(), Pid, Service, IslandNo, Role]),
                yes
            catch
                _:Pg2Err ->
                    unregister(Service),
                    exit(Pg2Err)
            end
    catch
        error:_ ->
            no
    end.

-spec unregister_name(Group :: group_name()) -> ok.
unregister_name({Service, IslandNo, Role}=Group) ->
    %% try our best to leave, but pg2 will auto-remove this process when it exits anyway
    Pid = whereis(Service),
    pg2:leave(Group, Pid),
    catch unregister(Service),
    error_logger:info_msg("node ~s pid ~p left service ~s island ~B as ~s",
                          [node(), Pid, Service, IslandNo, Role]),
    ok.

-spec whereis_name(Island :: island_name()) -> pid() | undefined;
                  (Group :: group_name()) -> pid() | undefined.
whereis_name({Service, IslandNo}) ->
    case pg2:get_members({Service, IslandNo, primary}) of
        [_|_]=Pids ->
            get_closest_pid(Pids);
        _ ->
            case pg2:get_members({Service, IslandNo, fallback}) of
                [_|_]=Pids ->
                    get_closest_pid(Pids);
                _ ->
                    undefined
            end
    end;
whereis_name({_, _, _}=Group) ->
    case pg2:get_members(Group) of
        [_|_]=Pids ->
            get_closest_pid(Pids);
        _ ->
            undefined
    end.

-spec send(Group :: group_name(), Msg :: any()) -> Msg :: any().
send(Group, Msg) ->
    case whereis_name(Group) of
        undefined -> Msg;
        Pid -> Pid ! Msg
    end.

%%
%% private
%%

get_closest_pid([Pid | _]) when node(Pid) =:= node() ->
    Pid;
get_closest_pid([Pid | []]) ->
    Pid;
get_closest_pid([_ | Rest]) ->
    get_closest_pid(Rest).
