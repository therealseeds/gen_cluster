-module(gen_cluster_server).
-behaviour(gen_server).

-callback dispatch_call(Request :: any(),
                        From :: any(),
                        Group :: gen_client:group_name()) -> _Ignore.

-callback dispatch_cast(Msg :: any(),
                        Group :: gen_client:group_name()) -> _Ignore.

%% public api
-export([start_link/3,
         child_spec/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type opts() :: #{
            island => pos_integer(),
            role => gen_cluster_client:role()
           }.
-record(state, {
          group :: gen_cluster_client:group_name(),
          mod :: atom(),
          mod_state :: any()
         }).

%%
%% public api
%%

-spec start_link(Name :: gen_cluster_client:group_name(),
                 Mod :: atom(),
                 Opts :: [{atom(), any()}] | opts()) -> Res when
      Res :: {ok, Pid :: pid()} |
             {error, {already_started, Pid :: pid()}} |
             {error, Reason :: any()}.
                        
start_link(Name, Mod, Opts) when is_list(Opts) ->
    start_link(Name, Mod, maps:from_list(Opts));
start_link(Name, Mod, Opts) ->
    Group = {Name, maps:get(island, Opts, 1), maps:get(role, Opts, primary)},
    gen_server:start_link({via, gen_cluster_client, Group}, ?MODULE, {Group, Mod}, []).
    
child_spec(Name, Mod, Opts) ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, [Name, Mod, Opts]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [?MODULE]}.

%%
%% gen_server callbacks
%%

init({Group, Mod}) ->
    error_logger:info_msg(?MODULE_STRING" ~p starting on node ~p", [Group, node()]),
    State = #state{
               group = Group,
               mod = Mod
              },
    {ok, State}.

handle_call(Request, From, State) ->
    try
        (State#state.mod):dispatch_call(Request, From, State#state.group)
    catch
        throw:Err -> exit(Err)
    end,
    {noreply, State}.

handle_cast(Msg, State) ->
    try
        (State#state.mod):dispatch_cast(Msg, State#state.group)
    catch
        throw:Err -> exit(Err)
    end,
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    error_logger:info_msg(?MODULE_STRING" ~p stopping on node ~p: ~p", [State#state.group, node(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
