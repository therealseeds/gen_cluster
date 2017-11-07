-module(gen_cluster_alarm_handler).
-behaviour(gen_event).

%% api
-export([start/0]).

%% gen_event
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

%%
%% api
%%

start() ->
    case lists:member(?MODULE, gen_event:which_handlers(alarm_handler)) of
        false ->
            gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {?MODULE, undefined});
        true ->
            ok
    end.

%%
%% gen_event
%%

init(_Args) ->
    net_kernel:monitor_nodes(true, [nodedown_reason]),
    erlang:system_monitor(self(), [{long_gc, 50},
                                   {long_schedule, 50},
                                   busy_port, busy_dist_port]),
    {ok, nostate}.

handle_call(_Request, State) ->
    {ok, {error, unhandled}, State}.

%% memsup

handle_event({set_alarm, {system_memory_high_watermark, []}}, State) ->
    {Total, Alloc, _Worst} = memsup:get_memory_data(),
    TotalMB = Total div (1024*1024),
    AllocMB = Alloc div (1024*1024),
    AllocPct = round((Alloc / Total) * 100),
    error_logger:warning_msg("system high memory usage: ~BMB/~BMB (~B%)", [TotalMB, AllocMB, AllocPct]),
    {ok, State};
handle_event({set_alarm, {process_memory_high_watermark, Pid}}, State) ->
    {Total, _Alloc, Worst} = memsup:get_memory_data(),
    TotalMB = Total div (1024*1024),
    WorstMB = Worst div (1024*1024),
    WorstPct = round((Worst / Total) * 100),
    error_logger:warning_msg("process high memory usage: ~p: ~BMB/~BMB (~B%)", [Pid, TotalMB, WorstMB, WorstPct]),
    {ok, State};

%%
%% disksup
%%

handle_event({set_alarm, {{disk_almost_full, MountedOn}, []}}, State) ->
    case lists:keyfind(MountedOn, 1, disksup:get_disk_data()) of
        {MountedOn, TotalKB, UsedPct} ->
            TotalMB = TotalKB div 1024,
            UsedMB = TotalKB * UsedPct div (100 * 1024),
            error_logger:warning_msg("disk almost full: ~s: ~BMB/~BMB (~B%)", [MountedOn, TotalMB, UsedMB, UsedPct]);
        false ->
            error_logger:warning_msg("disk almost full: ~s", [MountedOn])
    end,
    {ok, State};

%%
%% mnesia
%%

handle_event({mnesia_system_event, {mnesia_up, _Node}=Event}, State) ->
    error_logger:info_report([Event]),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_down, _Node}=Event}, State) ->
    error_logger:info_report([Event]),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_checkpoint_activated, _Checkpoint}}, State) ->
    {ok, State};
handle_event({mnesia_system_event, {mnesia_checkpoint_deactivated, _Checkpoint}}, State) ->
    {ok, State};
handle_event({mnesia_system_event, {mnesia_overload, _Details}=Event}, State) ->
    error_logger:info_report([Event]),
    {ok, State};
handle_event({mnesia_system_event, {inconsistent_database, Reason, Node}}, State) ->
    error_logger:info_report([{inconsistent_database, Reason}, {node, Node}]),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_fatal, Format, Args, _BinCore}}, State) ->
    error_logger:error_msg("mnesia_fatal: "++Format, Args),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_error, Format, Args}}, State) ->
    error_logger:error_msg("mnesia_error: "++Format, Args),
    {ok, State};
handle_event({mnesia_system_event, {mnesia_info, _Format, _Args}}, State) ->
    {ok, State};
handle_event({mnesia_system_event, _MnesiaEvent}=Event, State) ->
    error_logger:warning_report([Event]),
    {ok, State};
handle_event({mnesia_table_event, _}, State) ->
    {ok, State};
handle_event(Event, State) ->
    error_logger:warning_report([Event]),
    {ok, State}.

%%
%% net_kernel:monitor_nodes
%%

handle_info({nodeup, _Node, _InfoList}, State) ->
    {ok, State};
handle_info({nodedown, Node, InfoList}, State) ->
    case lists:keyfind(nodedown_reason, 1, InfoList) of
        {nodedown_reason, Reason} ->
            error_logger:warning_report([{nodedown, Node}, {reason, Reason}]);
        false ->
            ok
    end,
    {ok, State};

%%
%% erlang:system_monitor
%%

handle_info({monitor, GcPid, long_gc, Info}, State) ->
    error_logger:warning_report([{long_gc, Info}, {pid, GcPid}]),
    {ok, State};
handle_info({monitor, PidOrPort, long_schedule, Info}, State) ->
    error_logger:warning_report([{long_schedule, Info}, {pid, PidOrPort}]),
    {ok, State};
handle_info({monitor, SusPid, busy_port, Port}, State) ->
    error_logger:warning_report([{busy_port, Port}, {pid, SusPid}]),
    {ok, State};
handle_info({monitor, _SusPid, busy_dist_port, Port}, State) ->
    error_logger:warning_report([{busy_dist_port, Port}]),
    {ok, State};

handle_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
