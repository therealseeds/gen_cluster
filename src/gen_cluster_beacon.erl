-module(gen_cluster_beacon).

-behaviour(gen_server).

-define(PING_INTERVAL, 60000).

%% api
-export([child_spec/0, start_link/0]).

%% gen_server
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-record(state, {
	  last_ping = 0 :: non_neg_integer()
	 }).

child_spec() ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, []},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [?MODULE]}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%
%% gen_server
%%

init([]) ->
    case net_adm:host_file() of
        {error, enoent} -> error_logger:warning_report("~/.hosts.erlang file is missing");
	_ -> ok
    end,
    State = do_ping(#state{}),
    {ok, State, get_timeout(State)}.

handle_call(_Req, _From, State) ->
    {reply, {error, bad_call}, State, get_timeout(State)}.

handle_cast(_Req, State) ->
    {noreply, State, get_timeout(State)}.

handle_info(timeout, State) ->
    NewState = do_ping(State),
    {noreply, NewState, get_timeout(NewState)};

handle_info(_Req, State) ->
    {noreply, State, get_timeout(State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% private stuff
%%

do_ping(State) ->
    try net_adm:world() of
	_Nodes -> ok
    catch
	exit:{error, enoent} -> ok;
	exit:{error, HostsErr} ->
            error_logger:warning_report({"~/.hosts.erlang file error!!!", HostsErr})
    end,
    State#state{last_ping = erlang:monotonic_time(millisecond)}.

get_timeout(State) ->
    get_timeout(State, erlang:monotonic_time(millisecond)).

get_timeout(#state{last_ping = LastTs}, Now) when LastTs + ?PING_INTERVAL > Now ->
    LastTs + ?PING_INTERVAL - Now;
get_timeout(_State, _Now) ->
    infinity.
