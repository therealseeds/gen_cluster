-module(gen_cluster_sup).

-behaviour(supervisor).

%% api
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%
%% api
%%

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%
%% supervisor callbacks
%%

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Mods = [gen_cluster_beacon],
    ChildSpecs = [Mod:child_spec() || Mod <- Mods],
    {ok, { {one_for_one, 5, 1}, ChildSpecs} }.
