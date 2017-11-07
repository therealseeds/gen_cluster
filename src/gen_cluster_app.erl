-module(gen_cluster_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%
%% application callbacks
%%

start(_StartType, _StartArgs) ->
    gen_cluster_alarm_handler:start(),
    gen_cluster_sup:start_link().

stop(_State) ->
    ok.
