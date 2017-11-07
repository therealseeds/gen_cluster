-module(gen_cluster_mnesia).

-export([write/3,
         read/3,
         delete/3,
         select/2,
         select/3,
         select/4,
         select_page/5,
         frag_names/1]).

-export([create_table/4,
         join/1]).

-type access_context() :: transaction | {transaction, Retries :: pos_integer()} |
                          sync_transaction | {sync_transaction, Retries :: pos_integer()} |
                          async_dirty | sync_dirty | ets.

-spec write(Tab :: atom(),
            Record :: tuple(),
            AccessContext :: access_context()) -> Res when
      Res :: ok.
write(Tab, Record, AccessContext) ->
    mnesia:activity(AccessContext, fun mnesia:write/3, [Tab, Record, write], mnesia_frag).

-spec read(Tab :: atom(),
           Key :: any(),
           AccessContext :: access_context()) -> Res when
      Res :: any().
read(Tab, Key, AccessContext) ->
    mnesia:activity(AccessContext, fun mnesia:read/2, [Tab, Key], mnesia_frag).

-spec delete(Tab :: atom(),
             Key :: any(),
             AccessContext :: access_context()) -> ok.
delete(Tab, Key, AccessContext) ->
    mnesia:activity(AccessContext, fun mnesia:delete/3, [Tab, Key, write], mnesia_frag).

-spec select(Cont :: any(),
             AccessContext :: access_context()) -> Res when
      Res :: {[Object :: any()], NewCont :: any()} | '$end_of_table'.
select(Cont, AccessContext) ->
    mnesia:activity(AccessContext, fun mnesia:select/1, [Cont], mnesia_frag).

-spec select(Tab :: atom(),
             MatchSpec :: ets:match_spec(),
             AccessContext :: access_context()) -> any().
select(Tab, MatchSpec, AccessContext) ->
    mnesia:activity(AccessContext, fun mnesia:select/3, [Tab, MatchSpec, read], mnesia_frag).

-spec select(Tab :: atom(),
             MatchSpec :: ets:match_spec(),
             NObjects :: pos_integer(),
             AccessContext :: access_context()) -> Res when
      Res :: {[Object :: any()], Cont :: any()} | '$end_of_table'.
select(Tab, MatchSpec, NObjects, AccessContext) ->
    mnesia:activity(AccessContext, fun mnesia:select/4, [Tab, MatchSpec, NObjects, read], mnesia_frag).

-spec select_page(Tab :: atom(),
                  MatchSpec :: ets:match_spec(),
                  Count :: pos_integer(),
                  Start :: pos_integer(),
                  AccessContext :: access_context()) -> Res when
      Res :: [any()].
select_page(Tab, MatchSpec, Count, Start, AccessContext) when Start > 0, Count > 0 ->
    case select(Tab, MatchSpec, Count, AccessContext) of
        {Res, Cont} ->
            ResLen = length(Res),
            NewRes = lists:sublist(Res, min(Start, 1 + ResLen), Count),
            select_page_cont(Cont, Count - length(NewRes), max(1, Start - ResLen), AccessContext, [NewRes]);
        '$end_of_table' -> []
    end.

select_page_cont(Cont, Count, Start, AccessContext, Acc) when Count > 0 ->
    case select(Cont, AccessContext) of
        {Res, NewCont} ->
            ResLen = length(Res),
            NewRes = lists:sublist(Res, min(Start, 1 + ResLen), Count),
            select_page_cont(NewCont, Count - length(NewRes), max(1, Start - ResLen), AccessContext, [NewRes | Acc]);
        '$end_of_table' ->
            select_page_cont(undefined, 0, Start, AccessContext, Acc)
    end;
select_page_cont(_Cont, _Count, _Start, _AccessContext, Acc) ->
    lists:append(lists:reverse(Acc)).

-spec frag_names(Tab :: atom()) -> [atom()].
frag_names(Tab) ->
    mnesia:activity(async_dirty, fun mnesia:table_info/2, [Tab, frag_names], mnesia_frag).

-spec create_table(Tab :: atom(),
                   NFrags :: pos_integer(),
                   Fields :: [atom()],
                   Type :: disc_copies | ram_copies) -> Res when
      Res :: {atomic, ok} | {aborted, any()}.
create_table(Tab, NFrags, Fields, disc_copies) ->
    FragProps = [{n_fragments, NFrags},
                 {n_disc_copies, 1}],
    mnesia:create_table(Tab, [{attributes, Fields},
                              {disc_copies, [node()]},
                              {frag_properties, FragProps}]);
create_table(Tab, NFrags, Fields, ram_copies) ->
    FragProps = [{n_fragments, NFrags}],
    mnesia:create_table(Tab, [{attributes, Fields},
                              {frag_properties, FragProps}]).

-spec join(atom()) -> [{atomic, ok}].
join(Master) ->
    ok = mnesia:start(),
    %% {ok, _} = mnesia:change_config(extra_db_nodes, [Master]),
    ok = mnesia:set_master_nodes([Master]),
    %% disc_copies schema allows node to rejoin and download even ram tables without another join/1
    {atomic, ok} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [{atomic, ok} = mnesia:add_table_copy(Tab, node(), disc_copies) || Tab <- Tabs].
