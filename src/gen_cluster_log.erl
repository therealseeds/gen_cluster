-module(gen_cluster_log).

-export([build_logs/1]).
-export_type([param/0]).

-type param() :: {atom(), any()} | atom().

-spec build_logs([param()]) -> {string(), [any()]}.
build_logs(Logs) ->
    try
        build_logs(Logs, [], [])
    catch
        _:Err ->
            {"crashed in "?MODULE_STRING": ~1000p: ~1000p", [Err, erlang:get_stacktrace()]}
    end.

build_logs([{Atom, Str} | Rest], Fmt, Params)
  when is_atom(Atom) orelse is_binary(Atom),
       is_atom(Str) orelse is_binary(Str) orelse is_list(Str) ->
    case is_printable(Str) of
        true ->
            build_logs(Rest, ["~s [~s]" | Fmt], [Str, Atom | Params]);
        false ->
            build_logs(Rest, ["~s {~s}" | Fmt], [gen_cluster_util:to_hex(gen_cluster_util:to_bin(Str)), Atom | Params])
    end;
build_logs([{Atom, Tuple} | Rest], Fmt, Params)
  when is_atom(Atom), is_tuple(Tuple) ->
    build_logs(Rest, ["~s [~1000p]" | Fmt], [Tuple, Atom | Params]);
build_logs([{Atom, Num} | Rest], Fmt, Params)
  when is_atom(Atom), is_number(Num) ->
    build_logs(Rest, ["~s [~w]" | Fmt], [Num, Atom | Params]);
build_logs([{Atom, Value} | Rest], Fmt, Params)
  when is_atom(Atom) ->
    build_logs(Rest, ["~s {~1000p}" | Fmt], [Value, Atom | Params]);
build_logs([Atom | Rest], Fmt, Params)
  when is_atom(Atom) ->
    build_logs(Rest, ["~s [1]" | Fmt], [Atom | Params]);
build_logs([Value | Rest], Fmt, Params) ->
    %% careful about this recursion!
    build_logs([{'_', Value} | Rest], Fmt, Params);
build_logs([], Fmt, Params) ->
    {string:join(lists:reverse(Fmt), " "), lists:reverse(Params)}.

is_printable(Atom) when is_atom(Atom) ->
    true;
is_printable([C | Rest]) when C >= $ , C =< $~, C /= $[, C /= $] ->
    is_printable(Rest);
is_printable(<< C, BinRest/binary >>) when C >= $ , C =< $~, C /= $[, C /= $] ->
    is_printable(BinRest);
is_printable([Str | Rest]) when is_list(Str); is_binary(Str) ->
    is_printable(Str) andalso is_printable(Rest);
is_printable(<<>>) ->
    true;
is_printable([]) ->
    true;
is_printable(_Bad) ->
    false.
