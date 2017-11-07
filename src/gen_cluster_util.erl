-module(gen_cluster_util).

-export([flatten_iolist/1,
         to_str/1,
         to_bin/1,
         to_num/1,
         trunc_bin/2,
         from_hex/1,
         to_hex/1]).

-spec flatten_iolist(iodata()) -> string().
flatten_iolist(<<Bin/binary>>) ->
    binary_to_list(Bin);
flatten_iolist(Iolist) ->
    lists:reverse(flatten_iolist(Iolist, [])).
flatten_iolist([BinOrList | Rest], Done)
  when is_binary(BinOrList);
       is_list(BinOrList) ->
    flatten_iolist(Rest, flatten_iolist(BinOrList, Done));
flatten_iolist([C | Rest], Done) ->
    flatten_iolist(Rest, [C | Done]);
flatten_iolist(<<Bin/binary>>, Done) ->
    flatten_iolist([lists:reverse(binary_to_list(Bin)) | Done]);
flatten_iolist([], Done) ->
    Done.

-spec to_str(iodata() | atom()) -> string().
to_str(Str) when is_list(Str) ->
    flatten_iolist(Str);
to_str(<<Bin/binary>>) ->
    binary_to_list(Bin);
to_str(Atom) when is_atom(Atom) ->
    atom_to_list(Atom).

-spec to_bin(iodata() | atom()) -> binary().
to_bin(<<Bin/binary>>) ->
    Bin;
to_bin(Str) when is_list(Str) ->
    iolist_to_binary(Str);
to_bin(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).

-spec to_num(string:chardata() | number()) -> number().
to_num(Num) when is_number(Num) ->
    Num;
to_num(<<BinVal/binary>>) ->
    to_num(binary_to_list(BinVal));
to_num(StrVal) when is_list(StrVal) ->
    case catch string:to_float(StrVal) of
        {NumVal, []} when is_float(NumVal) ->
            NumVal;
        {error, no_float} ->
            case catch string:to_integer(StrVal) of
                {NumVal, []} when is_integer(NumVal) ->
                    NumVal;
                _ ->
                    exit(badarg)
            end;
        _ ->
            exit(badarg)
    end;
to_num(_) ->
    exit(badarg).

-spec trunc_bin(binary(), non_neg_integer()) -> binary().
trunc_bin(<<Bin/binary>>, MaxSize) ->
    case Bin of
        <<TruncBin:MaxSize/binary, _/binary>> -> TruncBin;
        _ -> Bin
    end.

-spec to_hex([byte()] | binary()) -> binary().
to_hex(Str) when is_list(Str) ->
    << <<(nibble_to_hex(Byte div 16#10)), (nibble_to_hex(Byte rem 16#10))>> || Byte <- Str >>;
to_hex(<<Bin/binary>>) ->
    << <<(nibble_to_hex(N))>> || <<N:4/integer>> <= Bin >>.

-spec from_hex(iodata()) -> binary().
from_hex(<<Bin/binary>>) when size(Bin) rem 2 == 0 ->
    << <<(binary_to_integer(B, 16))>> || <<B:2/bytes>> <= Bin >>;
from_hex(Str) when not is_binary(Str) ->
    from_hex(to_bin(Str)).

nibble_to_hex(N) when N >= 16#A, N =< 16#F ->
    $a + N - 16#A;
nibble_to_hex(N) when N >= 0, N =< 9 ->
    $0 + N.
