-module(prop_thinking).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_same_size() ->
    ?FORALL(L, list(number()),
        length(L) =:= length(lists:sort(L))).

prop_no_added() ->
    ?FORALL(L, list(number()),
        begin
            Sorted  = lists:sort(L),
            lists:all(fun(Element) -> lists:member(Element, L) end, Sorted)
        end).

prop_no_removed() ->
    ?FORALL(L, list(number()),
        begin
            Sorted  = lists:sort(L),
            lists:all(fun(Element) -> lists:member(Element, Sorted) end, L)
        end).


prop_biggest() ->
    ?FORALL(List, (non_empty(list(integer()))),
            begin
                model_biggest(List) =:= thinking:biggest(List)
            end).

prop_last() ->
    %% pick a list and a last number
    ?FORALL({List, KnownLast}, {list(number()), number()},
            begin
                KnownList = List ++
                                [KnownLast], % known number appended to list
                KnownLast =:=
                    lists:last(KnownList) % known last number is found
            end).

prop_sort() ->
       ?FORALL(List, list(term()),
              is_ordered(lists:sort(List))).

prop_keysort1() ->
    ?FORALL(List, list({term(),term()}),
        begin
            is_key_ordered(lists:keysort(1, List))
        end).

prop_keysort2() ->
    ?FORALL(List, non_empty(list(non_empty(list()))),
        begin
            Tuples = [list_to_tuple(L) || L <- List],
            Pos = lists:min([tuple_size(T) || T <- Tuples]),
            Sorted = lists:keysort(Pos, Tuples),
            Keys = extract_keys(Pos, Sorted),
            Keys == lists:sort(Keys)
        end).

prop_symmetric1() ->
    ?FORALL(Data, list({atom(), any()}),
        Data =:= decode1(encode1(Data))).

prop_symmetric2() ->
    ?FORALL(Data, list({atom(), any()}),
        begin
            Encoded = encode2(Data),
            is_binary(Encoded) andalso Data =:= decode2(Encoded)
        end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
model_biggest(List) -> lists:last(lists:sort(List)).

is_ordered([A,B|T]) ->
       A =< B andalso is_ordered([B|T]);
is_ordered(_) -> % lists with fewer than 2 elements
       true.

is_key_ordered([{A,_},{B,_}=BTuple|T]) ->
    A =< B andalso is_key_ordered([BTuple|T]);
is_key_ordered(_) -> true.

extract_keys(Pos, List) -> 
    [element(Pos, T) || T <- List].

encode1(T) -> T.
decode1(T) -> T.

encode2(T) -> term_to_binary(T).
decode2(T) -> binary_to_term(T).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%%% nothing!