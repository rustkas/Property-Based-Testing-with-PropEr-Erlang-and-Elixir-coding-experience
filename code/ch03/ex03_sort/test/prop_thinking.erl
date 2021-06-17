-module(prop_thinking).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
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
 

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
model_biggest(List) -> lists:last(lists:sort(List)).

is_ordered([A,B|T]) ->
       A =< B andalso is_ordered([B|T]);
is_ordered(_) -> % lists with fewer than 2 elements
       true.