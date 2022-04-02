-module(prop_exercises).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% with error
prop_set_union() ->
    ?FORALL({ListA, ListB},
            {list(number()), list(number())},
            begin
                SetA = sets:from_list(ListA),
                SetB = sets:from_list(ListB),
                ModelUnion = lists:sort(ListA ++ ListB),
                lists:sort(sets:to_list(sets:union(SetA, SetB))) =:=
                    ModelUnion
            end).

prop_set_union_fixed() ->
    ?FORALL({ListA, ListB},
            {list(number()), list(number())},
            begin
                SetA = sets:from_list(ListA),
                SetB = sets:from_list(ListB),
                ModelUnion = lists:usort(ListA ++ ListB),
                lists:sort(sets:to_list(sets:union(SetA, SetB))) =:=
                    ModelUnion
            end).
