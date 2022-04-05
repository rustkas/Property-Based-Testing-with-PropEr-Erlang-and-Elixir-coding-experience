-module(prop_exercises).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_dict_merge1() ->
        ?FORALL({ListA, ListB}, {non_empty(list({integer(), integer()})), non_empty(list({integer(), integer()}))},
                  begin
            Merged = dict:merge(fun(_Key, V1, _V2) -> V1 end,
                                  dict:from_list(ListA),
                                  dict:from_list(ListB)),
            MergedAsList = dict:to_list(Merged),
            lists:usort(extract_keys(ListA ++ ListB))
             =:=
            extract_keys(lists:sort(MergedAsList))
        
         end).


 extract_keys(List) -> 
         [K || {K,_} <- List].


