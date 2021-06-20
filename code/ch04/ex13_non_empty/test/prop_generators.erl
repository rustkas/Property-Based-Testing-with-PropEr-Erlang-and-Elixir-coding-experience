-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_non_empty1()->
?FORALL(List, non_empty(list(non_neg_integer())), 
 		lists:all(fun(Elem)-> is_integer(Elem) end, List)).

prop_non_empty2()->
?FORALL(List, non_empty1(list(bitstring(64))), 
 		lists:all(fun(Elem)-> is_bitstring(Elem) end, List)).

prop_non_empty3()->
?FORALL(List, non_empty(list(oneof([non_neg_integer(),integer(),bitstring(64)]))), 
 		lists:all(fun(Elem)-> is_integer(Elem) or is_bitstring(Elem) end, List)).

prop_non_empty_map()->
?FORALL(List, non_empty(list(map(non_neg_integer(),float()))), 
 		lists:all(fun(Elem)-> is_map(Elem) end, List)).

prop_non_empty_list()->
?FORALL(List, non_empty(list(non_neg_integer())), 
 		lists:all(fun(Elem)-> is_integer(Elem) end, List)).

prop_non_empty_string()->
?FORALL(List, non_empty(list(string())), 
 		lists:all(fun(Elem)-> is_list(Elem) end, List)).
		
prop_non_bitstring()->
?FORALL(List, non_empty(list(bitstring(64))), 
 		lists:all(fun(Elem)-> is_bitstring(Elem) end, List)).		

prop_non_tuple1()->
?FORALL(List, non_empty(list(tuple())), 
 		lists:all(fun(Elem)-> is_tuple(Elem) end, List)).				

prop_non_tuple2()->
?FORALL(List, non_empty(list(tuple([integer(), string()]))), 
 		lists:all(fun(Elem)-> is_tuple(Elem) end, List)).						
		
%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%
non_empty1(ListOrBinGenerator) ->
         ?SUCHTHAT(L, ListOrBinGenerator, L =/= [] andalso L =/= <<>>).

non_empty_map(Gen) ->
     ?SUCHTHAT(G, Gen, G =/= #{}).

non_empty_list(Gen) ->
     ?SUCHTHAT(L, Gen, L =/= []).

non_empty_string(Gen) ->
     ?SUCHTHAT(S, Gen, S =/= "").
	 
non_empty_bitstring(Gen) ->
     ?SUCHTHAT(B, Gen, B =/= <<>>).

non_empty_tuple(Gen) ->
     ?SUCHTHAT(T, Gen, T =/= {}).
