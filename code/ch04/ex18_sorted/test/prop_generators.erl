-module(prop_generators).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_mostly_sorted() ->
    ?FORALL(String, mostly_sorted(),
        begin
		    %io:format("~p~n",[String]),
			true = is_list(String),
           lists:last(lists:sort(String)) =:= biggest(String)

        end).

prop_sorted_list() ->
    ?FORALL(String, sorted_list(),
        begin
		    %io:format("~p~n",[String]),
			true = is_list(String),
           lists:last(lists:sort(String)) =:= biggest(String)

        end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
biggest([Head | Tail]) -> biggest(Tail, Head).

biggest([], Biggest) -> Biggest;
biggest([Head | Tail], Biggest) when Head >= Biggest ->
    biggest(Tail, Head);
biggest([Head | Tail], Biggest) when Head < Biggest ->
    biggest(Tail, Biggest).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
mostly_sorted() ->
    ?LET(Lists,
         non_empty(list(frequency([
             {5, sorted_list()},
             {1, list()}
         ]))),
                    lists:append(Lists)).

sorted_list() ->
           ?LET(L, non_empty(list()), lists:sort(L)).
