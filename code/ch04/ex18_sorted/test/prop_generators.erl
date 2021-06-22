-module(prop_generators).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_mostly_sorted() ->
    ?FORALL(String, mostly_sorted(),
        begin
		    io:format("~p~n",[String]),
            true
        end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
mostly_sorted() ->
    ?LET(Lists,
         list(frequency([
             {5, sorted_list()},
             {1, list()}
         ])),
                    lists:append(Lists)).

sorted_list() ->
           ?LET(L, list(), lists:sort(L)).
