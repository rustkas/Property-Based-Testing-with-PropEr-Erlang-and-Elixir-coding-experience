-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_aggregate() ->
    Suits = [club, diamond, heart, spade],
    ?FORALL(Hand, (vector(10, {oneof(Suits), choose(1, 10)})), 
             begin 
			 io:format("~p~n", [Hand]),
			 aggregate(Hand, true)
			 end). % `true' makes it always pass
