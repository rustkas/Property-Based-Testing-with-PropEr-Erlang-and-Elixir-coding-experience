-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_queue_naive() ->
    ?FORALL(List,
            list({term(), term()}),
            begin
                Queue = queue:from_list(List),
                queue:is_queue(Queue)
            end).

prop_queue_nicer() ->
    ?FORALL(Q, queue(), queue:is_queue(Q)).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
queue() ->
    ?LET(List, list({term(), term()}), queue:from_list(List)).
