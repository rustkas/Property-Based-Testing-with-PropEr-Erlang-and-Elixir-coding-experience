-module(prop_base).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Type, (term()), begin boolean(Type) end).

prop_biggest() ->
    ?FORALL(List, (non_empty(list(integer()))),
            begin
                lists:last(lists:sort(List)) =:= biggest(List)
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
boolean(_) -> true.

biggest([Head | Tail]) -> biggest(Tail, Head).

biggest([], Biggest) -> Biggest;
biggest([Head | Tail], Biggest) when Head >= Biggest ->
    biggest(Tail, Head);
biggest([Head | Tail], Biggest) when Head < Biggest ->
    biggest(Tail, Biggest).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
mytype() -> term().
