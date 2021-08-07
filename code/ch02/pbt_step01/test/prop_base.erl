-module(prop_base).

-include_lib("proper/include/proper.hrl").
-import(base,[biggest/1]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Type, (term()), begin boolean(Type) end).

prop_biggest() ->
    ?FORALL(List, (list(integer())),
            begin
                biggest(List) =:= lists:last(lists:sort(List))
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
boolean(_) -> true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
% mytype() -> term().
