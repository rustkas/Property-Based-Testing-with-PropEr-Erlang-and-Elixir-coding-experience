-module(base_test).

-import(base,[biggest/1]).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

biggest_test() ->
    ?assert((5 =:= biggest([1, 2, 3, 4, 5]))),
    ?assert((8 =:= biggest([3, 8, 7, -1]))),
    ?assert((0 =:= biggest([0]))),
    ?assert((-5 =:= biggest([-10, -5, -901]))).

-endif.
