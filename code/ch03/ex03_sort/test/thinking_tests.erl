-module(thinking_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

last_test() ->
    ?assert((-23 =:= lists:last([-23]))),
    ?assert((5 =:= lists:last([1, 2, 3, 4, 5]))),
    ?assert((3 =:= lists:last([5, 4, 3]))).

-endif.
