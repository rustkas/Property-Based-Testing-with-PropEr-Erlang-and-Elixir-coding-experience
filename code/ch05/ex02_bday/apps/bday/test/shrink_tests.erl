-module(shrink_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

shrink_01_test0()->
	Expected = [{"\n", ""}, {"", ""}, {"", ""}],
    Text = "\"\n\",,\r\n,,",
    [Result] = bday_csv_tuple_regex:decode(Text),
    ?assertEqual(Expected, Result).

shrink_02_test()->
	Expected = [{",\r\n,\"", "\""}],
    Text = ",\r\n,\"\r\n\"",
    [Result] = bday_csv_tuple_regex:decode(Text),
    ?assertEqual(Expected, Result).
-endif.
