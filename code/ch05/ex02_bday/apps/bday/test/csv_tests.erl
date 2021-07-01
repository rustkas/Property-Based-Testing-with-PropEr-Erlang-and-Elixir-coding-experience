-module(csv_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% @doc One-column CSV files are inherently ambiguous due to
%% trailing CRLF in RFC 4180. This bug is expected
one_column_bug_test() ->
    ?assertEqual("\r\n\r\n", bday_csv:encode([#{"" => ""}, #{"" => ""}])),
    ?assertEqual([#{"" => ""}], bday_csv:decode("\r\n\r\n")).

-endif.
