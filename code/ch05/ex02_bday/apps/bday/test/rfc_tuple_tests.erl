-module(rfc_tuple_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

rfc_record_per_line_test() ->
    Expected = [[{"aaa", "zzz"}, {"bbb", "yyy"}, {"ccc", "xxx"}]],
    Result = bday_csv_tuple:decode("aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n"),
    ?assertEqual(Expected, Result).

rfc_optional_trailing_crlf_test() ->
    Expected = [[{"aaa", "zzz"}, {"bbb", "yyy"}, {"ccc", "xxx"}]],
    Result = bday_csv_tuple:decode("aaa,bbb,ccc\r\nzzz,yyy,xxx"),
    ?assertEqual(Expected, Result).

rfc_double_quote_test() ->
    Expected = [[{"\"aaa\"", "zzz"}, {"\"bbb\"", "yyy"}, {"\"ccc\"", "xxx"}]],
    Result = bday_csv_tuple:decode("\"aaa\",\"bbb\",\"ccc\"\r\nzzz,yyy,xxx"),
    ?assertEqual(Expected, Result).


rfc_crlf_escape_test() ->
    Expected = [[{"\"aaa\"", "zzz"}, {"\"b\r\nbb\"","yyy"}, {"\"ccc\"", "xxx"}]],
	Result = bday_csv_tuple:decode("\"aaa\",\"b\r\nbb\",\"ccc\"\r\nzzz,yyy,xxx"),
	?assertEqual(Expected, Result).

-endif.
