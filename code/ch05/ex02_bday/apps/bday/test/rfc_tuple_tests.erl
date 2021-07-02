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
    Expected = [[{"\"aaa\"", "zzz"}, {"\"b\r\nbb\"", "yyy"}, {"\"ccc\"", "xxx"}]],
    Result = bday_csv_tuple:decode("\"aaa\",\"b\r\nbb\",\"ccc\"\r\nzzz,yyy,xxx"),
    ?assertEqual(Expected, Result).

rfc_double_quote_escape_test() ->
    %% Since we decided headers are mandatory, this test adds a line
    %% with empty values (CLRF,,) to the example from the RFC.
    Expected = [[{"\"aaa\"", ""}, {"\"b\"bb\"", ""}, {"\"ccc\"", ""}]],
    Result = bday_csv_tuple:decode("\"aaa\",\"b\"\"bb\",\"ccc\"\r\n,,"),

    ?assertEqual(Expected, Result).

%% @doc this counterexample is taken literally from the RFC
dupe_keys_unsupported_test() ->
    CSV = "field_name,field_name,field_name\r\n"
          "aaa,bbb,ccc\r\n"
          "zzz,yyy,xxx\r\n",
    Result = bday_csv_tuple:decode(CSV),
    List = lists:flatten(Result),
    ?assertEqual(6, length(List)),
    lists:foreach(fun(Elem) -> ?assertMatch({"field_name", _}, Elem) end, List).

-endif.
