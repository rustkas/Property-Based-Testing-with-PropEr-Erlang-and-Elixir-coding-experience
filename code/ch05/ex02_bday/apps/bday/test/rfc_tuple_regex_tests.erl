-module(rfc_tuple_regex_tests).

%-define(RESEARCH, true).
%-define(CHECK_SPLIT, true).


%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").



-ifdef(RESEARCH).

research_test() ->
    Text = "\"aaa\",\"b\r\nbb\",\"ccc\"\r\nzzz,yyy,xxx",
    
    %[Result] = bday_csv_tuple_regex:decode(Text),
    ?debugFmt("~p~n", [Result]).

rfc_record_per_line_test() ->
    Expected = [{"aaa", "zzz"}, {"bbb", "yyy"}, {"ccc", "xxx"}],
    Text = "aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n",
    [Result] = bday_csv_tuple_regex:decode(Text),
    ?assertEqual(Expected, Result).

rfc_optional_trailing_crlf_test() ->
    Expected = [{"aaa", "zzz"}, {"bbb", "yyy"}, {"ccc", "xxx"}],
    Text = "aaa,bbb,ccc\r\nzzz,yyy,xxx",
    [Result] = bday_csv_tuple_regex:decode(Text),
    ?assertEqual(Expected, Result).

rfc_double_quote_test() ->
    Expected = [{"aaa", "zzz"}, {"bbb", "yyy"}, {"ccc", "xxx"}],
    Text = "\"aaa\",\"bbb\",\"ccc\"\r\nzzz,yyy,xxx",
    [Result] = bday_csv_tuple_regex:decode(Text),
    ?assertEqual(Expected, Result).

-else.

rfc_crlf_escape_test() ->
    Expected = [{"aaa", "zzz"}, {"\"b\r\nbb\"", "yyy"}, {"ccc", "xxx"}],
    Text = "\"aaa\",\"b\r\nbb\",\"ccc\"\r\nzzz,yyy,xxx",
	[Result] = bday_csv_tuple_regex:decode(Text),
    ?assertEqual(Expected, Result).

rfc_double_quote_escape_test() ->
    %% Since we decided headers are mandatory, this test adds a line
    %% with empty values (CLRF,,) to the example from the RFC.
    Expected = [{"aaa", ""}, {"b\"bb", ""}, {"ccc", ""}],
    Text = "\"aaa\",\"b\"\"bb\",\"ccc\"\r\n,,",

	[Result] = bday_csv_tuple_regex:decode(Text),
    ?assertEqual(Expected, Result).

%% @doc this counterexample is taken literally from the RFC
dupe_keys_test() ->
    CSV = "field_name,field_name,field_name\r\n"
          "aaa,bbb,ccc\r\n"
          "zzz,yyy,xxx\r\n",
    Result = bday_csv_tuple_regex:decode(CSV),
    List = lists:flatten(Result),
		
	%?debugFmt("Result = ~p~n",[Result]),
	%?debugFmt("Length = ~p~n",[Length]).
	
    ?assertEqual(6, length(List)),
    lists:foreach(fun(Elem) -> ?assertMatch({"field_name", _}, Elem) end, List).

-endif.
-endif.
